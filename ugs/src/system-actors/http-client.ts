#!/usr/bin/env bun
/**
 * HTTPClientActor - HTTP client with host whitelisting and rate limiting
 *
 * Pure actor model: HTTPClientActor IS the capability.
 * Access control happens through routing, not helper objects.
 *
 * Features:
 * - Host whitelisting (allowedHosts)
 * - Method restrictions (GET, POST, PUT, DELETE, PATCH)
 * - Rate limiting (sliding window)
 * - Request timeout enforcement
 * - Automatic JSON/text response parsing
 */

import { Actor, createResponse, createErrorResponse } from '@agentic-primer/actors';
import type { Message, MessageResponse, MessageRouter } from '@agentic-primer/actors';

/**
 * HTTP methods
 */
export type HTTPMethod = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'PATCH';

/**
 * HTTPClientActor configuration
 */
export interface HTTPClientConfig {
  /** Allowed HTTP methods */
  methods: HTTPMethod[];

  /** Allowed hosts (whitelist) */
  allowedHosts: string[];

  /** Rate limiting configuration */
  rateLimit: {
    requests: number; // Number of requests
    window: number;   // Time window in milliseconds
  };

  /** Default request timeout in milliseconds */
  timeout: number;
}

/**
 * HTTP response
 */
export interface HTTPResponse {
  status: number;
  headers: Record<string, string>;
  body?: any;
}

/**
 * Rate limiter using sliding window algorithm
 */
class RateLimiter {
  private requests: number[] = [];

  constructor(
    public config: { requests: number; window: number }
  ) {}

  tryAcquire(): boolean {
    const now = Date.now();
    const windowStart = now - this.config.window;

    // Remove old requests outside window
    this.requests = this.requests.filter(time => time > windowStart);

    // Check if under limit
    if (this.requests.length < this.config.requests) {
      this.requests.push(now);
      return true;
    }

    return false;
  }

  reset(): void {
    this.requests = [];
  }

  getCurrentCount(): number {
    const now = Date.now();
    const windowStart = now - this.config.window;
    this.requests = this.requests.filter(time => time > windowStart);
    return this.requests.length;
  }
}

/**
 * HTTPClientActor - Provides HTTP client functionality with internal validation
 *
 * @example
 * ```typescript
 * const http = new HTTPClientActor('http', router, {
 *   methods: ['GET', 'POST'],
 *   allowedHosts: ['api.anthropic.com', 'api.github.com'],
 *   rateLimit: { requests: 100, window: 60000 },
 *   timeout: 30000
 * });
 * router.registerActor('/workflows/system/http', http);
 *
 * // Make request
 * const response = await actor.ask(
 *   address('/workflows/system/http'),
 *   'http.post',
 *   {
 *     url: 'https://api.anthropic.com/v1/messages',
 *     headers: { 'x-api-key': apiKey },
 *     body: { model: 'claude-sonnet-4.5', messages: [...] }
 *   }
 * );
 * ```
 */
export class HTTPClientActor extends Actor {
  private allowedHosts: Set<string>;
  private allowedMethods: Set<HTTPMethod>;
  private rateLimiter: RateLimiter;
  private defaultTimeout: number;

  constructor(id: string, router: MessageRouter, config: HTTPClientConfig) {
    super(id, router);
    this.allowedHosts = new Set(config.allowedHosts);
    this.allowedMethods = new Set(config.methods);
    this.rateLimiter = new RateLimiter(config.rateLimit);
    this.defaultTimeout = config.timeout;
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      // Route to handler based on message type
      if (type === 'http.get') {
        return await this.handleRequest(message, 'GET', payload);
      }

      if (type === 'http.post') {
        return await this.handleRequest(message, 'POST', payload);
      }

      if (type === 'http.put') {
        return await this.handleRequest(message, 'PUT', payload);
      }

      if (type === 'http.delete') {
        return await this.handleRequest(message, 'DELETE', payload);
      }

      if (type === 'http.patch') {
        return await this.handleRequest(message, 'PATCH', payload);
      }

      return createErrorResponse(message, `Unknown message type: ${type}`);
    } catch (error: any) {
      this.logError('HTTP request failed', {
        type,
        url: payload?.url,
        error: error.message,
      });
      return createErrorResponse(message, error.message);
    }
  }

  private async handleRequest(
    message: Message,
    method: HTTPMethod,
    payload: {
      url: string;
      headers?: Record<string, string>;
      body?: any;
      timeout?: number;
    }
  ): Promise<MessageResponse> {
    // 1. Validate method
    if (!this.allowedMethods.has(method)) {
      return createErrorResponse(
        message,
        `Method '${method}' not in allowedMethods: [${Array.from(this.allowedMethods).join(', ')}]`
      );
    }

    // 2. Validate host
    const host = this.extractHost(payload.url);
    if (!this.allowedHosts.has(host)) {
      return createErrorResponse(
        message,
        `Host '${host}' not in allowedHosts: [${Array.from(this.allowedHosts).join(', ')}]`
      );
    }

    // 3. Check rate limit
    if (!this.rateLimiter.tryAcquire()) {
      return createErrorResponse(
        message,
        `Rate limit exceeded: max ${this.rateLimiter.config.requests} requests per ${this.rateLimiter.config.window}ms`
      );
    }

    // 4. Execute request with timeout
    const timeout = payload.timeout ?? this.defaultTimeout;
    const response = await this.executeWithTimeout(
      method,
      payload.url,
      payload.headers,
      payload.body,
      timeout
    );

    return createResponse(message, response);
  }

  private async executeWithTimeout(
    method: HTTPMethod,
    url: string,
    headers: Record<string, string> = {},
    body?: any,
    timeout: number = this.defaultTimeout
  ): Promise<HTTPResponse> {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeout);

    try {
      const response = await fetch(url, {
        method,
        headers,
        body: body ? JSON.stringify(body) : undefined,
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      // Parse response body
      const contentType = response.headers.get('content-type');
      let responseBody: any;

      if (contentType?.includes('application/json')) {
        responseBody = await response.json();
      } else if (response.status !== 204 && response.headers.get('content-length') !== '0') {
        responseBody = await response.text();
      }

      // Check for HTTP errors (4xx, 5xx)
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      return {
        status: response.status,
        headers: Object.fromEntries(response.headers.entries()),
        body: responseBody,
      };
    } catch (error: any) {
      clearTimeout(timeoutId);

      if (error.name === 'AbortError') {
        throw new Error(`Request timeout after ${timeout}ms`);
      }

      throw error;
    }
  }

  /**
   * Extract hostname from URL
   */
  private extractHost(url: string): string {
    try {
      const parsed = new URL(url);
      return parsed.hostname;
    } catch {
      throw new Error(`Invalid URL: ${url}`);
    }
  }

  /**
   * Get current rate limit status (for testing/debugging)
   */
  getRateLimitStatus(): { current: number; max: number; window: number } {
    return {
      current: this.rateLimiter.getCurrentCount(),
      max: this.rateLimiter.config.requests,
      window: this.rateLimiter.config.window,
    };
  }

  /**
   * Reset rate limiter (for testing)
   */
  resetRateLimit(): void {
    this.rateLimiter.reset();
  }
}
