#!/usr/bin/env bun
/**
 * HTTPClientActor Tests
 *
 * Comprehensive tests covering:
 * - Method validation (allowed/denied)
 * - Host validation (allowed/denied)
 * - Rate limiting
 * - Timeout handling
 * - Error scenarios (network errors, 4xx/5xx)
 */

import { describe, test, expect, beforeEach, mock } from 'bun:test';
import { HTTPClientActor } from '../http-client.ts';
import { MessageRouter } from '../../messaging/router.ts';
import { GraphStore } from '../../graph.ts';
import { address, createMessage } from '@agentic-primer/actors';

describe('HTTPClientActor', () => {
  let router: MessageRouter;
  let store: GraphStore;
  let httpActor: HTTPClientActor;

  beforeEach(() => {
    store = new GraphStore();
    router = new MessageRouter(store);

    httpActor = new HTTPClientActor('http-test', router, {
      methods: ['GET', 'POST', 'PUT'],
      allowedHosts: ['jsonplaceholder.typicode.com', 'httpbin.org', 'example.com'],
      rateLimit: { requests: 10, window: 1000 }, // 10 req/sec
      timeout: 5000,
    });

    router.registerActor('/system/http', httpActor);
  });

  describe('Method Validation', () => {
    test('allows configured GET method', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/get' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
      expect(response.payload.status).toBe(200);
    }, { from: address('test') });

    test('allows configured POST method', async () => {
      const message = createMessage(address('/system/http'), 'http.post', {
          url: 'https://httpbin.org/post',
          body: { test: 'data' },
        }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
      expect(response.payload.status).toBe(200);
    });

    test('allows configured PUT method', async () => {
      const message = createMessage(address('/system/http'), 'http.put', {
          url: 'https://httpbin.org/put',
          body: { test: 'data' },
        }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
      expect(response.payload.status).toBe(200);
    });

    test('denies non-configured DELETE method', async () => {
      const message = createMessage(address('/system/http'), 'http.delete', { url: 'https://httpbin.org/delete' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain("Method 'DELETE' not in allowedMethods");
      expect(response.error).toContain('GET, POST, PUT');
    }, { from: address('test') });

    test('denies non-configured PATCH method', async () => {
      const message = createMessage(address('/system/http'), 'http.patch', { url: 'https://httpbin.org/patch' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain("Method 'PATCH' not in allowedMethods");
    }, { from: address('test') });
  });

  describe('Host Validation', () => {
    test('allows whitelisted host', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/get' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
    }, { from: address('test') });

    test('denies non-whitelisted host', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://evil.com/steal-data' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain("Host 'evil.com' not in allowedHosts");
      expect(response.error).toContain('jsonplaceholder.typicode.com');
      expect(response.error).toContain('httpbin.org');
    }, { from: address('test') });

    test('handles subdomain correctly', async () => {
      // Subdomain NOT in allowedHosts should be denied
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://api.httpbin.org/get' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain("Host 'api.httpbin.org' not in allowedHosts");
    }, { from: address('test') });

    test('handles IP address correctly', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'http://127.0.0.1:8080/test' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain("Host '127.0.0.1' not in allowedHosts");
    }, { from: address('test') });

    test('rejects invalid URL', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'not-a-valid-url' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('Invalid URL');
    }, { from: address('test') });
  });

  describe('Rate Limiting', () => {
    test('allows requests within rate limit', async () => {
      httpActor.resetRateLimit();

      // Make 5 requests (under limit of 10)
      for (let i = 0; i < 5; i++) {
        const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/get' }, { from: address('test') });

        const response = await httpActor.receive(message);
        expect(response.success).toBe(true);
      }

      const status = httpActor.getRateLimitStatus();
      expect(status.current).toBe(5);
      expect(status.max).toBe(10);
    }, { from: address('test') });

    test('denies requests exceeding rate limit', async () => {
      httpActor.resetRateLimit();

      // Make 10 requests (at limit)
      for (let i = 0; i < 10; i++) {
        const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/get' }, { from: address('test') });

        await httpActor.receive(message);
      }

      // 11th request should be denied
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/get' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('Rate limit exceeded');
      expect(response.error).toContain('max 10 requests per 1000ms');
    }, { from: address('test') });

    test('rate limit resets after window', async () => {
      httpActor.resetRateLimit();

      // Make 10 requests (at limit)
      for (let i = 0; i < 10; i++) {
        const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/get' }, { from: address('test') });

        await httpActor.receive(message);
      }

      // Wait for window to expire
      await new Promise(resolve => setTimeout(resolve, 1100));

      // Should be allowed again
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/get' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
    }, { from: address('test') });
  });

  describe('Timeout Handling', () => {
    test('completes normal request within timeout', async () => {
      const message = createMessage(address('/system/http'), 'http.get', {
          url: 'https://httpbin.org/delay/1', // 1 second delay
          timeout: 3000, // 3 second timeout
        }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
    }, { from: address('test') });

    test('timeouts slow request', async () => {
      const message = createMessage(address('/system/http'), 'http.get', {
          url: 'https://httpbin.org/delay/10', // 10 second delay
          timeout: 1000, // 1 second timeout
        }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('Request timeout after 1000ms');
    }, { from: address('test') });

    test('uses default timeout when not specified', async () => {
      // Create actor with short default timeout
      const shortTimeoutActor = new HTTPClientActor('http-short', router, {
        methods: ['GET'],
        allowedHosts: ['httpbin.org'],
        rateLimit: { requests: 10, window: 1000 },
        timeout: 500, // 500ms default
      });

      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/delay/2' } // No timeout specified
      );

      const response = await shortTimeoutActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('Request timeout after 500ms');
    }, { from: address('test') });
  });

  describe('Response Handling', () => {
    test('parses JSON response', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/json' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
      expect(response.payload.status).toBe(200);
      expect(response.payload.body).toBeDefined();
      expect(typeof response.payload.body).toBe('object');
    }, { from: address('test') });

    test('parses text response', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/html' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
      expect(response.payload.status).toBe(200);
      expect(typeof response.payload.body).toBe('string');
    }, { from: address('test') });

    test('includes response headers', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/get' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
      expect(response.payload.headers).toBeDefined();
      expect(response.payload.headers['content-type']).toContain('application/json');
    }, { from: address('test') });

    test('handles POST with request body', async () => {
      const requestBody = { name: 'test', value: 123 };

      const message = createMessage(address('/system/http'), 'http.post', {
          url: 'https://httpbin.org/post',
          headers: { 'Content-Type': 'application/json' },
          body: requestBody,
        }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
      expect(response.payload.status).toBe(200);
      expect(response.payload.body.json).toEqual(requestBody);
    });

    test('handles custom headers', async () => {
      const message = createMessage(address('/system/http'), 'http.get', {
          url: 'https://httpbin.org/headers',
          headers: {
            'X-Custom-Header': 'test-value',
            'Authorization': 'Bearer token123',
          },
        }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
      expect(response.payload.body.headers['X-Custom-Header']).toBe('test-value');
      expect(response.payload.body.headers['Authorization']).toBe('Bearer token123');
    });
  });

  describe('Error Scenarios', () => {
    test('handles HTTP 404 error', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/status/404' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('HTTP 404');
    }, { from: address('test') });

    test('handles HTTP 500 error', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/status/500' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('HTTP 500');
    }, { from: address('test') });

    test('handles HTTP 403 error', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/status/403' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('HTTP 403');
    }, { from: address('test') });

    test('handles unknown message type', async () => {
      const message = createMessage(address('/system/http'), 'http.unknown', { url: 'https://httpbin.org/get' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('Unknown message type: http.unknown');
    }, { from: address('test') });

    test('handles network connection failure gracefully', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://example.com:9999/nonexistent' }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(false);
      // Should contain error message (connection refused, timeout, etc)
      expect(response.error).toBeDefined();
    }, { from: address('test') });
  });

  describe('Edge Cases', () => {
    test('handles empty response body', async () => {
      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/status/204' } // 204 No Content
      );

      const response = await httpActor.receive(message);
      // 204 should fail because it's not in 2xx success range that returns content
      // Actually 204 is successful but has no content
      expect(response.success).toBe(true);
      expect(response.payload.status).toBe(204);
    }, { from: address('test') });

    test('handles PUT request', async () => {
      const message = createMessage(address('/system/http'), 'http.put', {
          url: 'https://httpbin.org/put',
          body: { updated: true },
        }, { from: address('test') });

      const response = await httpActor.receive(message);
      expect(response.success).toBe(true);
      expect(response.payload.status).toBe(200);
    });

    test('rate limit status reflects current count', async () => {
      httpActor.resetRateLimit();

      expect(httpActor.getRateLimitStatus().current).toBe(0);

      const message = createMessage(address('/system/http'), 'http.get', { url: 'https://httpbin.org/get' }, { from: address('test') });

      await httpActor.receive(message);
      expect(httpActor.getRateLimitStatus().current).toBe(1);

      await httpActor.receive(message);
      expect(httpActor.getRateLimitStatus().current).toBe(2);
    }, { from: address('test') });
  });
});
