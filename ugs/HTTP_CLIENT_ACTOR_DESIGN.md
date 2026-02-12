# HTTPClientActor Design (Pure Actor Model)

**Date:** 2026-02-07
**Status:** Design Phase
**Branch:** feature/path-addressing
**Bead:** simplify-net.1

---

## Overview

HTTPClientActor is a **system actor** that provides HTTP client functionality with internal host whitelisting, method restrictions, rate limiting, and timeout enforcement. Following the pure actor model, HTTPClientActor IS the capability - access control happens through routing, not helper objects.

**Core Principle:** Capabilities are actors. Access control = routing decisions.

---

## Pure Actor Model Architecture

### What This Means

**HTTPClientActor is NOT a wrapper:**
```typescript
// ❌ OLD: Helper class approach
class HTTPCapability {
  async get(url: string) {
    this.validateHost(url); // Validation in helper
    return await this.router.ask(httpActor, 'http.get', { url });
  }
}

// ✅ NEW: Pure actor approach
class HTTPClientActor extends Actor {
  async receive(message: Message) {
    if (message.type === 'http.get') {
      const host = this.extractHost(message.payload.url);
      if (!this.allowedHosts.has(host)) {
        return createErrorResponse(message, 'Host not allowed');
      }
      // Execute HTTP request
    }
  }
}

// Access control = routing
router.registerActor('/workflows/system/http', workflowsHttp);
// NOT registered for /domain → access denied by absence
```

---

## Design Principles

### 1. System Actors ARE Capabilities

HTTPClientActor is the HTTP capability. Configuration happens at construction.

```typescript
const workflowsHttp = new HTTPClientActor('workflows-http', router, {
  methods: ['GET', 'POST', 'PUT'],
  allowedHosts: ['api.anthropic.com', 'api.github.com'],
  rateLimit: { requests: 100, window: 60000 }, // 100 req/min
  timeout: 30000 // 30 seconds
});
```

### 2. Routing Determines Access

If an actor path is registered, access is granted. If not registered, access is implicitly denied.

```typescript
// Workflows can make HTTP requests
router.registerActor('/workflows/system/http', workflowsHttp);

// Session knowledge can make HTTP requests (different config)
router.registerActor('/session-knowledge/system/http', knowledgeHttp);

// No HTTP for /domain → messages fail with "Actor not found"
```

### 3. Internal Validation

HTTPClientActor validates every message against its configuration before execution.

```typescript
async receive(message: Message): Promise<MessageResponse> {
  if (message.type === 'http.post') {
    // Check method allowed
    if (!this.allowedMethods.has('POST')) {
      return createErrorResponse(message, 'POST method not allowed');
    }

    // Check host allowed
    const host = this.extractHost(message.payload.url);
    if (!this.allowedHosts.has(host)) {
      return createErrorResponse(message,
        `Host '${host}' not in allowedHosts: [${Array.from(this.allowedHosts).join(', ')}]`
      );
    }

    // Check rate limit
    if (!this.rateLimiter.tryAcquire()) {
      return createErrorResponse(message, 'Rate limit exceeded');
    }

    // Execute request with timeout
    const response = await this.executeRequest(message.payload);
    return createSuccessResponse(message, response);
  }
}
```

### 4. Message-Based Protocol

All HTTP operations are messages. No direct method calls.

```typescript
// GET request
await actor.ask(address('/workflows/system/http'), 'http.get', {
  url: 'https://api.github.com/repos/owner/repo',
  headers: { 'Authorization': 'token xyz' }
});

// POST request
await actor.ask(address('/workflows/system/http'), 'http.post', {
  url: 'https://api.anthropic.com/v1/messages',
  headers: { 'x-api-key': apiKey, 'content-type': 'application/json' },
  body: { model: 'claude-sonnet-4.5', messages: [...] },
  timeout: 30000
});

// PUT request
await actor.ask(address('/workflows/system/http'), 'http.put', {
  url: 'https://api.github.com/repos/owner/repo/issues/1',
  headers: { 'Authorization': 'token xyz' },
  body: { state: 'closed' }
});

// DELETE request
await actor.ask(address('/workflows/system/http'), 'http.delete', {
  url: 'https://api.example.com/resource/123',
  headers: { 'Authorization': 'Bearer xyz' }
});
```

---

## HTTPClientActor Interface

### Constructor

```typescript
export class HTTPClientActor extends Actor {
  private allowedHosts: Set<string>;
  private allowedMethods: Set<HTTPMethod>;
  private rateLimiter: RateLimiter;
  private defaultTimeout: number;

  constructor(
    id: string,
    router: MessageRouter,
    config: HTTPClientConfig
  ) {
    super(id, router);
    this.allowedHosts = new Set(config.allowedHosts);
    this.allowedMethods = new Set(config.methods);
    this.rateLimiter = new RateLimiter(config.rateLimit);
    this.defaultTimeout = config.timeout;
  }
}
```

### Configuration

```typescript
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

export type HTTPMethod = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'PATCH';
```

### Message Protocol

**http.get** (GET request)
```typescript
{
  type: 'http.get',
  payload: {
    url: 'https://api.github.com/repos/owner/repo',
    headers?: Record<string, string>,
    timeout?: number
  }
}

// Response
{
  success: true,
  payload: {
    status: 200,
    headers: { 'content-type': 'application/json', ... },
    body: { id: 123, name: 'repo', ... }
  }
}
```

**http.post** (POST request)
```typescript
{
  type: 'http.post',
  payload: {
    url: 'https://api.anthropic.com/v1/messages',
    headers: { 'x-api-key': 'xyz', 'content-type': 'application/json' },
    body: { model: 'claude-sonnet-4.5', messages: [...] },
    timeout?: number
  }
}

// Response
{
  success: true,
  payload: {
    status: 200,
    headers: { ... },
    body: { id: 'msg_abc', content: [...], ... }
  }
}
```

**http.put** (PUT request)
```typescript
{
  type: 'http.put',
  payload: {
    url: 'https://api.example.com/resource/123',
    headers: { 'authorization': 'Bearer xyz' },
    body: { status: 'updated' },
    timeout?: number
  }
}

// Response
{
  success: true,
  payload: {
    status: 200,
    headers: { ... },
    body: { ... }
  }
}
```

**http.delete** (DELETE request)
```typescript
{
  type: 'http.delete',
  payload: {
    url: 'https://api.example.com/resource/123',
    headers: { 'authorization': 'Bearer xyz' },
    timeout?: number
  }
}

// Response
{
  success: true,
  payload: {
    status: 204,
    headers: { ... }
  }
}
```

---

## Implementation

### Core Logic

```typescript
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

      return createErrorResponse(message, `Unknown message type: ${type}`);
    } catch (error: any) {
      this.logError('HTTP request failed', {
        type,
        url: payload.url,
        error: error.message
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
      return createErrorResponse(message,
        `Method '${method}' not in allowedMethods: [${Array.from(this.allowedMethods).join(', ')}]`
      );
    }

    // 2. Validate host
    const host = this.extractHost(payload.url);
    if (!this.allowedHosts.has(host)) {
      return createErrorResponse(message,
        `Host '${host}' not in allowedHosts: [${Array.from(this.allowedHosts).join(', ')}]`
      );
    }

    // 3. Check rate limit
    if (!this.rateLimiter.tryAcquire()) {
      return createErrorResponse(message,
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

    return createSuccessResponse(message, response);
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
        signal: controller.signal
      });

      clearTimeout(timeoutId);

      // Parse response body
      const contentType = response.headers.get('content-type');
      let responseBody: any;

      if (contentType?.includes('application/json')) {
        responseBody = await response.json();
      } else {
        responseBody = await response.text();
      }

      // Check for HTTP errors
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      return {
        status: response.status,
        headers: Object.fromEntries(response.headers.entries()),
        body: responseBody
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
}

/**
 * Rate limiter using sliding window
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
}

interface HTTPResponse {
  status: number;
  headers: Record<string, string>;
  body?: any;
}
```

---

## Usage Examples

### Setup: Register HTTPClientActors

```typescript
const router = new MessageRouter(store, programManager);

// Workflows namespace HTTP (full access)
const workflowsHttp = new HTTPClientActor('workflows-http', router, {
  methods: ['GET', 'POST', 'PUT', 'DELETE'],
  allowedHosts: ['api.anthropic.com', 'api.github.com', 'api.openai.com'],
  rateLimit: { requests: 100, window: 60000 }, // 100 req/min
  timeout: 30000
});
router.registerActor('/workflows/system/http', workflowsHttp);

// Session knowledge HTTP (embeddings only)
const knowledgeHttp = new HTTPClientActor('knowledge-http', router, {
  methods: ['POST'],
  allowedHosts: ['api.cloudflare.com'], // Only Cloudflare Workers AI
  rateLimit: { requests: 50, window: 60000 }, // 50 req/min
  timeout: 10000
});
router.registerActor('/session-knowledge/system/http', knowledgeHttp);

// Domain namespace: NO HTTP ACCESS (not registered)
```

### Actors Use HTTPClientActor via Messages

```typescript
class WorkflowActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'create-github-issue') {
      // Send message to HTTPClientActor
      const response = await this.ask(
        address('/workflows/system/http'),
        'http.post',
        {
          url: 'https://api.github.com/repos/owner/repo/issues',
          headers: {
            'Authorization': `token ${process.env.GITHUB_TOKEN}`,
            'Content-Type': 'application/json'
          },
          body: {
            title: 'New issue',
            body: 'Description'
          }
        }
      );

      if (!response.success) {
        return createErrorResponse(message, response.error);
      }

      return createSuccessResponse(message, {
        issue: response.payload.body
      });
    }
  }
}
```

### Access Control Through Routing

```typescript
// ✅ Workflows actor CAN make HTTP requests
const workflowActor = new WorkflowActor('workflow', router);
router.registerActor('/workflows/orchestrator', workflowActor);

await workflowActor.ask(
  address('/workflows/system/http'),
  'http.get',
  { url: 'https://api.github.com/user' }
);

// ❌ Domain actor CANNOT make HTTP requests (not registered)
const domainActor = new Actor('domain', router);
router.registerActor('/domain/logic', domainActor);

await domainActor.ask(
  address('/domain/system/http'), // Not registered
  'http.get',
  { url: 'https://api.github.com/user' }
);
// Error: Actor not found at /domain/system/http
```

---

## Error Handling

### Access Denied Errors

```typescript
// Method not allowed
{
  success: false,
  error: "Method 'DELETE' not in allowedMethods: [GET, POST, PUT]"
}

// Host not allowed
{
  success: false,
  error: "Host 'evil.com' not in allowedHosts: [api.anthropic.com, api.github.com]"
}

// Rate limit exceeded
{
  success: false,
  error: "Rate limit exceeded: max 100 requests per 60000ms"
}

// Request timeout
{
  success: false,
  error: "Request timeout after 30000ms"
}

// HTTP error
{
  success: false,
  error: "HTTP 404: Not Found"
}

// Network error
{
  success: false,
  error: "Failed to fetch: Network connection failed"
}
```

### Clear Error Messages

All errors include:
- **What was denied:** Method, host, or resource
- **Why it was denied:** Not in allowedMethods/allowedHosts, rate limit, timeout
- **What is allowed:** List of allowedMethods or allowedHosts

---

## Rate Limiting Implementation

### Sliding Window Algorithm

```typescript
class RateLimiter {
  private requests: number[] = []; // Timestamps

  tryAcquire(): boolean {
    const now = Date.now();
    const windowStart = now - this.config.window;

    // Remove expired requests
    this.requests = this.requests.filter(time => time > windowStart);

    // Check limit
    if (this.requests.length < this.config.requests) {
      this.requests.push(now);
      return true; // Acquired
    }

    return false; // Rate limit exceeded
  }
}
```

**Example:**
- Config: `{ requests: 10, window: 60000 }` (10 req/min)
- Request at t=0: Acquired (1/10)
- Request at t=1000: Acquired (2/10)
- ... 9 more requests ...
- Request at t=5000: Denied (10/10)
- Request at t=61000: Acquired (window reset)

---

## Testing Strategy

### Test Coverage (>20 tests)

1. **Method Validation:**
   - Allow configured methods ✅
   - Deny non-configured methods ❌
   - All method types (GET, POST, PUT, DELETE)

2. **Host Validation:**
   - Allow whitelisted hosts ✅
   - Deny non-whitelisted hosts ❌
   - Subdomain handling
   - IP address handling

3. **Rate Limiting:**
   - Requests within limit ✅
   - Requests exceeding limit ❌
   - Sliding window behavior
   - Rate limit reset

4. **Timeout Handling:**
   - Normal requests complete ✅
   - Slow requests timeout ❌
   - Custom timeout override
   - Timeout cleanup

5. **Error Scenarios:**
   - Network errors (connection refused)
   - HTTP 4xx errors (client errors)
   - HTTP 5xx errors (server errors)
   - Invalid URL format
   - Missing required headers

6. **Response Handling:**
   - JSON response parsing
   - Text response parsing
   - Empty response body
   - Large response bodies

---

## Benefits

### 1. Security by Default
- Host whitelisting prevents unauthorized API access
- Method restrictions limit attack surface
- Rate limiting prevents abuse

### 2. Clear Access Control
- Access = routing registration
- No access = actor not registered
- Explicit and auditable

### 3. Resource Protection
- Rate limiting prevents API quota exhaustion
- Timeout enforcement prevents hanging requests
- Per-namespace configuration

### 4. Testability
- Mock router for unit tests
- Virtual HTTP responses
- No external dependencies in tests

---

## Next Steps

1. ✅ Design approved (this document)
2. Implement HTTPClientActor (simplify-net.2)
3. Create comprehensive tests (simplify-net.3)
4. Document usage patterns
5. Integration examples

---

**Document End**
