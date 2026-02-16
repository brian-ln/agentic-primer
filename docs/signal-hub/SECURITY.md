# Signal Hub Security Model

**Status:** Design Phase - Phase 3
**Last Updated:** 2026-02-16
**Protocol Version:** 0.1.0

---

## Overview

Signal Hub security is built on defense-in-depth principles with multiple layers:

1. **Authentication** - JWT token validation on connection (MVP)
2. **Authorization** - Actor-to-actor permissions and topic ACLs (Phase 2 - Future)
3. **Integrity** - HMAC signatures for message tampering detection (Phase 3 - Future)
4. **Transport** - WSS (WebSocket Secure) encryption
5. **Rate Limiting** - Per-actor throttling and backpressure
6. **Audit** - Connection and message logging

**Security Principle:** Server-enforced identity. Clients cannot spoof `from` addresses.

---

## 1. Authentication Flow (MVP)

### 1.1 Connection with JWT

Signal Hub uses JWT (JSON Web Tokens) for authentication in the `hub:connect` metadata.

```typescript
// Client initiates connection
const connectMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(browser/client-ui)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:connect',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: null,
  metadata: {
    protocolVersion: '0.1.0',
    authToken: 'bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ1c2VyLTEyMyIsImFjdG9ySWQiOiJicm93c2VyL2NsaWVudC11aSIsImV4cCI6MTczOTczMTU1M30.signature',
    capabilities: ['send', 'broadcast', 'subscribe']
  },
  ttl: 5000,
  signature: null
};
```

### 1.2 Server-Side JWT Validation

**Step-by-step validation:**

```typescript
import { decode, verify } from 'jsonwebtoken';

async function validateJWT(authToken: string, jwtSecret: string): Promise<ActorIdentity> {
  // 1. Remove "bearer " prefix
  const token = authToken.replace(/^bearer\s+/i, '');

  // 2. Verify signature and decode
  try {
    const decoded = verify(token, jwtSecret, {
      algorithms: ['HS256', 'RS256'],
      issuer: 'signal-hub',
      maxAge: '24h'
    }) as JWTPayload;

    // 3. Extract actor identity
    return {
      actorId: decoded.actorId,
      userId: decoded.sub,
      capabilities: decoded.capabilities ?? [],
      expiresAt: decoded.exp * 1000
    };
  } catch (err) {
    if (err.name === 'TokenExpiredError') {
      throw new AuthError('TOKEN_EXPIRED', 'JWT token has expired');
    }
    if (err.name === 'JsonWebTokenError') {
      throw new AuthError('INVALID_TOKEN', 'JWT signature invalid');
    }
    throw err;
  }
}

interface JWTPayload {
  sub: string;          // User ID
  actorId: string;      // Actor canonical address path (e.g., "browser/client-ui")
  capabilities: string[];
  iss: string;          // Issuer: "signal-hub"
  exp: number;          // Expiration (Unix timestamp)
  iat: number;          // Issued at
}

interface ActorIdentity {
  actorId: string;
  userId: string;
  capabilities: string[];
  expiresAt: number;
}
```

### 1.3 Server Response with Verified Identity

```typescript
// Server validates JWT and responds
const connectedMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/client-ui)',
  type: 'hub:connected',
  pattern: 'tell',
  correlationId: connectMsg.id,
  timestamp: Date.now(),
  payload: {
    sessionId: 'sess-abc123',
    serverVersion: '0.1.0',
    maxMessageSize: 1048576,
    heartbeatInterval: 25000,
    capabilities: {
      maxActorsPerInstance: 50000,
      supportsBackpressure: true,
      supportedContentTypes: ['json', 'msgpack']
    }
  },
  metadata: {
    actorIdentity: '@(browser/client-ui)',  // Server-verified identity
    tokenExpiresAt: 1739731553000
  },
  ttl: null,
  signature: null
};
```

**Critical:** The server extracts the actor identity from the JWT and stores it in the session. All subsequent messages from this connection have their `from` field **server-enforced** to this verified identity.

### 1.4 Server-Enforced `from` Field

**Security invariant:** Clients cannot spoof message sender addresses.

```typescript
class SignalHubSession {
  private verifiedIdentity: CanonicalAddress;

  constructor(identity: ActorIdentity) {
    this.verifiedIdentity = `@(${identity.actorId})`;
  }

  // All outgoing messages stamped with verified identity
  async sendMessage(msg: SharedMessage): Promise<void> {
    // CRITICAL: Ignore client-provided 'from', use server-verified identity
    const secureMsg: SharedMessage = {
      ...msg,
      from: this.verifiedIdentity  // Server-enforced
    };

    await this.deliverMessage(secureMsg);
  }
}
```

**Attack scenario prevented:**

```typescript
// ❌ Malicious client tries to impersonate another actor
const maliciousMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(admin/privileged-actor)',  // Spoofed address
  to: '@(cloudflare/signal-hub)',
  type: 'hub:broadcast',
  pattern: 'tell',
  correlationId: null,
  timestamp: Date.now(),
  payload: { message: { type: 'system:shutdown', payload: {} } },
  metadata: {},
  ttl: null,
  signature: null
};

// ✅ Server replaces 'from' with verified identity
// Actual message delivered:
// from: '@(browser/client-ui)'  (NOT the spoofed address)
```

### 1.5 Token Refresh and Renewal

**Token lifecycle:**

- **Issue:** JWT issued during user authentication (outside Signal Hub)
- **Expiry:** 24 hours (configurable via `maxAge`)
- **Refresh:** Client must obtain new JWT before expiry

**Refresh strategy:**

```typescript
// Client monitors token expiry
class SignalHubClient {
  private tokenExpiresAt: number;

  private startTokenRefreshTimer() {
    const timeUntilExpiry = this.tokenExpiresAt - Date.now();
    const refreshAt = timeUntilExpiry - (5 * 60 * 1000);  // 5 min before expiry

    setTimeout(async () => {
      const newToken = await this.authService.refreshToken();

      // Reconnect with new token
      await this.disconnect();
      await this.connect(newToken);
    }, refreshAt);
  }
}
```

**Alternative: Refresh without reconnect (Phase 2 - Future)**

```typescript
// New message type: hub:refresh_token
const refreshMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(browser/client-ui)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:refresh_token',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: null,
  metadata: {
    authToken: 'bearer <new-jwt>'
  },
  ttl: 5000,
  signature: null
};

// Server validates new token, updates session
const refreshedMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/client-ui)',
  type: 'hub:token_refreshed',
  pattern: 'tell',
  correlationId: refreshMsg.id,
  timestamp: Date.now(),
  payload: {
    tokenExpiresAt: Date.now() + (24 * 60 * 60 * 1000)
  },
  metadata: {},
  ttl: null,
  signature: null
};
```

### 1.6 Unauthorized Error Response

**When JWT validation fails:**

```typescript
const unauthorizedMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/client-ui)',
  type: 'hub:unauthorized',
  pattern: 'tell',
  correlationId: connectMsg.id,
  timestamp: Date.now(),
  payload: {
    action: 'connect',
    reason: 'Invalid JWT signature'
  },
  metadata: {
    errorCode: 'INVALID_TOKEN',
    retryable: false
  },
  ttl: null,
  signature: null
};
```

**Client behavior:**

- Do NOT retry with same token
- Prompt user to re-authenticate
- Obtain new JWT and reconnect

---

## 2. Authorization Model (Phase 2 - Future)

### 2.1 Actor-to-Actor Permissions

**Capability-based access control (CBAC):**

```typescript
interface ActorPermissions {
  actorId: CanonicalAddress;
  allowedTargets: CanonicalAddress[];  // Empty = allow all
  deniedTargets: CanonicalAddress[];   // Explicit deny
  capabilities: string[];
}

// Example: Actor can only send to specific targets
const permissions: ActorPermissions = {
  actorId: '@(browser/widget-123)',
  allowedTargets: [
    '@(local/coordinator-main)',
    '@(browser/widget-*)'  // Wildcard pattern
  ],
  deniedTargets: [
    '@(admin/*)'  // Cannot send to admin actors
  ],
  capabilities: ['send', 'subscribe']  // Cannot broadcast
};
```

**Server-side enforcement:**

```typescript
async function checkSendPermission(
  from: CanonicalAddress,
  to: CanonicalAddress,
  permissions: ActorPermissions
): Promise<boolean> {
  // 1. Check capabilities
  if (!permissions.capabilities.includes('send')) {
    return false;
  }

  // 2. Check explicit deny
  if (matchesPattern(to, permissions.deniedTargets)) {
    return false;
  }

  // 3. Check allowed targets (empty = allow all)
  if (permissions.allowedTargets.length === 0) {
    return true;
  }

  return matchesPattern(to, permissions.allowedTargets);
}

function matchesPattern(address: string, patterns: string[]): boolean {
  return patterns.some(pattern => {
    const regex = new RegExp('^' + pattern.replace('*', '.*') + '$');
    return regex.test(address);
  });
}
```

**Permission denied response:**

```typescript
const unauthorizedMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/widget-123)',
  type: 'hub:unauthorized',
  pattern: 'tell',
  correlationId: sendMsg.id,
  timestamp: Date.now(),
  payload: {
    action: 'send',
    reason: 'Actor @(browser/widget-123) not authorized to send to @(admin/privileged-actor)'
  },
  metadata: {
    errorCode: 'PERMISSION_DENIED',
    retryable: false
  },
  ttl: null,
  signature: null
};
```

### 2.2 Topic-Level ACLs (Pub/Sub)

**Topic permissions:**

```typescript
interface TopicACL {
  topic: string;
  publishers: CanonicalAddress[];   // Who can publish
  subscribers: CanonicalAddress[];  // Who can subscribe
  publicRead: boolean;              // Anyone can subscribe?
  publicWrite: boolean;             // Anyone can publish?
}

// Example: Private admin topic
const adminTopicACL: TopicACL = {
  topic: 'admin:commands',
  publishers: ['@(admin/*)'],       // Only admin actors
  subscribers: ['@(admin/*)'],      // Only admin actors
  publicRead: false,
  publicWrite: false
};

// Example: Public events topic
const eventsTopicACL: TopicACL = {
  topic: 'events',
  publishers: ['@(*)'],             // Anyone can publish
  subscribers: ['@(*)'],            // Anyone can subscribe
  publicRead: true,
  publicWrite: true
};
```

**Server-side enforcement:**

```typescript
async function checkSubscribePermission(
  actorId: CanonicalAddress,
  topic: string,
  acl: TopicACL
): Promise<boolean> {
  if (acl.publicRead) return true;

  return matchesPattern(actorId, acl.subscribers);
}

async function checkPublishPermission(
  actorId: CanonicalAddress,
  topic: string,
  acl: TopicACL
): Promise<boolean> {
  if (acl.publicWrite) return true;

  return matchesPattern(actorId, acl.publishers);
}
```

### 2.3 Rate Limiting Per Actor

**Per-actor rate limits:**

```typescript
interface RateLimit {
  actorId: CanonicalAddress;
  maxMessagesPerSecond: number;
  maxBroadcastsPerMinute: number;
  maxRegistrations: number;
}

// Example: Standard actor limits
const standardLimit: RateLimit = {
  actorId: '@(*)',  // Default for all actors
  maxMessagesPerSecond: 100,
  maxBroadcastsPerMinute: 10,
  maxRegistrations: 5
};

// Example: Elevated limits for coordinator
const coordinatorLimit: RateLimit = {
  actorId: '@(local/coordinator-*)',
  maxMessagesPerSecond: 1000,
  maxBroadcastsPerMinute: 60,
  maxRegistrations: 100
};
```

**Token bucket implementation:**

```typescript
class TokenBucketRateLimiter {
  private tokens: number;
  private lastRefill: number;
  private maxTokens: number;
  private refillRate: number;  // tokens per second

  constructor(maxTokens: number, refillRate: number) {
    this.tokens = maxTokens;
    this.maxTokens = maxTokens;
    this.refillRate = refillRate;
    this.lastRefill = Date.now();
  }

  async checkLimit(): Promise<{ allowed: boolean; retryAfter?: number }> {
    this.refill();

    if (this.tokens >= 1) {
      this.tokens -= 1;
      return { allowed: true };
    }

    // Calculate retry-after in milliseconds
    const retryAfter = Math.ceil((1 - this.tokens) / this.refillRate * 1000);
    return { allowed: false, retryAfter };
  }

  private refill() {
    const now = Date.now();
    const elapsed = (now - this.lastRefill) / 1000;
    const tokensToAdd = elapsed * this.refillRate;

    this.tokens = Math.min(this.maxTokens, this.tokens + tokensToAdd);
    this.lastRefill = now;
  }
}
```

**Rate limit error response:**

```typescript
const rateLimitedMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/widget-123)',
  type: 'hub:rate_limited',
  pattern: 'tell',
  correlationId: sendMsg.id,
  timestamp: Date.now(),
  payload: {
    retryAfter: 1000  // Milliseconds
  },
  metadata: {
    errorCode: 'RATE_LIMIT_EXCEEDED',
    limit: 100,       // Max messages per second
    window: 1000,     // Window in milliseconds
    retryable: true
  },
  ttl: null,
  signature: null
};
```

**Client-side backoff:**

```typescript
class SignalHubClient {
  async sendWithRateLimit(msg: SharedMessage): Promise<void> {
    try {
      await this.send(msg);
    } catch (err) {
      if (err.type === 'hub:rate_limited') {
        const retryAfter = err.payload.retryAfter;
        await sleep(retryAfter);
        return this.sendWithRateLimit(msg);  // Retry
      }
      throw err;
    }
  }
}
```

### 2.4 Capability-Based Access Control

**Dynamic capabilities in JWT:**

```typescript
interface JWTPayload {
  sub: string;
  actorId: string;
  capabilities: string[];  // ['send', 'broadcast', 'subscribe', 'register', 'discover']
  iss: string;
  exp: number;
  iat: number;
}

// Example JWT with limited capabilities
const limitedJWT = {
  sub: 'user-456',
  actorId: 'browser/widget-123',
  capabilities: ['send', 'subscribe'],  // Cannot broadcast or register
  iss: 'signal-hub',
  exp: Math.floor(Date.now() / 1000) + (24 * 60 * 60),
  iat: Math.floor(Date.now() / 1000)
};
```

**Server enforces capabilities:**

```typescript
function checkCapability(session: SignalHubSession, requiredCap: string): boolean {
  return session.capabilities.includes(requiredCap);
}

// Before processing hub:broadcast
if (!checkCapability(session, 'broadcast')) {
  return {
    type: 'hub:unauthorized',
    payload: {
      action: 'broadcast',
      reason: 'Actor lacks broadcast capability'
    }
  };
}

// Before processing hub:register
if (!checkCapability(session, 'register')) {
  return {
    type: 'hub:unauthorized',
    payload: {
      action: 'register',
      reason: 'Actor lacks register capability'
    }
  };
}
```

---

## 3. HMAC Signatures (Phase 3 - Future)

### 3.1 Message Integrity with HMAC

**Use `SharedMessage.signature` field for tampering detection:**

```typescript
import { createHmac } from 'crypto';

function signMessage(msg: SharedMessage, secret: string): SharedMessage {
  // 1. Create canonical representation (exclude signature field)
  const canonical = JSON.stringify({
    id: msg.id,
    from: msg.from,
    to: msg.to,
    type: msg.type,
    payload: msg.payload,
    pattern: msg.pattern,
    correlationId: msg.correlationId,
    timestamp: msg.timestamp,
    metadata: msg.metadata,
    ttl: msg.ttl
  });

  // 2. Compute HMAC-SHA256
  const hmac = createHmac('sha256', secret);
  hmac.update(canonical);
  const signature = hmac.digest('base64');

  // 3. Return message with signature
  return {
    ...msg,
    signature
  };
}

function verifySignature(msg: SharedMessage, secret: string): boolean {
  const providedSignature = msg.signature;
  if (!providedSignature) return false;

  // Recompute signature
  const expectedSignature = signMessage({ ...msg, signature: null }, secret).signature;

  // Constant-time comparison to prevent timing attacks
  return timingSafeEqual(
    Buffer.from(providedSignature, 'base64'),
    Buffer.from(expectedSignature ?? '', 'base64')
  );
}
```

### 3.2 Shared Secret Management

**Option 1: Per-actor secrets (most secure)**

```typescript
interface ActorSecret {
  actorId: CanonicalAddress;
  secret: string;  // Base64-encoded 32-byte secret
  rotatedAt: number;
  expiresAt: number;
}

// Store in Durable Object storage or KV
async function getActorSecret(actorId: CanonicalAddress): Promise<string> {
  const record = await env.ACTOR_SECRETS.get(actorId);
  if (!record) throw new Error('No secret found for actor');

  const parsed: ActorSecret = JSON.parse(record);

  // Check expiry
  if (parsed.expiresAt < Date.now()) {
    throw new Error('Secret expired');
  }

  return parsed.secret;
}
```

**Option 2: Global secret (simpler, less secure)**

```typescript
// Store in Cloudflare Workers environment variable
const GLOBAL_HMAC_SECRET = env.SIGNAL_HUB_HMAC_SECRET;

// All actors use same secret
function signMessage(msg: SharedMessage): SharedMessage {
  return signMessageWithSecret(msg, GLOBAL_HMAC_SECRET);
}
```

**Option 3: Hybrid (best practice)**

- **Control plane messages** (connect, register, discover) → Global secret
- **Data plane messages** (send, broadcast, publish) → Per-actor secrets

### 3.3 When to Validate Signatures

**Validation strategy:**

```typescript
const SIGNATURE_REQUIRED_TYPES = [
  'hub:connect',      // Authentication critical
  'hub:register',     // Registry integrity
  'hub:send',         // Message integrity
  'hub:broadcast',    // High-impact messages
  'hub:publish'       // Topic integrity
];

const SIGNATURE_OPTIONAL_TYPES = [
  'hub:heartbeat',    // Low-value, high-volume
  'hub:discover',     // Read-only operation
  'hub:list_actors'   // Read-only operation
];

function shouldValidateSignature(msgType: string): boolean {
  return SIGNATURE_REQUIRED_TYPES.includes(msgType);
}

async function processMessage(msg: SharedMessage, session: SignalHubSession): Promise<void> {
  if (shouldValidateSignature(msg.type)) {
    const secret = await getActorSecret(session.verifiedIdentity);

    if (!verifySignature(msg, secret)) {
      return {
        type: 'hub:error',
        payload: {
          code: 'invalid_signature',
          message: 'Message signature verification failed',
          retryable: false
        }
      };
    }
  }

  // Process message
  await this.handleMessage(msg, session);
}
```

### 3.4 Signature Failure Response

```typescript
const signatureErrorMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/widget-123)',
  type: 'hub:error',
  pattern: 'tell',
  correlationId: failedMsg.id,
  timestamp: Date.now(),
  payload: {
    code: 'invalid_signature',
    message: 'HMAC signature verification failed',
    details: {
      expectedAlgorithm: 'HMAC-SHA256',
      receivedSignature: 'base64...',
      messageType: 'hub:send'
    },
    retryable: false
  },
  metadata: {},
  ttl: null,
  signature: null  // Error messages not signed
};
```

### 3.5 Secret Rotation

**Graceful secret rotation:**

```typescript
interface ActorSecretWithRotation {
  actorId: CanonicalAddress;
  currentSecret: string;
  previousSecret: string | null;  // For grace period
  rotatedAt: number;
  gracePeriodEnds: number;  // rotatedAt + 24 hours
  expiresAt: number;
}

function verifySignatureWithRotation(
  msg: SharedMessage,
  secretRecord: ActorSecretWithRotation
): boolean {
  // Try current secret
  if (verifySignature(msg, secretRecord.currentSecret)) {
    return true;
  }

  // During grace period, accept previous secret
  if (secretRecord.previousSecret && Date.now() < secretRecord.gracePeriodEnds) {
    return verifySignature(msg, secretRecord.previousSecret);
  }

  return false;
}

// Rotate secret workflow
async function rotateActorSecret(actorId: CanonicalAddress): Promise<void> {
  const currentRecord = await getActorSecret(actorId);
  const newSecret = generateRandomSecret();  // 32 bytes, base64-encoded

  const rotatedRecord: ActorSecretWithRotation = {
    actorId,
    currentSecret: newSecret,
    previousSecret: currentRecord.currentSecret,
    rotatedAt: Date.now(),
    gracePeriodEnds: Date.now() + (24 * 60 * 60 * 1000),  // 24 hours
    expiresAt: Date.now() + (90 * 24 * 60 * 60 * 1000)    // 90 days
  };

  await env.ACTOR_SECRETS.put(actorId, JSON.stringify(rotatedRecord));

  // Notify actor of secret rotation
  await sendMessage({
    type: 'hub:secret_rotated',
    payload: {
      newSecret,
      gracePeriodEnds: rotatedRecord.gracePeriodEnds
    }
  });
}
```

---

## 4. Security Best Practices

### 4.1 Transport Security

**Require WSS (WebSocket Secure):**

```typescript
// Server-side: Reject non-secure connections in production
const url = new URL(request.url);
if (env.ENVIRONMENT === 'production' && url.protocol !== 'wss:') {
  return new Response('Secure WebSocket (wss://) required', { status: 426 });
}

// Client-side: Always use wss:// in production
const hubUrl = env.NODE_ENV === 'production'
  ? 'wss://hub.example.com/connect'
  : 'ws://localhost:8787/connect';
```

**TLS 1.3 enforced by Cloudflare Workers** - no additional configuration needed.

### 4.2 Token Storage (Client-Side)

**Browser storage options:**

| Storage | Security | Use Case |
|---------|----------|----------|
| `localStorage` | ⚠️ Vulnerable to XSS | Avoid for JWT tokens |
| `sessionStorage` | ⚠️ Vulnerable to XSS | Avoid for JWT tokens |
| In-memory only | ✅ Secure | **Recommended** - token lost on page refresh |
| `httpOnly` cookie | ✅ Secure | Ideal but requires custom auth server |

**Recommended approach:**

```typescript
class SignalHubClient {
  private authToken: string | null = null;  // In-memory only

  async connect(token: string) {
    this.authToken = token;  // Never persisted to storage

    const ws = new WebSocket('wss://hub.example.com/connect');
    ws.send(JSON.stringify({
      type: 'hub:connect',
      metadata: { authToken: `bearer ${this.authToken}` }
    }));
  }

  // Token lost on page refresh → user must re-authenticate
}
```

**If persistence required:**

```typescript
// Use Web Crypto API to encrypt before storing
async function encryptToken(token: string, password: string): Promise<string> {
  const encoder = new TextEncoder();
  const data = encoder.encode(token);

  const key = await crypto.subtle.importKey(
    'raw',
    encoder.encode(password),
    { name: 'AES-GCM' },
    false,
    ['encrypt']
  );

  const iv = crypto.getRandomValues(new Uint8Array(12));
  const encrypted = await crypto.subtle.encrypt(
    { name: 'AES-GCM', iv },
    key,
    data
  );

  // Store encrypted token + IV in localStorage
  return btoa(JSON.stringify({
    encrypted: Array.from(new Uint8Array(encrypted)),
    iv: Array.from(iv)
  }));
}
```

### 4.3 Replay Attack Prevention

**Message ID deduplication:**

```typescript
class ReplayProtection {
  private seenMessages: Map<string, number> = new Map();
  private cleanupInterval: number = 5 * 60 * 1000;  // 5 minutes

  constructor() {
    setInterval(() => this.cleanup(), this.cleanupInterval);
  }

  isReplay(msgId: string, timestamp: number): boolean {
    // Check if message ID already seen
    if (this.seenMessages.has(msgId)) {
      return true;  // Replay detected
    }

    // Check timestamp freshness (reject messages >5 min old)
    const age = Date.now() - timestamp;
    if (age > 5 * 60 * 1000) {
      return true;  // Too old, likely replay
    }

    // Record message ID
    this.seenMessages.set(msgId, timestamp);
    return false;
  }

  private cleanup() {
    const cutoff = Date.now() - this.cleanupInterval;
    for (const [msgId, timestamp] of this.seenMessages.entries()) {
      if (timestamp < cutoff) {
        this.seenMessages.delete(msgId);
      }
    }
  }
}

// Server-side enforcement
async function processMessage(msg: SharedMessage, replayProtection: ReplayProtection): Promise<void> {
  if (replayProtection.isReplay(msg.id, msg.timestamp)) {
    return {
      type: 'hub:error',
      payload: {
        code: 'replay_detected',
        message: 'Message ID already processed or timestamp too old',
        retryable: false
      }
    };
  }

  await this.handleMessage(msg);
}
```

**Alternative: Nonce-based replay protection**

```typescript
// Client includes incrementing nonce in metadata
const msg: SharedMessage = {
  // ... other fields
  metadata: {
    nonce: 12345,  // Monotonically increasing per connection
    timestamp: Date.now()
  }
};

// Server tracks last seen nonce per session
class SessionNonceTracker {
  private lastNonce: number = 0;

  isReplay(nonce: number): boolean {
    if (nonce <= this.lastNonce) {
      return true;  // Out-of-order or duplicate
    }
    this.lastNonce = nonce;
    return false;
  }
}
```

### 4.4 Audit Logging

**Log security-relevant events:**

```typescript
interface AuditEvent {
  timestamp: number;
  eventType: string;
  actorId: CanonicalAddress;
  sessionId: string;
  details: Record<string, unknown>;
  severity: 'info' | 'warning' | 'error';
}

// Events to log
const AUDIT_EVENTS = {
  CONNECTION_ESTABLISHED: 'connection_established',
  CONNECTION_FAILED: 'connection_failed',
  AUTH_SUCCESS: 'auth_success',
  AUTH_FAILURE: 'auth_failure',
  PERMISSION_DENIED: 'permission_denied',
  RATE_LIMIT_EXCEEDED: 'rate_limit_exceeded',
  SIGNATURE_INVALID: 'signature_invalid',
  MESSAGE_SENT: 'message_sent',
  MESSAGE_RECEIVED: 'message_received',
  ACTOR_REGISTERED: 'actor_registered',
  ACTOR_UNREGISTERED: 'actor_unregistered',
  BROADCAST_SENT: 'broadcast_sent'
};

async function logAuditEvent(event: AuditEvent, env: Env): Promise<void> {
  // Option 1: Cloudflare Workers Analytics Engine
  await env.ANALYTICS.writeDataPoint({
    indexes: [event.eventType, event.actorId],
    doubles: [event.timestamp],
    blobs: [JSON.stringify(event.details)]
  });

  // Option 2: Cloudflare Workers Logs
  console.log(JSON.stringify({
    type: 'audit',
    ...event
  }));

  // Option 3: External SIEM (via Queue)
  if (event.severity === 'error') {
    await env.AUDIT_QUEUE.send(event);
  }
}

// Example: Log failed authentication
await logAuditEvent({
  timestamp: Date.now(),
  eventType: AUDIT_EVENTS.AUTH_FAILURE,
  actorId: '@(browser/client-ui)',
  sessionId: 'sess-abc',
  details: {
    reason: 'Invalid JWT signature',
    ipAddress: request.headers.get('CF-Connecting-IP')
  },
  severity: 'warning'
}, env);
```

**Query audit logs:**

```typescript
// Using Cloudflare Workers Analytics Engine API
const query = `
  SELECT
    eventType,
    COUNT(*) as count,
    index1 as actorId
  FROM ANALYTICS_DATASET
  WHERE
    eventType = 'auth_failure'
    AND timestamp > NOW() - INTERVAL '1' HOUR
  GROUP BY eventType, actorId
  ORDER BY count DESC
`;

// Identify brute-force attempts
const results = await env.ANALYTICS.query(query);
```

---

## 5. Threat Model

### 5.1 Address Spoofing

**Threat:** Malicious client sends message with forged `from` address to impersonate another actor.

**Attack scenario:**

```typescript
// Attacker tries to impersonate admin actor
const maliciousMsg: SharedMessage = {
  from: '@(admin/privileged-actor)',  // Spoofed
  to: '@(cloudflare/signal-hub)',
  type: 'hub:broadcast',
  payload: { message: { type: 'system:shutdown', payload: {} } }
};
```

**Mitigation:** Server-enforced `from` field (Section 1.4)

- Server extracts identity from JWT on connect
- All subsequent messages stamped with verified identity
- Client-provided `from` field ignored

**Status:** ✅ Mitigated in MVP

### 5.2 Man-in-the-Middle (MITM)

**Threat:** Attacker intercepts WebSocket traffic to read/modify messages.

**Attack scenario:**

- Attacker on same network as client
- Intercepts unencrypted `ws://` connection
- Reads JWT token from `hub:connect` metadata
- Replays token to impersonate client

**Mitigation:** WSS (WebSocket Secure) encryption (Section 4.1)

- TLS 1.3 encryption (handled by Cloudflare)
- JWT token encrypted in transit
- Certificate validation (browser enforced)

**Status:** ✅ Mitigated in MVP (WSS required in production)

### 5.3 Replay Attacks

**Threat:** Attacker captures valid message and replays it later.

**Attack scenario:**

```typescript
// Attacker captures legitimate message
const capturedMsg: SharedMessage = {
  id: '123e4567-e89b-12d3-a456-426614174000',
  from: '@(browser/client-ui)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:send',
  timestamp: 1739731000000,
  payload: { targetAddress: '@(local/coordinator)', message: { type: 'task:delete', payload: { taskId: 'task-456' } } }
};

// Replays message hours later
await ws.send(JSON.stringify(capturedMsg));
```

**Mitigation:** Message ID deduplication + timestamp validation (Section 4.3)

- Server maintains short-lived seen-set of message IDs (TTL: 5 minutes)
- Reject messages with timestamp >5 minutes old
- Reject messages with duplicate IDs

**Status:** ✅ Mitigated in MVP

### 5.4 Denial of Service (DoS)

**Threat:** Attacker floods Signal Hub with requests to exhaust resources.

**Attack scenarios:**

1. **Connection flood:** Open 10K WebSocket connections
2. **Message flood:** Send 100K messages per second
3. **Broadcast amplification:** Single broadcast triggers 10K deliveries
4. **Registry exhaustion:** Register 100K actors to fill memory

**Mitigation:**

**Connection flood:**
- Rate limit connections per IP (100 connections/minute)
- Cloudflare DDoS protection at edge
- Max connections per Signal Hub instance: 50K

**Message flood:**
- Per-actor rate limiting (Section 2.3)
- Token bucket: 100 messages/second default
- Backpressure via `hub:pause` (Section 1.4 in MESSAGE_TYPES.md)

**Broadcast amplification:**
- Async broadcast queue (Section 6.2 in PROTOCOL_DESIGN_PLAN_V2.md)
- Max throughput: 1K actors/sec
- Broadcast capability restricted via JWT

**Registry exhaustion:**
- Max actors per instance: 50K (Section 6.3 in PROTOCOL_DESIGN_PLAN_V2.md)
- Actor registration TTL (auto-expire after 5 min)
- Max registrations per actor: 5

**Status:** ✅ Mitigated in MVP

### 5.5 Unauthorized Access

**Threat:** Attacker connects without valid JWT or with expired token.

**Attack scenario:**

```typescript
// Attacker tries to connect without auth token
const connectMsg: SharedMessage = {
  type: 'hub:connect',
  metadata: {
    protocolVersion: '0.1.0'
    // Missing authToken
  }
};

// Or with invalid token
const connectMsg2: SharedMessage = {
  type: 'hub:connect',
  metadata: {
    protocolVersion: '0.1.0',
    authToken: 'bearer invalid-jwt-token'
  }
};
```

**Mitigation:** JWT validation (Section 1.2)

- Server verifies JWT signature with secret key
- Check token expiration (reject if `exp` < now)
- Validate issuer (`iss` must be 'signal-hub')
- Respond with `hub:unauthorized` on failure

**Status:** ✅ Mitigated in MVP

### 5.6 Message Tampering (Phase 3 - Future)

**Threat:** Attacker modifies message in transit (if WSS compromised) or at rest.

**Attack scenario:**

```typescript
// Original message
const originalMsg: SharedMessage = {
  type: 'hub:send',
  payload: { targetAddress: '@(local/coordinator)', message: { type: 'task:assign', payload: { taskId: 'task-456', priority: 1 } } }
};

// Attacker modifies priority
const tamperedMsg: SharedMessage = {
  ...originalMsg,
  payload: { ...originalMsg.payload, message: { type: 'task:assign', payload: { taskId: 'task-456', priority: 0 } } }  // Changed priority
};
```

**Mitigation:** HMAC signatures (Section 3)

- Client signs message with shared secret
- Server verifies signature before processing
- Any modification invalidates signature

**Status:** ⏳ Phase 3 (future implementation)

---

## 6. Configuration Guidance

### 6.1 JWT Configuration

**Environment variables (Cloudflare Workers):**

```toml
# wrangler.toml
[env.production]
vars = { ENVIRONMENT = "production" }

[env.production.secrets]
JWT_SECRET = "<base64-encoded-secret>"  # wrangler secret put JWT_SECRET
JWT_ISSUER = "signal-hub"
JWT_MAX_AGE = "24h"
JWT_ALGORITHM = "HS256"  # or "RS256" for asymmetric
```

**Generate JWT secret:**

```bash
# Generate 256-bit secret for HS256
openssl rand -base64 32

# Or use Node.js
node -e "console.log(require('crypto').randomBytes(32).toString('base64'))"
```

**JWT payload structure:**

```typescript
interface JWTPayload {
  sub: string;          // User ID (required)
  actorId: string;      // Actor canonical address path (required)
  capabilities: string[];  // Actor capabilities (required)
  iss: string;          // Issuer: "signal-hub" (required)
  exp: number;          // Expiration (Unix timestamp, required)
  iat: number;          // Issued at (required)
  nbf?: number;         // Not before (optional)
  jti?: string;         // JWT ID (optional, for revocation)
}
```

**Issue JWT (external auth service):**

```typescript
import jwt from 'jsonwebtoken';

function issueSignalHubToken(userId: string, actorId: string, capabilities: string[]): string {
  const payload: JWTPayload = {
    sub: userId,
    actorId,
    capabilities,
    iss: 'signal-hub',
    exp: Math.floor(Date.now() / 1000) + (24 * 60 * 60),  // 24 hours
    iat: Math.floor(Date.now() / 1000)
  };

  return jwt.sign(payload, process.env.JWT_SECRET, { algorithm: 'HS256' });
}

// Example usage
const token = issueSignalHubToken(
  'user-123',
  'browser/client-ui',
  ['send', 'broadcast', 'subscribe', 'register']
);
```

### 6.2 Token Expiry Configuration

**Recommended expiry times:**

| Environment | Expiry | Rationale |
|-------------|--------|-----------|
| Development | 7 days | Developer convenience |
| Staging | 24 hours | Balance security/usability |
| Production | 1-4 hours | High security, frequent refresh |

**Configurable via environment:**

```typescript
const JWT_EXPIRY_SECONDS = {
  development: 7 * 24 * 60 * 60,    // 7 days
  staging: 24 * 60 * 60,            // 24 hours
  production: 4 * 60 * 60           // 4 hours
}[env.ENVIRONMENT];
```

### 6.3 HMAC Configuration (Phase 3 - Future)

**Environment variables:**

```toml
[env.production.secrets]
HMAC_GLOBAL_SECRET = "<base64-encoded-secret>"
HMAC_ALGORITHM = "sha256"
HMAC_SIGNATURE_REQUIRED = "true"  # Enforce signatures on all messages
```

**Per-actor secrets storage:**

```typescript
// Store in Durable Object or KV
await env.ACTOR_SECRETS.put(
  '@(browser/widget-123)',
  JSON.stringify({
    actorId: '@(browser/widget-123)',
    secret: '<base64-encoded-32-byte-secret>',
    rotatedAt: Date.now(),
    expiresAt: Date.now() + (90 * 24 * 60 * 60 * 1000)  // 90 days
  })
);
```

### 6.4 Rate Limit Configuration

**Default limits (environment variables):**

```toml
[env.production]
RATE_LIMIT_MESSAGES_PER_SECOND = "100"
RATE_LIMIT_BROADCASTS_PER_MINUTE = "10"
RATE_LIMIT_REGISTRATIONS_PER_ACTOR = "5"
RATE_LIMIT_CONNECTIONS_PER_IP = "100"
```

**Per-actor overrides:**

```typescript
interface ActorRateLimitOverride {
  actorPattern: string;  // Glob pattern
  maxMessagesPerSecond: number;
  maxBroadcastsPerMinute: number;
}

const RATE_LIMIT_OVERRIDES: ActorRateLimitOverride[] = [
  {
    actorPattern: '@(local/coordinator-*)',
    maxMessagesPerSecond: 1000,
    maxBroadcastsPerMinute: 60
  },
  {
    actorPattern: '@(admin/*)',
    maxMessagesPerSecond: 500,
    maxBroadcastsPerMinute: 30
  }
];
```

---

## 7. Migration Path

### 7.1 MVP → Phase 2 (Authorization)

**Phase 1 (MVP):**
- JWT authentication
- Server-enforced identity
- Basic rate limiting
- WSS encryption

**Phase 2 additions:**
- Actor-to-actor permissions (Section 2.1)
- Topic-level ACLs (Section 2.2)
- Capability-based access control (Section 2.4)
- Fine-grained rate limits (Section 2.3)

**Migration steps:**

1. **Deploy Phase 2 with permissive defaults:**
   ```typescript
   const DEFAULT_PERMISSIONS: ActorPermissions = {
     actorId: '@(*)',  // All actors
     allowedTargets: [],  // Empty = allow all
     deniedTargets: [],
     capabilities: ['send', 'broadcast', 'subscribe', 'register', 'discover']
   };
   ```

2. **Gradually restrict permissions:**
   ```typescript
   // Week 1: Identify permission patterns from audit logs
   // Week 2: Apply restrictions to non-critical actors
   // Week 3: Apply restrictions to all actors
   // Week 4: Enable enforcement
   ```

3. **Backward compatibility:**
   - Existing connections continue with full permissions
   - New connections subject to new ACLs
   - JWT must include `capabilities` field (default to all if missing)

### 7.2 Phase 2 → Phase 3 (HMAC Signatures)

**Phase 2 state:**
- Full authorization model
- No message integrity verification

**Phase 3 additions:**
- HMAC signatures (Section 3)
- Per-actor secrets
- Signature validation

**Migration steps:**

1. **Deploy Phase 3 with signatures optional:**
   ```typescript
   const SIGNATURE_ENFORCEMENT = env.ENFORCE_SIGNATURES === 'true';

   if (SIGNATURE_ENFORCEMENT && !verifySignature(msg, secret)) {
     return { type: 'hub:error', payload: { code: 'invalid_signature' } };
   } else if (!verifySignature(msg, secret)) {
     // Log warning but allow
     console.warn('Invalid signature (not enforced)', msg.id);
   }
   ```

2. **Distribute secrets to actors:**
   ```typescript
   // On first connect, server generates actor secret
   const secret = generateRandomSecret();
   await env.ACTOR_SECRETS.put(actorId, secret);

   // Send secret to actor (once)
   await sendMessage({
     type: 'hub:secret_issued',
     payload: { secret }
   });
   ```

3. **Enable enforcement gradually:**
   - Week 1: Signatures optional, log missing signatures
   - Week 2: Enforce on `hub:connect`, `hub:register` only
   - Week 3: Enforce on all control plane messages
   - Week 4: Enforce on all messages (full Phase 3)

4. **Backward compatibility:**
   - Old clients without signatures rejected with `hub:error` (code: `signature_required`)
   - Client upgrade required before enforcement

---

## 8. Security Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Client (Browser/SEAG)                       │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 1. Obtain JWT from Auth Service                              │ │
│  │    POST /auth/login → JWT (sub, actorId, capabilities, exp) │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                ↓                                    │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 2. Connect to Signal Hub with JWT                            │ │
│  │    WSS connection + hub:connect (metadata.authToken)         │ │
│  └───────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
                                 ↓ (WSS encrypted)
┌─────────────────────────────────────────────────────────────────────┐
│                    Cloudflare Edge (TLS 1.3)                        │
└─────────────────────────────────────────────────────────────────────┘
                                 ↓
┌─────────────────────────────────────────────────────────────────────┐
│              Signal Hub Durable Object (Worker)                     │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 3. Validate JWT                                               │ │
│  │    - Verify signature (HMAC-SHA256 or RS256)                 │ │
│  │    - Check expiration (exp > now)                            │ │
│  │    - Validate issuer (iss = "signal-hub")                    │ │
│  │    - Extract actorId from payload                            │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                ↓                                    │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 4. Create Session with Verified Identity                     │ │
│  │    session = {                                                │ │
│  │      sessionId: 'sess-abc',                                   │ │
│  │      verifiedIdentity: '@(browser/client-ui)',  // From JWT  │ │
│  │      capabilities: ['send', 'broadcast', 'subscribe'],       │ │
│  │      tokenExpiresAt: 1739731553000                           │ │
│  │    }                                                          │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                ↓                                    │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 5. Server-Enforce 'from' Field on All Messages               │ │
│  │    msg.from = session.verifiedIdentity  // Ignore client     │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                ↓                                    │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 6. Rate Limiting (Token Bucket)                               │ │
│  │    - Check rate limit before processing                       │ │
│  │    - Return hub:rate_limited if exceeded                      │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                ↓                                    │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 7. Authorization (Phase 2 - Future)                           │ │
│  │    - Check actor-to-actor permissions                         │ │
│  │    - Check topic ACLs                                         │ │
│  │    - Check capabilities                                       │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                ↓                                    │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 8. HMAC Validation (Phase 3 - Future)                         │ │
│  │    - Verify msg.signature with actor secret                   │ │
│  │    - Reject if invalid                                        │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                ↓                                    │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 9. Replay Protection                                          │ │
│  │    - Check message ID in seen-set                             │ │
│  │    - Check timestamp freshness (<5 min old)                   │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                ↓                                    │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 10. Process Message                                           │ │
│  │     - Route to target actor                                   │ │
│  │     - Broadcast to subscribers                                │ │
│  │     - Update actor registry                                   │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                ↓                                    │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │ 11. Audit Logging                                             │ │
│  │     - Log to Analytics Engine                                 │ │
│  │     - Queue security events                                   │ │
│  └───────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 9. Code Examples

### 9.1 Complete Authentication Flow

```typescript
// client.ts - Browser/SEAG client
import { SignalHubClient } from './signal-hub-client';

async function connectToSignalHub() {
  // 1. Obtain JWT from auth service
  const authResponse = await fetch('https://auth.example.com/token', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ userId: 'user-123', actorId: 'browser/client-ui' })
  });
  const { token } = await authResponse.json();

  // 2. Connect to Signal Hub with JWT
  const hub = new SignalHubClient('wss://hub.example.com/connect');

  try {
    await hub.connect(token);
    console.log('Connected to Signal Hub');

    // 3. Register actor
    await hub.register({
      actorAddress: '@(browser/client-ui)',
      capabilities: ['render', 'handle-click'],
      metadata: { version: '1.0.0' },
      ttlSeconds: 300
    });

    // 4. Start heartbeat
    hub.startHeartbeat();

    // 5. Send message
    await hub.send({
      targetAddress: '@(local/coordinator-main)',
      message: { type: 'task:request', payload: { taskType: 'compute' } },
      requireAck: true
    });

  } catch (err) {
    if (err.type === 'hub:unauthorized') {
      console.error('Authentication failed:', err.payload.reason);
      // Redirect to login
    }
  }
}
```

### 9.2 Server-Side JWT Validation

```typescript
// signal-hub-do.ts - Durable Object
import { verify } from 'jsonwebtoken';
import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';

export class SignalHubDO {
  private env: Env;
  private sessions: Map<string, Session> = new Map();

  async handleConnect(msg: SharedMessage, ws: WebSocket): Promise<void> {
    const authToken = msg.metadata.authToken as string | undefined;

    if (!authToken) {
      return this.sendError(ws, msg.id, {
        code: 'unauthorized',
        message: 'Missing authToken in metadata',
        retryable: false
      });
    }

    // Validate JWT
    let identity: ActorIdentity;
    try {
      identity = await this.validateJWT(authToken);
    } catch (err) {
      return this.sendError(ws, msg.id, {
        code: 'unauthorized',
        message: err.message,
        retryable: false
      });
    }

    // Create session with verified identity
    const sessionId = crypto.randomUUID();
    const session: Session = {
      sessionId,
      ws,
      verifiedIdentity: `@(${identity.actorId})` as CanonicalAddress,
      capabilities: identity.capabilities,
      tokenExpiresAt: identity.expiresAt,
      connectedAt: Date.now(),
      rateLimiter: new TokenBucketRateLimiter(100, 100)  // 100 msg/sec
    };

    this.sessions.set(sessionId, session);

    // Send hub:connected response
    await this.sendMessage(ws, {
      id: crypto.randomUUID(),
      from: '@(cloudflare/signal-hub)' as CanonicalAddress,
      to: session.verifiedIdentity,
      type: 'hub:connected',
      pattern: 'tell',
      correlationId: msg.id,
      timestamp: Date.now(),
      payload: {
        sessionId,
        serverVersion: '0.1.0',
        maxMessageSize: 1048576,
        heartbeatInterval: 25000,
        capabilities: {
          maxActorsPerInstance: 50000,
          supportsBackpressure: true,
          supportedContentTypes: ['json', 'msgpack']
        }
      },
      metadata: {
        actorIdentity: session.verifiedIdentity,
        tokenExpiresAt: session.tokenExpiresAt
      },
      ttl: null,
      signature: null
    });
  }

  private async validateJWT(authToken: string): Promise<ActorIdentity> {
    const token = authToken.replace(/^bearer\s+/i, '');

    try {
      const decoded = verify(token, this.env.JWT_SECRET, {
        algorithms: ['HS256'],
        issuer: 'signal-hub',
        maxAge: '24h'
      }) as JWTPayload;

      return {
        actorId: decoded.actorId,
        userId: decoded.sub,
        capabilities: decoded.capabilities ?? [],
        expiresAt: decoded.exp * 1000
      };
    } catch (err) {
      if (err.name === 'TokenExpiredError') {
        throw new Error('JWT token has expired');
      }
      if (err.name === 'JsonWebTokenError') {
        throw new Error('JWT signature invalid');
      }
      throw err;
    }
  }

  async handleMessage(msg: SharedMessage, session: Session): Promise<void> {
    // CRITICAL: Server-enforce 'from' field
    const secureMsg: SharedMessage = {
      ...msg,
      from: session.verifiedIdentity  // Ignore client-provided 'from'
    };

    // Rate limiting
    const rateLimitResult = await session.rateLimiter.checkLimit();
    if (!rateLimitResult.allowed) {
      return this.sendError(session.ws, msg.id, {
        code: 'rate_limited',
        message: 'Rate limit exceeded',
        retryable: true,
        retryAfter: rateLimitResult.retryAfter
      });
    }

    // Replay protection
    if (this.replayProtection.isReplay(secureMsg.id, secureMsg.timestamp)) {
      return this.sendError(session.ws, msg.id, {
        code: 'replay_detected',
        message: 'Message ID already processed or timestamp too old',
        retryable: false
      });
    }

    // Process message
    await this.routeMessage(secureMsg, session);
  }
}

interface Session {
  sessionId: string;
  ws: WebSocket;
  verifiedIdentity: CanonicalAddress;
  capabilities: string[];
  tokenExpiresAt: number;
  connectedAt: number;
  rateLimiter: TokenBucketRateLimiter;
}

interface ActorIdentity {
  actorId: string;
  userId: string;
  capabilities: string[];
  expiresAt: number;
}

interface JWTPayload {
  sub: string;
  actorId: string;
  capabilities: string[];
  iss: string;
  exp: number;
  iat: number;
}
```

### 9.3 HMAC Signature Verification (Phase 3)

```typescript
// hmac.ts - Signature utilities
import { createHmac, timingSafeEqual } from 'crypto';
import type { SharedMessage } from '@agentic-primer/protocols';

export function signMessage(msg: SharedMessage, secret: string): SharedMessage {
  const canonical = JSON.stringify({
    id: msg.id,
    from: msg.from,
    to: msg.to,
    type: msg.type,
    payload: msg.payload,
    pattern: msg.pattern,
    correlationId: msg.correlationId,
    timestamp: msg.timestamp,
    metadata: msg.metadata,
    ttl: msg.ttl
  });

  const hmac = createHmac('sha256', secret);
  hmac.update(canonical);
  const signature = hmac.digest('base64');

  return { ...msg, signature };
}

export function verifySignature(msg: SharedMessage, secret: string): boolean {
  if (!msg.signature) return false;

  const expectedSignature = signMessage({ ...msg, signature: null }, secret).signature;

  if (!expectedSignature) return false;

  return timingSafeEqual(
    Buffer.from(msg.signature, 'base64'),
    Buffer.from(expectedSignature, 'base64')
  );
}

// Usage in Signal Hub DO
async handleMessage(msg: SharedMessage, session: Session): Promise<void> {
  // HMAC validation (Phase 3)
  if (this.env.ENFORCE_SIGNATURES === 'true') {
    const secret = await this.getActorSecret(session.verifiedIdentity);

    if (!verifySignature(msg, secret)) {
      return this.sendError(session.ws, msg.id, {
        code: 'invalid_signature',
        message: 'HMAC signature verification failed',
        retryable: false
      });
    }
  }

  // Continue processing...
}
```

---

## Summary

**Security model status:**

| Feature | MVP (Phase 1) | Phase 2 (Future) | Phase 3 (Future) |
|---------|---------------|------------------|------------------|
| JWT Authentication | ✅ | ✅ | ✅ |
| Server-Enforced Identity | ✅ | ✅ | ✅ |
| WSS Encryption | ✅ | ✅ | ✅ |
| Rate Limiting | ✅ Basic | ✅ Fine-grained | ✅ |
| Replay Protection | ✅ | ✅ | ✅ |
| Actor-to-Actor Permissions | ❌ | ✅ | ✅ |
| Topic ACLs | ❌ | ✅ | ✅ |
| HMAC Signatures | ❌ | ❌ | ✅ |
| Secret Rotation | ❌ | ❌ | ✅ |
| Audit Logging | ✅ Basic | ✅ Detailed | ✅ |

**Next steps:**

1. ✅ Phase 3 (Security) documented
2. ⏳ Phase 4 (Connection Lifecycle)
3. ⏳ Phase 5 (Delivery Guarantees)
4. ⏳ Phase 6 (Scalability)

**Security principle:** Defense-in-depth with server-enforced identity at the core.
