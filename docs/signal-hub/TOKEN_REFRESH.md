# Signal Hub Token Refresh Protocol

**Status:** MVP (Phase 1)
**Last Updated:** 2026-02-16
**Protocol Version:** 0.1.0

---

## Overview

Token refresh allows clients to update their JWT tokens without disconnecting from Signal Hub, avoiding:
- Connection interruption during active work
- Reconnection overhead (handshake, re-registration)
- Potential message loss during reconnection
- Unnecessary load on Signal Hub (connection churn)

**Priority:** HIGH (P1) - Production deployments with 1-4 hour token expiry require this feature.

---

## Token Lifecycle

```
┌────────────────────────────────────────────────────────────┐
│  T=0: Initial Connection                                   │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Client obtains JWT from auth service (expires T+4h) │  │
│  │ Client connects: hub:connect (authToken: jwt1)      │  │
│  │ Server responds: hub:connected (tokenExpiresAt: T+4h)│ │
│  └──────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────┘

                          ⏰ Timer running...

┌────────────────────────────────────────────────────────────┐
│  T=3h55m: Token Refresh (5 min before expiry)             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Client obtains new JWT from auth service (jwt2)     │  │
│  │ Client sends: hub:refresh_token (authToken: jwt2)   │  │
│  │ Server validates jwt2, updates session              │  │
│  │ Server responds: hub:token_refreshed (T+8h)         │  │
│  └──────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────┐
│  T=4h: Grace Period (old token expired)                   │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Old token (jwt1) expires                             │  │
│  │ Grace period active: 5 minutes                       │  │
│  │ Messages with old token timestamp still accepted    │  │
│  └──────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────┐
│  T=4h5m: Grace Period Ends                                 │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Only new token (jwt2) accepted                       │  │
│  │ Connection remains active, no reconnection needed    │  │
│  └──────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────┘

                          ⏰ Cycle repeats...
```

---

## Message Specifications

### hub:refresh_token

**Direction:** Client → Server
**Pattern:** `ask` (expects `hub:token_refreshed` or `hub:unauthorized`)
**Purpose:** Update session JWT without reconnecting

```typescript
interface HubRefreshTokenMessage {
  id: string;
  from: CanonicalAddress;
  to: '@(cloudflare/signal-hub)';
  type: 'hub:refresh_token';
  pattern: 'ask';
  correlationId: null;
  timestamp: number;
  payload: null;
  metadata: {
    authToken: string;  // Bearer token with new JWT
    nonce?: string;     // Optional nonce for replay prevention
  };
  ttl: 5000;  // 5 second timeout
  signature: string | null;
}
```

**Validation Rules:**
1. `authToken` MUST be valid JWT
2. JWT `actorId` MUST match current session's verified identity
3. JWT `sub` (user ID) SHOULD match current session
4. Refresh rate limited to 1 request per minute per session
5. Nonce (if provided) MUST be unique (replay prevention)

**Example:**

```typescript
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
    authToken: 'bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ1c2VyLTEyMyIsImFjdG9ySWQiOiJicm93c2VyL2NsaWVudC11aSIsImV4cCI6MTczOTczMTU1M30.signature',
    nonce: crypto.randomUUID()
  },
  ttl: 5000,
  signature: null
};
```

---

### hub:token_refreshed

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:refresh_token`)
**Purpose:** Confirm token updated successfully

```typescript
interface HubTokenRefreshedMessage {
  id: string;
  from: '@(cloudflare/signal-hub)';
  to: CanonicalAddress;
  type: 'hub:token_refreshed';
  pattern: 'tell';
  correlationId: string;  // Original hub:refresh_token message ID
  timestamp: number;
  payload: {
    tokenExpiresAt: number;  // Epoch ms when new token expires
  };
  metadata: Record<string, unknown>;
  ttl: null;
  signature: string | null;
}
```

**Example:**

```typescript
const refreshedMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/client-ui)',
  type: 'hub:token_refreshed',
  pattern: 'tell',
  correlationId: refreshMsg.id,
  timestamp: Date.now(),
  payload: {
    tokenExpiresAt: Date.now() + (4 * 60 * 60 * 1000)  // 4 hours from now
  },
  metadata: {},
  ttl: null,
  signature: null
};
```

---

## Implementation

### Client-Side Implementation

```typescript
import { SignalHubClient } from '@agentic-primer/signal-hub-client';

class SignalHubClient {
  private tokenExpiresAt: number;
  private refreshTimer: NodeJS.Timeout | null = null;

  async connect(authToken: string): Promise<void> {
    // Connect with initial token
    const connectMsg: SharedMessage = {
      type: 'hub:connect',
      metadata: {
        protocolVersion: '0.1.0',
        authToken: `bearer ${authToken}`,
        capabilities: ['send', 'broadcast', 'subscribe']
      }
    };

    const response = await this.sendAndWait(connectMsg, 5000);

    if (response.type === 'hub:connected') {
      this.tokenExpiresAt = response.metadata.tokenExpiresAt;
      this.startTokenRefreshTimer();
    }
  }

  private startTokenRefreshTimer(): void {
    if (this.refreshTimer) {
      clearTimeout(this.refreshTimer);
    }

    const timeUntilExpiry = this.tokenExpiresAt - Date.now();
    const refreshAt = timeUntilExpiry - (5 * 60 * 1000);  // 5 min before expiry

    if (refreshAt > 0) {
      this.refreshTimer = setTimeout(async () => {
        await this.refreshToken();
      }, refreshAt);
    } else {
      console.warn('Token already expired or expires soon, refreshing immediately');
      this.refreshToken();
    }
  }

  private async refreshToken(): Promise<void> {
    try {
      // 1. Obtain new JWT from external auth service
      const newToken = await this.authService.refreshToken();

      // 2. Send refresh request to Signal Hub
      const refreshMsg: SharedMessage = {
        id: crypto.randomUUID(),
        from: this.verifiedIdentity,
        to: '@(cloudflare/signal-hub)',
        type: 'hub:refresh_token',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: {
          authToken: `bearer ${newToken}`,
          nonce: crypto.randomUUID()
        },
        ttl: 5000,
        signature: null
      };

      const response = await this.sendAndWait(refreshMsg, 5000);

      if (response.type === 'hub:token_refreshed') {
        // Success - update expiration and schedule next refresh
        this.tokenExpiresAt = response.payload.tokenExpiresAt;
        this.startTokenRefreshTimer();
        console.log('Token refreshed successfully, expires at:', new Date(this.tokenExpiresAt));
      } else if (response.type === 'hub:unauthorized') {
        // Token refresh failed - must reconnect
        console.error('Token refresh failed:', response.payload.reason);
        await this.handleTokenRefreshFailure();
      } else if (response.type === 'hub:rate_limited') {
        // Rate limited - retry after delay
        const retryAfter = response.payload.retryAfter;
        console.warn(`Token refresh rate limited, retrying in ${retryAfter}ms`);
        setTimeout(() => this.refreshToken(), retryAfter);
      }

    } catch (err) {
      console.error('Token refresh error:', err);
      await this.handleTokenRefreshFailure();
    }
  }

  private async handleTokenRefreshFailure(): Promise<void> {
    // Fallback: Reconnect with fresh auth
    console.warn('Token refresh failed, reconnecting with new auth...');

    try {
      // 1. Obtain fresh token from auth service
      const freshToken = await this.authService.login();

      // 2. Gracefully disconnect
      await this.disconnect();

      // 3. Reconnect with fresh token
      await this.connect(freshToken);

      // 4. Re-register actor
      await this.register(this.actorConfig);

      console.log('Successfully reconnected after token refresh failure');
    } catch (reconnectErr) {
      console.error('Reconnection failed:', reconnectErr);
      // Prompt user to re-authenticate manually
      this.emit('authentication_required', { reason: 'token_refresh_failed' });
    }
  }

  async disconnect(): Promise<void> {
    if (this.refreshTimer) {
      clearTimeout(this.refreshTimer);
      this.refreshTimer = null;
    }
    // ... rest of disconnect logic
  }
}
```

---

### Server-Side Implementation

```typescript
import { verify } from 'jsonwebtoken';

class SignalHubDO {
  private env: Env;
  private sessions: Map<string, Session> = new Map();
  private refreshRateLimiter: RefreshRateLimiter;
  private usedNonces: SeenCache;  // TTL-based cache

  async handleRefreshToken(msg: SharedMessage, session: Session): Promise<void> {
    const authToken = msg.metadata.authToken as string | undefined;
    const nonce = msg.metadata.nonce as string | undefined;

    // 1. Validate request format
    if (!authToken) {
      return this.sendUnauthorized(session.ws, msg.id, {
        action: 'refresh_token',
        reason: 'Missing authToken in metadata'
      });
    }

    // 2. Check rate limit (1 refresh per minute per session)
    if (!this.refreshRateLimiter.canRefresh(session.sessionId)) {
      return this.sendRateLimited(session.ws, msg.id, {
        retryAfter: 60000  // 1 minute
      });
    }

    // 3. Check nonce replay (if provided)
    if (nonce) {
      if (this.usedNonces.has(nonce)) {
        return this.sendUnauthorized(session.ws, msg.id, {
          action: 'refresh_token',
          reason: 'Nonce already used (replay attack detected)'
        });
      }
      this.usedNonces.add(nonce, 60000);  // 1 min TTL
    }

    // 4. Validate new JWT
    let newIdentity: ActorIdentity;
    try {
      newIdentity = await this.validateJWT(authToken);
    } catch (err) {
      return this.sendUnauthorized(session.ws, msg.id, {
        action: 'refresh_token',
        reason: err.message
      });
    }

    // 5. CRITICAL: Verify actor identity matches current session
    const currentActorId = session.verifiedIdentity.replace(/^@\(|\)$/g, '');
    if (newIdentity.actorId !== currentActorId) {
      return this.sendUnauthorized(session.ws, msg.id, {
        action: 'refresh_token',
        reason: `Token actorId mismatch: expected ${currentActorId}, got ${newIdentity.actorId}`
      });
    }

    // 6. Update session with new token expiration (grace period handling)
    session.previousTokenExp = session.tokenExpiresAt;
    session.gracePeriodEnds = Date.now() + (5 * 60 * 1000);  // 5 min grace
    session.tokenExpiresAt = newIdentity.expiresAt;
    session.capabilities = newIdentity.capabilities;

    // 7. Respond with success
    await this.sendMessage(session.ws, {
      id: crypto.randomUUID(),
      from: '@(cloudflare/signal-hub)',
      to: session.verifiedIdentity,
      type: 'hub:token_refreshed',
      pattern: 'tell',
      correlationId: msg.id,
      timestamp: Date.now(),
      payload: {
        tokenExpiresAt: session.tokenExpiresAt
      },
      metadata: {},
      ttl: null,
      signature: null
    });
  }

  private async validateJWT(authToken: string): Promise<ActorIdentity> {
    const token = authToken.replace(/^bearer\s+/i, '');

    try {
      const decoded = verify(token, this.env.JWT_SECRET, {
        algorithms: ['HS256', 'RS256'],
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
}

interface Session {
  sessionId: string;
  ws: WebSocket;
  verifiedIdentity: CanonicalAddress;
  capabilities: string[];
  tokenExpiresAt: number;
  previousTokenExp: number | null;  // For grace period
  gracePeriodEnds: number | null;   // Grace period end time
  connectedAt: number;
  rateLimiter: TokenBucketRateLimiter;
}
```

---

## Grace Period Handling

**Problem:** Messages sent with old token may arrive after refresh completes

**Solution:** 5-minute grace period where both old and new tokens are valid

```typescript
class SignalHubSession {
  private currentTokenExp: number;
  private previousTokenExp: number | null = null;
  private gracePeriodEnds: number | null = null;

  async refreshToken(newIdentity: ActorIdentity): Promise<void> {
    // Save old token expiration for grace period
    this.previousTokenExp = this.currentTokenExp;
    this.gracePeriodEnds = Date.now() + (5 * 60 * 1000);  // 5 min grace

    // Update to new token
    this.currentTokenExp = newIdentity.expiresAt;
  }

  isTokenValid(msgTimestamp: number): boolean {
    const now = Date.now();

    // Current token still valid
    if (now < this.currentTokenExp) {
      return true;
    }

    // During grace period, accept messages sent with old token
    if (this.gracePeriodEnds && now < this.gracePeriodEnds) {
      // Message was sent before grace period ended
      if (msgTimestamp < this.gracePeriodEnds) {
        return true;
      }
    }

    return false;
  }
}
```

**Grace period timeline:**

```
T=0:     Token refresh completes
         currentTokenExp = T+4h
         previousTokenExp = T (just expired)
         gracePeriodEnds = T+5min

T+0 to T+5min:
         - Messages with timestamp < T+5min accepted (old token)
         - Messages with timestamp >= now accepted (new token)

T+5min:  Grace period ends
         - Only new token accepted
         - previousTokenExp discarded
```

---

## Security Considerations

### 1. Actor Identity Switching Prevention

**Threat:** Attacker tries to switch actor identity during refresh

```typescript
// ❌ SECURITY VIOLATION
const oldToken = {
  actorId: 'browser/client-ui',
  sub: 'user-123'
};

const newToken = {
  actorId: 'admin/privileged-actor',  // Different actor!
  sub: 'user-123'
};
```

**Mitigation:** Server validates actor identity match

```typescript
if (newIdentity.actorId !== currentActorId) {
  throw new Error('Token actorId does not match current session');
}
```

### 2. Rate Limiting

**Threat:** Attacker floods server with refresh requests

**Mitigation:** 1 refresh per minute per session

```typescript
class RefreshRateLimiter {
  private lastRefresh: Map<string, number> = new Map();
  private readonly MIN_REFRESH_INTERVAL = 60000;  // 1 minute

  canRefresh(sessionId: string): boolean {
    const lastRefreshTime = this.lastRefresh.get(sessionId) ?? 0;
    const timeSinceLastRefresh = Date.now() - lastRefreshTime;

    if (timeSinceLastRefresh < this.MIN_REFRESH_INTERVAL) {
      return false;  // Too frequent
    }

    this.lastRefresh.set(sessionId, Date.now());
    return true;
  }
}
```

### 3. Replay Attack Prevention

**Threat:** Attacker captures and replays refresh request

**Mitigation:** Optional nonce with deduplication

```typescript
// Client includes unique nonce
const refreshMsg: SharedMessage = {
  type: 'hub:refresh_token',
  metadata: {
    authToken: 'bearer <jwt>',
    nonce: crypto.randomUUID()
  }
};

// Server tracks used nonces (60s TTL)
if (this.usedNonces.has(msg.metadata.nonce)) {
  throw new Error('Nonce already used (replay attack)');
}
this.usedNonces.add(msg.metadata.nonce, 60000);
```

### 4. Token Rotation

**Best practice:** Issue new JWT with different signature on each refresh

```typescript
// Auth service generates fresh JWT on each refresh request
async function refreshToken(oldToken: string): Promise<string> {
  const decoded = verify(oldToken, JWT_SECRET);

  // Generate NEW JWT with fresh signature
  return sign({
    sub: decoded.sub,
    actorId: decoded.actorId,
    capabilities: decoded.capabilities,
    iss: 'signal-hub',
    exp: Math.floor(Date.now() / 1000) + (4 * 60 * 60),  // 4h
    iat: Math.floor(Date.now() / 1000),
    jti: crypto.randomUUID()  // NEW JWT ID
  }, JWT_SECRET);
}
```

---

## Error Handling

### Token Refresh Failures

| Error Type | Reason | Client Action |
|------------|--------|---------------|
| `hub:unauthorized` | Invalid JWT signature | Reconnect with fresh auth |
| `hub:unauthorized` | Token expired | Reconnect with fresh auth |
| `hub:unauthorized` | Actor ID mismatch | Reconnect with fresh auth |
| `hub:unauthorized` | Nonce replay | Don't retry, report to security team |
| `hub:rate_limited` | Too many refresh requests | Wait `retryAfter` ms, retry once |
| `hub:error` (timeout) | Server timeout | Retry with exponential backoff |

### Fallback Strategy

```typescript
async refreshToken(): Promise<void> {
  try {
    await this.sendRefreshTokenRequest();
  } catch (err) {
    if (err.type === 'hub:unauthorized') {
      // Token refresh failed, must reconnect
      console.warn('Token refresh failed, reconnecting...');
      await this.handleTokenRefreshFailure();
    } else if (err.type === 'hub:rate_limited') {
      // Rate limited, retry after delay
      const retryAfter = err.payload.retryAfter;
      setTimeout(() => this.refreshToken(), retryAfter);
    } else {
      // Other error, log and reconnect
      console.error('Token refresh error:', err);
      await this.handleTokenRefreshFailure();
    }
  }
}

async handleTokenRefreshFailure(): Promise<void> {
  // 1. Obtain fresh token from auth service
  const freshToken = await this.authService.login();

  // 2. Gracefully disconnect
  await this.disconnect();

  // 3. Reconnect with fresh token
  await this.connect(freshToken);

  // 4. Re-register actor
  await this.register(this.actorConfig);
}
```

---

## Testing

### Unit Tests

```typescript
describe('Token Refresh', () => {
  it('should refresh token successfully', async () => {
    const client = new SignalHubClient();
    await client.connect(oldToken);

    const newToken = await authService.refreshToken();
    await client.refreshToken(newToken);

    expect(client.tokenExpiresAt).toBeGreaterThan(Date.now());
  });

  it('should reject token with different actorId', async () => {
    const client = new SignalHubClient();
    await client.connect(token1);  // actorId: browser/client-ui

    const token2 = createToken({ actorId: 'admin/privileged' });

    await expect(client.refreshToken(token2)).rejects.toThrow('actorId mismatch');
  });

  it('should enforce rate limit (1/min)', async () => {
    const client = new SignalHubClient();
    await client.connect(token);

    await client.refreshToken(newToken1);
    await expect(client.refreshToken(newToken2)).rejects.toThrow('rate_limited');
  });

  it('should reject replayed nonce', async () => {
    const client = new SignalHubClient();
    await client.connect(token);

    const nonce = crypto.randomUUID();
    await client.refreshToken(newToken, { nonce });

    await expect(client.refreshToken(newToken, { nonce })).rejects.toThrow('replay');
  });

  it('should accept messages during grace period', async () => {
    const server = new SignalHubDO();
    const session = await server.connect(client, oldToken);

    await server.refreshToken(session, newToken);

    // Message sent before refresh with old token timestamp
    const oldMsg = createMessage({ timestamp: Date.now() - 1000 });
    expect(server.isTokenValid(session, oldMsg.timestamp)).toBe(true);
  });
});
```

### Integration Tests

```typescript
describe('Token Refresh Integration', () => {
  it('should refresh token without disconnecting', async () => {
    const client = new SignalHubClient();
    await client.connect(token);
    await client.register({ actorAddress: '@(browser/test)' });

    // Send message before refresh
    await client.send({ to: '@(other/actor)', message: { type: 'ping' } });

    // Refresh token
    await client.refreshToken(newToken);

    // Send message after refresh (connection still active)
    await client.send({ to: '@(other/actor)', message: { type: 'pong' } });

    expect(client.isConnected()).toBe(true);
  });

  it('should handle concurrent messages during refresh', async () => {
    const client = new SignalHubClient();
    await client.connect(token);

    // Start token refresh (async)
    const refreshPromise = client.refreshToken(newToken);

    // Send messages while refresh is in flight
    const sendPromises = [
      client.send({ to: '@(actor1)', message: { type: 'msg1' } }),
      client.send({ to: '@(actor2)', message: { type: 'msg2' } }),
      client.send({ to: '@(actor3)', message: { type: 'msg3' } })
    ];

    // All should succeed
    await Promise.all([refreshPromise, ...sendPromises]);
  });
});
```

---

## Monitoring and Metrics

### Key Metrics

| Metric | Description | Alert Threshold |
|--------|-------------|-----------------|
| `token_refresh_success_count` | Successful token refreshes | N/A |
| `token_refresh_failure_count` | Failed token refreshes | >10% of requests |
| `token_refresh_rate_limited_count` | Rate limited requests | >1% of requests |
| `token_refresh_latency_p95` | 95th percentile latency | >1s |
| `grace_period_messages_count` | Messages accepted during grace period | N/A |

### CloudWatch Logs

```typescript
// Log successful refresh
console.log({
  type: 'token_refresh',
  status: 'success',
  sessionId: session.sessionId,
  actorId: session.verifiedIdentity,
  oldExpiry: session.previousTokenExp,
  newExpiry: session.tokenExpiresAt,
  timestamp: Date.now()
});

// Log refresh failure
console.error({
  type: 'token_refresh',
  status: 'failure',
  sessionId: session.sessionId,
  actorId: session.verifiedIdentity,
  reason: err.message,
  timestamp: Date.now()
});
```

---

## Production Checklist

- [ ] Token refresh message types added to MESSAGE_TYPES.md
- [ ] Security documentation updated in SECURITY.md
- [ ] Client implementation with retry logic
- [ ] Server implementation with rate limiting
- [ ] Grace period handling implemented
- [ ] Actor identity validation enforced
- [ ] Nonce replay prevention enabled
- [ ] Unit tests passing (100% coverage)
- [ ] Integration tests passing
- [ ] Monitoring and alerting configured
- [ ] Documentation reviewed and approved

---

**Status:** Ready for implementation (MVP - Phase 1)
**Priority:** HIGH (P1)
**Blocks:** Production deployment with 1-4 hour token expiry
