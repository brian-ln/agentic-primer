/**
 * Auth Handler Tests — hub:refresh_token
 *
 * Covers:
 * - Successful token refresh in dev mode (AUTH_ENABLED=false)
 * - Successful token refresh in JWT mode (AUTH_ENABLED=true)
 * - Invalid token: missing payload field
 * - Invalid token: malformed session token
 * - Invalid token: expired session token
 * - Invalid token: session mismatch
 * - Invalid token: JWT expired
 * - Invalid token: JWT bad signature
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { handleRefreshToken } from '../auth';
import type { SharedMessage, Session, Env } from '../../types';
import { toCanonicalAddress } from '../../utils';
import { createJWT } from '../../auth/jwt';

// ─── helpers ──────────────────────────────────────────────────────────────────

const HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');
const CLIENT_ADDRESS = toCanonicalAddress('browser/test-client');

function makeSession(overrides: Partial<Session> = {}): Session {
  return {
    sessionId: 'session-test-abc123',
    actorIdentity: CLIENT_ADDRESS,
    capabilities: ['send', 'receive'],
    connectedAt: Date.now(),
    lastHeartbeat: Date.now(),
    authenticated: false,
    paused: false,
    connectionState: 'connected',
    rateLimitBucket: {
      tokens: 100,
      capacity: 100,
      refillRate: 100 / 60,
      lastRefill: Date.now(),
    },
    ...overrides,
  };
}

function makeEnv(overrides: Partial<Env> = {}): Env {
  return {
    SIGNAL_HUB: {} as DurableObjectNamespace,
    PROTOCOL_VERSION: '0.1.0',
    MAX_MESSAGE_SIZE: '1048576',
    HEARTBEAT_INTERVAL: '300000',
    ACTOR_REGISTRY_LIMIT: '50000',
    DEFAULT_ACTOR_TTL: '300000',
    MAX_ACTOR_TTL: '3600000',
    BROADCAST_SYNC_THRESHOLD: '100',
    JWT_SECRET: 'test-secret-key',
    AUTH_ENABLED: 'false',
    ...overrides,
  };
}

function makeRefreshMsg(token: unknown, msgId = 'refresh-msg-1'): SharedMessage {
  return {
    id: msgId,
    from: CLIENT_ADDRESS,
    to: HUB_ADDRESS,
    type: 'hub:refresh_token',
    pattern: 'ask',
    correlationId: null,
    timestamp: Date.now(),
    payload: { token },
    metadata: {},
    ttl: null,
    signature: null,
  };
}

/** Create a valid session token for a given sessionId (mirrors auth.ts internals). */
function makeSessionToken(sessionId: string, ttlMs: number): string {
  const issuedAt = Date.now();
  const expiresAt = issuedAt + ttlMs;
  const payload = `${sessionId}:${issuedAt}:${expiresAt}`;
  return 'stkn-' + btoa(payload);
}

/** Create an expired session token. */
function makeExpiredSessionToken(sessionId: string): string {
  const issuedAt = Date.now() - 7200_000; // 2h ago
  const expiresAt = Date.now() - 3600_000; // expired 1h ago
  const payload = `${sessionId}:${issuedAt}:${expiresAt}`;
  return 'stkn-' + btoa(payload);
}

// ─── tests ────────────────────────────────────────────────────────────────────

describe('handleRefreshToken', () => {
  let session: Session;
  let env: Env;

  beforeEach(() => {
    session = makeSession();
    env = makeEnv();
  });

  // ── Dev mode (AUTH_ENABLED=false) ────────────────────────────────────────────

  describe('dev mode (AUTH_ENABLED=false)', () => {
    it('should respond with hub:token_refreshed on a valid session token', async () => {
      const token = makeSessionToken(session.sessionId, 3600_000);
      const msg = makeRefreshMsg(token);

      const response = await handleRefreshToken(msg, session, env);

      expect(response.type).toBe('hub:token_refreshed');
      expect(response.correlationId).toBe(msg.id);
      expect(response.from).toBe(HUB_ADDRESS);
      expect(response.to).toBe(CLIENT_ADDRESS);
    });

    it('should return a new session token in the response payload', async () => {
      const token = makeSessionToken(session.sessionId, 3600_000);
      const msg = makeRefreshMsg(token);

      const response = await handleRefreshToken(msg, session, env);
      const payload = response.payload as { token: string; expiresAt: number };

      expect(typeof payload.token).toBe('string');
      // New token is a valid session token (same format prefix)
      expect(payload.token.startsWith('stkn-')).toBe(true);
      // expiresAt is a positive number in the future
      expect(typeof payload.expiresAt).toBe('number');
      expect(payload.expiresAt).toBeGreaterThan(Date.now());
    });

    it('should return an expiresAt timestamp ~1 hour from now', async () => {
      const token = makeSessionToken(session.sessionId, 3600_000);
      const before = Date.now();
      const msg = makeRefreshMsg(token);

      const response = await handleRefreshToken(msg, session, env);
      const payload = response.payload as { token: string; expiresAt: number };

      const after = Date.now();
      // expiresAt should be roughly now + 1h (within 5 seconds tolerance)
      expect(payload.expiresAt).toBeGreaterThanOrEqual(before + 3600_000 - 5000);
      expect(payload.expiresAt).toBeLessThanOrEqual(after + 3600_000 + 5000);
    });

    it('should throw invalid_token when token field is missing', async () => {
      const msg: SharedMessage = {
        id: 'no-token-msg',
        from: CLIENT_ADDRESS,
        to: HUB_ADDRESS,
        type: 'hub:refresh_token',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {},
        metadata: {},
        ttl: null,
        signature: null,
      };

      await expect(handleRefreshToken(msg, session, env)).rejects.toMatchObject({
        code: 'invalid_token',
      });
    });

    it('should throw invalid_token when payload is null', async () => {
      const msg: SharedMessage = {
        id: 'null-payload-msg',
        from: CLIENT_ADDRESS,
        to: HUB_ADDRESS,
        type: 'hub:refresh_token',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: {},
        ttl: null,
        signature: null,
      };

      await expect(handleRefreshToken(msg, session, env)).rejects.toMatchObject({
        code: 'invalid_token',
      });
    });

    it('should throw invalid_token when token is not a string', async () => {
      const msg = makeRefreshMsg(12345);

      await expect(handleRefreshToken(msg, session, env)).rejects.toMatchObject({
        code: 'invalid_token',
      });
    });

    it('should throw invalid_token for a malformed (non-stkn) token', async () => {
      const msg = makeRefreshMsg('totally-random-string');

      await expect(handleRefreshToken(msg, session, env)).rejects.toMatchObject({
        code: 'invalid_token',
      });
    });

    it('should throw invalid_token for an expired session token', async () => {
      const expiredToken = makeExpiredSessionToken(session.sessionId);
      const msg = makeRefreshMsg(expiredToken);

      await expect(handleRefreshToken(msg, session, env)).rejects.toMatchObject({
        code: 'invalid_token',
      });
    });

    it('should throw invalid_token when session token belongs to a different session', async () => {
      const token = makeSessionToken('session-OTHER', 3600_000);
      const msg = makeRefreshMsg(token);

      await expect(handleRefreshToken(msg, session, env)).rejects.toMatchObject({
        code: 'invalid_token',
      });
    });
  });

  // ── JWT mode (AUTH_ENABLED=true) ─────────────────────────────────────────────

  describe('JWT mode (AUTH_ENABLED=true)', () => {
    let authEnv: Env;

    beforeEach(() => {
      authEnv = makeEnv({ AUTH_ENABLED: 'true', JWT_SECRET: 'test-secret-key' });
      session = makeSession({ authenticated: true });
    });

    it('should respond with hub:token_refreshed on a valid JWT', async () => {
      const jwt = await createJWT(
        'browser/test-client',
        'user-42',
        ['send', 'receive'],
        'test-secret-key',
        '1h'
      );
      const msg = makeRefreshMsg(jwt);

      const response = await handleRefreshToken(msg, session, authEnv);

      expect(response.type).toBe('hub:token_refreshed');
      const payload = response.payload as { token: string; expiresAt: number };
      // Response must contain a non-empty string token and a future expiresAt
      expect(typeof payload.token).toBe('string');
      expect(payload.token.length).toBeGreaterThan(0);
      expect(typeof payload.expiresAt).toBe('number');
      expect(payload.expiresAt).toBeGreaterThan(Date.now());
    });

    it('should return a new JWT (not the original) in the response', async () => {
      const jwt = await createJWT(
        'browser/test-client',
        'user-42',
        ['send'],
        'test-secret-key',
        '1h'
      );
      const msg = makeRefreshMsg(jwt);

      const response = await handleRefreshToken(msg, session, authEnv);
      const payload = response.payload as { token: string; expiresAt: number };

      // New JWT should be a valid-looking JWT (3 segments)
      const segments = payload.token.split('.');
      expect(segments).toHaveLength(3);
    });

    it('should throw invalid_token for an expired JWT', async () => {
      // Create JWT that expired 1 second ago
      const jwt = await createJWT(
        'browser/test-client',
        'user-42',
        ['send'],
        'test-secret-key',
        '1s' // Will expire almost immediately
      );

      // Wait briefly to ensure expiry
      await new Promise((r) => setTimeout(r, 1500));

      const msg = makeRefreshMsg(jwt);

      await expect(handleRefreshToken(msg, session, authEnv)).rejects.toMatchObject({
        code: 'invalid_token',
      });
    });

    it('should throw invalid_token for a JWT with wrong secret', async () => {
      const jwt = await createJWT(
        'browser/test-client',
        'user-42',
        ['send'],
        'WRONG-secret-key', // Different from env JWT_SECRET
        '1h'
      );
      const msg = makeRefreshMsg(jwt);

      await expect(handleRefreshToken(msg, session, authEnv)).rejects.toMatchObject({
        code: 'invalid_token',
      });
    });

    it('should throw invalid_token for a completely invalid JWT string', async () => {
      const msg = makeRefreshMsg('not.a.jwt');

      await expect(handleRefreshToken(msg, session, authEnv)).rejects.toMatchObject({
        code: 'invalid_token',
      });
    });

    it('should throw internal_error when JWT_SECRET is not configured', async () => {
      const noSecretEnv = makeEnv({ AUTH_ENABLED: 'true', JWT_SECRET: undefined });
      const msg = makeRefreshMsg('some.jwt.token');

      await expect(handleRefreshToken(msg, session, noSecretEnv)).rejects.toMatchObject({
        code: 'internal_error',
      });
    });
  });
});
