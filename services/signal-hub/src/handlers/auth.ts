/**
 * Authentication Handlers
 *
 * Handles: hub:refresh_token
 */

import type { SharedMessage, Session, Env } from '../types';
import { HubError } from '../types';
import { createReply, toCanonicalAddress } from '../utils';
import { validateJWT, createJWT } from '../auth/jwt';

const SIGNAL_HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');

/** Default token TTL for refreshed tokens: 1 hour */
const REFRESHED_TOKEN_TTL_SECONDS = 3600;

/** Session token prefix used in AUTH_ENABLED=false mode */
const SESSION_TOKEN_PREFIX = 'stkn-';

/**
 * Parse a session token (AUTH_ENABLED=false mode).
 *
 * Session tokens encode `<sessionId>:<issuedAt>:<expiresAt>` in base64.
 * Returns null if the string is not a valid session token.
 */
function parseSessionToken(
  token: string
): { sessionId: string; issuedAt: number; expiresAt: number } | null {
  if (!token.startsWith(SESSION_TOKEN_PREFIX)) return null;
  try {
    const encoded = token.slice(SESSION_TOKEN_PREFIX.length);
    const decoded = atob(encoded);
    const [sessionId, issuedAtStr, expiresAtStr] = decoded.split(':');
    if (!sessionId || !issuedAtStr || !expiresAtStr) return null;
    return {
      sessionId,
      issuedAt: parseInt(issuedAtStr, 10),
      expiresAt: parseInt(expiresAtStr, 10),
    };
  } catch {
    return null;
  }
}

/**
 * Create a session token (AUTH_ENABLED=false mode).
 *
 * Encodes `<sessionId>:<issuedAt>:<expiresAt>` in base64.
 */
function createSessionToken(sessionId: string, ttlMs: number): string {
  const issuedAt = Date.now();
  const expiresAt = issuedAt + ttlMs;
  const payload = `${sessionId}:${issuedAt}:${expiresAt}`;
  return SESSION_TOKEN_PREFIX + btoa(payload);
}

/**
 * Handle hub:refresh_token message
 *
 * Accepts a token (JWT when AUTH_ENABLED=true, session token otherwise),
 * validates it, and issues a fresh token of the same kind.
 *
 * Responds with:
 * - hub:token_refreshed on success: { token, expiresAt }
 * - hub:error code invalid_token on validation failure
 *
 * @param msg     - Incoming SharedMessage with payload { token: string }
 * @param session - Current session state
 * @param env     - Environment bindings
 * @returns hub:token_refreshed response message
 * @throws HubError('invalid_token') when the supplied token is invalid or expired
 */
export async function handleRefreshToken(
  msg: SharedMessage,
  session: Session,
  env: Env
): Promise<SharedMessage> {
  const payload = msg.payload as Record<string, unknown>;

  if (!payload || typeof payload.token !== 'string' || !payload.token) {
    throw new HubError('invalid_token', 'hub:refresh_token requires payload { token: string }');
  }

  const incomingToken = payload.token;
  const authEnabled = env.AUTH_ENABLED === 'true';
  const ttlMs = REFRESHED_TOKEN_TTL_SECONDS * 1000;

  let newToken: string;
  let expiresAt: number;

  if (authEnabled) {
    // ── JWT mode ─────────────────────────────────────────────────────────────
    if (!env.JWT_SECRET) {
      throw new HubError('internal_error', 'JWT_SECRET not configured');
    }

    // Validate existing JWT (throws HubError('unauthorized') on failure)
    let identity;
    try {
      identity = await validateJWT(incomingToken, env.JWT_SECRET);
    } catch (err) {
      // Map jwt validation errors to invalid_token for refresh context
      throw new HubError(
        'invalid_token',
        err instanceof Error ? err.message : 'Token validation failed'
      );
    }

    // Issue fresh JWT with same claims
    expiresAt = Date.now() + ttlMs;
    newToken = await createJWT(
      identity.actorId,
      identity.userId,
      identity.capabilities,
      env.JWT_SECRET,
      `${REFRESHED_TOKEN_TTL_SECONDS}s`
    );
  } else {
    // ── Session token mode (AUTH_ENABLED=false) ───────────────────────────────
    const parsed = parseSessionToken(incomingToken);

    if (!parsed) {
      throw new HubError(
        'invalid_token',
        'Token is not a valid session token'
      );
    }

    if (Date.now() >= parsed.expiresAt) {
      throw new HubError(
        'invalid_token',
        'Session token has expired'
      );
    }

    // Verify the token belongs to the requesting session
    if (parsed.sessionId !== session.sessionId) {
      throw new HubError(
        'invalid_token',
        'Token does not belong to this session'
      );
    }

    expiresAt = Date.now() + ttlMs;
    newToken = createSessionToken(session.sessionId, ttlMs);
  }

  const responsePayload = {
    token: newToken,
    expiresAt,
  };

  return createReply('hub:token_refreshed', responsePayload, msg, SIGNAL_HUB_ADDRESS);
}
