/**
 * get-token.ts
 *
 * Main entrypoint for obtaining a valid Bearer token.
 *
 * Algorithm:
 *   1. Check ~/.config/bln/auth-<clientId>.json
 *   2. If token exists and not expiring within 60s, return it
 *   3. Otherwise, run device flow and persist the new token
 */

import type { GetTokenOptions, StoredToken } from './types.js';
import { deleteToken, readToken, writeToken } from './token-store.js';
import { runDeviceFlow } from './device-flow.js';

const DEFAULT_CLIENT_ID = 'brainstorm-daemon';
const DEFAULT_SCOPES = ['write'];
const EXPIRY_BUFFER_SECONDS = 60;

/**
 * Returns a valid access token, running device flow if needed.
 */
export async function getToken(options?: GetTokenOptions): Promise<string> {
  const clientId = options?.clientId ?? DEFAULT_CLIENT_ID;
  const scopes = options?.scopes ?? DEFAULT_SCOPES;

  const stored = readToken(clientId);

  if (stored && isTokenValid(stored)) {
    return stored.access_token;
  }

  // Token missing or expired — initiate device flow
  const access_token = await runDeviceFlow(clientId, scopes);

  // We don't have expires_in from the poll response at this point;
  // default to 1 hour (the auth service's typical token lifetime).
  const expires_at = Math.floor(Date.now() / 1000) + 3600;

  const newToken: StoredToken = {
    access_token,
    expires_at,
    scope: scopes.join(' '),
    client_id: clientId,
  };
  writeToken(newToken);

  return access_token;
}

/**
 * Returns true if the stored token is present and not expiring within the buffer window.
 */
function isTokenValid(token: StoredToken): boolean {
  const nowSeconds = Math.floor(Date.now() / 1000);
  return token.expires_at > nowSeconds + EXPIRY_BUFFER_SECONDS;
}

/**
 * Read cached token without triggering device flow. Returns null if missing or expired.
 */
export function getCachedToken(clientId?: string): StoredToken | null {
  const id = clientId ?? DEFAULT_CLIENT_ID;
  const stored = readToken(id);
  if (!stored || !isTokenValid(stored)) return null;
  return stored;
}

/**
 * Clear stored token for clientId (force re-login on next getToken call).
 */
export function clearToken(clientId?: string): void {
  const id = clientId ?? DEFAULT_CLIENT_ID;
  deleteToken(id);
}
