/**
 * @agentic-primer/auth-client
 *
 * Device flow OAuth client for brianln.ai auth service.
 * Use in Node/Bun processes — NOT in Cloudflare Workers.
 *
 * @example
 *   import { getToken, createAuthHeaders } from '@agentic-primer/auth-client';
 *
 *   // Get a Bearer token (runs device flow if not cached)
 *   const token = await getToken({ clientId: 'brainstorm-daemon', scopes: ['write'] });
 *
 *   // Or get ready-to-use fetch headers
 *   const headers = await createAuthHeaders();
 *   await fetch('https://brianln.ai/...', { method: 'POST', headers });
 */

export { getToken, getCachedToken, clearToken } from './get-token.js';
export { runDeviceFlow } from './device-flow.js';
export type { StoredToken, DeviceAuthResponse, TokenResponse, GetTokenOptions } from './types.js';

import { getToken } from './get-token.js';
import type { GetTokenOptions } from './types.js';

/**
 * Create Authorization + Content-Type headers suitable for a fetch() call.
 * Calls getToken() internally; triggers device flow if no valid token is cached.
 */
export async function createAuthHeaders(
  options?: GetTokenOptions
): Promise<Record<string, string>> {
  const token = await getToken(options);
  return {
    Authorization: `Bearer ${token}`,
    'Content-Type': 'application/json',
  };
}
