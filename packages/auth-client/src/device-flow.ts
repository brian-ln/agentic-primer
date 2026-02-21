/**
 * device-flow.ts
 *
 * Implements the OAuth 2.0 Device Authorization Grant (RFC 8628).
 *
 * POST /device        → get device_code + user_code
 * Print to stderr     → user visits verification_uri and enters user_code
 * Poll /device/token  → wait for approval or expiry
 */

import type { DeviceAuthResponse, TokenApprovedResponse, TokenResponse } from './types.js';

const AUTH_BASE = 'https://brianln.ai';

/**
 * Initiate device flow, print instructions to stderr, and poll until the user
 * approves or the code expires.
 *
 * @returns The approved access_token string.
 * @throws  If the device code expires or a non-recoverable error occurs.
 */
export async function runDeviceFlow(clientId: string, scopes: string[]): Promise<string> {
  // Step 1: Request device code
  const deviceRes = await fetch(`${AUTH_BASE}/device`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: new URLSearchParams({
      client_id: clientId,
      scope: scopes.join(' '),
    }),
  });

  if (!deviceRes.ok) {
    const text = await deviceRes.text();
    throw new Error(`Device authorization request failed (${deviceRes.status}): ${text}`);
  }

  const device = await deviceRes.json() as DeviceAuthResponse;

  // Step 2: Print instructions to stderr (stdout must stay clean for piping)
  process.stderr.write(
    `\n[auth] Login required for ${clientId}\n` +
    `[auth] Visit: ${device.verification_uri}\n` +
    `[auth] Enter code: ${device.user_code}\n` +
    `[auth] Waiting for approval...\n`
  );

  // Step 3: Poll for token
  const pollIntervalMs = (device.interval ?? 5) * 1000;
  const expiresAt = Date.now() + device.expires_in * 1000;

  while (Date.now() < expiresAt) {
    await sleep(pollIntervalMs);

    const tokenRes = await fetch(`${AUTH_BASE}/device/token`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: new URLSearchParams({
        grant_type: 'urn:ietf:params:oauth:grant-type:device_code',
        device_code: device.device_code,
        client_id: clientId,
      }),
    });

    if (!tokenRes.ok) {
      // Treat non-2xx as a transient failure and keep polling unless we are past expiry
      continue;
    }

    const body = await tokenRes.json() as TokenResponse;

    if ('error' in body) {
      if (body.error === 'slow_down') {
        // Server asked us to back off — add an extra interval
        await sleep(pollIntervalMs);
        continue;
      }
      if (body.error === 'authorization_pending') {
        // Normal — keep waiting
        continue;
      }
      throw new Error(`Device token error: ${body.error}`);
    }

    // Approved!
    const approved = body as TokenApprovedResponse;
    process.stderr.write(`[auth] Approved! Token acquired for ${clientId}\n`);
    return approved.access_token;
  }

  throw new Error(`Device authorization timed out for ${clientId} — code expired`);
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
