/**
 * JWT Token Generation for Integration Tests
 *
 * Generates test JWTs for Signal Hub authentication.
 */

import * as jose from 'jose';
import type { CanonicalAddress } from '@agentic-primer/protocols';

export interface JWTClaims {
  sub: CanonicalAddress;
  capabilities: string[];
  exp?: number;
  iat?: number;
}

/**
 * Generate a test JWT with specified claims
 */
export async function generateTestJWT(
  claims: JWTClaims,
  secret: string = 'dev-secret-change-in-production'
): Promise<string> {
  const now = Math.floor(Date.now() / 1000);

  const payload = {
    sub: claims.sub,
    capabilities: claims.capabilities,
    iat: claims.iat ?? now,
    exp: claims.exp ?? now + 3600, // Default: 1 hour
  };

  const secretKey = new TextEncoder().encode(secret);

  const jwt = await new jose.SignJWT(payload)
    .setProtectedHeader({ alg: 'HS256' })
    .sign(secretKey);

  return jwt;
}

/**
 * Generate JWT for SEAG actor
 */
export async function generateSeagActorJWT(
  actorAddress: CanonicalAddress = '@(local/test-seag-actor)' as CanonicalAddress,
  capabilities: string[] = ['compute', 'inference']
): Promise<string> {
  return generateTestJWT({
    sub: actorAddress,
    capabilities,
  });
}

/**
 * Generate JWT for browser actor
 */
export async function generateBrowserActorJWT(
  actorAddress: CanonicalAddress = '@(browser/test-browser-actor)' as CanonicalAddress,
  capabilities: string[] = ['ui', 'interaction']
): Promise<string> {
  return generateTestJWT({
    sub: actorAddress,
    capabilities,
  });
}

/**
 * Generate expired JWT (for testing authentication errors)
 */
export async function generateExpiredJWT(
  actorAddress: CanonicalAddress = '@(local/test-actor)' as CanonicalAddress
): Promise<string> {
  const now = Math.floor(Date.now() / 1000);

  return generateTestJWT({
    sub: actorAddress,
    capabilities: ['test'],
    iat: now - 7200, // 2 hours ago
    exp: now - 3600, // Expired 1 hour ago
  });
}
