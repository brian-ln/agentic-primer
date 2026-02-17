/**
 * JWT Authentication using jose library
 *
 * Uses Web Crypto API compatible jose library for JWT validation
 * in Cloudflare Workers environment.
 */

import * as jose from 'jose';
import type { ActorIdentity, JWTPayload } from '../types';
import { HubError } from '../types';

/**
 * Validate JWT token and extract actor identity
 *
 * @param authToken - JWT token with optional "bearer " prefix
 * @param jwtSecret - Secret key for verification
 * @returns Validated actor identity
 * @throws HubError with code 'unauthorized' if validation fails
 */
export async function validateJWT(
  authToken: string,
  jwtSecret: string
): Promise<ActorIdentity> {
  // Remove "bearer " prefix (case-insensitive)
  const token = authToken.replace(/^bearer\s+/i, '');

  // Prepare secret key for jose (Web Crypto API)
  const secret = new TextEncoder().encode(jwtSecret);

  try {
    // Verify signature and decode payload
    const { payload } = await jose.jwtVerify(token, secret, {
      algorithms: ['HS256', 'RS256'],
      issuer: 'signal-hub',
      maxTokenAge: '24h',
    });

    // Extract claims
    const decoded = payload as unknown as JWTPayload;

    // Validate required fields
    if (!decoded.sub || !decoded.actorId) {
      throw new HubError(
        'unauthorized',
        'JWT missing required claims (sub, actorId)'
      );
    }

    // Extract actor identity
    return {
      actorId: decoded.actorId,
      userId: decoded.sub,
      capabilities: decoded.capabilities ?? [],
      expiresAt: (decoded.exp ?? 0) * 1000, // Convert to ms
    };
  } catch (err: unknown) {
    // Handle jose-specific errors
    if (err instanceof jose.errors.JWTExpired) {
      throw new HubError('unauthorized', 'JWT token has expired');
    }

    if (err instanceof jose.errors.JWSSignatureVerificationFailed) {
      throw new HubError('unauthorized', 'JWT signature verification failed');
    }

    if (err instanceof jose.errors.JWTClaimValidationFailed) {
      throw new HubError(
        'unauthorized',
        `JWT claim validation failed: ${(err as Error).message}`
      );
    }

    // Re-throw HubError as-is
    if (err instanceof HubError) {
      throw err;
    }

    // Generic error
    throw new HubError(
      'unauthorized',
      `JWT validation failed: ${err instanceof Error ? err.message : 'unknown error'}`
    );
  }
}

/**
 * Create a JWT token (for testing)
 *
 * @param actorId - Actor canonical address path (e.g., "browser/client-ui")
 * @param userId - User ID
 * @param capabilities - Actor capabilities
 * @param jwtSecret - Secret key for signing
 * @param expiresIn - Token expiration (default: 24h)
 * @returns Signed JWT token
 */
export async function createJWT(
  actorId: string,
  userId: string,
  capabilities: string[],
  jwtSecret: string,
  expiresIn: string = '24h'
): Promise<string> {
  const secret = new TextEncoder().encode(jwtSecret);

  const token = await new jose.SignJWT({
    actorId,
    capabilities,
  })
    .setProtectedHeader({ alg: 'HS256' })
    .setSubject(userId)
    .setIssuer('signal-hub')
    .setIssuedAt()
    .setExpirationTime(expiresIn)
    .sign(secret);

  return token;
}
