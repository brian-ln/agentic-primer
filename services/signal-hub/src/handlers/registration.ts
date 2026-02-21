/**
 * Actor Registration Handlers
 *
 * Handles: hub:register, hub:unregister, hub:discover, hub:list_actors, hub:renew
 */

import type {
  SharedMessage,
  ActorRegistration,
  CanonicalAddress,
  Env,
} from '../types';
import { HubError } from '../types';
import {
  createReply,
  toCanonicalAddress,
  generateRenewalToken,
  matchPattern,
  isExpired,
  log,
} from '../utils';

const SIGNAL_HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');

/**
 * Handle hub:register message
 */
export function handleRegister(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  connectionId: string,
  env: Env
): SharedMessage {
  const payload = msg.payload as {
    actorAddress: CanonicalAddress;
    capabilities: string[];
    metadata: Record<string, unknown>;
    ttlSeconds?: number;
  };

  // Validate payload
  if (!payload.actorAddress || typeof payload.actorAddress !== 'string') {
    throw new HubError('internal_error', 'actorAddress is required');
  }

  if (!Array.isArray(payload.capabilities) || payload.capabilities.length === 0) {
    throw new HubError('internal_error', 'capabilities must be a non-empty array');
  }

  // Calculate TTL
  const defaultTTL = parseInt(env.DEFAULT_ACTOR_TTL, 10);
  const maxTTL = parseInt(env.MAX_ACTOR_TTL, 10);
  const requestedTTL = (payload.ttlSeconds ?? defaultTTL / 1000) * 1000; // Convert to ms
  const ttl = Math.min(requestedTTL, maxTTL);

  const registeredAt = Date.now();
  const expiresAt = registeredAt + ttl;

  // Check registry capacity (50K actor limit due to Durable Object constraints)
  const registryLimit = parseInt(env.ACTOR_REGISTRY_LIMIT, 10);
  if (registry.size >= registryLimit && !registry.has(payload.actorAddress)) {
    throw new HubError(
      'rate_limited',
      `Registry at capacity (${registry.size}/${registryLimit} actors). Cannot register new actors. Existing actors can still renew registrations.`,
      {
        currentSize: registry.size,
        limit: registryLimit,
        action: 'retry_later_or_use_different_hub',
      }
    );
  }

  // Get existing version or start at 1
  const existing = registry.get(payload.actorAddress);
  const version = existing ? existing.version + 1 : 1;

  // Generate renewal token
  const renewalToken = generateRenewalToken();

  // Create registration
  const registration: ActorRegistration = {
    actorAddress: payload.actorAddress,
    capabilities: payload.capabilities,
    metadata: payload.metadata ?? {},
    connectionId,
    registeredAt,
    expiresAt,
    version,
    renewalToken,
  };

  // Store in registry (last-write-wins)
  registry.set(payload.actorAddress, registration);

  log(env, 'actor_registered', {
    actorAddress: payload.actorAddress,
    version,
    capabilities: payload.capabilities,
    registrySize: registry.size,
  });

  // Send hub:registered response
  const responsePayload = {
    actorAddress: payload.actorAddress,
    expiresAt,
    renewalToken,
    version,
  };

  return createReply('hub:registered', responsePayload, msg, SIGNAL_HUB_ADDRESS);
}

/**
 * Handle hub:unregister message
 */
export function handleUnregister(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  env: Env
): void {
  const payload = msg.payload as { actorAddress: CanonicalAddress };

  if (!payload.actorAddress) {
    throw new HubError('internal_error', 'actorAddress is required');
  }

  const deleted = registry.delete(payload.actorAddress);
  if (deleted) {
    log(env, 'actor_unregistered', { actorAddress: payload.actorAddress });
  }
}

/**
 * Handle hub:discover message
 */
export function handleDiscover(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  env: Env
): SharedMessage {
  const payload = msg.payload as {
    pattern: string;
    capability?: string;
    limit?: number;
  };

  if (!payload.pattern || typeof payload.pattern !== 'string') {
    throw new HubError('internal_error', 'pattern is required');
  }

  const limit = Math.min(payload.limit ?? 100, 100);

  log(env, 'handle_discover', { pattern: payload.pattern, capability: payload.capability, registrySize: registry.size });

  // Filter actors matching pattern and capability
  const matches: ActorRegistration[] = [];
  for (const [address, registration] of registry.entries()) {
    // Skip expired registrations
    if (isExpired(registration.expiresAt)) {
      registry.delete(address); // Cleanup
      continue;
    }

    // Test pattern match
    if (!matchPattern(payload.pattern, registration.actorAddress)) {
      continue;
    }

    // Test capability match (if capability filter provided)
    if (payload.capability && !registration.capabilities.includes(payload.capability)) {
      continue;
    }

    matches.push(registration);

    if (matches.length >= limit) {
      break;
    }
  }

  log(env, 'discover_results', { matchCount: matches.length, hasMore: matches.length === limit });

  // Map to client-expected format: { address, capabilities }
  const actors = matches.map(registration => ({
    address: registration.actorAddress,
    capabilities: registration.capabilities,
  }));

  const responsePayload = {
    actors,
    hasMore: matches.length === limit,
  };

  return createReply('hub:discovered', responsePayload, msg, SIGNAL_HUB_ADDRESS);
}

/**
 * Handle hub:list_actors message
 */
export function handleListActors(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>
): SharedMessage {
  const payload = msg.payload as {
    offset?: number;
    limit?: number;
  };

  const offset = Math.max(payload.offset ?? 0, 0);
  const limit = Math.min(payload.limit ?? 100, 100);

  // Cleanup expired registrations
  for (const [address, registration] of registry.entries()) {
    if (isExpired(registration.expiresAt)) {
      registry.delete(address);
    }
  }

  // Convert to array and paginate
  const allActors = Array.from(registry.values());
  const total = allActors.length;
  const actors = allActors.slice(offset, offset + limit);

  const responsePayload = {
    actors,
    total,
    offset,
    limit,
    hasMore: offset + limit < total,
  };

  return createReply('hub:actor_list', responsePayload, msg, SIGNAL_HUB_ADDRESS);
}

/**
 * Handle hub:renew message
 */
export function handleRenew(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  env: Env
): SharedMessage {
  const payload = msg.payload as { renewalToken: string };

  if (!payload.renewalToken || typeof payload.renewalToken !== 'string') {
    throw new HubError('internal_error', 'renewalToken is required');
  }

  // Find registration by renewal token
  let targetActor: ActorRegistration | null = null;
  for (const registration of registry.values()) {
    if (registration.renewalToken === payload.renewalToken) {
      targetActor = registration;
      break;
    }
  }

  if (!targetActor) {
    throw new HubError('unauthorized', 'Invalid or expired renewal token');
  }

  // Calculate new expiration
  const defaultTTL = parseInt(env.DEFAULT_ACTOR_TTL, 10);
  const newExpiresAt = Date.now() + defaultTTL;

  // Generate new renewal token
  const newRenewalToken = generateRenewalToken();

  // Update registration
  targetActor.expiresAt = newExpiresAt;
  targetActor.renewalToken = newRenewalToken;
  targetActor.version += 1;

  registry.set(targetActor.actorAddress, targetActor);

  log(env, 'actor_renewed', {
    actorAddress: targetActor.actorAddress,
    version: targetActor.version,
    expiresAt: newExpiresAt,
  });

  // Send hub:renewed response
  const responsePayload = {
    expiresAt: newExpiresAt,
    renewalToken: newRenewalToken,
  };

  return createReply('hub:renewed', responsePayload, msg, SIGNAL_HUB_ADDRESS);
}
