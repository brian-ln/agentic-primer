/**
 * Message Delivery Handlers
 *
 * Handles: hub:send, hub:broadcast
 */

import type {
  SharedMessage,
  ActorRegistration,
  CanonicalAddress,
  Env,
} from '../types';
import { HubError } from '../types';
import { createReply, createMessage, toCanonicalAddress, isExpired, log } from '../utils';

const SIGNAL_HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');

// Resource protection: maximum broadcast recipient count
const MAX_BROADCAST_RECIPIENTS = 1000;

/**
 * Handle hub:send message - point-to-point delivery
 *
 * CRITICAL: Uses flat payload structure per protocol spec (commit 63ed8e8, 12f3c41)
 *
 * Input format:
 * {
 *   to: '@(browser/widget-123)',
 *   type: 'hub:send',
 *   payload: {
 *     type: 'task:assign',      // Application type
 *     data: { taskId: '456' }   // Application data (NOT nested!)
 *   }
 * }
 *
 * Forwarded as:
 * {
 *   to: '@(browser/widget-123)',
 *   type: 'task:assign',         // From payload.type
 *   payload: { taskId: '456' }   // From payload.data
 * }
 */
export function handleSend(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  connections: Map<string, WebSocket>,
  env: Env,
  sendMessage: (ws: WebSocket, message: SharedMessage) => void
): SharedMessage | null {
  const payload = msg.payload as {
    to?: string;
    type: string;
    data: unknown;
  };

  // Validate payload structure
  if (!payload || typeof payload !== 'object') {
    throw new HubError('internal_error', 'payload must be an object');
  }

  if (!payload.type || typeof payload.type !== 'string') {
    throw new HubError('internal_error', 'payload.type is required');
  }

  // NOTE: Message size check now happens before JSON.parse in SignalHub.webSocketMessage()
  // This prevents DoS attacks from large payloads exhausting memory during parsing

  // CRITICAL: Target address is in payload.to, NOT msg.to!
  const targetAddress = (payload.to as CanonicalAddress) || msg.to;

  log(env, 'handle_send', {
    from: msg.from,
    targetAddress,
    payloadType: payload.type,
    registrySize: registry.size,
    connectionsSize: connections.size,
  });

  const targetActor = registry.get(targetAddress);

  if (!targetActor) {
    // Actor not registered
    if (msg.pattern === 'ask') {
      // Return error for ask pattern
      return createReply(
        'hub:unknown_actor',
        {
          actorAddress: targetAddress,
          message: 'Actor not registered with Signal Hub',
        },
        msg,
        SIGNAL_HUB_ADDRESS
      );
    }
    // For tell pattern, silently drop
    console.warn(`Target actor not found: ${targetAddress}`);
    return null;
  }

  // Check if registration expired
  if (isExpired(targetActor.expiresAt)) {
    registry.delete(targetAddress);

    if (msg.pattern === 'ask') {
      return createReply(
        'hub:unknown_actor',
        {
          actorAddress: targetAddress,
          message: 'Actor registration expired',
        },
        msg,
        SIGNAL_HUB_ADDRESS
      );
    }
    return null;
  }

  // Get WebSocket connection
  const ws = connections.get(targetActor.connectionId);
  if (!ws) {
    console.error('[handleSend] WebSocket not found for connection:', targetActor.connectionId);

    if (msg.pattern === 'ask') {
      return createReply(
        'hub:error',
        {
          code: 'internal_error',
          message: 'Target actor connection not found',
          retryable: false,
        },
        msg,
        SIGNAL_HUB_ADDRESS
      );
    }
    return null;
  }

  // Create forwarded message with flat structure
  // Extract type and data from payload, NOT nested!
  // CRITICAL: Use original sender as 'from', not SIGNAL_HUB_ADDRESS
  const forwardedMessage: SharedMessage = createMessage(
    payload.type, // Application type from payload.type
    payload.data, // Application data from payload.data
    msg.from, // Use original sender, NOT the hub
    targetAddress,
    'tell',
    {
      ...msg.metadata, // Preserve user metadata from original message
      forwarded: true,
      via: SIGNAL_HUB_ADDRESS,
      originalMessageId: msg.id,
    }
  );

  // Preserve TTL if set
  if (msg.ttl) {
    forwardedMessage.ttl = msg.ttl;
  }

  // Send to target
  try {
    sendMessage(ws, forwardedMessage);

    // Send delivery acknowledgment if requested
    if (msg.pattern === 'ask') {
      return createReply(
        'hub:delivery_ack',
        {
          messageId: msg.id,
          deliveredAt: Date.now(),
          status: 'delivered',
        },
        msg,
        SIGNAL_HUB_ADDRESS
      );
    }

    return null;
  } catch (err) {
    console.error('[handleSend] Failed to send message to', targetAddress, ':', err);

    if (msg.pattern === 'ask') {
      return createReply(
        'hub:error',
        {
          code: 'internal_error',
          message: 'Failed to deliver message to target actor',
          details: { error: err instanceof Error ? err.message : 'unknown' },
          retryable: true,
        },
        msg,
        SIGNAL_HUB_ADDRESS
      );
    }

    return null;
  }
}

/**
 * Handle hub:broadcast message - fan-out to all registered actors
 *
 * Uses flat payload structure:
 * {
 *   type: 'hub:broadcast',
 *   payload: {
 *     type: 'system:shutdown',   // Application type
 *     data: { reason: 'maintenance' }  // Application data
 *   }
 * }
 */
export function handleBroadcast(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  connections: Map<string, WebSocket>,
  env: Env,
  sendMessage: (ws: WebSocket, message: SharedMessage) => void
): SharedMessage {
  const payload = msg.payload as {
    type: string;
    data: unknown;
    excludeSelf?: boolean;
  };

  // Validate payload
  if (!payload || typeof payload !== 'object') {
    throw new HubError('internal_error', 'payload must be an object');
  }

  if (!payload.type || typeof payload.type !== 'string') {
    throw new HubError('internal_error', 'payload.type is required');
  }

  const excludeSelf = payload.excludeSelf ?? false;
  const targetCapability = msg.metadata?.targetCapability as string | undefined;

  const syncThreshold = parseInt(env.BROADCAST_SYNC_THRESHOLD, 10);

  // Collect target actors
  const targets: ActorRegistration[] = [];

  for (const [address, registration] of registry.entries()) {
    // Skip expired
    if (isExpired(registration.expiresAt)) {
      registry.delete(address);
      continue;
    }

    // Skip self if excludeSelf
    if (excludeSelf && address === msg.from) {
      continue;
    }

    // Filter by capability if specified
    if (
      targetCapability &&
      !registration.capabilities.includes(targetCapability)
    ) {
      continue;
    }

    targets.push(registration);
  }

  // For MVP, only support synchronous broadcast
  // (Async queue implementation deferred to Phase 2)
  if (targets.length > syncThreshold) {
    console.warn(
      `Broadcast to ${targets.length} actors exceeds sync threshold (${syncThreshold}). Consider implementing async queue.`
    );
  }

  log(env, 'handle_broadcast', { from: msg.from, payloadType: payload.type, targetCount: targets.length });

  let deliveredCount = 0;
  let failedCount = 0;

  // Create broadcast message with flat structure
  // CRITICAL: Use original sender as 'from', not SIGNAL_HUB_ADDRESS
  const broadcastMessage: SharedMessage = {
    id: crypto.randomUUID(),
    from: msg.from, // Use original sender, NOT the hub
    to: toCanonicalAddress('broadcast/all'), // Placeholder
    type: payload.type, // Application type from payload.type
    payload: payload.data, // Application data from payload.data
    pattern: 'tell',
    correlationId: msg.id,
    timestamp: Date.now(),
    metadata: {
      broadcast: true,
      via: SIGNAL_HUB_ADDRESS,
    },
    ttl: msg.ttl ?? null,
    signature: null,
  };

  // Send to all targets
  for (const target of targets) {
    const ws = connections.get(target.connectionId);
    if (!ws) {
      failedCount++;
      continue;
    }

    try {
      // Update 'to' field for each recipient
      const recipientMessage = {
        ...broadcastMessage,
        id: crypto.randomUUID(), // New ID per recipient
        to: target.actorAddress,
      };

      sendMessage(ws, recipientMessage);
      deliveredCount++;
    } catch (err) {
      console.error('[handleBroadcast] Failed to broadcast to', target.actorAddress, ':', err);
      failedCount++;
    }
  }

  // Send broadcast acknowledgment
  const ackPayload = {
    messageId: msg.id,
    deliveredCount,
    failedCount,
  };

  return createReply('hub:broadcast_ack', ackPayload, msg, SIGNAL_HUB_ADDRESS);
}
