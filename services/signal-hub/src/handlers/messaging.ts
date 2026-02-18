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

  // ADDRESS RESOLUTION: The registry is a Map<CanonicalAddress, ActorRegistration> — one entry
  // per actor address. When the same actor address re-registers (e.g., reconnect), handleRegister
  // overwrites the previous entry (last-write-wins). This means an actor address always resolves
  // to exactly one session: the most recent registration. There is no ambiguity by design.
  // If an old session races with a new one, handleDuplicateConnection() in SignalHub.ts closes
  // the old session before the new registration is stored, ensuring a clean handoff.
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
          resolution: 'Try retrying the request; if the error recurs contact the hub operator.',
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
          resolution: 'Try retrying the request; if the error recurs contact the hub operator.',
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
 *
 * Performance optimization: the invariant message body (including the
 * potentially large payload.data) is serialized to JSON exactly once.
 * Per-recipient fields (id, to) are prepended as a small prefix string,
 * keeping serialization cost O(1) regardless of recipient count.
 *
 * When sendRaw is provided (production path), the pre-built JSON string is
 * sent directly to each WebSocket without re-serialization. When only
 * sendMessage is provided (test/fallback path), a standard SharedMessage
 * object is passed per recipient.
 */
export function handleBroadcast(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  connections: Map<string, WebSocket>,
  env: Env,
  sendMessage: (ws: WebSocket, message: SharedMessage) => void,
  sendRaw?: (ws: WebSocket, raw: string) => void
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

  // Resource protection: enforce maximum broadcast recipient limit
  if (targets.length > MAX_BROADCAST_RECIPIENTS) {
    console.error(
      `[handleBroadcast] Broadcast rejected: ${targets.length} recipients exceeds max (${MAX_BROADCAST_RECIPIENTS})`
    );
    return createReply(
      'hub:error',
      {
        code: 'BROADCAST_LIMIT_EXCEEDED',
        message: `Broadcast recipient count (${targets.length}) exceeds maximum allowed (${MAX_BROADCAST_RECIPIENTS})`,
        resolution: 'Try narrowing the broadcast target using capability filters to reduce the recipient count.',
      },
      msg,
      SIGNAL_HUB_ADDRESS
    );
  }

  let deliveredCount = 0;
  let failedCount = 0;

  // Create broadcast message template with flat structure.
  // CRITICAL: Use original sender as 'from', not SIGNAL_HUB_ADDRESS.
  const timestamp = Date.now();
  const broadcastMessage: SharedMessage = {
    id: crypto.randomUUID(), // placeholder — overridden per recipient
    from: msg.from,
    to: toCanonicalAddress('broadcast/all'), // placeholder — overridden per recipient
    type: payload.type,
    payload: payload.data,
    pattern: 'tell',
    correlationId: msg.id,
    timestamp,
    metadata: {
      broadcast: true,
      via: SIGNAL_HUB_ADDRESS,
    },
    ttl: msg.ttl ?? null,
    signature: null,
  };

  if (sendRaw) {
    // --- Stringify-once fast path ---
    // Serialize the invariant body (all fields except id and to) exactly once.
    // The JSON object is built with id/to as the first two keys so we can
    // replace only the leading portion for each recipient via string prefix.
    //
    // Serialized layout:
    //   {"id":"<uuid>","to":"<addr>","from":...,"type":...,"payload":...,...}
    //                               ^--- suffix is invariant for all recipients
    //
    // We pre-serialize the suffix (everything after the variant id+to prefix)
    // so that payload.data — which may be large — is only JSON.stringified once.
    const invariantSuffix = JSON.stringify({
      from: broadcastMessage.from,
      type: broadcastMessage.type,
      payload: broadcastMessage.payload,
      pattern: broadcastMessage.pattern,
      correlationId: broadcastMessage.correlationId,
      timestamp: broadcastMessage.timestamp,
      metadata: broadcastMessage.metadata,
      ttl: broadcastMessage.ttl,
      signature: broadcastMessage.signature,
    });
    // invariantSuffix starts with "{" — we'll strip it and prepend the variant prefix
    const invariantTail = invariantSuffix.slice(1); // drop leading "{"

    for (const target of targets) {
      const ws = connections.get(target.connectionId);
      if (!ws) {
        failedCount++;
        continue;
      }

      try {
        const recipientId = crypto.randomUUID();
        // Build full JSON: {"id":"<id>","to":"<addr>",<invariantTail>}
        const raw =
          '{"id":' +
          JSON.stringify(recipientId) +
          ',"to":' +
          JSON.stringify(target.actorAddress) +
          ',' +
          invariantTail;

        sendRaw(ws, raw);
        deliveredCount++;
      } catch (err) {
        console.error('[handleBroadcast] Failed to broadcast to', target.actorAddress, ':', err);
        failedCount++;
      }
    }
  } else {
    // --- Standard path (tests / no sendRaw provided) ---
    for (const target of targets) {
      const ws = connections.get(target.connectionId);
      if (!ws) {
        failedCount++;
        continue;
      }

      try {
        const recipientMessage = {
          ...broadcastMessage,
          id: crypto.randomUUID(),
          to: target.actorAddress,
        };

        sendMessage(ws, recipientMessage);
        deliveredCount++;
      } catch (err) {
        console.error('[handleBroadcast] Failed to broadcast to', target.actorAddress, ':', err);
        failedCount++;
      }
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
