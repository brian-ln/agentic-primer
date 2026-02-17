/**
 * Pub/Sub Handlers
 *
 * Handles: hub:subscribe, hub:publish, hub:unsubscribe
 */

import type {
  SharedMessage,
  CanonicalAddress,
  TopicSubscription,
  ActorRegistration,
} from '../types';
import { HubError } from '../types';
import { createReply, createMessage, toCanonicalAddress, isExpired } from '../utils';

const SIGNAL_HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');

/**
 * Handle hub:subscribe message
 */
export function handleSubscribe(
  msg: SharedMessage,
  subscriptions: Map<string, Set<CanonicalAddress>>,
  actorAddress: CanonicalAddress
): SharedMessage {
  const payload = msg.payload as {
    topic: string;
    durable?: boolean;
  };

  if (!payload.topic || typeof payload.topic !== 'string') {
    throw new HubError('internal_error', 'topic is required');
  }

  const { topic } = payload;

  // Get or create topic subscription set
  let subscribers = subscriptions.get(topic);
  if (!subscribers) {
    subscribers = new Set<CanonicalAddress>();
    subscriptions.set(topic, subscribers);
  }

  // Add subscriber
  subscribers.add(actorAddress);

  // Generate subscription ID
  const subscriptionId = `sub-${crypto.randomUUID()}`;

  console.log(`Actor ${actorAddress} subscribed to topic: ${topic}`);

  // Send hub:subscribed response
  const responsePayload = {
    topic,
    subscriptionId,
  };

  return createReply('hub:subscribed', responsePayload, msg, SIGNAL_HUB_ADDRESS);
}

/**
 * Handle hub:publish message
 *
 * Uses flat payload structure:
 * {
 *   type: 'hub:publish',
 *   payload: {
 *     topic: 'events',
 *     type: 'event:created',        // Application type
 *     data: { eventId: '123' }      // Application data
 *   }
 * }
 */
export function handlePublish(
  msg: SharedMessage,
  subscriptions: Map<string, Set<CanonicalAddress>>,
  registry: Map<string, ActorRegistration>,
  connections: Map<string, WebSocket>
): SharedMessage {
  const payload = msg.payload as {
    topic: string;
    type: string;
    data: unknown;
  };

  if (!payload.topic || typeof payload.topic !== 'string') {
    throw new HubError('internal_error', 'topic is required');
  }

  if (!payload.type || typeof payload.type !== 'string') {
    throw new HubError('internal_error', 'payload.type is required');
  }

  const { topic } = payload;

  // Get subscribers
  const subscribers = subscriptions.get(topic);
  if (!subscribers || subscribers.size === 0) {
    console.log(`No subscribers for topic: ${topic}`);

    return createReply(
      'hub:published',
      {
        topic,
        subscriberCount: 0,
      },
      msg,
      SIGNAL_HUB_ADDRESS
    );
  }

  let deliveredCount = 0;

  // Create publication message with flat structure
  const publicationMessage: SharedMessage = {
    id: crypto.randomUUID(),
    from: SIGNAL_HUB_ADDRESS,
    to: toCanonicalAddress(`topic/${topic}`), // Placeholder
    type: payload.type, // Application type from payload.type
    payload: payload.data, // Application data from payload.data
    pattern: 'tell',
    correlationId: msg.id,
    timestamp: Date.now(),
    metadata: {
      topic,
      publication: true,
      originalFrom: msg.from,
    },
    ttl: msg.ttl ?? null,
    signature: null,
  };

  // Send to all subscribers
  for (const subscriberAddress of subscribers) {
    const actor = registry.get(subscriberAddress);
    if (!actor) {
      // Cleanup stale subscription
      subscribers.delete(subscriberAddress);
      continue;
    }

    // Skip expired actors
    if (isExpired(actor.expiresAt)) {
      registry.delete(subscriberAddress);
      subscribers.delete(subscriberAddress);
      continue;
    }

    const ws = connections.get(actor.connectionId);
    if (!ws) {
      console.warn(`WebSocket not found for subscriber: ${subscriberAddress}`);
      continue;
    }

    try {
      // Update 'to' field for each recipient
      const recipientMessage = {
        ...publicationMessage,
        id: crypto.randomUUID(), // New ID per recipient
        to: subscriberAddress,
      };

      ws.send(JSON.stringify(recipientMessage));
      deliveredCount++;
    } catch (err) {
      console.error(`Failed to publish to ${subscriberAddress}:`, err);
    }
  }

  console.log(
    `Published ${payload.type} to topic ${topic}: ${deliveredCount} subscribers`
  );

  // Send hub:published response
  const responsePayload = {
    topic,
    subscriberCount: deliveredCount,
  };

  return createReply('hub:published', responsePayload, msg, SIGNAL_HUB_ADDRESS);
}

/**
 * Handle hub:unsubscribe message
 */
export function handleUnsubscribe(
  msg: SharedMessage,
  subscriptions: Map<string, Set<CanonicalAddress>>,
  actorAddress: CanonicalAddress
): void {
  const payload = msg.payload as { subscriptionId: string };

  if (!payload.subscriptionId || typeof payload.subscriptionId !== 'string') {
    throw new HubError('internal_error', 'subscriptionId is required');
  }

  // For MVP, we don't track subscription IDs explicitly
  // Instead, remove actor from all topics (could be improved in Phase 2)
  let removed = false;
  for (const [topic, subscribers] of subscriptions.entries()) {
    if (subscribers.has(actorAddress)) {
      subscribers.delete(actorAddress);
      console.log(`Actor ${actorAddress} unsubscribed from topic: ${topic}`);
      removed = true;

      // Cleanup empty topic sets
      if (subscribers.size === 0) {
        subscriptions.delete(topic);
      }
    }
  }

  if (!removed) {
    console.warn(
      `No active subscriptions found for actor: ${actorAddress} (subscriptionId: ${payload.subscriptionId})`
    );
  }
}

/**
 * Remove all subscriptions for an actor (cleanup on disconnect)
 */
export function cleanupSubscriptions(
  subscriptions: Map<string, Set<CanonicalAddress>>,
  actorAddress: CanonicalAddress
): void {
  for (const [topic, subscribers] of subscriptions.entries()) {
    subscribers.delete(actorAddress);

    // Cleanup empty topic sets
    if (subscribers.size === 0) {
      subscriptions.delete(topic);
    }
  }
}
