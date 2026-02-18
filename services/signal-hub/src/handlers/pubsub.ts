/**
 * Pub/Sub Handlers
 *
 * Handles: hub:subscribe, hub:publish, hub:unsubscribe
 */

import type {
  SharedMessage,
  CanonicalAddress,
  ActorRegistration,
} from '../types';
import { HubError } from '../types';
import { createReply, toCanonicalAddress, isExpired } from '../utils';

const SIGNAL_HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');

/**
 * Handle hub:subscribe message
 */
export function handleSubscribe(
  msg: SharedMessage,
  subscriptions: Map<string, Set<CanonicalAddress>>,
  actorAddress: CanonicalAddress,
  actorTopics?: Map<CanonicalAddress, Set<string>>
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

  // Maintain inverse index: actorTopics[actor] -> Set<topic>
  if (actorTopics) {
    let topics = actorTopics.get(actorAddress);
    if (!topics) {
      topics = new Set<string>();
      actorTopics.set(actorAddress, topics);
    }
    topics.add(topic);
  }

  // Generate subscription ID
  const subscriptionId = `sub-${crypto.randomUUID()}`;

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
  connections: Map<string, WebSocket>,
  sendMessage: (ws: WebSocket, message: SharedMessage) => void
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
  // CRITICAL: Use original sender as 'from', not SIGNAL_HUB_ADDRESS
  const publicationMessage: SharedMessage = {
    id: crypto.randomUUID(),
    from: msg.from, // Use original sender, NOT the hub
    to: toCanonicalAddress(`topic/${topic}`), // Placeholder
    type: payload.type, // Application type from payload.type
    payload: payload.data, // Application data from payload.data
    pattern: 'tell',
    correlationId: msg.id,
    timestamp: Date.now(),
    metadata: {
      topic,
      publication: true,
      via: SIGNAL_HUB_ADDRESS,
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

      sendMessage(ws, recipientMessage);
      deliveredCount++;
    } catch (err) {
      console.error(`Failed to publish to ${subscriberAddress}:`, err);
    }
  }

  // Send hub:published response
  const responsePayload = {
    topic,
    subscriberCount: deliveredCount,
  };

  return createReply('hub:published', responsePayload, msg, SIGNAL_HUB_ADDRESS);
}

/**
 * Handle hub:unsubscribe message
 *
 * Removes the actor from the specified topic subscription only.
 * Other topic subscriptions for the same actor are preserved.
 *
 * Protocol: payload must contain { topic: string }
 */
export function handleUnsubscribe(
  msg: SharedMessage,
  subscriptions: Map<string, Set<CanonicalAddress>>,
  actorAddress: CanonicalAddress,
  actorTopics?: Map<CanonicalAddress, Set<string>>
): void {
  const payload = msg.payload as { topic: string };

  if (!payload.topic || typeof payload.topic !== 'string') {
    throw new HubError('internal_error', 'topic is required');
  }

  const { topic } = payload;

  // Remove actor from the specified topic only
  const subscribers = subscriptions.get(topic);
  if (!subscribers || !subscribers.has(actorAddress)) {
    console.warn(
      `No active subscription found for actor: ${actorAddress} on topic: ${topic}`
    );
    return;
  }

  subscribers.delete(actorAddress);

  // Cleanup empty topic sets
  if (subscribers.size === 0) {
    subscriptions.delete(topic);
  }

  // Maintain inverse index: remove topic from actorTopics[actor]
  if (actorTopics) {
    const topics = actorTopics.get(actorAddress);
    if (topics) {
      topics.delete(topic);
      if (topics.size === 0) {
        actorTopics.delete(actorAddress);
      }
    }
  }
}

/**
 * Remove all subscriptions for an actor (cleanup on disconnect)
 *
 * When actorTopics inverse index is provided, this runs in O(S) where S is
 * the number of topics the actor is subscribed to, instead of O(T×S) where T
 * is the total number of topics.
 */
export function cleanupSubscriptions(
  subscriptions: Map<string, Set<CanonicalAddress>>,
  actorAddress: CanonicalAddress,
  actorTopics?: Map<CanonicalAddress, Set<string>>
): void {
  if (actorTopics) {
    // O(S) fast path: use inverse index to find only topics this actor is in
    const topics = actorTopics.get(actorAddress);
    if (topics) {
      for (const topic of topics) {
        const subscribers = subscriptions.get(topic);
        if (subscribers) {
          subscribers.delete(actorAddress);
          if (subscribers.size === 0) {
            subscriptions.delete(topic);
          }
        }
      }
      actorTopics.delete(actorAddress);
    }
  } else {
    // O(T×S) fallback: scan all topics (no inverse index available)
    for (const [topic, subscribers] of subscriptions.entries()) {
      subscribers.delete(actorAddress);

      // Cleanup empty topic sets
      if (subscribers.size === 0) {
        subscriptions.delete(topic);
      }
    }
  }
}
