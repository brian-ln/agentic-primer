/**
 * Remote Proxy Actor - Entangled distributed actor pattern
 *
 * Makes a remote actor appear as a local actor in the browser's actor system.
 * Messages sent to the proxy are routed through a SystemBridge to the remote actor.
 *
 * Generalized from brianln.ai/src/actors/transports/BrainProxyActor.ts
 * Uses @agentic-primer/actors types.
 */

import type {
  Message,
  Address,
  ActorBehavior,
  ActorSystem,
} from '@agentic-primer/actors';

/**
 * State for a remote proxy actor.
 */
export interface RemoteProxyState {
  /** Address of the local proxy */
  localAddress?: Address;
  /** Addresses subscribed to push notifications from the remote actor */
  subscribers: Set<Address>;
  /** Types of messages to broadcast to subscribers */
  broadcastTypes: Set<string>;
}

/**
 * Create a behavior for a remote proxy actor.
 *
 * The proxy receives messages from the remote actor (via SystemBridge) and
 * broadcasts specified message types to subscribers.
 *
 * @param broadcastTypes - Message types to broadcast to subscribers
 */
export function createRemoteProxyBehavior(
  broadcastTypes: string[] = ['STATE_UPDATE']
): ActorBehavior<RemoteProxyState, any> {
  return async (state, message: Message, context) => {
    switch (message.type) {
      case 'SUBSCRIBE': {
        const subscriber = message.payload?.subscriber as Address;
        if (subscriber) {
          state.subscribers.add(subscriber);
        }
        return state;
      }

      case 'UNSUBSCRIBE': {
        const subscriber = message.payload?.subscriber as Address;
        if (subscriber) {
          state.subscribers.delete(subscriber);
        }
        return state;
      }

      default: {
        // Broadcast matching message types to all subscribers
        if (state.broadcastTypes.has(message.type)) {
          for (const subscriber of state.subscribers) {
            context.send(subscriber, message.type, message.payload);
          }
        }
        return state;
      }
    }
  };
}

/**
 * Spawn a remote proxy actor in the local ActorSystem.
 *
 * @param system - The local actor system
 * @param name - Name for the proxy actor (e.g., 'brain')
 * @param broadcastTypes - Message types to broadcast to subscribers
 * @returns Address of the spawned proxy actor
 *
 * @example
 * ```typescript
 * const brainAddr = spawnRemoteProxy(system, 'brain', [
 *   'STATE_UPDATE',
 *   'SIGNAL_PUSHED',
 *   'BRIEFING_PUSHED',
 * ]);
 *
 * // Other actors can subscribe
 * system.send(brainAddr, 'SUBSCRIBE', { subscriber: uiActorAddr });
 * ```
 */
export function spawnRemoteProxy(
  system: ActorSystem,
  name: string,
  broadcastTypes: string[] = ['STATE_UPDATE']
): Address {
  const behavior = createRemoteProxyBehavior(broadcastTypes);
  const initialState: RemoteProxyState = {
    subscribers: new Set(),
    broadcastTypes: new Set(broadcastTypes),
  };

  return system.spawn(behavior, initialState, name);
}
