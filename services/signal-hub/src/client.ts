/**
 * Signal Hub Client Convenience API
 *
 * Provides helper functions for constructing well-formed Signal Hub messages
 * from client code without needing to manually assemble the flat payload shape.
 */

import type { CanonicalAddress } from './types';
import { toCanonicalAddress } from './utils';

/**
 * Construct and serialize a Signal Hub message ready to send over WebSocket.
 *
 * Signal Hub uses a flat payload structure where the application message type
 * and data fields sit alongside each other inside the outer message payload:
 *
 *   {
 *     type: 'hub:send',
 *     payload: {
 *       type: 'task:assign',   // application type  — from `type` argument
 *       to: '@(worker/foo)',   // target (if present in payload)
 *       data: { taskId: '1' } // application data  — from `payload` argument
 *     }
 *   }
 *
 * @param from - Canonical address of the sender, e.g. '@(browser/my-ui)'
 * @param to - Canonical address of the target, e.g. '@(cloudflare/signal-hub)'
 * @param hubMessageType - Hub-level message type, e.g. 'hub:send', 'hub:broadcast'
 * @param type - Application-level message type embedded in payload.type
 * @param payload - Application data fields spread into the payload alongside type
 * @returns JSON-serialized string ready for WebSocket.send()
 */
export function createClientMessage(
  from: string,
  to: string,
  hubMessageType: string,
  type: string,
  payload: Record<string, unknown>
): string {
  const message = {
    id: crypto.randomUUID(),
    from: from as CanonicalAddress,
    to: to as CanonicalAddress,
    type: hubMessageType,
    payload: {
      type,
      ...payload,
    },
    pattern: 'tell' as const,
    correlationId: null,
    timestamp: Date.now(),
    metadata: {},
    ttl: null,
    signature: null,
  };

  return JSON.stringify(message);
}

/**
 * Construct a hub:send message targeting a specific actor.
 *
 * This is the most common client operation: sending a point-to-point message
 * to a registered actor via the Signal Hub.
 *
 * @param from - Canonical address of the sender
 * @param targetActor - Canonical address of the destination actor
 * @param type - Application-level message type (e.g. 'task:assign')
 * @param data - Application data to deliver
 * @returns JSON-serialized hub:send message
 *
 * @example
 * const msg = createSendMessage(
 *   '@(browser/ui)',
 *   '@(worker/task-processor)',
 *   'task:assign',
 *   { taskId: '123', priority: 'high' }
 * );
 * ws.send(msg);
 */
export function createSendMessage(
  from: string,
  targetActor: string,
  type: string,
  data: Record<string, unknown>
): string {
  return createClientMessage(from, toCanonicalAddress('cloudflare/signal-hub'), 'hub:send', type, {
    to: targetActor,
    data,
  });
}

/**
 * Construct a hub:broadcast message to all registered actors.
 *
 * @param from - Canonical address of the sender
 * @param type - Application-level message type (e.g. 'system:shutdown')
 * @param data - Application data to broadcast
 * @param options - Optional broadcast options (excludeSelf, targetCapability)
 * @returns JSON-serialized hub:broadcast message
 *
 * @example
 * const msg = createBroadcastMessage(
 *   '@(browser/admin)',
 *   'system:shutdown',
 *   { reason: 'maintenance', at: Date.now() + 60_000 }
 * );
 * ws.send(msg);
 */
export function createBroadcastMessage(
  from: string,
  type: string,
  data: Record<string, unknown>,
  options: { excludeSelf?: boolean; targetCapability?: string } = {}
): string {
  return createClientMessage(from, toCanonicalAddress('cloudflare/signal-hub'), 'hub:broadcast', type, {
    data,
    ...options,
  });
}
