/**
 * Flow Control / Backpressure Handlers
 *
 * Handles: hub:pause, hub:resume, hub:queue_stats
 */

import type { SharedMessage, Session, QueueStats } from '../types';
import { createReply, toCanonicalAddress } from '../utils';

const SIGNAL_HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');

/**
 * Handle hub:queue_stats message
 */
export function handleQueueStats(
  msg: SharedMessage,
  queueStats: QueueStats
): SharedMessage {
  const responsePayload = {
    queueDepth: queueStats.pending,
    processingRate: 0, // TODO: Calculate from metrics in Phase 2
    pauseThreshold: 1000, // Configurable threshold
    resumeThreshold: 100,
  };

  return createReply(
    'hub:queue_stats_response',
    responsePayload,
    msg,
    SIGNAL_HUB_ADDRESS
  );
}

/**
 * Send hub:pause to client (backpressure signal)
 */
export function sendPause(
  ws: WebSocket,
  reason: string,
  sendMessage: (ws: WebSocket, message: SharedMessage) => void
): void {
  const pauseMessage: SharedMessage = {
    id: crypto.randomUUID(),
    from: SIGNAL_HUB_ADDRESS,
    to: toCanonicalAddress('client'), // Generic client address
    type: 'hub:pause',
    payload: { reason },
    pattern: 'tell',
    correlationId: null,
    timestamp: Date.now(),
    metadata: {},
    ttl: null,
    signature: null,
  };

  sendMessage(ws, pauseMessage);
  console.log(`Sent hub:pause to client: ${reason}`);
}

/**
 * Send hub:resume to client (backpressure released)
 */
export function sendResume(
  ws: WebSocket,
  sendMessage: (ws: WebSocket, message: SharedMessage) => void
): void {
  const resumeMessage: SharedMessage = {
    id: crypto.randomUUID(),
    from: SIGNAL_HUB_ADDRESS,
    to: toCanonicalAddress('client'), // Generic client address
    type: 'hub:resume',
    payload: {},
    pattern: 'tell',
    correlationId: null,
    timestamp: Date.now(),
    metadata: {},
    ttl: null,
    signature: null,
  };

  sendMessage(ws, resumeMessage);
  console.log('Sent hub:resume to client');
}

/**
 * Update session pause state
 */
export function updatePauseState(session: Session, paused: boolean): void {
  session.paused = paused;
}
