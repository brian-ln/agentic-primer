/**
 * Flow Control Handler Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { handleQueueStats, sendPause, sendResume, updatePauseState } from '../flowcontrol';
import type { SharedMessage, Session, QueueStats } from '../../types';
import { toCanonicalAddress } from '../../utils';

describe('Flow Control Handlers', () => {
  let queueStats: QueueStats;
  let session: Session;

  beforeEach(() => {
    queueStats = {
      pending: 0,
      processed: 0,
      failed: 0,
      paused: false,
    };

    session = {
      sessionId: 'session-test',
      actorIdentity: null,
      capabilities: [],
      connectedAt: Date.now(),
      lastHeartbeat: Date.now(),
      authenticated: false,
      paused: false,
      connectionState: 'connected' as const,
      rateLimitBucket: { tokens: 100, capacity: 100, refillRate: 100 / 60, lastRefill: Date.now() },
    };
  });

  describe('handleQueueStats', () => {
    it('should return queue statistics', () => {
      queueStats.pending = 42;
      queueStats.processed = 1000;
      queueStats.failed = 5;

      const statsMsg: SharedMessage = {
        id: 'msg-1',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:queue_stats',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleQueueStats(statsMsg, queueStats);

      expect(response.type).toBe('hub:queue_stats_response');
      expect(response.payload).toHaveProperty('queueDepth', 42);
      expect(response.payload).toHaveProperty('processingRate', 0);
      expect(response.payload).toHaveProperty('pauseThreshold', 1000);
      expect(response.payload).toHaveProperty('resumeThreshold', 100);
    });

    it('should correlate response to request', () => {
      const statsMsg: SharedMessage = {
        id: 'msg-2',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:queue_stats',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleQueueStats(statsMsg, queueStats);

      expect(response.correlationId).toBe('msg-2');
    });
  });

  describe('sendPause', () => {
    it('should send pause message to WebSocket', () => {
      const sentMessages: string[] = [];
      const mockWs = {
        send: (data: string) => {
          sentMessages.push(data);
        },
      } as unknown as WebSocket;

      sendPause(mockWs, 'Queue overload');

      expect(sentMessages.length).toBe(1);

      const pauseMsg = JSON.parse(sentMessages[0]);
      expect(pauseMsg.type).toBe('hub:pause');
      expect(pauseMsg.from).toBe('@(cloudflare/signal-hub)');
      expect(pauseMsg.payload).toHaveProperty('reason', 'Queue overload');
    });
  });

  describe('sendResume', () => {
    it('should send resume message to WebSocket', () => {
      const sentMessages: string[] = [];
      const mockWs = {
        send: (data: string) => {
          sentMessages.push(data);
        },
      } as unknown as WebSocket;

      sendResume(mockWs);

      expect(sentMessages.length).toBe(1);

      const resumeMsg = JSON.parse(sentMessages[0]);
      expect(resumeMsg.type).toBe('hub:resume');
      expect(resumeMsg.from).toBe('@(cloudflare/signal-hub)');
    });
  });

  describe('updatePauseState', () => {
    it('should set session to paused', () => {
      expect(session.paused).toBe(false);

      updatePauseState(session, true);

      expect(session.paused).toBe(true);
    });

    it('should set session to resumed', () => {
      session.paused = true;

      updatePauseState(session, false);

      expect(session.paused).toBe(false);
    });
  });
});
