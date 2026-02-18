/**
 * DX Improvements Tests (Bead 907 WS4.2)
 *
 * Covers:
 * 1. All hub:error responses include a `resolution` field with actionable hints
 * 2. createClientMessage / createSendMessage / createBroadcastMessage client helpers
 */

import { describe, it, expect } from 'vitest';
import { createErrorMessage } from '../../utils';
import { createClientMessage, createSendMessage, createBroadcastMessage } from '../../client';
import { toCanonicalAddress } from '../../utils';
import type { SharedMessage } from '../../types';

// ─── Shared test fixture ────────────────────────────────────────────────────

const HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');

function makeMsg(overrides: Partial<SharedMessage> = {}): SharedMessage {
  return {
    id: 'msg-test-1',
    from: toCanonicalAddress('browser/client'),
    to: HUB_ADDRESS,
    type: 'hub:heartbeat',
    payload: null,
    pattern: 'tell',
    correlationId: null,
    timestamp: Date.now(),
    metadata: {},
    ttl: null,
    signature: null,
    ...overrides,
  };
}

// ─── Error resolution hints ─────────────────────────────────────────────────

describe('hub:error resolution field', () => {
  const originalMsg = makeMsg();

  it('includes resolution for version_mismatch', () => {
    const err = createErrorMessage('version_mismatch', 'Protocol mismatch', originalMsg, HUB_ADDRESS);
    const payload = err.payload as Record<string, unknown>;
    expect(err.type).toBe('hub:error');
    expect(payload.code).toBe('version_mismatch');
    expect(typeof payload.resolution).toBe('string');
    expect((payload.resolution as string).length).toBeGreaterThan(0);
    expect(payload.resolution).toMatch(/protocol version|serverVersion/i);
  });

  it('includes resolution for unauthorized', () => {
    const err = createErrorMessage('unauthorized', 'Not authenticated', originalMsg, HUB_ADDRESS);
    const payload = err.payload as Record<string, unknown>;
    expect(payload.resolution).toMatch(/hub:connect/i);
  });

  it('includes resolution for rate_limited', () => {
    const err = createErrorMessage('rate_limited', 'Too many messages', originalMsg, HUB_ADDRESS);
    const payload = err.payload as Record<string, unknown>;
    expect(payload.resolution).toMatch(/slowing|retryAfter/i);
  });

  it('includes resolution for unknown_actor', () => {
    const err = createErrorMessage('unknown_actor', 'Actor not found', originalMsg, HUB_ADDRESS);
    const payload = err.payload as Record<string, unknown>;
    expect(payload.resolution).toMatch(/hub:register/i);
  });

  it('includes resolution for message_too_large', () => {
    const err = createErrorMessage('message_too_large', 'Message exceeds limit', originalMsg, HUB_ADDRESS);
    const payload = err.payload as Record<string, unknown>;
    expect(payload.resolution).toMatch(/payload size|split/i);
  });

  it('includes resolution for message_expired', () => {
    const err = createErrorMessage('message_expired', 'TTL expired', originalMsg, HUB_ADDRESS);
    const payload = err.payload as Record<string, unknown>;
    expect(payload.resolution).toMatch(/timestamp|TTL/i);
  });

  it('includes resolution for timeout', () => {
    const err = createErrorMessage('timeout', 'Request timed out', originalMsg, HUB_ADDRESS);
    const payload = err.payload as Record<string, unknown>;
    expect(payload.resolution).toMatch(/retrying|connectivity/i);
  });

  it('includes resolution for internal_error', () => {
    const err = createErrorMessage('internal_error', 'Something went wrong', originalMsg, HUB_ADDRESS);
    const payload = err.payload as Record<string, unknown>;
    expect(payload.resolution).toMatch(/retrying|hub operator/i);
  });

  it('includes a fallback resolution for unknown error codes', () => {
    const err = createErrorMessage('some_future_code', 'Unknown issue', originalMsg, HUB_ADDRESS);
    const payload = err.payload as Record<string, unknown>;
    expect(typeof payload.resolution).toBe('string');
    expect((payload.resolution as string).length).toBeGreaterThan(0);
  });

  it('preserves code, message and details alongside resolution', () => {
    const details = { retryAfter: 30 };
    const err = createErrorMessage('rate_limited', 'Slow down', originalMsg, HUB_ADDRESS, details);
    const payload = err.payload as Record<string, unknown>;
    expect(payload.code).toBe('rate_limited');
    expect(payload.message).toBe('Slow down');
    expect(payload.resolution).toBeDefined();
    expect(payload.details).toEqual(details);
  });

  it('sets correlationId from original message', () => {
    const err = createErrorMessage('internal_error', 'Oops', originalMsg, HUB_ADDRESS);
    expect(err.correlationId).toBe('msg-test-1');
  });
});

// ─── Client helpers ─────────────────────────────────────────────────────────

describe('createClientMessage', () => {
  it('returns a valid JSON string', () => {
    const result = createClientMessage(
      '@(browser/ui)',
      '@(cloudflare/signal-hub)',
      'hub:send',
      'task:assign',
      { to: '@(worker/foo)', data: { taskId: '1' } }
    );
    expect(typeof result).toBe('string');
    expect(() => JSON.parse(result)).not.toThrow();
  });

  it('constructs the flat payload shape with type and payload fields merged', () => {
    const result = createClientMessage(
      '@(browser/ui)',
      '@(cloudflare/signal-hub)',
      'hub:send',
      'task:assign',
      { to: '@(worker/foo)', data: { taskId: '42' } }
    );
    const parsed = JSON.parse(result);
    expect(parsed.type).toBe('hub:send');
    expect(parsed.payload.type).toBe('task:assign');
    expect(parsed.payload.to).toBe('@(worker/foo)');
    expect(parsed.payload.data).toEqual({ taskId: '42' });
  });

  it('sets required SharedMessage envelope fields', () => {
    const result = createClientMessage(
      '@(browser/ui)',
      '@(cloudflare/signal-hub)',
      'hub:send',
      'ping',
      {}
    );
    const parsed = JSON.parse(result);
    expect(typeof parsed.id).toBe('string');
    expect(parsed.from).toBe('@(browser/ui)');
    expect(parsed.to).toBe('@(cloudflare/signal-hub)');
    expect(parsed.pattern).toBe('tell');
    expect(typeof parsed.timestamp).toBe('number');
    expect(parsed.correlationId).toBeNull();
    expect(parsed.signature).toBeNull();
    expect(parsed.ttl).toBeNull();
  });

  it('generates a unique id on each call', () => {
    const r1 = JSON.parse(createClientMessage('@(a)', '@(b)', 'hub:send', 'test', {}));
    const r2 = JSON.parse(createClientMessage('@(a)', '@(b)', 'hub:send', 'test', {}));
    expect(r1.id).not.toBe(r2.id);
  });
});

describe('createSendMessage', () => {
  it('produces a hub:send message targeting a specific actor', () => {
    const result = createSendMessage(
      '@(browser/ui)',
      '@(worker/task-processor)',
      'task:assign',
      { taskId: '123' }
    );
    const parsed = JSON.parse(result);
    expect(parsed.type).toBe('hub:send');
    expect(parsed.payload.type).toBe('task:assign');
    expect(parsed.payload.to).toBe('@(worker/task-processor)');
    expect(parsed.payload.data).toEqual({ taskId: '123' });
  });

  it('sets from address correctly', () => {
    const result = createSendMessage('@(browser/my-actor)', '@(worker/foo)', 'ping', {});
    const parsed = JSON.parse(result);
    expect(parsed.from).toBe('@(browser/my-actor)');
  });

  it('routes through Signal Hub address', () => {
    const result = createSendMessage('@(browser/ui)', '@(worker/foo)', 'ping', {});
    const parsed = JSON.parse(result);
    expect(parsed.to).toBe('@(cloudflare/signal-hub)');
  });
});

describe('createBroadcastMessage', () => {
  it('produces a hub:broadcast message', () => {
    const result = createBroadcastMessage(
      '@(browser/admin)',
      'system:shutdown',
      { reason: 'maintenance' }
    );
    const parsed = JSON.parse(result);
    expect(parsed.type).toBe('hub:broadcast');
    expect(parsed.payload.type).toBe('system:shutdown');
    expect(parsed.payload.data).toEqual({ reason: 'maintenance' });
  });

  it('supports optional excludeSelf flag', () => {
    const result = createBroadcastMessage(
      '@(browser/admin)',
      'system:update',
      {},
      { excludeSelf: true }
    );
    const parsed = JSON.parse(result);
    expect(parsed.payload.excludeSelf).toBe(true);
  });

  it('supports optional targetCapability filter', () => {
    const result = createBroadcastMessage(
      '@(browser/admin)',
      'task:notify',
      {},
      { targetCapability: 'notifications' }
    );
    const parsed = JSON.parse(result);
    expect(parsed.payload.targetCapability).toBe('notifications');
  });

  it('routes through Signal Hub address', () => {
    const result = createBroadcastMessage('@(browser/admin)', 'ping', {});
    const parsed = JSON.parse(result);
    expect(parsed.to).toBe('@(cloudflare/signal-hub)');
  });
});
