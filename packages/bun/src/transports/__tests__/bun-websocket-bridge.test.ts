/// <reference types="bun-types" />

import { describe, it, expect, mock, beforeEach } from 'bun:test';
import { BunWebSocketBridge, type ConnectionData } from '../bun-websocket-bridge.ts';
import type { ActorSystem } from '@agentic-primer/actors';

// ---------------------------------------------------------------------------
// Minimal mocks
// ---------------------------------------------------------------------------

/** Base ws object shape — shared by all mock helpers. */
function makeWsBase(id: ReturnType<typeof crypto.randomUUID>) {
  return {
    data: { id },
    remoteAddress: '127.0.0.1',
    close: mock(() => {}),
    subscribe: mock(() => {}),
    unsubscribe: mock(() => {}),
    publish: mock(() => 0),
    publishText: mock(() => 0),
    publishBinary: mock(() => 0),
    ping: mock(() => 0),
    pong: mock(() => 0),
    readyState: 1,
    binaryType: 'arraybuffer' as const,
  };
}

/**
 * Normal mock: send() returns 1 (success) and records the message.
 * Status semantics (bun-types): 0 = dropped, -1 = backpressured, >0 = sent.
 */
function makeWs(id: ReturnType<typeof crypto.randomUUID> = crypto.randomUUID()): Bun.ServerWebSocket<ConnectionData> & { sentMessages: string[] } {
  const sentMessages: string[] = [];
  return {
    ...makeWsBase(id),
    sentMessages,
    send: mock((msg: string | Uint8Array) => {
      sentMessages.push(typeof msg === 'string' ? msg : new TextDecoder().decode(msg));
      return 1; // success
    }),
  } as unknown as Bun.ServerWebSocket<ConnectionData> & { sentMessages: string[] };
}

/**
 * Backpressure mock: first `dropFirst` calls return 0 (dropped, NOT sent),
 * subsequent calls return 1 (success) and record the message.
 */
function makeDroppedWs(
  id: ReturnType<typeof crypto.randomUUID> = crypto.randomUUID(),
  dropFirst = 1,
): Bun.ServerWebSocket<ConnectionData> & { sentMessages: string[] } {
  const sentMessages: string[] = [];
  let dropsLeft = dropFirst;
  return {
    ...makeWsBase(id),
    sentMessages,
    send: mock((msg: string | Uint8Array) => {
      if (dropsLeft > 0) { dropsLeft--; return 0; } // dropped
      sentMessages.push(typeof msg === 'string' ? msg : new TextDecoder().decode(msg));
      return 1; // success
    }),
  } as unknown as Bun.ServerWebSocket<ConnectionData> & { sentMessages: string[] };
}

/**
 * Congested mock: first `bufferFirst` calls return -1 (backpressured/buffered
 * by Bun, message IS in Bun's buffer), subsequent calls return 1 (success).
 * Does NOT record messages that are backpressured (Bun holds them).
 */
function makeCongestedWs(
  id: ReturnType<typeof crypto.randomUUID> = crypto.randomUUID(),
  bufferFirst = 1,
): Bun.ServerWebSocket<ConnectionData> & { sentMessages: string[] } {
  const sentMessages: string[] = [];
  let buffersLeft = bufferFirst;
  return {
    ...makeWsBase(id),
    sentMessages,
    send: mock((msg: string | Uint8Array) => {
      if (buffersLeft > 0) { buffersLeft--; return -1; } // backpressured
      sentMessages.push(typeof msg === 'string' ? msg : new TextDecoder().decode(msg));
      return 1; // success
    }),
  } as unknown as Bun.ServerWebSocket<ConnectionData> & { sentMessages: string[] };
}

function makeSystem(): { send: ReturnType<typeof mock> } {
  return { send: mock(() => Promise.resolve()) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('BunWebSocketBridge', () => {
  let system: ReturnType<typeof makeSystem>;
  let bridge: BunWebSocketBridge;

  beforeEach(() => {
    system = makeSystem();
    bridge = new BunWebSocketBridge(system as unknown as ActorSystem);
  });

  // -------------------------------------------------------------------------
  // 1. HEARTBEAT_PING → HEARTBEAT_PONG
  // -------------------------------------------------------------------------
  describe('HEARTBEAT_PING → HEARTBEAT_PONG', () => {
    it('replies with correct PONG shape and matching id', () => {
      const ws = makeWs();
      bridge.handleOpen(ws);

      const ping = {
        type: 'HEARTBEAT_PING',
        to: '__system__',
        from: '__client__',
        payload: { timestamp: Date.now() },
        id: 'heartbeat-12345',
      };

      bridge.handleMessage(ws, JSON.stringify(ping));

      expect(ws.sentMessages).toHaveLength(1);
      const pong = JSON.parse(ws.sentMessages[0]) as Record<string, unknown>;
      expect(pong.type).toBe('HEARTBEAT_PONG');
      expect(pong.id).toBe('heartbeat-12345');
      expect(pong.to).toBe('__client__');
      expect(pong.from).toBe('__system__');
      expect(pong.payload).toEqual({});
    });

    it('does NOT call system.send for heartbeat messages', () => {
      const ws = makeWs();
      bridge.handleOpen(ws);

      const ping = {
        type: 'HEARTBEAT_PING',
        to: '__system__',
        from: '__client__',
        payload: { timestamp: Date.now() },
        id: 'heartbeat-abc',
      };

      bridge.handleMessage(ws, JSON.stringify(ping));
      expect(system.send).not.toHaveBeenCalled();
    });
  });

  // -------------------------------------------------------------------------
  // 2. Actor message routing
  // -------------------------------------------------------------------------
  describe('actor message routing', () => {
    it('routes actor messages to system.send', () => {
      const ws = makeWs();
      bridge.handleOpen(ws);

      const msg = {
        to: 'test/actor',
        from: 'browser/client',
        type: 'ping',
        payload: { value: 42 },
        id: 'msg-001',
      };

      bridge.handleMessage(ws, JSON.stringify(msg));

      expect(system.send).toHaveBeenCalledTimes(1);
      // The first argument is an Address (address() wraps in @(...)), second is type, third is payload.
      const [toArg, typeArg, payloadArg] = (system.send as ReturnType<typeof mock>).mock.calls[0];
      // address('test/actor') produces '@(test/actor)'
      expect(String(toArg)).toBe('@(test/actor)');
      expect(typeArg).toBe('ping');
      expect(payloadArg).toEqual({ value: 42 });
    });

    it('does not call system.send when message has no "to" field', () => {
      const ws = makeWs();
      bridge.handleOpen(ws);
      bridge.handleMessage(ws, JSON.stringify({ someOtherField: 'value' }));
      expect(system.send).not.toHaveBeenCalled();
    });
  });

  // -------------------------------------------------------------------------
  // 3. Broadcast
  // -------------------------------------------------------------------------
  describe('broadcast', () => {
    it('sends to all tracked connections', () => {
      const ws1 = makeWs('00000000-0000-0000-0000-000000000001');
      const ws2 = makeWs('00000000-0000-0000-0000-000000000002');
      const ws3 = makeWs('00000000-0000-0000-0000-000000000003');

      bridge.handleOpen(ws1);
      bridge.handleOpen(ws2);
      bridge.handleOpen(ws3);

      bridge.broadcast({ type: 'ANNOUNCE', payload: 'hello' });

      for (const ws of [ws1, ws2, ws3]) {
        expect(ws.sentMessages).toHaveLength(1);
        const parsed = JSON.parse(ws.sentMessages[0]) as Record<string, unknown>;
        expect(parsed.type).toBe('ANNOUNCE');
        expect(parsed.payload).toBe('hello');
      }
    });

    it('sends nothing when there are no connections', () => {
      // Should not throw
      expect(() => bridge.broadcast({ type: 'TEST' })).not.toThrow();
    });
  });

  // -------------------------------------------------------------------------
  // 4. Connection tracking
  // -------------------------------------------------------------------------
  describe('connection tracking', () => {
    it('handleOpen adds the connection', () => {
      expect(bridge.getConnectionCount()).toBe(0);
      const ws = makeWs();
      bridge.handleOpen(ws);
      expect(bridge.getConnectionCount()).toBe(1);
    });

    it('handleClose removes the connection', () => {
      const ws = makeWs();
      bridge.handleOpen(ws);
      expect(bridge.getConnectionCount()).toBe(1);
      bridge.handleClose(ws, 1000, 'Normal closure');
      expect(bridge.getConnectionCount()).toBe(0);
    });

    it('tracks multiple connections independently', () => {
      const ws1 = makeWs('aaaaaaaa-0000-0000-0000-000000000001');
      const ws2 = makeWs('bbbbbbbb-0000-0000-0000-000000000001');
      bridge.handleOpen(ws1);
      bridge.handleOpen(ws2);
      expect(bridge.getConnectionCount()).toBe(2);

      bridge.handleClose(ws1, 1000, '');
      expect(bridge.getConnectionCount()).toBe(1);

      bridge.handleClose(ws2, 1000, '');
      expect(bridge.getConnectionCount()).toBe(0);
    });
  });

  // -------------------------------------------------------------------------
  // 5. getConnectionCount
  // -------------------------------------------------------------------------
  describe('getConnectionCount', () => {
    it('returns 0 initially', () => {
      expect(bridge.getConnectionCount()).toBe(0);
    });

    it('increments and decrements correctly', () => {
      const ws = makeWs();
      bridge.handleOpen(ws);
      expect(bridge.getConnectionCount()).toBe(1);
      bridge.handleClose(ws, 1000, '');
      expect(bridge.getConnectionCount()).toBe(0);
    });
  });

  // -------------------------------------------------------------------------
  // 6. createWebSocketHandlers
  // -------------------------------------------------------------------------
  describe('createWebSocketHandlers', () => {
    it('returns handlers bound to the bridge instance', () => {
      const handlers = bridge.createWebSocketHandlers();
      expect(typeof handlers.open).toBe('function');
      expect(typeof handlers.message).toBe('function');
      expect(typeof handlers.close).toBe('function');
      expect(typeof handlers.drain).toBe('function');

      const ws = makeWs();
      handlers.open(ws);
      expect(bridge.getConnectionCount()).toBe(1);

      handlers.close(ws, 1000, '');
      expect(bridge.getConnectionCount()).toBe(0);
    });
  });

  // -------------------------------------------------------------------------
  // 7. handleDrain — no queue
  // -------------------------------------------------------------------------
  it('handleDrain is a no-op when no messages are queued', () => {
    const ws = makeWs();
    bridge.handleOpen(ws);
    expect(() => bridge.handleDrain(ws)).not.toThrow();
    expect(ws.sentMessages).toHaveLength(0);
  });

  // -------------------------------------------------------------------------
  // 8. Backpressure / drain queue
  // -------------------------------------------------------------------------
  describe('backpressure / drain queue', () => {
    it('queues a dropped message (status 0) and retries on drain', () => {
      const ws = makeDroppedWs('00000000-0000-0000-0000-000000000010', 1);
      bridge.handleOpen(ws);

      bridge.broadcast({ type: 'TEST' });
      expect(ws.sentMessages).toHaveLength(0); // dropped, not delivered

      bridge.handleDrain(ws);
      expect(ws.sentMessages).toHaveLength(1);
      expect(JSON.parse(ws.sentMessages[0]).type).toBe('TEST');
    });

    it('flushes multiple queued messages in order on drain', () => {
      const ws = makeDroppedWs('00000000-0000-0000-0000-000000000011', 1);
      bridge.handleOpen(ws);

      bridge.broadcast({ type: 'FIRST' });   // dropped → queued
      bridge.broadcast({ type: 'SECOND' });  // queued (socket congested)
      expect(ws.sentMessages).toHaveLength(0);

      bridge.handleDrain(ws);
      expect(ws.sentMessages).toHaveLength(2);
      expect(JSON.parse(ws.sentMessages[0]).type).toBe('FIRST');
      expect(JSON.parse(ws.sentMessages[1]).type).toBe('SECOND');
    });

    it('stops draining when still dropped (status 0) — retains remaining queue', () => {
      // 2 total drops: 1 from broadcast, 1 from first drain attempt
      const ws = makeDroppedWs('00000000-0000-0000-0000-000000000012', 2);
      bridge.handleOpen(ws);

      bridge.broadcast({ type: 'RETRY' }); // dropped → queued
      expect(ws.sentMessages).toHaveLength(0);

      bridge.handleDrain(ws); // still dropped — nothing sent yet
      expect(ws.sentMessages).toHaveLength(0);

      bridge.handleDrain(ws); // now succeeds
      expect(ws.sentMessages).toHaveLength(1);
    });

    it('records congestion (status -1) and queues subsequent broadcasts until drain', () => {
      const ws = makeCongestedWs('00000000-0000-0000-0000-000000000013', 1);
      bridge.handleOpen(ws);

      bridge.broadcast({ type: 'BUFFERED' }); // -1: Bun buffered it, socket marked congested
      bridge.broadcast({ type: 'QUEUED' });   // socket congested, queued locally
      // BUFFERED is in Bun's hands; QUEUED is in our drain queue
      expect(ws.sentMessages).toHaveLength(0);

      bridge.handleDrain(ws); // flushes QUEUED
      expect(ws.sentMessages).toHaveLength(1);
      expect(JSON.parse(ws.sentMessages[0]).type).toBe('QUEUED');
    });

    it('only queues backpressured connections — other connections receive immediately', () => {
      const wsOk = makeWs('00000000-0000-0000-0000-000000000014');
      const wsDrop = makeDroppedWs('00000000-0000-0000-0000-000000000015', 1);
      bridge.handleOpen(wsOk);
      bridge.handleOpen(wsDrop);

      bridge.broadcast({ type: 'MIXED' });
      expect(wsOk.sentMessages).toHaveLength(1);   // delivered immediately
      expect(wsDrop.sentMessages).toHaveLength(0); // dropped

      bridge.handleDrain(wsDrop);
      expect(wsDrop.sentMessages).toHaveLength(1); // delivered on drain
    });

    it('handleClose clears the drain queue', () => {
      // drops every call
      const ws = makeDroppedWs('00000000-0000-0000-0000-000000000016', 999);
      bridge.handleOpen(ws);
      bridge.broadcast({ type: 'PENDING' }); // queued

      bridge.handleClose(ws, 1000, '');
      // No throw, connection removed, queue cleaned up
      expect(bridge.getConnectionCount()).toBe(0);
      expect(() => bridge.handleDrain(ws)).not.toThrow();
    });
  });

  // -------------------------------------------------------------------------
  // 8. Binary message (Uint8Array)
  // -------------------------------------------------------------------------
  it('handles binary Uint8Array messages', () => {
    const ws = makeWs();
    bridge.handleOpen(ws);

    const msg = {
      to: 'actor/target',
      type: 'binary-test',
      payload: {},
    };
    const encoded = new TextEncoder().encode(JSON.stringify(msg));

    bridge.handleMessage(ws, encoded);
    expect(system.send).toHaveBeenCalledTimes(1);
  });
});
