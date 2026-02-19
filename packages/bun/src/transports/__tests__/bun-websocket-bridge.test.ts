/// <reference types="bun-types" />

import { describe, it, expect, mock, beforeEach } from 'bun:test';
import { BunWebSocketBridge, type ConnectionData } from '../bun-websocket-bridge.ts';
import type { ActorSystem } from '@agentic-primer/actors';

// ---------------------------------------------------------------------------
// Minimal mocks
// ---------------------------------------------------------------------------

function makeWs(id: string = crypto.randomUUID()): Bun.ServerWebSocket<ConnectionData> & { sentMessages: string[] } {
  const sentMessages: string[] = [];
  const ws = {
    data: { id },
    remoteAddress: '127.0.0.1',
    sentMessages,
    send: mock((msg: string | Uint8Array) => {
      sentMessages.push(typeof msg === 'string' ? msg : new TextDecoder().decode(msg));
      return 0;
    }),
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
  } as unknown as Bun.ServerWebSocket<ConnectionData> & { sentMessages: string[] };
  return ws;
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
      const ws1 = makeWs('id-1');
      const ws2 = makeWs('id-2');
      const ws3 = makeWs('id-3');

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
      const ws1 = makeWs('a');
      const ws2 = makeWs('b');
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
  // 7. handleDrain is a no-op
  // -------------------------------------------------------------------------
  it('handleDrain does not throw', () => {
    const ws = makeWs();
    bridge.handleOpen(ws);
    expect(() => bridge.handleDrain(ws)).not.toThrow();
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
