import { describe, it, expect, mock, beforeEach } from 'bun:test';
import {
  heartbeatMiddleware,
  actorRoutingMiddleware,
  composeWsMiddleware,
  deserializeWsMessage,
  routeWsActorMessage,
  makeHeartbeatPong,
  type WsContext,
  type WsMiddleware,
} from './bridge-utils.ts';
import type { ActorSystem } from '../actor-system.ts';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function makeCtx(data: Record<string, unknown>, sent: string[] = []): WsContext {
  return {
    data,
    send: (json: string) => sent.push(json),
  };
}

function makeSystem(): { send: ReturnType<typeof mock> } {
  return { send: mock(() => Promise.resolve()) };
}

const noop = () => Promise.resolve();

// ---------------------------------------------------------------------------
// heartbeatMiddleware
// ---------------------------------------------------------------------------

describe('heartbeatMiddleware', () => {
  it('replies with HEARTBEAT_PONG for HEARTBEAT_PING', async () => {
    const sent: string[] = [];
    const ctx = makeCtx({
      type: 'HEARTBEAT_PING',
      id: 'hb-123',
      to: '__system__',
      from: '__client__',
      payload: { timestamp: 1000 },
    }, sent);

    await heartbeatMiddleware(ctx, noop);

    expect(sent).toHaveLength(1);
    const pong = JSON.parse(sent[0]);
    expect(pong.type).toBe('HEARTBEAT_PONG');
    expect(pong.id).toBe('hb-123');
    expect(pong.to).toBe('__client__');
    expect(pong.from).toBe('__system__');
    expect(pong.payload).toEqual({});
  });

  it('echoes the id verbatim', async () => {
    const sent: string[] = [];
    const ctx = makeCtx({ type: 'HEARTBEAT_PING', id: 'specific-id-xyz' }, sent);
    await heartbeatMiddleware(ctx, noop);
    expect(JSON.parse(sent[0]).id).toBe('specific-id-xyz');
  });

  it('does NOT call next() for HEARTBEAT_PING — consumes the message', async () => {
    const next = mock(noop);
    const ctx = makeCtx({ type: 'HEARTBEAT_PING', id: 'hb-1' });
    await heartbeatMiddleware(ctx, next);
    expect(next).not.toHaveBeenCalled();
  });

  it('calls next() for non-heartbeat messages — passes through', async () => {
    const next = mock(noop);
    const ctx = makeCtx({ type: 'actor/ping', to: 'some/actor' });
    await heartbeatMiddleware(ctx, next);
    expect(next).toHaveBeenCalledTimes(1);
  });

  it('does not send anything for non-heartbeat messages', async () => {
    const sent: string[] = [];
    const ctx = makeCtx({ type: 'actor/ping', to: 'some/actor' }, sent);
    await heartbeatMiddleware(ctx, noop);
    expect(sent).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// actorRoutingMiddleware
// ---------------------------------------------------------------------------

describe('actorRoutingMiddleware', () => {
  let system: ReturnType<typeof makeSystem>;

  beforeEach(() => { system = makeSystem(); });

  it('calls system.send for messages with { to, type }', async () => {
    const middleware = actorRoutingMiddleware(system as unknown as ActorSystem);
    const ctx = makeCtx({ to: 'test/actor', type: 'ping', payload: { x: 1 } });
    await middleware(ctx, noop);
    expect(system.send).toHaveBeenCalledTimes(1);
    const [toArg, typeArg, payloadArg] = (system.send as ReturnType<typeof mock>).mock.calls[0];
    expect(String(toArg)).toBe('@(test/actor)');
    expect(typeArg).toBe('ping');
    expect(payloadArg).toEqual({ x: 1 });
  });

  it('always calls next() — never consumes the message', async () => {
    const next = mock(noop);
    const middleware = actorRoutingMiddleware(system as unknown as ActorSystem);
    const ctx = makeCtx({ to: 'test/actor', type: 'ping' });
    await middleware(ctx, next);
    expect(next).toHaveBeenCalledTimes(1);
  });

  it('calls next() even when message is not routable', async () => {
    const next = mock(noop);
    const middleware = actorRoutingMiddleware(system as unknown as ActorSystem);
    const ctx = makeCtx({ someOtherField: 'value' });
    await middleware(ctx, next);
    expect(next).toHaveBeenCalledTimes(1);
  });

  it('does not call system.send when message has no `to` field', async () => {
    const middleware = actorRoutingMiddleware(system as unknown as ActorSystem);
    await middleware(makeCtx({ type: 'ping' }), noop);
    expect(system.send).not.toHaveBeenCalled();
  });

  it('does not call system.send when message has no `type` field', async () => {
    const middleware = actorRoutingMiddleware(system as unknown as ActorSystem);
    await middleware(makeCtx({ to: 'test/actor' }), noop);
    expect(system.send).not.toHaveBeenCalled();
  });
});

// ---------------------------------------------------------------------------
// composeWsMiddleware
// ---------------------------------------------------------------------------

describe('composeWsMiddleware', () => {
  it('runs middlewares in order', async () => {
    const order: number[] = [];
    const m1: WsMiddleware = async (_ctx, next) => { order.push(1); await next(); };
    const m2: WsMiddleware = async (_ctx, next) => { order.push(2); await next(); };
    const m3: WsMiddleware = async (_ctx, next) => { order.push(3); await next(); };

    const handle = composeWsMiddleware(m1, m2, m3);
    await handle(JSON.stringify({ type: 'test' }), () => {});

    expect(order).toEqual([1, 2, 3]);
  });

  it('stops at a middleware that does not call next()', async () => {
    const order: number[] = [];
    const m1: WsMiddleware = async (_ctx, next) => { order.push(1); await next(); };
    const m2: WsMiddleware = async () => { order.push(2); /* no next */ };
    const m3: WsMiddleware = async (_ctx, next) => { order.push(3); await next(); };

    const handle = composeWsMiddleware(m1, m2, m3);
    await handle(JSON.stringify({ type: 'test' }), () => {});

    expect(order).toEqual([1, 2]);
  });

  it('supports before + after logic around next()', async () => {
    const events: string[] = [];
    const wrapping: WsMiddleware = async (_ctx, next) => {
      events.push('before');
      await next();
      events.push('after');
    };
    const inner: WsMiddleware = async (_ctx, next) => {
      events.push('inner');
      await next();
    };

    await composeWsMiddleware(wrapping, inner)(JSON.stringify({}), () => {});
    expect(events).toEqual(['before', 'inner', 'after']);
  });

  it('integrates heartbeat + actor routing correctly', async () => {
    const system = makeSystem();
    const sent: string[] = [];
    const handle = composeWsMiddleware(
      heartbeatMiddleware,
      actorRoutingMiddleware(system as unknown as ActorSystem),
    );

    // Heartbeat should reply and NOT route to system
    await handle(
      JSON.stringify({ type: 'HEARTBEAT_PING', id: 'hb-1', to: '__system__', from: '__client__', payload: { timestamp: 0 } }),
      (json) => sent.push(json),
    );
    expect(sent).toHaveLength(1);
    expect(JSON.parse(sent[0]).type).toBe('HEARTBEAT_PONG');
    expect(system.send).not.toHaveBeenCalled();

    // Actor message should route and NOT send a reply
    await handle(
      JSON.stringify({ type: 'ping', to: 'test/actor', payload: {} }),
      (json) => sent.push(json),
    );
    expect(sent).toHaveLength(1); // still 1 — no reply sent for actor messages
    expect(system.send).toHaveBeenCalledTimes(1);
  });

  it('accepts binary Uint8Array messages', async () => {
    const seen: Record<string, unknown>[] = [];
    const capture: WsMiddleware = async (ctx, next) => { seen.push(ctx.data); await next(); };

    const handle = composeWsMiddleware(capture);
    const encoded = new TextEncoder().encode(JSON.stringify({ type: 'binary-test', to: 'a/b' }));
    await handle(encoded, () => {});

    expect(seen[0].type).toBe('binary-test');
  });

  it('accepts ArrayBuffer messages', async () => {
    const seen: Record<string, unknown>[] = [];
    const capture: WsMiddleware = async (ctx, next) => { seen.push(ctx.data); await next(); };

    const handle = composeWsMiddleware(capture);
    const encoded = new TextEncoder().encode(JSON.stringify({ type: 'buf-test' })).buffer;
    await handle(encoded, () => {});

    expect(seen[0].type).toBe('buf-test');
  });

  it('catches middleware errors without propagating', async () => {
    const throwing: WsMiddleware = async () => { throw new Error('boom'); };
    const handle = composeWsMiddleware(throwing);
    await expect(handle(JSON.stringify({}), () => {})).resolves.toBeUndefined();
  });

  it('catches double-next errors without propagating', async () => {
    const doubleNext: WsMiddleware = async (_ctx, next) => {
      await next();
      await next(); // second call should throw inside compose
    };
    const handle = composeWsMiddleware(doubleNext);
    await expect(handle(JSON.stringify({}), () => {})).resolves.toBeUndefined();
  });

  it('ctx.send is bound to the send argument', async () => {
    const sent: string[] = [];
    const sender: WsMiddleware = async (ctx, next) => { ctx.send('hello'); await next(); };
    await composeWsMiddleware(sender)(JSON.stringify({}), (json) => sent.push(json));
    expect(sent).toEqual(['hello']);
  });

  it('extra middleware passed after built-ins runs last', async () => {
    const system = makeSystem();
    const order: string[] = [];
    const extra: WsMiddleware = async (_ctx, next) => { order.push('extra'); await next(); };

    const handle = composeWsMiddleware(
      heartbeatMiddleware,
      actorRoutingMiddleware(system as unknown as ActorSystem),
      extra,
    );

    await handle(JSON.stringify({ type: 'ping', to: 'a/b' }), () => {});
    expect(order).toContain('extra');
  });
});

// ---------------------------------------------------------------------------
// deserializeWsMessage
// ---------------------------------------------------------------------------

describe('deserializeWsMessage', () => {
  it('parses a JSON string', () => {
    const result = deserializeWsMessage('{"type":"test","value":42}');
    expect(result).toEqual({ type: 'test', value: 42 });
  });

  it('decodes a Uint8Array', () => {
    const encoded = new TextEncoder().encode(JSON.stringify({ type: 'binary' }));
    expect(deserializeWsMessage(encoded)).toEqual({ type: 'binary' });
  });

  it('decodes an ArrayBuffer', () => {
    const encoded = new TextEncoder().encode(JSON.stringify({ type: 'arraybuf' })).buffer;
    expect(deserializeWsMessage(encoded)).toEqual({ type: 'arraybuf' });
  });

  it('uses serde for binary when provided', () => {
    const fakeData = { type: 'decoded-by-serde' };
    const serde = { contentType: 'application/msgpack', deserialize: mock(() => fakeData), serialize: mock(() => new Uint8Array()) };
    const encoded = new Uint8Array([1, 2, 3]);
    expect(deserializeWsMessage(encoded, serde)).toBe(fakeData);
    expect(serde.deserialize).toHaveBeenCalledWith(encoded);
  });

  it('does not use serde for string messages', () => {
    const serde = { contentType: 'application/msgpack', deserialize: mock(() => ({})), serialize: mock(() => new Uint8Array()) };
    deserializeWsMessage('{"type":"str"}', serde);
    expect(serde.deserialize).not.toHaveBeenCalled();
  });
});

// ---------------------------------------------------------------------------
// routeWsActorMessage
// ---------------------------------------------------------------------------

describe('routeWsActorMessage', () => {
  it('routes a valid message and returns true', () => {
    const system = makeSystem();
    const result = routeWsActorMessage(
      { to: 'target/actor', type: 'ping', payload: { x: 1 } },
      system as unknown as ActorSystem,
    );
    expect(result).toBe(true);
    expect(system.send).toHaveBeenCalledTimes(1);
  });

  it('returns false and does not route when `to` is missing', () => {
    const system = makeSystem();
    expect(routeWsActorMessage({ type: 'ping' }, system as unknown as ActorSystem)).toBe(false);
    expect(system.send).not.toHaveBeenCalled();
  });

  it('returns false and does not route when `type` is missing', () => {
    const system = makeSystem();
    expect(routeWsActorMessage({ to: 'target/actor' }, system as unknown as ActorSystem)).toBe(false);
    expect(system.send).not.toHaveBeenCalled();
  });

  it('wraps the `to` field in an Address', () => {
    const system = makeSystem();
    routeWsActorMessage({ to: 'my/actor', type: 'hello' }, system as unknown as ActorSystem);
    const [toArg] = (system.send as ReturnType<typeof mock>).mock.calls[0];
    expect(String(toArg)).toBe('@(my/actor)');
  });
});

// ---------------------------------------------------------------------------
// makeHeartbeatPong
// ---------------------------------------------------------------------------

describe('makeHeartbeatPong', () => {
  it('returns correct shape', () => {
    const pong = makeHeartbeatPong({ id: 'test-id' });
    expect(pong).toEqual({
      type: 'HEARTBEAT_PONG',
      to: '__client__',
      from: '__system__',
      payload: {},
      id: 'test-id',
    });
  });
});
