import { describe, it, expect, mock, beforeEach } from 'bun:test';
import {
  heartbeatMiddleware,
  actorRoutingMiddleware,
  composeWsMiddleware,
  deserializeWsMessage,
  routeWsActorMessage,
  makeHeartbeatPong,
  fnv32a,
  addressToChannelId,
  encodeBinaryFrame,
  decodeBinaryFrame,
  handleBinaryFrame,
  binaryChannelMiddleware,
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
    const spy = mock(() => {});
    const original = console.error;
    console.error = spy;
    try {
      await handle(JSON.stringify({}), () => {});
    } finally {
      console.error = original;
    }
    expect(spy).toHaveBeenCalledTimes(1);
  });

  it('catches double-next errors without propagating', async () => {
    const doubleNext: WsMiddleware = async (_ctx, next) => {
      await next();
      await next(); // second call throws inside compose — should be swallowed
    };
    const handle = composeWsMiddleware(doubleNext);
    const spy = mock(() => {});
    const original = console.error;
    console.error = spy;
    try {
      await handle(JSON.stringify({}), () => {});
    } finally {
      console.error = original;
    }
    expect(spy).toHaveBeenCalledTimes(1);
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

// ---------------------------------------------------------------------------
// fnv32a / addressToChannelId
// ---------------------------------------------------------------------------

describe('fnv32a', () => {
  it('returns a number', () => {
    expect(typeof fnv32a('hello')).toBe('number');
  });

  it('is deterministic for the same input', () => {
    const addr = 'ai/inference/bln_ai/nim/kimi-k2.5';
    expect(fnv32a(addr)).toBe(fnv32a(addr));
  });

  it('produces different values for different inputs', () => {
    expect(fnv32a('ai/tts/bln_ai/deepgram/aura-2')).not.toBe(
      fnv32a('ai/stt/bln_ai/deepgram/nova-3')
    );
  });

  it('returns an unsigned 32-bit integer (0 ≤ n ≤ 0xFFFFFFFF)', () => {
    const h = fnv32a('any-address');
    expect(h).toBeGreaterThanOrEqual(0);
    expect(h).toBeLessThanOrEqual(0xFFFFFFFF);
  });

  it('empty string returns the FNV-1a offset basis', () => {
    // fnv32a('') = 0x811c9dc5 (2166136261) — offset basis XOR'd with nothing
    expect(fnv32a('')).toBe(0x811c9dc5);
  });
});

describe('addressToChannelId', () => {
  it('returns same result as fnv32a', () => {
    const addr = 'ai/stt/bln_ai/deepgram/nova-3';
    expect(addressToChannelId(addr)).toBe(fnv32a(addr));
  });
});

// ---------------------------------------------------------------------------
// encodeBinaryFrame / decodeBinaryFrame
// ---------------------------------------------------------------------------

describe('encodeBinaryFrame / decodeBinaryFrame', () => {
  it('round-trips channelId and payload', () => {
    const channelId = 0xDEADBEEF;
    const payload = new Uint8Array([1, 2, 3, 4, 5]);
    const frame = encodeBinaryFrame(channelId, payload);

    const decoded = decodeBinaryFrame(frame);
    expect(decoded).not.toBeNull();
    expect(decoded!.channelId).toBe(channelId >>> 0); // unsigned
    expect(decoded!.payload).toEqual(payload);
  });

  it('encoded frame length is 4 + payload.byteLength', () => {
    const payload = new Uint8Array(100);
    expect(encodeBinaryFrame(0, payload).byteLength).toBe(104);
  });

  it('channelId is stored little-endian (first 4 bytes)', () => {
    const frame = encodeBinaryFrame(1, new Uint8Array(0));
    expect(frame[0]).toBe(1);
    expect(frame[1]).toBe(0);
    expect(frame[2]).toBe(0);
    expect(frame[3]).toBe(0);
  });

  it('decodeBinaryFrame returns null for frames shorter than 4 bytes', () => {
    expect(decodeBinaryFrame(new Uint8Array(3))).toBeNull();
    expect(decodeBinaryFrame(new Uint8Array(0))).toBeNull();
  });

  it('handles empty payload', () => {
    const frame = encodeBinaryFrame(42, new Uint8Array(0));
    const decoded = decodeBinaryFrame(frame);
    expect(decoded).not.toBeNull();
    expect(decoded!.channelId).toBe(42);
    expect(decoded!.payload.byteLength).toBe(0);
  });

  it('preserves large channelId (0xFFFFFFFF)', () => {
    const frame = encodeBinaryFrame(0xFFFFFFFF, new Uint8Array([9]));
    const decoded = decodeBinaryFrame(frame);
    expect(decoded!.channelId).toBe(0xFFFFFFFF);
  });
});

// ---------------------------------------------------------------------------
// handleBinaryFrame
// ---------------------------------------------------------------------------

describe('handleBinaryFrame', () => {
  it('routes to system.send when channelId is registered', () => {
    const system = makeSystem();
    const addr = 'ai/stt/bln_ai/deepgram/nova-3';
    const channelId = fnv32a(addr);
    const channelMap = new Map([[channelId, addr]]);

    const payload = new Uint8Array([0xAA, 0xBB]);
    const frame = encodeBinaryFrame(channelId, payload);

    const routed = handleBinaryFrame(frame, channelMap, system as unknown as ActorSystem);

    expect(routed).toBe(true);
    expect(system.send).toHaveBeenCalledTimes(1);
    const [toArg, typeArg, payloadArg] = (system.send as ReturnType<typeof mock>).mock.calls[0];
    expect(String(toArg)).toBe(`@(${addr})`);
    expect(typeArg).toBe('audio.frame');
    expect(payloadArg).toEqual(payload);
  });

  it('returns false when channelId is not registered', () => {
    const system = makeSystem();
    const frame = encodeBinaryFrame(0x12345678, new Uint8Array([1]));
    const routed = handleBinaryFrame(frame, new Map(), system as unknown as ActorSystem);
    expect(routed).toBe(false);
    expect(system.send).not.toHaveBeenCalled();
  });

  it('returns false for frames shorter than 4 bytes', () => {
    const system = makeSystem();
    const routed = handleBinaryFrame(new Uint8Array(2), new Map(), system as unknown as ActorSystem);
    expect(routed).toBe(false);
    expect(system.send).not.toHaveBeenCalled();
  });
});

// ---------------------------------------------------------------------------
// binaryChannelMiddleware
// ---------------------------------------------------------------------------

describe('binaryChannelMiddleware', () => {
  it('registers address and sends channel:opened on channel:open', async () => {
    const channelMap = new Map<number, string>();
    const addr = 'ai/tts/bln_ai/deepgram/aura-2';
    const expectedChannelId = fnv32a(addr);
    const sent: string[] = [];

    const mw = binaryChannelMiddleware(channelMap);
    const ctx = makeCtx({ type: 'channel:open', address: addr }, sent);
    await mw(ctx, mock(noop));

    // Should have registered in the channel map
    expect(channelMap.get(expectedChannelId)).toBe(addr);

    // Should have sent channel:opened
    expect(sent).toHaveLength(1);
    const opened = JSON.parse(sent[0]);
    expect(opened.type).toBe('channel:opened');
    expect(opened.channelId).toBe(expectedChannelId);
    expect(opened.address).toBe(addr);
  });

  it('consumes channel:open — does not call next()', async () => {
    const channelMap = new Map<number, string>();
    const next = mock(noop);
    const mw = binaryChannelMiddleware(channelMap);
    const ctx = makeCtx({ type: 'channel:open', address: 'ai/stt/bln_ai/deepgram/nova-3' });
    await mw(ctx, next);
    expect(next).not.toHaveBeenCalled();
  });

  it('sends channel:error on channelId collision (different address)', async () => {
    const addr1 = 'ai/tts/bln_ai/deepgram/aura-2';
    const channelId = fnv32a(addr1);
    // Pre-populate map with a different address that hashes to the same id
    const channelMap = new Map([[channelId, 'some/other/address']]);
    const sent: string[] = [];

    const mw = binaryChannelMiddleware(channelMap);
    const ctx = makeCtx({ type: 'channel:open', address: addr1 }, sent);
    await mw(ctx, mock(noop));

    expect(sent).toHaveLength(1);
    const err = JSON.parse(sent[0]);
    expect(err.type).toBe('channel:error');
    expect(err.code).toBe('channel_id_collision');
    // Map should not have been overwritten
    expect(channelMap.get(channelId)).toBe('some/other/address');
  });

  it('allows re-opening the same address (idempotent)', async () => {
    const addr = 'ai/stt/bln_ai/deepgram/nova-3';
    const channelId = fnv32a(addr);
    const channelMap = new Map([[channelId, addr]]); // already registered
    const sent: string[] = [];

    const mw = binaryChannelMiddleware(channelMap);
    await mw(makeCtx({ type: 'channel:open', address: addr }, sent), mock(noop));

    // No error, channel:opened sent again
    expect(sent).toHaveLength(1);
    expect(JSON.parse(sent[0]).type).toBe('channel:opened');
  });

  it('removes address from channelMap on channel:close', async () => {
    const addr = 'ai/tts/bln_ai/deepgram/aura-2';
    const channelId = fnv32a(addr);
    const channelMap = new Map([[channelId, addr]]);

    const mw = binaryChannelMiddleware(channelMap);
    await mw(makeCtx({ type: 'channel:close', channelId }), mock(noop));

    expect(channelMap.has(channelId)).toBe(false);
  });

  it('consumes channel:close — does not call next()', async () => {
    const channelId = fnv32a('ai/tts/bln_ai/deepgram/aura-2');
    const channelMap = new Map([[channelId, 'any']]);
    const next = mock(noop);

    const mw = binaryChannelMiddleware(channelMap);
    await mw(makeCtx({ type: 'channel:close', channelId }), next);
    expect(next).not.toHaveBeenCalled();
  });

  it('passes non-channel messages to next()', async () => {
    const channelMap = new Map<number, string>();
    const next = mock(noop);
    const mw = binaryChannelMiddleware(channelMap);
    await mw(makeCtx({ type: 'actor/ping', to: 'ai/inference/bln_ai/nim/kimi-k2.5' }), next);
    expect(next).toHaveBeenCalledTimes(1);
  });

  it('integrates correctly with full middleware stack', async () => {
    const system = makeSystem();
    const addr = 'ai/inference/bln_ai/nim/kimi-k2.5';
    const channelMap = new Map<number, string>();
    const sent: string[] = [];

    const handle = composeWsMiddleware(
      binaryChannelMiddleware(channelMap),
      heartbeatMiddleware,
      actorRoutingMiddleware(system as unknown as ActorSystem),
    );

    // channel:open — should register and send channel:opened
    await handle(
      JSON.stringify({ type: 'channel:open', address: addr }),
      (json) => sent.push(json),
    );
    expect(sent).toHaveLength(1);
    expect(JSON.parse(sent[0]).type).toBe('channel:opened');
    expect(system.send).not.toHaveBeenCalled();

    // actor message — should route through to system
    await handle(
      JSON.stringify({ type: 'ai.inference.request', to: addr, payload: { messages: [] } }),
      (json) => sent.push(json),
    );
    expect(system.send).toHaveBeenCalledTimes(1);
  });
});
