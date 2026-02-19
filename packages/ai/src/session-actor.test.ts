/**
 * Integration tests for SessionActor
 *
 * Address: ai/session/<user-id>
 * Example: ai/session/user-abc123
 */

import { describe, it, expect, mock, beforeEach } from 'bun:test';
import { SessionActor, createSessionFactory } from './session-actor.ts';
import type { SessionActorConfig } from './session-actor.ts';
import { address } from '@agentic-primer/actors';
import type { Message } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

let seq = 0;
function msg(type: string, payload: unknown): Message {
  return {
    id: `test-${++seq}`,
    pattern: 'ask',
    type,
    payload,
    to: address('ai/session/user-abc123'),
    from: address('sender'),
    correlationId: `corr-${seq}`,
    timestamp: Date.now(),
  };
}

const BASE_CONFIG: SessionActorConfig = {
  namespace: 'bln_ai',
  defaultInferenceModel: 'nim/kimi-k2.5',
  defaultTtsVoice: 'deepgram/aura-2-en-us',
  defaultSttModel: 'deepgram/nova-3',
};

function makeSystem() {
  return { send: mock(() => {}) };
}

const VALID_ADDRESS = 'ai/session/user-abc123';

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

describe('SessionActor — constructor', () => {
  it('constructs successfully with a valid address', () => {
    const system = makeSystem();
    expect(() => new SessionActor(VALID_ADDRESS, BASE_CONFIG, system)).not.toThrow();
  });

  it('extracts userId from address segments', () => {
    const system = makeSystem();
    const actor = new SessionActor('ai/session/my-user-id', BASE_CONFIG, system);
    // Verify by sending SESSION_CREATE and checking the state userId
    return actor.receive(msg(AI_MESSAGE_TYPES.SESSION_CREATE, {})).then((response) => {
      expect((response.payload as { userId: string }).userId).toBe('my-user-id');
    });
  });

  it('handles multi-segment userId (ai/session/org/user)', async () => {
    const system = makeSystem();
    const actor = new SessionActor('ai/session/org/user', BASE_CONFIG, system);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_CREATE, {}));
    expect((response.payload as { userId: string }).userId).toBe('org/user');
  });
});

// ---------------------------------------------------------------------------
// Unknown message type
// ---------------------------------------------------------------------------

describe('SessionActor — unknown message type', () => {
  it('returns error response for unknown type', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg('ai.unknown.type', {}));
    expect(response.success).toBe(false);
    expect(response.error).toContain('unhandled message type');
  });
});

// ---------------------------------------------------------------------------
// ai.discover
// ---------------------------------------------------------------------------

describe('SessionActor — ai.discover', () => {
  it('returns address in payload', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { address: string }).address).toBe(VALID_ADDRESS);
  });

  it('returns type = session', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect((response.payload as { type: string }).type).toBe('session');
  });

  it('includes userId in meta', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    const meta = (response.payload as { meta: Record<string, string> }).meta;
    expect(meta.userId).toBe('user-abc123');
  });
});

// ---------------------------------------------------------------------------
// ai.health
// ---------------------------------------------------------------------------

describe('SessionActor — ai.health', () => {
  it('returns { status: "ok" }', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { status: string }).status).toBe('ok');
  });

  it('echoes token from request', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, { token: 'session-ping' }));
    expect((response.payload as { token: string }).token).toBe('session-ping');
  });
});

// ---------------------------------------------------------------------------
// SESSION_CREATE
// ---------------------------------------------------------------------------

describe('SessionActor — SESSION_CREATE', () => {
  it('returns state with userId', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_CREATE, { userId: 'user-abc123' }));
    expect(response.success).toBe(true);
    const payload = response.payload as { userId: string; activeActors: string[]; createdAt: number };
    expect(payload.userId).toBe('user-abc123');
    expect(Array.isArray(payload.activeActors)).toBe(true);
    expect(payload.activeActors).toHaveLength(0);
  });

  it('returns createdAt timestamp', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const before = Date.now();
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_CREATE, {}));
    const after = Date.now();
    const createdAt = (response.payload as { createdAt: number }).createdAt;
    expect(createdAt).toBeGreaterThanOrEqual(before);
    expect(createdAt).toBeLessThanOrEqual(after);
  });

  it('stores optional metadata in state', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.SESSION_CREATE, {
        userId: 'user-abc123',
        metadata: { plan: 'pro' },
      }),
    );
    const payload = response.payload as { metadata?: { plan: string } };
    expect(payload.metadata?.plan).toBe('pro');
  });
});

// ---------------------------------------------------------------------------
// SESSION_END
// ---------------------------------------------------------------------------

describe('SessionActor — SESSION_END', () => {
  it('returns stoppedActors list (initially empty)', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_END, {}));
    expect(response.success).toBe(true);
    const payload = response.payload as { userId: string; stoppedActors: string[] };
    expect(payload.userId).toBe('user-abc123');
    expect(Array.isArray(payload.stoppedActors)).toBe(true);
  });

  it('returns previously active actors in stoppedActors after inference forward', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);

    // Forward inference to register an active actor
    await actor.receive(msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
      messages: [{ role: 'user', content: 'hi' }],
    }));

    const response = await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_END, {}));
    const payload = response.payload as { stoppedActors: string[] };
    expect(payload.stoppedActors.length).toBeGreaterThan(0);
  });

  it('activeActors is empty after SESSION_END', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);

    await actor.receive(msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
      messages: [{ role: 'user', content: 'hi' }],
    }));
    await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_END, {}));

    // Verify state cleared via SESSION_CREATE which returns current state
    const stateResponse = await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_CREATE, {}));
    const payload = stateResponse.payload as { activeActors: string[] };
    expect(payload.activeActors).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// INFERENCE_REQUEST forwarding
// ---------------------------------------------------------------------------

describe('SessionActor — INFERENCE_REQUEST forwarding', () => {
  it('returns forwarded address string in response payload', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hello' }],
      }),
    );
    expect(response.success).toBe(true);
    const forwarded = (response.payload as { forwarded: string }).forwarded;
    expect(typeof forwarded).toBe('string');
    expect(forwarded).toContain('ai/inference/');
  });

  it('calls system.send with correct actor address', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hello' }],
      }),
    );
    expect(system.send).toHaveBeenCalledTimes(1);
    const toArg = String((system.send as ReturnType<typeof mock>).mock.calls[0][0]);
    expect(toArg).toContain('ai/inference/bln_ai/nim/kimi-k2.5');
  });

  it('adds forwarded inference actor to activeActors', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);

    await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hello' }],
      }),
    );

    const stateResponse = await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_CREATE, {}));
    const payload = stateResponse.payload as { activeActors: string[] };
    expect(payload.activeActors).toContain('ai/inference/bln_ai/nim/kimi-k2.5');
  });

  it('does not add duplicate actors when same inference is requested twice', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);

    await actor.receive(msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, { messages: [] }));
    await actor.receive(msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, { messages: [] }));

    const stateResponse = await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_CREATE, {}));
    const payload = stateResponse.payload as { activeActors: string[] };
    // Should only have one entry for the inference actor
    const inferenceActors = payload.activeActors.filter((a) => a.startsWith('ai/inference/'));
    expect(inferenceActors).toHaveLength(1);
  });
});

// ---------------------------------------------------------------------------
// TTS_REQUEST forwarding
// ---------------------------------------------------------------------------

describe('SessionActor — TTS_REQUEST forwarding', () => {
  it('returns forwarded address string containing ai/tts/', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.TTS_REQUEST, { text: 'Hello' }),
    );
    expect(response.success).toBe(true);
    const forwarded = (response.payload as { forwarded: string }).forwarded;
    expect(forwarded).toContain('ai/tts/');
  });
});

// ---------------------------------------------------------------------------
// STT_START forwarding
// ---------------------------------------------------------------------------

describe('SessionActor — STT_START forwarding', () => {
  it('returns forwarded address string containing ai/stt/', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.STT_START, {}));
    expect(response.success).toBe(true);
    const forwarded = (response.payload as { forwarded: string }).forwarded;
    expect(forwarded).toContain('ai/stt/');
  });
});

// ---------------------------------------------------------------------------
// session.state message
// ---------------------------------------------------------------------------

describe('SessionActor — session.state', () => {
  it('returns current session state', async () => {
    const system = makeSystem();
    const actor = new SessionActor(VALID_ADDRESS, BASE_CONFIG, system);
    const response = await actor.receive(msg('session.state', {}));
    expect(response.success).toBe(true);
    const payload = response.payload as { userId: string; activeActors: string[] };
    expect(payload.userId).toBe('user-abc123');
    expect(Array.isArray(payload.activeActors)).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

describe('createSessionFactory', () => {
  it('returns null for wrong prefix', () => {
    const system = makeSystem();
    const factory = createSessionFactory(BASE_CONFIG, system);
    expect(factory('ai/inference/ns/prov/model')).toBeNull();
  });

  it('returns null for address missing user-id segment', () => {
    const system = makeSystem();
    const factory = createSessionFactory(BASE_CONFIG, system);
    expect(factory('ai/session')).toBeNull();
  });

  it('returns SessionActor for correct prefix', () => {
    const system = makeSystem();
    const factory = createSessionFactory(BASE_CONFIG, system);
    const actor = factory('ai/session/user-abc123');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(SessionActor);
  });

  it('returns SessionActor for any user-id', () => {
    const system = makeSystem();
    const factory = createSessionFactory(BASE_CONFIG, system);
    const actor = factory('ai/session/any-user');
    expect(actor).toBeInstanceOf(SessionActor);
  });
});
