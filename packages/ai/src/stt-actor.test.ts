/**
 * Integration tests for SttActor and SystemSttActor
 *
 * Address: ai/stt/<namespace>/<provider>/<model>
 * Example: ai/stt/bln_ai/deepgram/nova-3
 */

import { describe, it, expect, mock, beforeEach } from 'bun:test';
import { SttActor, SystemSttActor, parseSttAddress, createSttFactory } from './stt-actor.ts';
import type { SttActorConfig } from './stt-actor.ts';
import { address } from '@agentic-primer/actors';
import type { Message } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

let seq = 0;
function msg(type: string, payload: unknown, from?: string): Message {
  return {
    id: `test-${++seq}`,
    pattern: 'ask',
    type,
    payload,
    to: address('ai/stt/bln_ai/deepgram/nova-3'),
    from: from ? address(from) : address('sender'),
    correlationId: `corr-${seq}`,
    timestamp: Date.now(),
  };
}

const BASE_CONFIG: SttActorConfig = {
  gatewayUrl: 'https://gateway.ai.cloudflare.com/v1/test-account/test-gateway',
  apiKey: 'test-deepgram-key',
  minBufferBytes: 4, // low threshold to trigger transcription in tests
};

const VALID_ADDRESS = 'ai/stt/bln_ai/deepgram/nova-3';

// ---------------------------------------------------------------------------
// parseSttAddress
// ---------------------------------------------------------------------------

describe('parseSttAddress', () => {
  it('parses a valid STT address', () => {
    const result = parseSttAddress('ai/stt/bln_ai/deepgram/nova-3');
    expect(result).not.toBeNull();
    expect(result!.namespace).toBe('bln_ai');
    expect(result!.provider).toBe('deepgram');
    expect(result!.model).toBe('nova-3');
  });

  it('handles model names with slashes', () => {
    const result = parseSttAddress('ai/stt/bln_ai/deepgram/nova-3/en-us');
    expect(result).not.toBeNull();
    expect(result!.model).toBe('nova-3/en-us');
  });

  it('returns null for wrong prefix (ai/tts/...)', () => {
    expect(parseSttAddress('ai/tts/bln_ai/deepgram/aura-2')).toBeNull();
  });

  it('returns null for too-short address', () => {
    expect(parseSttAddress('ai/stt/ns/provider')).toBeNull();
  });

  it('returns null for wrong top-level prefix', () => {
    expect(parseSttAddress('wrong/stt/ns/prov/model')).toBeNull();
  });

  it('returns null for empty string', () => {
    expect(parseSttAddress('')).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

describe('SttActor — constructor', () => {
  it('constructs successfully with a valid address', () => {
    expect(() => new SttActor(VALID_ADDRESS, BASE_CONFIG)).not.toThrow();
  });

  it('throws for invalid address', () => {
    expect(() => new SttActor('bad/address', BASE_CONFIG)).toThrow(
      /Invalid STT actor address/,
    );
  });

  it('throws for wrong-prefix address', () => {
    expect(() => new SttActor('ai/tts/ns/prov/voice', BASE_CONFIG)).toThrow();
  });
});

// ---------------------------------------------------------------------------
// Unknown message type
// ---------------------------------------------------------------------------

describe('SttActor — unknown message type', () => {
  it('returns error response for unknown type', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg('ai.unknown.type', {}));
    expect(response.success).toBe(false);
    expect(response.error).toContain('unhandled message type');
  });
});

// ---------------------------------------------------------------------------
// ai.discover
// ---------------------------------------------------------------------------

describe('SttActor — ai.discover', () => {
  it('returns address in payload', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { address: string }).address).toBe(VALID_ADDRESS);
  });

  it('returns type = stt', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect((response.payload as { type: string }).type).toBe('stt');
  });

  it('includes model and provider in meta', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    const meta = (response.payload as { meta: Record<string, string> }).meta;
    expect(meta.provider).toBe('deepgram');
    expect(meta.model).toBe('nova-3');
  });
});

// ---------------------------------------------------------------------------
// ai.health
// ---------------------------------------------------------------------------

describe('SttActor — ai.health', () => {
  it('returns { status: "ok" }', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { status: string }).status).toBe('ok');
  });

  it('echoes token from request', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, { token: 'stt-ping' }));
    expect((response.payload as { token: string }).token).toBe('stt-ping');
  });
});

// ---------------------------------------------------------------------------
// ai.stt.start
// ---------------------------------------------------------------------------

describe('SttActor — ai.stt.start', () => {
  it('returns status started with defaults', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.STT_START, {}));
    expect(response.success).toBe(true);
    const payload = response.payload as { status: string; encoding: string; sampleRate: number; language: string };
    expect(payload.status).toBe('started');
    expect(payload.encoding).toBe('linear16');
    expect(payload.sampleRate).toBe(16000);
    expect(payload.language).toBe('en-US');
  });

  it('accepts encoding and language overrides', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.STT_START, { encoding: 'opus', language: 'fr-FR', sampleRate: 48000 }),
    );
    const payload = response.payload as { encoding: string; language: string; sampleRate: number };
    expect(payload.encoding).toBe('opus');
    expect(payload.language).toBe('fr-FR');
    expect(payload.sampleRate).toBe(48000);
  });
});

// ---------------------------------------------------------------------------
// ai.stt.stop
// ---------------------------------------------------------------------------

describe('SttActor — ai.stt.stop', () => {
  it('returns error when there is no active session', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.STT_STOP, {}));
    // No session started for this sender — should be error
    expect(response.success).toBe(false);
    expect(response.error).toContain('No active STT session');
  });

  it('returns status stopped after a session was started', async () => {
    // Mock fetch so flushAndTranscribe does not throw if audio buffer is empty
    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        json: () => Promise.resolve({ results: { channels: [{ alternatives: [{ transcript: '' }] }] } }),
        text: () => Promise.resolve(''),
      }),
    ) as unknown as typeof fetch;

    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG);
    // Use same sender for both start and stop
    const startMsg = msg(AI_MESSAGE_TYPES.STT_START, {}, 'client-a');
    const stopMsg: Message = {
      ...msg(AI_MESSAGE_TYPES.STT_STOP, {}),
      from: address('client-a'),
    };
    await actor.receive(startMsg);
    const response = await actor.receive(stopMsg);
    expect(response.success).toBe(true);
    expect((response.payload as { status: string }).status).toBe('stopped');
  });
});

// ---------------------------------------------------------------------------
// audio.frame — happy path
// ---------------------------------------------------------------------------

describe('SttActor — audio.frame', () => {
  beforeEach(() => {
    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        json: () =>
          Promise.resolve({
            results: {
              channels: [{ alternatives: [{ transcript: 'hello world' }] }],
            },
          }),
        text: () => Promise.resolve(''),
      }),
    ) as unknown as typeof fetch;
  });

  it('buffers small audio frames below minBuffer threshold', async () => {
    // minBufferBytes=4, send 2 bytes — should not flush
    const actor = new SttActor('ai/stt/bln_ai/deepgram/nova-3', { ...BASE_CONFIG, minBufferBytes: 1024 });
    const audioMsg: Message = {
      ...msg(AI_MESSAGE_TYPES.AUDIO_FRAME, new Uint8Array([0xAA, 0xBB])),
    };
    const response = await actor.receive(audioMsg);
    expect(response.success).toBe(true);
    expect((response.payload as { buffered: number }).buffered).toBe(2);
  });

  it('flushes buffer when threshold exceeded (minBufferBytes=4)', async () => {
    const actor = new SttActor(VALID_ADDRESS, BASE_CONFIG); // minBufferBytes=4
    const audioMsg: Message = {
      ...msg(AI_MESSAGE_TYPES.AUDIO_FRAME, new Uint8Array([1, 2, 3, 4, 5])),
    };
    const response = await actor.receive(audioMsg);
    expect(response.success).toBe(true);
    // After flush, buffer resets to 0
    expect((response.payload as { buffered: number }).buffered).toBe(0);
  });
});

// ---------------------------------------------------------------------------
// SystemSttActor
// ---------------------------------------------------------------------------

describe('SystemSttActor', () => {
  it('constructs with system reference', () => {
    const systemMock = { send: mock(() => {}) };
    expect(
      () => new SystemSttActor(VALID_ADDRESS, BASE_CONFIG, systemMock),
    ).not.toThrow();
  });

  it('dispatches transcript to system.send on flush', async () => {
    const sent: unknown[] = [];
    const systemMock = {
      send: mock((_to: unknown, _type: string, _payload: unknown) => {
        sent.push({ _to, _type, _payload });
      }),
    };

    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        json: () =>
          Promise.resolve({
            results: {
              channels: [{ alternatives: [{ transcript: 'dispatched transcript' }] }],
            },
          }),
        text: () => Promise.resolve(''),
      }),
    ) as unknown as typeof fetch;

    const actor = new SystemSttActor(VALID_ADDRESS, BASE_CONFIG, systemMock);
    // Send audio above minBuffer threshold (4 bytes)
    const audioMsg: Message = {
      ...msg(AI_MESSAGE_TYPES.AUDIO_FRAME, new Uint8Array([1, 2, 3, 4, 5])),
      from: address('client-dispatch'),
    };
    await actor.receive(audioMsg);
    // system.send should have been called with the transcript
    expect(systemMock.send).toHaveBeenCalled();
  });
});

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

describe('createSttFactory', () => {
  const systemMock = { send: mock(() => {}) };

  it('returns null for wrong prefix', () => {
    const factory = createSttFactory(BASE_CONFIG, systemMock);
    expect(factory('ai/tts/ns/prov/voice')).toBeNull();
  });

  it('returns null for too-short address', () => {
    const factory = createSttFactory(BASE_CONFIG, systemMock);
    expect(factory('ai/stt/ns/prov')).toBeNull();
  });

  it('returns SystemSttActor for correct prefix', () => {
    const factory = createSttFactory(BASE_CONFIG, systemMock);
    const actor = factory('ai/stt/bln_ai/deepgram/nova-3');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(SystemSttActor);
  });
});
