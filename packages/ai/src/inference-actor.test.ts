/**
 * Integration tests for InferenceActor
 *
 * Address: ai/inference/<namespace>/<provider>/<model>
 * Example: ai/inference/bln_ai/nim/kimi-k2.5
 */

import { describe, it, expect, mock, beforeEach } from 'bun:test';
import {
  InferenceActor,
  parseInferenceAddress,
  createInferenceFactory,
} from './inference-actor.ts';
import type { InferenceActorConfig } from './inference-actor.ts';
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
    to: address('ai/inference/bln_ai/nim/kimi-k2.5'),
    from: address('sender'),
    correlationId: `corr-${seq}`,
    timestamp: Date.now(),
  };
}

const BASE_CONFIG: InferenceActorConfig = {
  gatewayUrl: 'https://gateway.ai.cloudflare.com/v1/test-account/test-gateway',
  apiKey: 'test-api-key',
};

const VALID_ADDRESS = 'ai/inference/bln_ai/nim/kimi-k2.5';

// ---------------------------------------------------------------------------
// parseInferenceAddress
// ---------------------------------------------------------------------------

describe('parseInferenceAddress', () => {
  it('parses a valid 5-segment inference address', () => {
    const result = parseInferenceAddress('ai/inference/bln_ai/nim/kimi-k2.5');
    expect(result).not.toBeNull();
    expect(result!.namespace).toBe('bln_ai');
    expect(result!.provider).toBe('nim');
    expect(result!.model).toBe('kimi-k2.5');
  });

  it('handles model names with slashes (org/model)', () => {
    const result = parseInferenceAddress('ai/inference/bln_ai/openai/gpt-4o/mini');
    expect(result).not.toBeNull();
    expect(result!.model).toBe('gpt-4o/mini');
  });

  it('returns null for wrong prefix (ai/tts/...)', () => {
    expect(parseInferenceAddress('ai/tts/bln_ai/deepgram/aura-2')).toBeNull();
  });

  it('returns null for too-short address', () => {
    expect(parseInferenceAddress('ai/inference/ns/provider')).toBeNull();
  });

  it('returns null for empty string', () => {
    expect(parseInferenceAddress('')).toBeNull();
  });

  it('returns null for completely wrong prefix', () => {
    expect(parseInferenceAddress('wrong/inference/ns/prov/model')).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

describe('InferenceActor — constructor', () => {
  it('constructs successfully with a valid address', () => {
    expect(() => new InferenceActor(VALID_ADDRESS, BASE_CONFIG)).not.toThrow();
  });

  it('throws for invalid address', () => {
    expect(() => new InferenceActor('bad/address', BASE_CONFIG)).toThrow(
      /Invalid inference actor address/,
    );
  });

  it('throws for address with wrong prefix', () => {
    expect(() => new InferenceActor('ai/tts/ns/prov/voice', BASE_CONFIG)).toThrow();
  });
});

// ---------------------------------------------------------------------------
// Unknown message type
// ---------------------------------------------------------------------------

describe('InferenceActor — unknown message type', () => {
  it('returns error response for unknown type', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg('ai.unknown.type', {}));
    expect(response.success).toBe(false);
    expect(response.error).toContain('unhandled message type');
  });
});

// ---------------------------------------------------------------------------
// ai.discover
// ---------------------------------------------------------------------------

describe('InferenceActor — ai.discover', () => {
  it('returns address in payload', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { address: string }).address).toBe(VALID_ADDRESS);
  });

  it('returns type = inference', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect((response.payload as { type: string }).type).toBe('inference');
  });

  it('includes provider and model in meta', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    const meta = (response.payload as { meta: Record<string, string> }).meta;
    expect(meta.provider).toBe('nim');
    expect(meta.model).toBe('kimi-k2.5');
  });
});

// ---------------------------------------------------------------------------
// ai.health
// ---------------------------------------------------------------------------

describe('InferenceActor — ai.health', () => {
  it('returns { status: "ok" }', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { status: string }).status).toBe('ok');
  });

  it('echoes token from request', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, { token: 'abc' }));
    expect((response.payload as { token: string }).token).toBe('abc');
  });
});

// ---------------------------------------------------------------------------
// ai.inference.request — happy path
// ---------------------------------------------------------------------------

describe('InferenceActor — ai.inference.request happy path', () => {
  let mockFetch: ReturnType<typeof mock>;

  beforeEach(() => {
    mockFetch = mock(() =>
      Promise.resolve({
        ok: true,
        json: () =>
          Promise.resolve({
            choices: [{ message: { content: 'inference response' }, finish_reason: 'stop' }],
            model: 'kimi-k2.5',
            usage: { prompt_tokens: 8, completion_tokens: 4, total_tokens: 12 },
          }),
        text: () => Promise.resolve(''),
      }),
    );
    global.fetch = mockFetch as unknown as typeof fetch;
  });

  it('returns content from gateway response', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hello' }],
      }),
    );
    expect(response.success).toBe(true);
    expect((response.payload as { content: string }).content).toBe('inference response');
  });

  it('maps usage tokens correctly', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hello' }],
      }),
    );
    const usage = (response.payload as { usage: { promptTokens: number; totalTokens: number } }).usage;
    expect(usage.promptTokens).toBe(8);
    expect(usage.totalTokens).toBe(12);
  });

  it('model override in payload is sent to gateway', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hi' }],
        model: 'gpt-4o',
      }),
    );
    const fetchCall = (mockFetch as ReturnType<typeof mock>).mock.calls[0];
    const body = JSON.parse((fetchCall[1] as RequestInit).body as string);
    expect(body.model).toBe('gpt-4o');
  });

  it('uses actor model when no override given', async () => {
    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hi' }],
      }),
    );
    const fetchCall = (mockFetch as ReturnType<typeof mock>).mock.calls[0];
    const body = JSON.parse((fetchCall[1] as RequestInit).body as string);
    expect(body.model).toBe('kimi-k2.5');
  });
});

// ---------------------------------------------------------------------------
// ai.inference.request — gateway error
// ---------------------------------------------------------------------------

describe('InferenceActor — gateway error', () => {
  it('returns error response when fetch returns ok=false', async () => {
    global.fetch = mock(() =>
      Promise.resolve({
        ok: false,
        status: 429,
        text: () => Promise.resolve('Rate limit exceeded'),
        json: () => Promise.resolve({}),
      }),
    ) as unknown as typeof fetch;

    const actor = new InferenceActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hi' }],
      }),
    );
    expect(response.success).toBe(false);
    expect(response.error).toContain('429');
  });
});

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

describe('createInferenceFactory', () => {
  it('returns null for wrong prefix', () => {
    const factory = createInferenceFactory(BASE_CONFIG);
    expect(factory('ai/tts/ns/prov/voice')).toBeNull();
  });

  it('returns null for incomplete address', () => {
    const factory = createInferenceFactory(BASE_CONFIG);
    expect(factory('ai/inference/ns/prov')).toBeNull();
  });

  it('returns InferenceActor for correct prefix', () => {
    const factory = createInferenceFactory(BASE_CONFIG);
    const actor = factory('ai/inference/bln_ai/nim/kimi-k2.5');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(InferenceActor);
  });
});
