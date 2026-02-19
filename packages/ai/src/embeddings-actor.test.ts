/**
 * Integration tests for EmbeddingsActor
 *
 * Address: ai/embeddings/<namespace>/<provider>/<model>
 * Example: ai/embeddings/bln_ai/nim/llama-3.3
 */

import { describe, it, expect, mock, beforeEach } from 'bun:test';
import {
  EmbeddingsActor,
  parseEmbeddingsAddress,
  createEmbeddingsFactory,
} from './embeddings-actor.ts';
import type { EmbeddingsActorConfig } from './embeddings-actor.ts';
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
    to: address('ai/embeddings/bln_ai/nim/llama-3.3'),
    from: address('sender'),
    correlationId: `corr-${seq}`,
    timestamp: Date.now(),
  };
}

const BASE_CONFIG: EmbeddingsActorConfig = {
  gatewayUrl: 'https://gateway.ai.cloudflare.com/v1/test-account/test-gateway',
  apiKey: 'test-api-key',
};

const VALID_ADDRESS = 'ai/embeddings/bln_ai/nim/llama-3.3';

// ---------------------------------------------------------------------------
// parseEmbeddingsAddress
// ---------------------------------------------------------------------------

describe('parseEmbeddingsAddress', () => {
  it('parses a valid embeddings address', () => {
    const result = parseEmbeddingsAddress('ai/embeddings/bln_ai/nim/llama-3.3');
    expect(result).not.toBeNull();
    expect(result!.namespace).toBe('bln_ai');
    expect(result!.provider).toBe('nim');
    expect(result!.model).toBe('llama-3.3');
  });

  it('handles model names with slashes (org/model)', () => {
    const result = parseEmbeddingsAddress('ai/embeddings/bln_ai/openai/text-embedding-3/small');
    expect(result).not.toBeNull();
    expect(result!.model).toBe('text-embedding-3/small');
  });

  it('returns null for wrong prefix (ai/inference/...)', () => {
    expect(parseEmbeddingsAddress('ai/inference/bln_ai/nim/model')).toBeNull();
  });

  it('returns null for too-short address', () => {
    expect(parseEmbeddingsAddress('ai/embeddings/ns/provider')).toBeNull();
  });

  it('returns null for wrong top-level prefix', () => {
    expect(parseEmbeddingsAddress('wrong/embeddings/ns/prov/model')).toBeNull();
  });

  it('returns null for empty string', () => {
    expect(parseEmbeddingsAddress('')).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

describe('EmbeddingsActor — constructor', () => {
  it('constructs successfully with a valid address', () => {
    expect(() => new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG)).not.toThrow();
  });

  it('throws for invalid address', () => {
    expect(() => new EmbeddingsActor('bad/address', BASE_CONFIG)).toThrow(
      /Invalid embeddings actor address/,
    );
  });

  it('throws for wrong-prefix address', () => {
    expect(() => new EmbeddingsActor('ai/tts/ns/prov/voice', BASE_CONFIG)).toThrow();
  });
});

// ---------------------------------------------------------------------------
// Unknown message type
// ---------------------------------------------------------------------------

describe('EmbeddingsActor — unknown message type', () => {
  it('returns error response for unknown type', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg('ai.unknown.type', {}));
    expect(response.success).toBe(false);
    expect(response.error).toContain('unhandled message type');
  });
});

// ---------------------------------------------------------------------------
// ai.discover
// ---------------------------------------------------------------------------

describe('EmbeddingsActor — ai.discover', () => {
  it('returns address in payload', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { address: string }).address).toBe(VALID_ADDRESS);
  });

  it('returns type = embeddings', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect((response.payload as { type: string }).type).toBe('embeddings');
  });

  it('includes provider and model in meta', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    const meta = (response.payload as { meta: Record<string, string> }).meta;
    expect(meta.provider).toBe('nim');
    expect(meta.model).toBe('llama-3.3');
  });
});

// ---------------------------------------------------------------------------
// ai.health
// ---------------------------------------------------------------------------

describe('EmbeddingsActor — ai.health', () => {
  it('returns { status: "ok" }', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { status: string }).status).toBe('ok');
  });

  it('echoes token from request', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, { token: 'embed-ping' }));
    expect((response.payload as { token: string }).token).toBe('embed-ping');
  });
});

// ---------------------------------------------------------------------------
// ai.embeddings.request — happy path
// ---------------------------------------------------------------------------

describe('EmbeddingsActor — ai.embeddings.request happy path', () => {
  let mockFetch: ReturnType<typeof mock>;

  beforeEach(() => {
    mockFetch = mock(() =>
      Promise.resolve({
        ok: true,
        json: () =>
          Promise.resolve({
            data: [
              { embedding: [0.1, 0.2, 0.3], index: 0, object: 'embedding' },
              { embedding: [0.4, 0.5, 0.6], index: 1, object: 'embedding' },
            ],
            model: 'llama-3.3',
            usage: { prompt_tokens: 6, total_tokens: 6 },
          }),
        text: () => Promise.resolve(''),
      }),
    );
    global.fetch = mockFetch as unknown as typeof fetch;
  });

  it('returns embeddings array in response', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, { inputs: ['hello', 'world'] }),
    );
    expect(response.success).toBe(true);
    const payload = response.payload as { embeddings: number[][] };
    expect(payload.embeddings).toHaveLength(2);
    expect(payload.embeddings[0]).toEqual([0.1, 0.2, 0.3]);
    expect(payload.embeddings[1]).toEqual([0.4, 0.5, 0.6]);
  });

  it('returns model in response payload', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, { inputs: ['hello'] }),
    );
    expect((response.payload as { model: string }).model).toBe('llama-3.3');
  });

  it('maps usage fields correctly', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, { inputs: ['hello'] }),
    );
    const usage = (response.payload as { usage: { promptTokens: number; totalTokens: number } }).usage;
    expect(usage.promptTokens).toBe(6);
    expect(usage.totalTokens).toBe(6);
  });

  it('model override in payload is sent to gateway', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    await actor.receive(
      msg(AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, {
        inputs: ['hello'],
        model: 'text-embedding-3-small',
      }),
    );
    const fetchCall = (mockFetch as ReturnType<typeof mock>).mock.calls[0];
    const body = JSON.parse((fetchCall[1] as RequestInit).body as string);
    expect(body.model).toBe('text-embedding-3-small');
  });

  it('sends inputs as the `input` field in the gateway request', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    await actor.receive(
      msg(AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, { inputs: ['foo', 'bar'] }),
    );
    const fetchCall = (mockFetch as ReturnType<typeof mock>).mock.calls[0];
    const body = JSON.parse((fetchCall[1] as RequestInit).body as string);
    expect(body.input).toEqual(['foo', 'bar']);
  });

  it('calls the /openai/v1/embeddings endpoint', async () => {
    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    await actor.receive(
      msg(AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, { inputs: ['test'] }),
    );
    const fetchCall = (mockFetch as ReturnType<typeof mock>).mock.calls[0];
    const url = fetchCall[0] as string;
    expect(url).toContain('/openai/v1/embeddings');
  });
});

// ---------------------------------------------------------------------------
// ai.embeddings.request — gateway error
// ---------------------------------------------------------------------------

describe('EmbeddingsActor — gateway error', () => {
  it('returns error response when fetch returns ok=false', async () => {
    global.fetch = mock(() =>
      Promise.resolve({
        ok: false,
        status: 422,
        text: () => Promise.resolve('Unprocessable Entity'),
        json: () => Promise.resolve({}),
      }),
    ) as unknown as typeof fetch;

    const actor = new EmbeddingsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, { inputs: ['hello'] }),
    );
    expect(response.success).toBe(false);
    expect(response.error).toContain('422');
  });
});

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

describe('createEmbeddingsFactory', () => {
  it('returns null for wrong prefix', () => {
    const factory = createEmbeddingsFactory(BASE_CONFIG);
    expect(factory('ai/inference/ns/prov/model')).toBeNull();
  });

  it('returns null for too-short address', () => {
    const factory = createEmbeddingsFactory(BASE_CONFIG);
    expect(factory('ai/embeddings/ns/prov')).toBeNull();
  });

  it('returns EmbeddingsActor for correct prefix', () => {
    const factory = createEmbeddingsFactory(BASE_CONFIG);
    const actor = factory('ai/embeddings/bln_ai/nim/llama-3.3');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(EmbeddingsActor);
  });
});
