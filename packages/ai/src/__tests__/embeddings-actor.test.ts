/**
 * Tests for EmbeddingsActor
 *
 * Coverage:
 * - parseEmbeddingsAddress: valid and invalid addresses
 * - EmbeddingsActor.receive: ai.embeddings.request → calls fetch with correct URL/body, returns embeddings
 * - EmbeddingsActor.receive: gateway returns error status → returns error response
 * - EmbeddingsActor.receive: unknown message type → error response
 * - createEmbeddingsFactory: returns actor for valid prefix, null for invalid
 */

import { describe, it, expect, afterEach } from 'bun:test';
import { createMessage, address } from '@agentic-primer/actors';
import {
  parseEmbeddingsAddress,
  EmbeddingsActor,
  createEmbeddingsFactory,
} from '../embeddings-actor.ts';
import { AI_MESSAGE_TYPES } from '../types.ts';
import type { EmbeddingsResponsePayload } from '../types.ts';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const makeMsg = (to: string, type: string, payload: unknown) =>
  createMessage(address(to), type, payload, { from: address('test/sender') });

const originalFetch = globalThis.fetch;

const mockFetch = (responseData: unknown, status = 200) => {
  globalThis.fetch = async () =>
    ({
      ok: status >= 200 && status < 300,
      status,
      text: async () => JSON.stringify(responseData),
      json: async () => responseData,
    }) as Response;
};

afterEach(() => {
  globalThis.fetch = originalFetch;
});

// ---------------------------------------------------------------------------
// parseEmbeddingsAddress
// ---------------------------------------------------------------------------

describe('parseEmbeddingsAddress', () => {
  it('parses a valid 5-segment address', () => {
    const result = parseEmbeddingsAddress('ai/embeddings/bln_ai/nim/llama-3.3');
    expect(result).not.toBeNull();
    expect(result?.namespace).toBe('bln_ai');
    expect(result?.provider).toBe('nim');
    expect(result?.model).toBe('llama-3.3');
  });

  it('parses a model name containing slashes (org/model)', () => {
    const result = parseEmbeddingsAddress('ai/embeddings/bln_ai/openai/org/model-name');
    expect(result).not.toBeNull();
    expect(result?.model).toBe('org/model-name');
  });

  it('returns null for wrong first segment', () => {
    expect(parseEmbeddingsAddress('x/embeddings/ns/prov/model')).toBeNull();
  });

  it('returns null for wrong second segment', () => {
    expect(parseEmbeddingsAddress('ai/embed/ns/prov/model')).toBeNull();
  });

  it('returns null for too-short address (fewer than 5 segments)', () => {
    expect(parseEmbeddingsAddress('ai/embeddings/ns/prov')).toBeNull();
  });

  it('returns null for empty string', () => {
    expect(parseEmbeddingsAddress('')).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// EmbeddingsActor.receive
// ---------------------------------------------------------------------------

describe('EmbeddingsActor.receive', () => {
  const actorAddress = 'ai/embeddings/bln_ai/nim/llama-3.3';
  const config = {
    gatewayUrl: 'https://gateway.example.com',
    apiKey: 'test-api-key',
  };

  it('calls fetch with correct URL and returns embeddings', async () => {
    let capturedUrl: string | undefined;
    let capturedOptions: RequestInit | undefined;

    globalThis.fetch = async (input: RequestInfo | URL, init?: RequestInit) => {
      capturedUrl = input.toString();
      capturedOptions = init;
      return {
        ok: true,
        status: 200,
        text: async () => '{}',
        json: async () => ({
          data: [{ embedding: [0.1, 0.2, 0.3], index: 0, object: 'embedding' }],
          model: 'llama-3.3',
          usage: { prompt_tokens: 5, total_tokens: 5 },
        }),
      } as Response;
    };

    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, {
      inputs: ['hello world'],
    });

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    expect(capturedUrl).toBe('https://gateway.example.com/openai/v1/embeddings');

    const body = JSON.parse(capturedOptions?.body as string);
    expect(body.model).toBe('llama-3.3');
    expect(body.input).toEqual(['hello world']);

    const payload = response.payload as EmbeddingsResponsePayload;
    expect(payload.embeddings).toEqual([[0.1, 0.2, 0.3]]);
    expect(payload.model).toBe('llama-3.3');
    expect(payload.usage?.promptTokens).toBe(5);
    expect(payload.usage?.totalTokens).toBe(5);
  });

  it('sends Authorization header when apiKey is configured', async () => {
    let capturedHeaders: Record<string, string> | undefined;

    globalThis.fetch = async (_input: RequestInfo | URL, init?: RequestInit) => {
      capturedHeaders = init?.headers as Record<string, string>;
      return {
        ok: true,
        status: 200,
        text: async () => '{}',
        json: async () => ({ data: [], model: 'llama-3.3' }),
      } as Response;
    };

    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, { inputs: ['test'] });

    await actor.receive(msg);

    expect(capturedHeaders?.['Authorization']).toBe('Bearer test-api-key');
  });

  it('respects model override from request payload', async () => {
    let capturedBody: { model?: string } | undefined;

    globalThis.fetch = async (_input: RequestInfo | URL, init?: RequestInit) => {
      capturedBody = JSON.parse(init?.body as string);
      return {
        ok: true,
        status: 200,
        text: async () => '{}',
        json: async () => ({ data: [], model: 'override-model' }),
      } as Response;
    };

    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, {
      inputs: ['text'],
      model: 'override-model',
    });

    const response = await actor.receive(msg);

    expect(capturedBody?.model).toBe('override-model');
    expect(response.success).toBe(true);
  });

  it('handles empty data array from gateway gracefully', async () => {
    mockFetch({ data: [], model: 'llama-3.3' });

    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, { inputs: [] });

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    const payload = response.payload as EmbeddingsResponsePayload;
    expect(payload.embeddings).toEqual([]);
  });

  it('returns error response when gateway returns non-ok status', async () => {
    mockFetch({ error: 'Unauthorized' }, 401);

    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, {
      inputs: ['test'],
    });

    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('401');
  });

  it('returns error response when gateway returns 500', async () => {
    mockFetch({ error: 'Internal Server Error' }, 500);

    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, {
      inputs: ['test'],
    });

    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('500');
  });

  it('returns error response for unknown message type', async () => {
    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, 'some.unknown.type', {});

    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('unhandled message type');
    expect(response.error).toContain('some.unknown.type');
  });

  it('throws when constructed with an invalid address', () => {
    expect(() => new EmbeddingsActor('ai/embeddings/ns/prov', config)).toThrow(
      'Invalid embeddings actor address',
    );
  });
});

// ---------------------------------------------------------------------------
// createEmbeddingsFactory
// ---------------------------------------------------------------------------

describe('createEmbeddingsFactory', () => {
  const config = { gatewayUrl: 'https://gateway.example.com', apiKey: 'key' };

  it('returns an EmbeddingsActor for a valid ai/embeddings/ address', () => {
    const factory = createEmbeddingsFactory(config);
    const actor = factory('ai/embeddings/bln_ai/nim/llama-3.3');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(EmbeddingsActor);
  });

  it('returns null for an address that does not match the embeddings prefix', () => {
    const factory = createEmbeddingsFactory(config);
    expect(factory('ai/credentials/nim')).toBeNull();
    expect(factory('ai/inference/ns/prov/model')).toBeNull();
    expect(factory('')).toBeNull();
  });

  it('returns null for an address with too few segments', () => {
    const factory = createEmbeddingsFactory(config);
    expect(factory('ai/embeddings/ns/prov')).toBeNull();
  });
});
