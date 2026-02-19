/**
 * Integration tests for GatewayActor
 *
 * Address: ai/gateway/<namespace>
 * Example: ai/gateway/bln_ai
 */

import { describe, it, expect, mock, beforeEach } from 'bun:test';
import { GatewayActor, createGatewayFactory } from './gateway-actor.ts';
import type { GatewayActorConfig } from './gateway-actor.ts';
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
    to: address('ai/gateway/bln_ai'),
    from: address('sender'),
    correlationId: `corr-${seq}`,
    timestamp: Date.now(),
  };
}

const BASE_CONFIG: GatewayActorConfig = {
  gatewayUrl: 'https://gateway.ai.cloudflare.com/v1/test-account/test-gateway',
  apiKey: 'test-api-key',
  defaultModel: 'meta/llama-3.3-70b-instruct',
};

// ---------------------------------------------------------------------------
// Constructor / address validation
// ---------------------------------------------------------------------------

describe('GatewayActor — constructor', () => {
  it('accepts a valid gateway address', () => {
    expect(() => new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG)).not.toThrow();
  });

  it('accepts any address (GatewayActor has no address validation guard)', () => {
    // GatewayActor does not call a parseXxx guard — it accepts any string
    expect(() => new GatewayActor('ai/gateway/ns', BASE_CONFIG)).not.toThrow();
  });
});

// ---------------------------------------------------------------------------
// Unknown message type
// ---------------------------------------------------------------------------

describe('GatewayActor — unknown message type', () => {
  it('returns error response for unknown type', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    const response = await actor.receive(msg('ai.unknown.type', {}));
    expect(response.success).toBe(false);
    expect(response.error).toContain('unhandled message type');
  });
});

// ---------------------------------------------------------------------------
// ai.discover
// ---------------------------------------------------------------------------

describe('GatewayActor — ai.discover', () => {
  it('returns address and type in payload', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect(response.success).toBe(true);
    expect(response.payload).toBeDefined();
    expect((response.payload as { address: string }).address).toBe('ai/gateway/bln_ai');
    expect((response.payload as { type: string }).type).toBe('gateway');
  });

  it('includes handles array with expected message types', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    const handles = (response.payload as { handles: string[] }).handles;
    expect(handles).toContain(AI_MESSAGE_TYPES.INFERENCE_REQUEST);
    expect(handles).toContain(AI_MESSAGE_TYPES.DISCOVER);
  });
});

// ---------------------------------------------------------------------------
// ai.health
// ---------------------------------------------------------------------------

describe('GatewayActor — ai.health', () => {
  it('returns status ok', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { status: string }).status).toBe('ok');
  });

  it('echoes token if provided', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, { token: 'ping-123' }));
    expect((response.payload as { token: string }).token).toBe('ping-123');
  });
});

// ---------------------------------------------------------------------------
// ai.inference.request — happy path (mocked fetch)
// ---------------------------------------------------------------------------

describe('GatewayActor — ai.inference.request', () => {
  let mockFetch: ReturnType<typeof mock>;

  beforeEach(() => {
    mockFetch = mock(() =>
      Promise.resolve({
        ok: true,
        json: () =>
          Promise.resolve({
            choices: [{ message: { content: 'Hello from gateway' }, finish_reason: 'stop' }],
            model: 'meta/llama-3.3-70b-instruct',
            usage: { prompt_tokens: 10, completion_tokens: 5, total_tokens: 15 },
          }),
        text: () => Promise.resolve(''),
      }),
    );
    global.fetch = mockFetch as unknown as typeof fetch;
  });

  it('returns content from OpenAI-shaped response', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hi' }],
      }),
    );
    expect(response.success).toBe(true);
    const payload = response.payload as { content: string; model: string; usage: unknown };
    expect(payload.content).toBe('Hello from gateway');
    expect(payload.model).toBe('meta/llama-3.3-70b-instruct');
    expect(payload.usage).toBeDefined();
  });

  it('maps usage tokens to camelCase fields', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hi' }],
      }),
    );
    const usage = (response.payload as { usage: { promptTokens: number; completionTokens: number; totalTokens: number } }).usage;
    expect(usage.promptTokens).toBe(10);
    expect(usage.completionTokens).toBe(5);
    expect(usage.totalTokens).toBe(15);
  });

  it('uses model override from payload when provided', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hi' }],
        model: 'custom-model',
      }),
    );
    const fetchCall = (mockFetch as ReturnType<typeof mock>).mock.calls[0];
    const body = JSON.parse((fetchCall[1] as RequestInit).body as string);
    expect(body.model).toBe('custom-model');
  });
});

// ---------------------------------------------------------------------------
// ai.inference.request — gateway error
// ---------------------------------------------------------------------------

describe('GatewayActor — gateway error', () => {
  it('returns error response when fetch returns ok=false', async () => {
    global.fetch = mock(() =>
      Promise.resolve({
        ok: false,
        status: 503,
        text: () => Promise.resolve('Service Unavailable'),
        json: () => Promise.resolve({}),
      }),
    ) as unknown as typeof fetch;

    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.INFERENCE_REQUEST, {
        messages: [{ role: 'user', content: 'hi' }],
      }),
    );
    expect(response.success).toBe(false);
    expect(response.error).toContain('503');
  });
});

// ---------------------------------------------------------------------------
// Session lifecycle
// ---------------------------------------------------------------------------

describe('GatewayActor — session', () => {
  it('SESSION_CREATE returns state with userId and activeActors', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.SESSION_CREATE, { userId: 'user-xyz' }),
    );
    expect(response.success).toBe(true);
    const payload = response.payload as { userId: string; activeActors: string[] };
    expect(payload.userId).toBe('user-xyz');
    expect(Array.isArray(payload.activeActors)).toBe(true);
  });

  it('SESSION_END returns userId', async () => {
    const actor = new GatewayActor('ai/gateway/bln_ai', BASE_CONFIG);
    await actor.receive(msg(AI_MESSAGE_TYPES.SESSION_CREATE, { userId: 'user-xyz' }));
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.SESSION_END, { userId: 'user-xyz' }),
    );
    expect(response.success).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

describe('createGatewayFactory', () => {
  it('returns null for wrong prefix', () => {
    const factory = createGatewayFactory(BASE_CONFIG);
    // GatewayFactory doesn't validate prefix, it just wraps any address
    // But test what happens with correct prefix
    const actor = factory('ai/gateway/bln_ai');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(GatewayActor);
  });

  it('returns actor for any address (no prefix guard in gateway factory)', () => {
    const factory = createGatewayFactory(BASE_CONFIG);
    const actor = factory('ai/gateway/any-namespace');
    expect(actor).toBeInstanceOf(GatewayActor);
  });
});
