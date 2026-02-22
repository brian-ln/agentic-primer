/**
 * Tests for AI actor discovery and health protocol
 *
 * Coverage:
 * - CredentialsActor: ai.discover → returns DiscoverResponsePayload with correct type/handles/meta
 * - CredentialsActor: ai.health → returns HealthResponsePayload with status:'ok'
 * - EmbeddingsActor: ai.discover → returns DiscoverResponsePayload with correct type/handles/meta
 * - EmbeddingsActor: ai.health → returns HealthResponsePayload with status:'ok' and token echo
 */

import { describe, it, expect } from 'bun:test';
import { createMessage, address } from '@agentic-primer/actors';
import { CredentialsActor } from '../credentials-actor.ts';
import { EmbeddingsActor } from '../embeddings-actor.ts';
import { AI_MESSAGE_TYPES } from '../types.ts';
import type { DiscoverResponsePayload, HealthResponsePayload } from '../types.ts';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const makeMsg = (to: string, type: string, payload: unknown = {}) =>
  createMessage(address(to), type, payload, { from: address('test/sender') });

// ---------------------------------------------------------------------------
// CredentialsActor — discovery protocol
// ---------------------------------------------------------------------------

describe('CredentialsActor discovery protocol', () => {
  const config = {
    credentials: {
      nim: { apiKey: 'nim-key-123' },
    },
  };

  it('handles ai.discover and returns DiscoverResponsePayload with type "credentials"', async () => {
    const actor = new CredentialsActor('ai/credentials/nim', config);
    const msg = makeMsg('ai/credentials/nim', AI_MESSAGE_TYPES.DISCOVER, {});

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    const payload = response.payload as DiscoverResponsePayload;
    expect(payload.address).toBe('ai/credentials/nim');
    expect(payload.type).toBe('credentials');
    expect(payload.handles).toContain(AI_MESSAGE_TYPES.CREDENTIALS_GET);
    expect(payload.handles).toContain(AI_MESSAGE_TYPES.DISCOVER);
    expect(payload.handles).toContain(AI_MESSAGE_TYPES.HEALTH);
  });

  it('includes target in meta from ai.discover response', async () => {
    const actor = new CredentialsActor('ai/credentials/deepgram', config);
    const msg = makeMsg('ai/credentials/deepgram', AI_MESSAGE_TYPES.DISCOVER, {});

    const response = await actor.receive(msg);

    const payload = response.payload as DiscoverResponsePayload;
    expect(payload.meta.target).toBe('deepgram');
  });

  it('handles ai.health and returns status ok', async () => {
    const actor = new CredentialsActor('ai/credentials/nim', config);
    const msg = makeMsg('ai/credentials/nim', AI_MESSAGE_TYPES.HEALTH, {});

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    const payload = response.payload as HealthResponsePayload;
    expect(payload.status).toBe('ok');
    expect(payload.address).toBe('ai/credentials/nim');
  });

  it('echoes token in ai.health response when token is provided', async () => {
    const actor = new CredentialsActor('ai/credentials/nim', config);
    const msg = makeMsg('ai/credentials/nim', AI_MESSAGE_TYPES.HEALTH, { token: 'ping-abc' });

    const response = await actor.receive(msg);

    const payload = response.payload as HealthResponsePayload;
    expect(payload.token).toBe('ping-abc');
  });
});

// ---------------------------------------------------------------------------
// EmbeddingsActor — discovery protocol
// ---------------------------------------------------------------------------

describe('EmbeddingsActor discovery protocol', () => {
  const actorAddress = 'ai/embeddings/bln_ai/nim/llama-3.3';
  const config = {
    gatewayUrl: 'https://gateway.example.com',
    apiKey: 'test-key',
  };

  it('handles ai.discover and returns DiscoverResponsePayload with type "embeddings"', async () => {
    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.DISCOVER, {});

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    const payload = response.payload as DiscoverResponsePayload;
    expect(payload.address).toBe(actorAddress);
    expect(payload.type).toBe('embeddings');
    expect(payload.handles).toContain(AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST);
    expect(payload.handles).toContain(AI_MESSAGE_TYPES.DISCOVER);
    expect(payload.handles).toContain(AI_MESSAGE_TYPES.HEALTH);
  });

  it('includes namespace, provider, and model in meta from ai.discover response', async () => {
    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.DISCOVER, {});

    const response = await actor.receive(msg);

    const payload = response.payload as DiscoverResponsePayload;
    expect(payload.meta.namespace).toBe('bln_ai');
    expect(payload.meta.provider).toBe('nim');
    expect(payload.meta.model).toBe('llama-3.3');
  });

  it('handles ai.health and returns status ok', async () => {
    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.HEALTH, {});

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    const payload = response.payload as HealthResponsePayload;
    expect(payload.status).toBe('ok');
    expect(payload.address).toBe(actorAddress);
  });

  it('echoes token in ai.health response when token is provided', async () => {
    const actor = new EmbeddingsActor(actorAddress, config);
    const msg = makeMsg(actorAddress, AI_MESSAGE_TYPES.HEALTH, { token: 'health-check-xyz' });

    const response = await actor.receive(msg);

    const payload = response.payload as HealthResponsePayload;
    expect(payload.token).toBe('health-check-xyz');
  });
});
