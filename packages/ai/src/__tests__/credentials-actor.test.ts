/**
 * Tests for CredentialsActor
 *
 * Coverage:
 * - parseCredentialsAddress: valid and invalid addresses
 * - CredentialsActor.receive: ai.credentials.get → returns credentials when found
 * - CredentialsActor.receive: ai.credentials.get → returns error when target not found
 * - CredentialsActor.receive: unknown message type → error response
 * - createCredentialsFactory: returns actor for valid prefix, null for invalid
 */

import { describe, it, expect } from 'bun:test';
import { createMessage, address } from '@agentic-primer/actors';
import {
  parseCredentialsAddress,
  CredentialsActor,
  createCredentialsFactory,
} from '../credentials-actor.ts';
import { AI_MESSAGE_TYPES } from '../types.ts';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const makeMsg = (to: string, type: string, payload: unknown) =>
  createMessage(address(to), type, payload, { from: address('test/sender') });

// ---------------------------------------------------------------------------
// parseCredentialsAddress
// ---------------------------------------------------------------------------

describe('parseCredentialsAddress', () => {
  it('parses a valid address with single-segment target', () => {
    const result = parseCredentialsAddress('ai/credentials/nim');
    expect(result).not.toBeNull();
    expect(result?.target).toBe('nim');
  });

  it('parses a valid address with multi-segment target', () => {
    const result = parseCredentialsAddress('ai/credentials/cf-aig');
    expect(result).not.toBeNull();
    expect(result?.target).toBe('cf-aig');
  });

  it('parses a valid address with slash in target', () => {
    const result = parseCredentialsAddress('ai/credentials/provider/sub');
    expect(result).not.toBeNull();
    expect(result?.target).toBe('provider/sub');
  });

  it('returns null for wrong first segment', () => {
    expect(parseCredentialsAddress('x/credentials/nim')).toBeNull();
  });

  it('returns null for wrong second segment', () => {
    expect(parseCredentialsAddress('ai/creds/nim')).toBeNull();
  });

  it('returns null for too-short address (no target segment)', () => {
    expect(parseCredentialsAddress('ai/credentials')).toBeNull();
  });

  it('returns null for empty string', () => {
    expect(parseCredentialsAddress('')).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// CredentialsActor.receive
// ---------------------------------------------------------------------------

describe('CredentialsActor.receive', () => {
  const config = {
    credentials: {
      nim: { apiKey: 'nim-key-123', gatewayUrl: 'https://gateway.example.com/nim' },
      deepgram: { apiKey: 'dg-key-456' },
    },
  };

  it('returns credentials when target is found', async () => {
    const actor = new CredentialsActor('ai/credentials/nim', config);
    const msg = makeMsg('ai/credentials/nim', AI_MESSAGE_TYPES.CREDENTIALS_GET, { target: 'nim' });

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    const payload = response.payload as { target: string; apiKey?: string; gatewayUrl?: string };
    expect(payload.target).toBe('nim');
    expect(payload.apiKey).toBe('nim-key-123');
    expect(payload.gatewayUrl).toBe('https://gateway.example.com/nim');
  });

  it('returns credentials with only apiKey when gatewayUrl not configured', async () => {
    const actor = new CredentialsActor('ai/credentials/deepgram', config);
    const msg = makeMsg('ai/credentials/deepgram', AI_MESSAGE_TYPES.CREDENTIALS_GET, { target: 'deepgram' });

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    const payload = response.payload as { target: string; apiKey?: string; gatewayUrl?: string };
    expect(payload.target).toBe('deepgram');
    expect(payload.apiKey).toBe('dg-key-456');
    expect(payload.gatewayUrl).toBeUndefined();
  });

  it('returns error response when target is not found', async () => {
    const actor = new CredentialsActor('ai/credentials/unknown', config);
    const msg = makeMsg('ai/credentials/unknown', AI_MESSAGE_TYPES.CREDENTIALS_GET, { target: 'unknown' });

    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('unknown');
  });

  it('returns error response for unknown message type', async () => {
    const actor = new CredentialsActor('ai/credentials/nim', config);
    const msg = makeMsg('ai/credentials/nim', 'some.unknown.type', {});

    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('unhandled message type');
    expect(response.error).toContain('some.unknown.type');
  });

  it('throws when constructed with an invalid address', () => {
    expect(() => new CredentialsActor('not/valid', config)).toThrow('Invalid credentials actor address');
  });
});

// ---------------------------------------------------------------------------
// createCredentialsFactory
// ---------------------------------------------------------------------------

describe('createCredentialsFactory', () => {
  const config = {
    credentials: {
      nim: { apiKey: 'key-abc' },
    },
  };

  it('returns a CredentialsActor for a valid ai/credentials/ address', () => {
    const factory = createCredentialsFactory(config);
    const actor = factory('ai/credentials/nim');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(CredentialsActor);
  });

  it('returns null for an address that does not match the credentials prefix', () => {
    const factory = createCredentialsFactory(config);
    expect(factory('ai/inference/ns/nim/model')).toBeNull();
    expect(factory('ai/gateway/bln_ai')).toBeNull();
    expect(factory('')).toBeNull();
  });
});
