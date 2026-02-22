/**
 * Integration tests for CredentialsActor
 *
 * Address: ai/credentials/<target>
 * Example: ai/credentials/nim, ai/credentials/deepgram
 */

import { describe, it, expect } from 'bun:test';
import {
  CredentialsActor,
  parseCredentialsAddress,
  createCredentialsFactory,
} from './credentials-actor.ts';
import type { CredentialsActorConfig } from './credentials-actor.ts';
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
    to: address('ai/credentials/nim'),
    from: address('sender'),
    correlationId: `corr-${seq}`,
    timestamp: Date.now(),
  };
}

const BASE_CONFIG: CredentialsActorConfig = {
  credentials: {
    nim: { apiKey: 'nim-api-key', gatewayUrl: 'https://nim.gateway.example.com' },
    deepgram: { apiKey: 'deepgram-api-key' },
    openai: { apiKey: 'openai-api-key', gatewayUrl: 'https://openai.gateway.example.com' },
  },
};

const VALID_ADDRESS = 'ai/credentials/nim';

// ---------------------------------------------------------------------------
// parseCredentialsAddress
// ---------------------------------------------------------------------------

describe('parseCredentialsAddress', () => {
  it('parses a valid credentials address', () => {
    const result = parseCredentialsAddress('ai/credentials/nim');
    expect(result).not.toBeNull();
    expect(result!.target).toBe('nim');
  });

  it('handles target with hyphens', () => {
    const result = parseCredentialsAddress('ai/credentials/cf-aig');
    expect(result).not.toBeNull();
    expect(result!.target).toBe('cf-aig');
  });

  it('handles target with slashes', () => {
    const result = parseCredentialsAddress('ai/credentials/org/deepgram');
    expect(result).not.toBeNull();
    expect(result!.target).toBe('org/deepgram');
  });

  it('returns null for wrong prefix (ai/inference/...)', () => {
    expect(parseCredentialsAddress('ai/inference/ns/prov/model')).toBeNull();
  });

  it('returns null for too-short address (missing target)', () => {
    expect(parseCredentialsAddress('ai/credentials')).toBeNull();
  });

  it('returns null for wrong top-level prefix', () => {
    expect(parseCredentialsAddress('wrong/credentials/nim')).toBeNull();
  });

  it('returns null for empty string', () => {
    expect(parseCredentialsAddress('')).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

describe('CredentialsActor — constructor', () => {
  it('constructs successfully with a valid address', () => {
    expect(() => new CredentialsActor(VALID_ADDRESS, BASE_CONFIG)).not.toThrow();
  });

  it('throws for invalid address', () => {
    expect(() => new CredentialsActor('bad/address', BASE_CONFIG)).toThrow(
      /Invalid credentials actor address/,
    );
  });

  it('throws for address missing target segment', () => {
    expect(() => new CredentialsActor('ai/credentials', BASE_CONFIG)).toThrow();
  });
});

// ---------------------------------------------------------------------------
// Unknown message type
// ---------------------------------------------------------------------------

describe('CredentialsActor — unknown message type', () => {
  it('returns error response for unknown type', async () => {
    const actor = new CredentialsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg('ai.unknown.type', {}));
    expect(response.success).toBe(false);
    expect(response.error).toContain('unhandled message type');
  });
});

// ---------------------------------------------------------------------------
// ai.discover
// ---------------------------------------------------------------------------

describe('CredentialsActor — ai.discover', () => {
  it('returns address in payload', async () => {
    const actor = new CredentialsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { address: string }).address).toBe(VALID_ADDRESS);
  });

  it('returns type = credentials', async () => {
    const actor = new CredentialsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect((response.payload as { type: string }).type).toBe('credentials');
  });

  it('includes target in meta', async () => {
    const actor = new CredentialsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    const meta = (response.payload as { meta: Record<string, string> }).meta;
    expect(meta.target).toBe('nim');
  });
});

// ---------------------------------------------------------------------------
// ai.health
// ---------------------------------------------------------------------------

describe('CredentialsActor — ai.health', () => {
  it('returns { status: "ok" }', async () => {
    const actor = new CredentialsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { status: string }).status).toBe('ok');
  });

  it('echoes token from request', async () => {
    const actor = new CredentialsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, { token: 'cred-ping' }));
    expect((response.payload as { token: string }).token).toBe('cred-ping');
  });
});

// ---------------------------------------------------------------------------
// ai.credentials.get — known target
// ---------------------------------------------------------------------------

describe('CredentialsActor — ai.credentials.get (known target)', () => {
  it('returns apiKey and gatewayUrl for known target', async () => {
    const actor = new CredentialsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.CREDENTIALS_GET, { target: 'nim' }),
    );
    expect(response.success).toBe(true);
    const payload = response.payload as { target: string; apiKey?: string; gatewayUrl?: string };
    expect(payload.target).toBe('nim');
    expect(payload.apiKey).toBe('nim-api-key');
    expect(payload.gatewayUrl).toBe('https://nim.gateway.example.com');
  });

  it('returns only apiKey when no gatewayUrl configured', async () => {
    const actor = new CredentialsActor('ai/credentials/deepgram', BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.CREDENTIALS_GET, { target: 'deepgram' }),
    );
    expect(response.success).toBe(true);
    const payload = response.payload as { apiKey?: string; gatewayUrl?: string };
    expect(payload.apiKey).toBe('deepgram-api-key');
    expect(payload.gatewayUrl).toBeUndefined();
  });

  it('returns credentials for any key in the config map', async () => {
    const actor = new CredentialsActor('ai/credentials/openai', BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.CREDENTIALS_GET, { target: 'openai' }),
    );
    expect(response.success).toBe(true);
    expect((response.payload as { apiKey: string }).apiKey).toBe('openai-api-key');
  });
});

// ---------------------------------------------------------------------------
// ai.credentials.get — unknown target
// ---------------------------------------------------------------------------

describe('CredentialsActor — ai.credentials.get (unknown target)', () => {
  it('returns error response for unknown target (target mismatch from scoping fix)', async () => {
    // After the target-scoping fix (FIX A), requesting a target other than the
    // actor's own target is rejected with a "target mismatch" error rather than
    // "no credentials found", because cross-target reads are now blocked.
    const actor = new CredentialsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.CREDENTIALS_GET, { target: 'nonexistent-provider' }),
    );
    expect(response.success).toBe(false);
    expect(response.error).toContain('target mismatch');
  });

  it('returns error response when actor own target is not in config', async () => {
    // An actor for a target not present in the config map returns a "no credentials" error.
    const actor = new CredentialsActor('ai/credentials/nonexistent-provider', BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.CREDENTIALS_GET, { target: 'nonexistent-provider' }),
    );
    expect(response.success).toBe(false);
    expect(response.error).toContain('no credentials found for target');
    expect(response.error).toContain('nonexistent-provider');
  });

  it('is case-sensitive — "NIM" != "nim"', async () => {
    // An actor at ai/credentials/nim rejects a request for target "NIM" (target mismatch).
    const actor = new CredentialsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.CREDENTIALS_GET, { target: 'NIM' }),
    );
    expect(response.success).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

describe('createCredentialsFactory', () => {
  it('returns null for wrong prefix', () => {
    const factory = createCredentialsFactory(BASE_CONFIG);
    expect(factory('ai/inference/ns/prov/model')).toBeNull();
  });

  it('returns null for address missing target segment', () => {
    const factory = createCredentialsFactory(BASE_CONFIG);
    expect(factory('ai/credentials')).toBeNull();
  });

  it('returns CredentialsActor for correct prefix', () => {
    const factory = createCredentialsFactory(BASE_CONFIG);
    const actor = factory('ai/credentials/nim');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(CredentialsActor);
  });

  it('returns CredentialsActor for hyphenated target', () => {
    const factory = createCredentialsFactory(BASE_CONFIG);
    const actor = factory('ai/credentials/cf-aig');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(CredentialsActor);
  });
});
