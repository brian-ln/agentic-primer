/**
 * CredentialsActor — credential store actor
 *
 * Address: ai/credentials/<target>
 * Example: ai/credentials/nim, ai/credentials/deepgram
 *
 * Address segments:
 *   [0] = 'ai'
 *   [1] = 'credentials'
 *   [2] = target  (e.g. 'nim', 'deepgram', 'cf-aig')
 *
 * Handles:
 *   ai.credentials.get  → CredentialsGetPayload  → CredentialsPayload
 */

import type { Message, MessageResponse, MessageHandler } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';
import type {
  CredentialsGetPayload,
  CredentialsPayload,
  DiscoverResponsePayload,
  HealthPayload,
  HealthResponsePayload,
} from './types.ts';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface CredentialsActorConfig {
  credentials: Record<string, { apiKey?: string; gatewayUrl?: string }>;
}

// ---------------------------------------------------------------------------
// Address parsing
// ---------------------------------------------------------------------------

export interface CredentialsAddress {
  target: string;
}

/**
 * Parse a credentials actor address into its constituent parts.
 * Input: 'ai/credentials/nim'
 */
export function parseCredentialsAddress(address: string): CredentialsAddress | null {
  const parts = address.split('/');
  // ai / credentials / <target>
  if (parts.length < 3 || parts[0] !== 'ai' || parts[1] !== 'credentials') return null;
  return {
    target: parts.slice(2).join('/'), // target may theoretically contain slashes
  };
}

// ---------------------------------------------------------------------------
// CredentialsActor
// ---------------------------------------------------------------------------

export class CredentialsActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly parsed: CredentialsAddress;
  private readonly config: CredentialsActorConfig;

  constructor(address: string, config: CredentialsActorConfig) {
    this.actorAddress = address;
    const parsed = parseCredentialsAddress(address);
    if (!parsed) throw new Error(`Invalid credentials actor address: ${address}`);
    this.parsed = parsed;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      if (message.type === AI_MESSAGE_TYPES.CREDENTIALS_GET) {
        return await this.handleCredentialsGet(message);
      }
      if (message.type === AI_MESSAGE_TYPES.DISCOVER) {
        return this.handleDiscover(message);
      }
      if (message.type === AI_MESSAGE_TYPES.HEALTH) {
        return this.handleHealth(message);
      }
      return createErrorResponse(
        message,
        `CredentialsActor(${this.actorAddress}): unhandled message type '${message.type}'`,
      );
    } catch (err) {
      return createErrorResponse(message, err instanceof Error ? err.message : String(err));
    }
  }

  private handleDiscover(message: Message): MessageResponse {
    const result: DiscoverResponsePayload = {
      address: this.actorAddress,
      type: 'credentials',
      handles: [AI_MESSAGE_TYPES.CREDENTIALS_GET, AI_MESSAGE_TYPES.DISCOVER, AI_MESSAGE_TYPES.HEALTH],
      meta: { target: this.parsed.target },
    };
    return createResponse(message, result);
  }

  private handleHealth(message: Message): MessageResponse {
    const payload = message.payload as HealthPayload;
    const result: HealthResponsePayload = { status: 'ok', address: this.actorAddress, token: payload?.token };
    return createResponse(message, result);
  }

  private async handleCredentialsGet(message: Message): Promise<MessageResponse> {
    const payload = message.payload as CredentialsGetPayload;
    const { target } = payload;

    const entry = this.config.credentials[target];
    if (!entry) {
      return createErrorResponse(
        message,
        `CredentialsActor(${this.actorAddress}): no credentials found for target '${target}'`,
      );
    }

    const result: CredentialsPayload = {
      target,
      apiKey: entry.apiKey,
      gatewayUrl: entry.gatewayUrl,
    };

    return createResponse(message, result);
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create an ActorFactory for the `ai/credentials/` prefix.
 *
 * @example
 * system.registerFactory({
 *   prefix: AI_PREFIXES.CREDENTIALS,
 *   factory: createCredentialsFactory({
 *     credentials: {
 *       nim: { apiKey: env.NIM_API_KEY, gatewayUrl: env.NIM_GATEWAY_URL },
 *       deepgram: { apiKey: env.DEEPGRAM_API_KEY },
 *     },
 *   }),
 * });
 */
export function createCredentialsFactory(
  config: CredentialsActorConfig,
): (address: string) => CredentialsActor | null {
  return (address: string) => {
    const parsed = parseCredentialsAddress(address);
    if (!parsed) return null;
    return new CredentialsActor(address, config);
  };
}
