/**
 * EmbeddingsActor — embedding vector generation actor
 *
 * Address: ai/embeddings/<namespace>/<provider>/<model>
 * Example: ai/embeddings/bln_ai/nim/llama-3.3
 *
 * Address segments:
 *   [0] = 'ai'
 *   [1] = 'embeddings'
 *   [2] = namespace (e.g. 'bln_ai')
 *   [3] = provider  (e.g. 'nim', 'openai')
 *   [4] = model     (e.g. 'llama-3.3', 'text-embedding-3-small')
 *
 * Handles:
 *   ai.embeddings.request  → EmbeddingsRequestPayload  → EmbeddingsResponsePayload
 *
 * Routes to /openai/v1/embeddings via AI Gateway (OpenAI-compatible format).
 */

import type { Message, MessageResponse, MessageHandler } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';
import type {
  EmbeddingsRequestPayload,
  EmbeddingsResponsePayload,
  DiscoverResponsePayload,
  HealthPayload,
  HealthResponsePayload,
} from './types.ts';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface EmbeddingsActorConfig {
  /** AI Gateway base URL for this provider. */
  gatewayUrl: string;
  /** Bearer token for the provider. */
  apiKey?: string;
}

// ---------------------------------------------------------------------------
// Address parsing
// ---------------------------------------------------------------------------

export interface EmbeddingsAddress {
  namespace: string;
  provider: string;
  model: string;
}

/**
 * Parse an embeddings actor address into its constituent parts.
 * Input: 'ai/embeddings/bln_ai/nim/llama-3.3'
 */
export function parseEmbeddingsAddress(address: string): EmbeddingsAddress | null {
  const parts = address.split('/');
  // ai / embeddings / <ns> / <provider> / <model>
  if (parts.length < 5 || parts[0] !== 'ai' || parts[1] !== 'embeddings') return null;
  return {
    namespace: parts[2],
    provider: parts[3],
    model: parts.slice(4).join('/'), // model may contain slashes (e.g. org/model)
  };
}

// ---------------------------------------------------------------------------
// OpenAI embeddings wire types (used for gateway response)
// ---------------------------------------------------------------------------

interface OaiEmbeddingObject {
  embedding: number[];
  index: number;
  object: string;
}

interface OaiEmbeddingsUsage {
  prompt_tokens?: number;
  total_tokens?: number;
}

interface OaiEmbeddingsResponse {
  data?: OaiEmbeddingObject[];
  model?: string;
  usage?: OaiEmbeddingsUsage;
}

// ---------------------------------------------------------------------------
// EmbeddingsActor
// ---------------------------------------------------------------------------

export class EmbeddingsActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly parsed: EmbeddingsAddress;
  private readonly config: EmbeddingsActorConfig;

  constructor(address: string, config: EmbeddingsActorConfig) {
    this.actorAddress = address;
    const parsed = parseEmbeddingsAddress(address);
    if (!parsed) throw new Error(`Invalid embeddings actor address: ${address}`);
    this.parsed = parsed;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      if (message.type === AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST) {
        return await this.handleEmbeddingsRequest(message);
      }
      if (message.type === AI_MESSAGE_TYPES.DISCOVER) {
        return this.handleDiscover(message);
      }
      if (message.type === AI_MESSAGE_TYPES.HEALTH) {
        return this.handleHealth(message);
      }
      return createErrorResponse(
        message,
        `EmbeddingsActor(${this.actorAddress}): unhandled message type '${message.type}'`,
      );
    } catch (err) {
      return createErrorResponse(message, err instanceof Error ? err.message : String(err));
    }
  }

  private handleDiscover(message: Message): MessageResponse {
    const result: DiscoverResponsePayload = {
      address: this.actorAddress,
      type: 'embeddings',
      handles: [AI_MESSAGE_TYPES.EMBEDDINGS_REQUEST, AI_MESSAGE_TYPES.DISCOVER, AI_MESSAGE_TYPES.HEALTH],
      meta: { namespace: this.parsed.namespace, provider: this.parsed.provider, model: this.parsed.model },
    };
    return createResponse(message, result);
  }

  private handleHealth(message: Message): MessageResponse {
    const payload = message.payload as HealthPayload;
    const result: HealthResponsePayload = { status: 'ok', address: this.actorAddress, token: payload?.token };
    return createResponse(message, result);
  }

  private async handleEmbeddingsRequest(message: Message): Promise<MessageResponse> {
    const payload = message.payload as EmbeddingsRequestPayload;
    const model = payload.model ?? this.parsed.model;

    const raw = await this.callGateway(model, payload) as OaiEmbeddingsResponse;

    const embeddings = (raw.data ?? []).map((item) => item.embedding);
    const usage = raw.usage;

    const result: EmbeddingsResponsePayload = {
      embeddings,
      model: raw.model ?? model,
      usage: usage
        ? {
            promptTokens: usage.prompt_tokens ?? 0,
            totalTokens: usage.total_tokens ?? 0,
          }
        : undefined,
    };

    return createResponse(message, result);
  }

  private async callGateway(model: string, payload: EmbeddingsRequestPayload): Promise<unknown> {
    const url = `${this.config.gatewayUrl}/openai/v1/embeddings`;
    const headers: Record<string, string> = { 'Content-Type': 'application/json' };
    if (this.config.apiKey) headers['Authorization'] = `Bearer ${this.config.apiKey}`;

    const response = await fetch(url, {
      method: 'POST',
      headers,
      body: JSON.stringify({
        model,
        input: payload.inputs,
      }),
    });

    if (!response.ok) {
      throw new Error(`Embeddings gateway error ${response.status}: ${await response.text()}`);
    }
    return response.json();
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create an ActorFactory for the `ai/embeddings/` prefix.
 *
 * @example
 * system.registerFactory({
 *   prefix: AI_PREFIXES.EMBEDDINGS,
 *   factory: createEmbeddingsFactory({ gatewayUrl: env.AI_GATEWAY_URL, apiKey: env.AI_API_KEY }),
 * });
 */
export function createEmbeddingsFactory(
  config: EmbeddingsActorConfig,
): (address: string) => EmbeddingsActor | null {
  return (address: string) => {
    const parsed = parseEmbeddingsAddress(address);
    if (!parsed) return null;
    return new EmbeddingsActor(address, config);
  };
}
