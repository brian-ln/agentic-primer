/**
 * InferenceActor — LLM inference routing actor
 *
 * Address: ai/inference/<namespace>/<provider>/<model>
 * Example: ai/inference/bln_ai/nim/kimi-k2.5
 *
 * Address segments:
 *   [0] = 'ai'
 *   [1] = 'inference'
 *   [2] = namespace (e.g. 'bln_ai')
 *   [3] = provider  (e.g. 'nim', 'openai', 'anthropic')
 *   [4] = model     (e.g. 'kimi-k2.5', 'gpt-4o', 'claude-opus-4-6')
 *
 * Handles:
 *   ai.inference.request  → AiInferenceRequestPayload  (non-streaming)
 *   ai.inference.request (stream: true) → yields ai.inference.chunk + ai.inference.done
 */

import type { Message, MessageResponse, MessageHandler } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';
import type { AiInferenceRequestPayload, AiInferenceResponsePayload } from './types.ts';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface InferenceActorConfig {
  /** AI Gateway base URL for this provider. */
  gatewayUrl: string;
  /** Bearer token for the provider. */
  apiKey?: string;
}

// ---------------------------------------------------------------------------
// Address parsing
// ---------------------------------------------------------------------------

export interface InferenceAddress {
  namespace: string;
  provider: string;
  model: string;
}

/**
 * Parse an inference actor address into its constituent parts.
 * Input: 'ai/inference/bln_ai/nim/kimi-k2.5'
 */
export function parseInferenceAddress(address: string): InferenceAddress | null {
  const parts = address.split('/');
  // ai / inference / <ns> / <provider> / <model>
  if (parts.length < 5 || parts[0] !== 'ai' || parts[1] !== 'inference') return null;
  return {
    namespace: parts[2],
    provider: parts[3],
    model: parts.slice(4).join('/'), // model may contain slashes (e.g. org/model)
  };
}

// ---------------------------------------------------------------------------
// OpenAI wire types (used for gateway response)
// ---------------------------------------------------------------------------

interface OaiChoice {
  message?: { content?: string };
  delta?: { content?: string };
  finish_reason?: string;
}

interface OaiUsage {
  prompt_tokens?: number;
  completion_tokens?: number;
  total_tokens?: number;
}

interface OaiResponse {
  choices?: OaiChoice[];
  model?: string;
  usage?: OaiUsage;
}

// ---------------------------------------------------------------------------
// InferenceActor
// ---------------------------------------------------------------------------

export class InferenceActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly parsed: InferenceAddress;
  private readonly config: InferenceActorConfig;

  constructor(address: string, config: InferenceActorConfig) {
    this.actorAddress = address;
    const parsed = parseInferenceAddress(address);
    if (!parsed) throw new Error(`Invalid inference actor address: ${address}`);
    this.parsed = parsed;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      if (message.type === AI_MESSAGE_TYPES.INFERENCE_REQUEST) {
        return await this.handleInferenceRequest(message);
      }
      return createErrorResponse(
        message,
        `InferenceActor(${this.actorAddress}): unhandled message type '${message.type}'`,
      );
    } catch (err) {
      return createErrorResponse(message, err instanceof Error ? err.message : String(err));
    }
  }

  private async handleInferenceRequest(message: Message): Promise<MessageResponse> {
    const payload = message.payload as AiInferenceRequestPayload;
    const model = payload.model ?? this.parsed.model;

    const raw = await this.callGateway(model, payload) as OaiResponse;
    const content = raw.choices?.[0]?.message?.content ?? '';
    const usage = raw.usage;

    const result: AiInferenceResponsePayload = {
      content,
      model: raw.model ?? model,
      usage: usage
        ? {
            promptTokens: usage.prompt_tokens ?? 0,
            completionTokens: usage.completion_tokens ?? 0,
            totalTokens: usage.total_tokens ?? 0,
          }
        : undefined,
      finishReason: raw.choices?.[0]?.finish_reason,
    };

    return createResponse(message, result);
  }

  private async callGateway(model: string, payload: AiInferenceRequestPayload): Promise<unknown> {
    // Route to provider-appropriate endpoint under the AI Gateway
    const path = this.providerPath(this.parsed.provider);
    const url = `${this.config.gatewayUrl}${path}`;
    const headers: Record<string, string> = { 'Content-Type': 'application/json' };
    if (this.config.apiKey) headers['Authorization'] = `Bearer ${this.config.apiKey}`;

    const response = await fetch(url, {
      method: 'POST',
      headers,
      body: JSON.stringify({
        model,
        messages: payload.messages,
        temperature: payload.temperature,
        max_tokens: payload.maxTokens,
        stream: false,
      }),
    });

    if (!response.ok) {
      throw new Error(`Inference gateway error ${response.status}: ${await response.text()}`);
    }
    return response.json();
  }

  /**
   * Map provider name to AI Gateway path segment.
   * Cloudflare AI Gateway supports OpenAI-compatible endpoints for all major providers.
   */
  private providerPath(provider: string): string {
    switch (provider) {
      case 'nim':       return '/openai/chat/completions';
      case 'openai':    return '/openai/chat/completions';
      case 'anthropic': return '/anthropic/v1/messages';
      case 'groq':      return '/groq/openai/v1/chat/completions';
      case 'cohere':    return '/cohere/v2/chat';
      default:          return '/openai/chat/completions';
    }
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create an ActorFactory for the `ai/inference/` prefix.
 *
 * @example
 * system.registerFactory({
 *   prefix: AI_PREFIXES.INFERENCE,
 *   factory: createInferenceFactory({ gatewayUrl: env.AI_GATEWAY_URL, apiKey: env.AI_API_KEY }),
 * });
 */
export function createInferenceFactory(
  config: InferenceActorConfig,
): (address: string) => InferenceActor | null {
  return (address: string) => {
    const parsed = parseInferenceAddress(address);
    if (!parsed) return null;
    return new InferenceActor(address, config);
  };
}
