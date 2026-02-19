/**
 * GatewayActor — AI Gateway proxy actor for a namespace (e.g. bln_ai)
 *
 * Address: ai/gateway/<namespace>
 * Example: ai/gateway/bln_ai
 *
 * Responsibilities:
 * - Proxies inference requests through Cloudflare AI Gateway
 * - Provides capability discovery: gateway.capabilities message
 * - Holds namespace-level configuration (gateway URL, API key)
 *
 * Usage with ActorSystem virtual factory:
 * ```typescript
 * system.registerFactory({
 *   prefix: AI_PREFIXES.GATEWAY,
 *   factory: createGatewayFactory({ gatewayUrl: env.AI_GATEWAY_URL }),
 * });
 * ```
 */

import type { Message, MessageResponse, MessageHandler } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';
import type {
  InferenceRequestPayload,
  InferenceResponsePayload,
  SessionStatePayload,
} from './types.ts';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface GatewayActorConfig {
  /**
   * Cloudflare AI Gateway base URL.
   * Format: https://gateway.ai.cloudflare.com/v1/<account-id>/<gateway-name>
   */
  gatewayUrl: string;
  /**
   * Bearer token / API key for the gateway.
   * In Workers: inject from env binding, not hardcoded.
   */
  apiKey?: string;
  /** Default model for inference requests (e.g. "meta/llama-3.3-70b-instruct"). */
  defaultModel?: string;
}

// ---------------------------------------------------------------------------
// Internal OpenAI-shaped response types
// ---------------------------------------------------------------------------

interface OpenAiChoice {
  message?: { content?: string };
  finish_reason?: string;
}

interface OpenAiUsage {
  prompt_tokens?: number;
  completion_tokens?: number;
  total_tokens?: number;
}

interface OpenAiChatResponse {
  choices?: OpenAiChoice[];
  model?: string;
  usage?: OpenAiUsage;
}

// ---------------------------------------------------------------------------
// GatewayActor
// ---------------------------------------------------------------------------

/**
 * Boundary actor for a single AI namespace.
 *
 * Implements the minimal MessageHandler interface so it can be registered
 * with any ActorSystem (functional or class-based) and used as a virtual actor.
 */
export class GatewayActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly config: GatewayActorConfig;
  private activeSessions = new Map<string, SessionStatePayload>();

  constructor(address: string, config: GatewayActorConfig) {
    this.actorAddress = address;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      switch (message.type) {
        case AI_MESSAGE_TYPES.INFERENCE_REQUEST:
          return await this.handleInferenceRequest(message);
        case AI_MESSAGE_TYPES.SESSION_CREATE:
          return this.handleSessionCreate(message);
        case AI_MESSAGE_TYPES.SESSION_END:
          return this.handleSessionEnd(message);
        case 'gateway.capabilities':
          return this.handleCapabilities(message);
        default:
          return createErrorResponse(
            message,
            `GatewayActor(${this.actorAddress}): unhandled message type '${message.type}'`,
          );
      }
    } catch (err) {
      return createErrorResponse(
        message,
        err instanceof Error ? err.message : String(err),
      );
    }
  }

  // --- Handlers ---

  private async handleInferenceRequest(message: Message): Promise<MessageResponse> {
    const payload = message.payload as InferenceRequestPayload;
    const model = payload.model ?? this.config.defaultModel ?? 'meta/llama-3.3-70b-instruct';

    const raw = await this.callGateway('/openai/chat/completions', {
      model,
      messages: payload.messages,
      temperature: payload.temperature,
      max_tokens: payload.maxTokens,
      stream: false,
    }) as OpenAiChatResponse;

    const content = raw.choices?.[0]?.message?.content ?? '';
    const usage = raw.usage;

    const result: InferenceResponsePayload = {
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

  private handleSessionCreate(message: Message): MessageResponse {
    const { userId } = message.payload as { userId: string };
    const session: SessionStatePayload = {
      userId,
      activeActors: [],
      createdAt: Date.now(),
    };
    this.activeSessions.set(userId, session);
    return createResponse(message, session);
  }

  private handleSessionEnd(message: Message): MessageResponse {
    const { userId } = message.payload as { userId: string };
    this.activeSessions.delete(userId);
    return createResponse(message, { userId });
  }

  private handleCapabilities(message: Message): MessageResponse {
    return createResponse(message, {
      address: this.actorAddress,
      gatewayUrl: this.config.gatewayUrl,
      defaultModel: this.config.defaultModel,
      capabilities: ['inference', 'tts', 'stt', 'embeddings'],
      activeSessions: this.activeSessions.size,
    });
  }

  // --- Gateway HTTP call ---

  private async callGateway(path: string, body: unknown): Promise<unknown> {
    const url = `${this.config.gatewayUrl}${path}`;
    const headers: Record<string, string> = { 'Content-Type': 'application/json' };
    if (this.config.apiKey) {
      headers['Authorization'] = `Bearer ${this.config.apiKey}`;
    }

    const response = await fetch(url, {
      method: 'POST',
      headers,
      body: JSON.stringify(body),
    });

    if (!response.ok) {
      const text = await response.text();
      throw new Error(`AI Gateway error ${response.status}: ${text}`);
    }

    return response.json();
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create an ActorFactory that provisions GatewayActors on demand.
 *
 * @example
 * system.registerFactory({
 *   prefix: AI_PREFIXES.GATEWAY,
 *   factory: createGatewayFactory({ gatewayUrl: env.AI_GATEWAY_URL }),
 * });
 */
export function createGatewayFactory(
  config: GatewayActorConfig,
): (address: string) => GatewayActor {
  return (address: string) => new GatewayActor(address, config);
}
