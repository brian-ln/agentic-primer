/**
 * SessionActor — Per-user session actor (replaces VoiceSession DO prototype)
 *
 * Address: ai/session/<user-id>
 * Example: ai/session/user-abc123
 *
 * Manages the lifecycle of a user's AI session:
 *   - Tracks which capability actors are active for this session
 *   - Provisions sub-actors (inference, TTS, STT) on demand
 *   - Routes capability requests to the right sub-actor
 *   - Handles session teardown (stops all active sub-actors)
 *
 * Handles:
 *   ai.session.create     → initialize session, return state
 *   ai.session.end        → tear down all active actors
 *   ai.inference.request  → forward to session's inference actor
 *   ai.tts.request        → forward to session's TTS actor
 *   ai.stt.start          → forward to session's STT actor
 *   ai.stt.stop           → forward to session's STT actor
 */

import type { Message, MessageResponse, MessageHandler } from '@agentic-primer/actors';
import { createResponse, createErrorResponse, address } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES, inferenceAddress, ttsAddress, sttAddress } from './index.ts';
import type {
  SessionCreatePayload,
  SessionStatePayload,
  DiscoverResponsePayload,
  HealthPayload,
  HealthResponsePayload,
} from './types.ts';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface SessionActorConfig {
  /** Default AI namespace (e.g. 'bln_ai'). */
  namespace: string;
  /** Default inference provider/model (e.g. 'nim/kimi-k2.5'). */
  defaultInferenceModel?: string;
  /** Default TTS provider/voice (e.g. 'deepgram/aura-2-en-us'). */
  defaultTtsVoice?: string;
  /** Default STT provider/model (e.g. 'deepgram/nova-3'). */
  defaultSttModel?: string;
}

// ---------------------------------------------------------------------------
// SessionActor
// ---------------------------------------------------------------------------

export class SessionActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly userId: string;
  private readonly config: SessionActorConfig;
  private readonly system: {
    send(to: ReturnType<typeof address>, type: string, payload: unknown): void;
  };
  private state: SessionStatePayload;

  constructor(
    actorAddress: string,
    config: SessionActorConfig,
    system: { send(to: ReturnType<typeof address>, type: string, payload: unknown): void },
  ) {
    this.actorAddress = actorAddress;
    this.config = config;
    this.system = system;

    // Extract userId from address: ai/session/<userId>
    const parts = actorAddress.split('/');
    this.userId = parts.slice(2).join('/') || 'unknown';

    this.state = {
      userId: this.userId,
      activeActors: [],
      createdAt: Date.now(),
    };
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      switch (message.type) {
        case AI_MESSAGE_TYPES.SESSION_CREATE:
          return this.handleCreate(message);
        case AI_MESSAGE_TYPES.SESSION_END:
          return this.handleEnd(message);
        case AI_MESSAGE_TYPES.INFERENCE_REQUEST:
          return this.forwardToInference(message);
        case AI_MESSAGE_TYPES.TTS_REQUEST:
          return this.forwardToTts(message);
        case AI_MESSAGE_TYPES.STT_START:
        case AI_MESSAGE_TYPES.STT_STOP:
          return this.forwardToStt(message);
        case 'session.state':
          return createResponse(message, this.state);
        case AI_MESSAGE_TYPES.DISCOVER:
          return this.handleDiscover(message);
        case AI_MESSAGE_TYPES.HEALTH:
          return this.handleHealth(message);
        default:
          return createErrorResponse(
            message,
            `SessionActor(${this.actorAddress}): unhandled message type '${message.type}'`,
          );
      }
    } catch (err) {
      return createErrorResponse(message, err instanceof Error ? err.message : String(err));
    }
  }

  // --- Handlers ---

  private handleDiscover(message: Message): MessageResponse {
    const result: DiscoverResponsePayload = {
      address: this.actorAddress,
      type: 'session',
      handles: [
        AI_MESSAGE_TYPES.SESSION_CREATE,
        AI_MESSAGE_TYPES.SESSION_END,
        AI_MESSAGE_TYPES.SESSION_STATE,
        AI_MESSAGE_TYPES.DISCOVER,
        AI_MESSAGE_TYPES.HEALTH,
      ],
      meta: { userId: this.userId },
    };
    return createResponse(message, result);
  }

  private handleHealth(message: Message): MessageResponse {
    const payload = message.payload as HealthPayload;
    const result: HealthResponsePayload = { status: 'ok', address: this.actorAddress, token: payload?.token };
    return createResponse(message, result);
  }

  private handleCreate(message: Message): MessageResponse {
    const payload = (message.payload ?? {}) as SessionCreatePayload;
    // Update metadata if provided
    if (payload.metadata) {
      this.state = { ...this.state, metadata: payload.metadata };
    }
    return createResponse(message, this.state);
  }

  private handleEnd(message: Message): MessageResponse {
    // Note: in a full ActorSystem integration, we'd call system.stop() on each
    // active sub-actor. For now, clear the active actors list.
    const stopped = [...this.state.activeActors];
    this.state = {
      ...this.state,
      activeActors: [],
    };
    return createResponse(message, { userId: this.userId, stoppedActors: stopped });
  }

  private forwardToInference(message: Message): MessageResponse {
    const ns = this.config.namespace;
    const model = this.config.defaultInferenceModel ?? 'nim/kimi-k2.5';
    const [provider, ...rest] = model.split('/');
    const modelName = rest.join('/') || provider;
    const addr = inferenceAddress(ns, provider, modelName);

    this.ensureActiveActor(addr);
    this.system.send(address(addr), message.type, message.payload);

    // Return immediately — response comes asynchronously via correlationId
    return createResponse(message, { forwarded: addr });
  }

  private forwardToTts(message: Message): MessageResponse {
    const ns = this.config.namespace;
    const voice = this.config.defaultTtsVoice ?? 'deepgram/aura-2-en-us';
    const [provider, ...rest] = voice.split('/');
    const voiceName = rest.join('/') || provider;
    const addr = ttsAddress(ns, provider, voiceName);

    this.ensureActiveActor(addr);
    this.system.send(address(addr), message.type, message.payload);

    return createResponse(message, { forwarded: addr });
  }

  private forwardToStt(message: Message): MessageResponse {
    const ns = this.config.namespace;
    const model = this.config.defaultSttModel ?? 'deepgram/nova-3';
    const [provider, ...rest] = model.split('/');
    const modelName = rest.join('/') || provider;
    const addr = sttAddress(ns, provider, modelName);

    this.ensureActiveActor(addr);
    this.system.send(address(addr), message.type, message.payload);

    return createResponse(message, { forwarded: addr });
  }

  // --- Helpers ---

  private ensureActiveActor(addr: string): void {
    if (!this.state.activeActors.includes(addr)) {
      this.state = {
        ...this.state,
        activeActors: [...this.state.activeActors, addr],
      };
    }
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create an ActorFactory for the `ai/session/` prefix.
 *
 * @example
 * system.registerFactory({
 *   prefix: AI_PREFIXES.SESSION,
 *   factory: createSessionFactory({ namespace: 'bln_ai' }, system),
 * });
 */
export function createSessionFactory(
  config: SessionActorConfig,
  system: { send(to: ReturnType<typeof address>, type: string, payload: unknown): void },
): (actorAddress: string) => SessionActor | null {
  return (actorAddress: string) => {
    const parts = actorAddress.split('/');
    if (parts.length < 3 || parts[0] !== 'ai' || parts[1] !== 'session') return null;
    return new SessionActor(actorAddress, config, system);
  };
}
