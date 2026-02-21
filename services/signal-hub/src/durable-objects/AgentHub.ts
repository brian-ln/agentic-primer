/**
 * AgentHub — Durable Object bridging the actor mesh and AI capability actors
 *
 * Extends DOActorSystem to register:
 *   - SessionActor factory   → ai/session/<user-id>
 *   - FluxRelayActor factory → ai/flux/<namespace>
 *
 * Each user's AI session is a virtual actor provisioned on demand.
 * FluxRelayActor maintains a persistent upstream WebSocket to @cf/deepgram/flux
 * for real-time STT, shared across all binary channels in a namespace.
 *
 * Env bindings required in wrangler.toml:
 *   CF_ACCOUNT_ID       — Cloudflare account ID (for AIG URL construction)
 *   CF_GATEWAY_NAME     — AI Gateway name (e.g. "bln-ai-gateway")
 *   CF_AIG_TOKEN        — AI Gateway token (secret)
 *   AI_NAMESPACE        — Namespace for actor addressing (e.g. "bln_ai")
 *   DEFAULT_INFERENCE   — Default inference provider/model (e.g. "nim/kimi-k2.5")
 *   DEFAULT_TTS_VOICE   — Default TTS voice (e.g. "deepgram/aura-2-en-us")
 *   DEFAULT_STT_MODEL   — Default STT model (e.g. "deepgram/nova-3")
 */

import { DOActorSystem } from '@agentic-primer/cloudflare';
import type { ActorSystem } from '@agentic-primer/actors';
import {
  createSessionFactory,
  createFluxRelayFactory,
  AI_PREFIXES,
} from '@agentic-primer/ai';

// ---------------------------------------------------------------------------
// AgentHub Env
// ---------------------------------------------------------------------------

export interface AgentHubEnv {
  /** Cloudflare account ID — used to construct the AIG WebSocket URL. */
  CF_ACCOUNT_ID: string;
  /** AI Gateway name (e.g. "bln-ai-gateway"). */
  CF_GATEWAY_NAME: string;
  /** AI Gateway token injected as a secret. */
  CF_AIG_TOKEN: string;
  /** Namespace for actor addressing (e.g. "bln_ai"). */
  AI_NAMESPACE: string;
  /** Default inference model path, e.g. "nim/kimi-k2.5". */
  DEFAULT_INFERENCE?: string;
  /** Default TTS voice path, e.g. "deepgram/aura-2-en-us". */
  DEFAULT_TTS_VOICE?: string;
  /** Default STT model path, e.g. "deepgram/nova-3". */
  DEFAULT_STT_MODEL?: string;
}

// ---------------------------------------------------------------------------
// AgentHub
// ---------------------------------------------------------------------------

/**
 * AgentHub Durable Object.
 *
 * Each AgentHub instance hosts a full ActorSystem that provisions AI capability
 * actors (sessions, flux relay) on demand via virtual actor factories.
 *
 * One instance per namespace is recommended (idFromName(namespace)).
 */
export class AgentHub extends DOActorSystem<AgentHubEnv> {
  protected override configure(system: ActorSystem): void {
    const env = this.env;

    // --- SessionActor factory ---
    // Provisions a per-user SessionActor at ai/session/<userId> on first message.
    system.registerFactory({
      prefix: AI_PREFIXES.SESSION,
      factory: createSessionFactory(
        {
          namespace: env.AI_NAMESPACE,
          defaultInferenceModel: env.DEFAULT_INFERENCE ?? 'nim/kimi-k2.5',
          defaultTtsVoice: env.DEFAULT_TTS_VOICE ?? 'deepgram/aura-2-en-us',
          defaultSttModel: env.DEFAULT_STT_MODEL ?? 'deepgram/nova-3',
        },
        system,
      ),
    });

    // --- FluxRelayActor factory ---
    // Provisions a per-namespace FluxRelayActor at ai/flux/<namespace> on first message.
    // Maintains a shared upstream Deepgram Flux WS for all binary audio channels
    // within the same namespace.
    const fluxWsUrl =
      `wss://gateway.ai.cloudflare.com/v1/${env.CF_ACCOUNT_ID}/${env.CF_GATEWAY_NAME}/deepgram/flux`;

    system.registerFactory({
      prefix: AI_PREFIXES.FLUX,
      factory: createFluxRelayFactory(
        {
          fluxWsUrl,
          aigToken: env.CF_AIG_TOKEN,
        },
        system,
      ),
    });
  }
}
