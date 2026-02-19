/**
 * @agentic-primer/ai
 *
 * AI capability actors for the agentic-primer actor mesh.
 *
 * Actor address namespace
 * -----------------------
 * All AI actors live under the `ai/` prefix. Each segment encodes a routing
 * dimension: capability → namespace → provider → model/variant.
 *
 *   ai/session/<user-id>            — per-user session actor
 *   ai/gateway/<namespace>          — AI gateway proxy (e.g. ai/gateway/bln_ai)
 *   ai/inference/<ns>/<prov>/<model> — LLM inference (e.g. ai/inference/bln_ai/nim/kimi-k2.5)
 *   ai/tts/<ns>/<prov>/<voice>      — text-to-speech (e.g. ai/tts/bln_ai/deepgram/aura-2)
 *   ai/stt/<ns>/<prov>/<model>      — speech-to-text (e.g. ai/stt/bln_ai/deepgram/nova-3)
 *   ai/embeddings/<ns>/<prov>/<mod> — embeddings (e.g. ai/embeddings/bln_ai/nim/llama-3.3)
 *   ai/credentials/<target>         — credential store (e.g. ai/credentials/cf-aig)
 *
 * Actors are provisioned on demand via the Virtual Actor pattern:
 * register an ActorFactory on your ActorSystem for each prefix you want to serve.
 *
 * @example
 * import { AI_PREFIXES } from '@agentic-primer/ai';
 * import { GatewayActorFactory } from '@agentic-primer/ai/gateway';
 *
 * system.registerFactory({
 *   prefix: AI_PREFIXES.GATEWAY,
 *   factory: GatewayActorFactory(env),
 * });
 */

// ---------------------------------------------------------------------------
// Protocol types
// ---------------------------------------------------------------------------

export * from './types.ts';

// ---------------------------------------------------------------------------
// Actors
// ---------------------------------------------------------------------------

export { GatewayActor, createGatewayFactory } from './gateway-actor.ts';
export type { GatewayActorConfig } from './gateway-actor.ts';

export { InferenceActor, createInferenceFactory, parseInferenceAddress } from './inference-actor.ts';
export type { InferenceActorConfig, InferenceAddress } from './inference-actor.ts';

export { TtsActor, createTtsFactory, parseTtsAddress } from './tts-actor.ts';
export type { TtsActorConfig, TtsAddress } from './tts-actor.ts';

export { SttActor, SystemSttActor, createSttFactory, parseSttAddress } from './stt-actor.ts';
export type { SttActorConfig, SttAddress } from './stt-actor.ts';

export { SessionActor, createSessionFactory } from './session-actor.ts';
export type { SessionActorConfig } from './session-actor.ts';

export { CredentialsActor, createCredentialsFactory, parseCredentialsAddress } from './credentials-actor.ts';
export type { CredentialsActorConfig, CredentialsAddress } from './credentials-actor.ts';

export { EmbeddingsActor, createEmbeddingsFactory, parseEmbeddingsAddress } from './embeddings-actor.ts';
export type { EmbeddingsActorConfig, EmbeddingsAddress } from './embeddings-actor.ts';

// ---------------------------------------------------------------------------
// Address prefix constants
// ---------------------------------------------------------------------------

/** Canonical address prefixes for AI actor namespaces. */
export const AI_PREFIXES = {
  /** Per-user session actors: ai/session/<user-id> */
  SESSION: 'ai/session/',
  /** AI gateway proxy actors: ai/gateway/<namespace> */
  GATEWAY: 'ai/gateway/',
  /** LLM inference actors: ai/inference/<ns>/<provider>/<model> */
  INFERENCE: 'ai/inference/',
  /** Text-to-speech actors: ai/tts/<ns>/<provider>/<voice> */
  TTS: 'ai/tts/',
  /** Speech-to-text actors: ai/stt/<ns>/<provider>/<model> */
  STT: 'ai/stt/',
  /** Embedding actors: ai/embeddings/<ns>/<provider>/<model> */
  EMBEDDINGS: 'ai/embeddings/',
  /** Credential store actors: ai/credentials/<target> */
  CREDENTIALS: 'ai/credentials/',
} as const;

// ---------------------------------------------------------------------------
// Address builder helpers
// ---------------------------------------------------------------------------

/** Build a gateway actor address. */
export function gatewayAddress(namespace: string): string {
  return `${AI_PREFIXES.GATEWAY}${namespace}`;
}

/** Build an inference actor address. */
export function inferenceAddress(namespace: string, provider: string, model: string): string {
  return `${AI_PREFIXES.INFERENCE}${namespace}/${provider}/${model}`;
}

/** Build a TTS actor address. */
export function ttsAddress(namespace: string, provider: string, voice: string): string {
  return `${AI_PREFIXES.TTS}${namespace}/${provider}/${voice}`;
}

/** Build an STT actor address. */
export function sttAddress(namespace: string, provider: string, model: string): string {
  return `${AI_PREFIXES.STT}${namespace}/${provider}/${model}`;
}

/** Build an embeddings actor address. */
export function embeddingsAddress(namespace: string, provider: string, model: string): string {
  return `${AI_PREFIXES.EMBEDDINGS}${namespace}/${provider}/${model}`;
}

/** Build a session actor address. */
export function sessionAddress(userId: string): string {
  return `${AI_PREFIXES.SESSION}${userId}`;
}

/** Build a credentials actor address. */
export function credentialsAddress(target: string): string {
  return `${AI_PREFIXES.CREDENTIALS}${target}`;
}
