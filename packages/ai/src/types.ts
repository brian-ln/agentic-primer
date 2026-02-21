/**
 * Canonical AI actor message payload types.
 *
 * Mirror of the JSON Schema definitions in:
 *   packages/protocols/schema/hub-messages.schema.json (ai-* definitions)
 *
 * These are the payloads carried by actor messages (Message.payload) between
 * AI capability actors. The message type strings are the canonical identifiers:
 *
 *   'ai.inference.request'   → InferenceRequestPayload
 *   'ai.inference.response'  → InferenceResponsePayload
 *   'ai.inference.chunk'     → InferenceChunkPayload (streaming)
 *   'ai.tts.request'         → TtsRequestPayload
 *   'ai.stt.start'           → SttStartPayload
 *   'ai.stt.transcript'      → SttTranscriptPayload
 *   'ai.session.create'      → SessionCreatePayload
 *   'ai.session.state'       → SessionStatePayload
 *   'audio.frame'            → Uint8Array (binary fast-path, no JSON payload)
 */

// ---------------------------------------------------------------------------
// Inference
// ---------------------------------------------------------------------------

export interface ChatMessage {
  role: 'system' | 'user' | 'assistant' | 'tool';
  content: string;
  [key: string]: unknown;
}

export interface InferenceRequestPayload {
  messages: ChatMessage[];
  /** Model override. Defaults to the actor's configured model. */
  model?: string;
  /** Request streaming chunks. */
  stream?: boolean;
  temperature?: number;
  maxTokens?: number;
  [key: string]: unknown;
}

export interface InferenceUsage {
  promptTokens: number;
  completionTokens: number;
  totalTokens: number;
}

export interface InferenceResponsePayload {
  content: string;
  model?: string;
  usage?: InferenceUsage;
  finishReason?: string;
  [key: string]: unknown;
}

/** One incremental token chunk from a streaming inference response. */
export interface InferenceChunkPayload {
  delta: string;
  index: number;
}

// ---------------------------------------------------------------------------
// Text-to-Speech
// ---------------------------------------------------------------------------

export type AudioEncoding = 'opus' | 'mp3' | 'linear16';

export interface TtsRequestPayload {
  text: string;
  /** Voice ID override. Defaults to actor's configured voice. */
  voice?: string;
  encoding?: AudioEncoding;
  sampleRate?: number;
}

// ---------------------------------------------------------------------------
// Speech-to-Text
// ---------------------------------------------------------------------------

export type SttAudioEncoding = 'opus' | 'mp3' | 'linear16' | 'mulaw';

export interface SttStartPayload {
  encoding?: SttAudioEncoding;
  sampleRate?: number;
  /** BCP-47 language code. Default: en-US. */
  language?: string;
  model?: string;
}

export interface SttWord {
  word: string;
  start: number;
  end: number;
  confidence?: number;
}

export interface SttTranscriptPayload {
  transcript: string;
  isFinal: boolean;
  confidence?: number;
  words?: SttWord[];
}

export interface SttErrorPayload {
  /** Human-readable error description. */
  error: string;
  /** Optional channel identifier from the originating channel open. */
  channelId?: string;
}

// ---------------------------------------------------------------------------
// Credentials
// ---------------------------------------------------------------------------

export interface CredentialsGetPayload {
  target: string;
}

export interface CredentialsPayload {
  target: string;
  apiKey?: string;
  gatewayUrl?: string;
}

// ---------------------------------------------------------------------------
// Embeddings
// ---------------------------------------------------------------------------

export interface EmbeddingsRequestPayload {
  inputs: string[];
  /** Model override. Defaults to the actor's configured model. */
  model?: string;
}

export interface EmbeddingsResponsePayload {
  embeddings: number[][];
  model?: string;
  usage?: {
    promptTokens: number;
    totalTokens: number;
  };
}

// ---------------------------------------------------------------------------
// Session
// ---------------------------------------------------------------------------

export interface SessionCreatePayload {
  userId: string;
  metadata?: Record<string, unknown>;
}

export interface SessionStatePayload {
  userId: string;
  /** Addresses of actors active within this session. */
  activeActors: string[];
  createdAt: number;
  metadata?: Record<string, unknown>;
}

// ---------------------------------------------------------------------------
// Message type string constants
// ---------------------------------------------------------------------------

/** Canonical message type identifiers for AI actor messages. */
export const AI_MESSAGE_TYPES = {
  INFERENCE_REQUEST: 'ai.inference.request',
  INFERENCE_RESPONSE: 'ai.inference.response',
  INFERENCE_CHUNK: 'ai.inference.chunk',
  INFERENCE_DONE: 'ai.inference.done',
  TTS_REQUEST: 'ai.tts.request',
  TTS_DONE: 'ai.tts.done',
  STT_START: 'ai.stt.start',
  STT_STOP: 'ai.stt.stop',
  STT_TRANSCRIPT: 'ai.stt.transcript',
  /** Upstream failure notification — sent to all active senders when Flux WS drops. */
  STT_ERROR: 'ai.stt.error',
  SESSION_CREATE: 'ai.session.create',
  SESSION_END: 'ai.session.end',
  SESSION_STATE: 'ai.session.state',
  /** Retrieve credentials for a named target. */
  CREDENTIALS_GET: 'ai.credentials.get',
  /** Request an embedding vector for a batch of inputs. */
  EMBEDDINGS_REQUEST: 'ai.embeddings.request',
  /** Binary audio frames are delivered via binary channel (not JSON). */
  AUDIO_FRAME: 'audio.frame',
  /** Discovery: query an actor's capabilities and metadata. */
  DISCOVER: 'ai.discover',
  /** Health: check whether an actor is operational. */
  HEALTH: 'ai.health',
} as const;

export type MessageType = (typeof AI_MESSAGE_TYPES)[keyof typeof AI_MESSAGE_TYPES];

// ---------------------------------------------------------------------------
// Discovery
// ---------------------------------------------------------------------------

export interface DiscoverPayload {
  /** Requesting actor address, for targeted responses. */
  from?: string;
}

export interface DiscoverResponsePayload {
  /** Actor address. */
  address: string;
  /** Actor type / capability name (e.g. 'inference', 'tts', 'embeddings'). */
  type: string;
  /** Supported message types. */
  handles: string[];
  /** Actor-specific metadata (model, provider, namespace, etc). */
  meta: Record<string, string | undefined>;
}

export interface HealthPayload {
  /** Optional echo token returned in response. */
  token?: string;
}

export interface HealthResponsePayload {
  status: 'ok' | 'degraded' | 'unavailable';
  address: string;
  /** Echo of request token if provided. */
  token?: string;
  /** Optional latency measurement (ms) or error message. */
  latency?: number;
  error?: string;
}
