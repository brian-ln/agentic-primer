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
  SESSION_CREATE: 'ai.session.create',
  SESSION_END: 'ai.session.end',
  SESSION_STATE: 'ai.session.state',
  /** Binary audio frames are delivered via binary channel (not JSON). */
  AUDIO_FRAME: 'audio.frame',
} as const;

export type MessageType = (typeof AI_MESSAGE_TYPES)[keyof typeof AI_MESSAGE_TYPES];
