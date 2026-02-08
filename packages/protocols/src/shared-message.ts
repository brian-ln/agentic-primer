/**
 * SharedMessage - Cross-Runtime Wire Format
 *
 * The shared envelope for messages crossing runtime boundaries.
 * Each actor system (simplify, brianln.ai, system-meta-model) converts
 * its internal message format to/from SharedMessage at bridge boundaries.
 *
 * Spec: ~/knowledge/specs/formal-model/layer-5-projections.md Sec 5.1
 *
 * NOT generated — hand-authored as the convergence point.
 */

import { z } from 'zod';

// ---------------------------------------------------------------------------
// Canonical Address
// ---------------------------------------------------------------------------

/**
 * Canonical address format: @(path)
 * Runtime-agnostic. Each runtime translates to/from its native format:
 *   simplify:          @(namespace/id)    → identity (already canonical)
 *   brianln.ai browser: local://name      → @(name)
 *   brianln.ai DO:     remote://do/name   → @(name)
 *   brianln.ai worker: remote://worker/n  → @(n)
 *   system-meta-model: "actor-name"       → @(actor-name)
 *
 * Spec: Layer 5 Sec 5.2
 */
export type CanonicalAddress = `@(${string})`;

/**
 * Runtime identifier for address translation
 */
export type Runtime = 'local' | 'cloudflare' | 'browser' | 'beam';

/**
 * Convert a runtime-specific address to canonical form.
 * Strips protocol prefixes, wraps in @().
 */
export function toCanonical(runtimeAddr: string, runtime: Runtime): CanonicalAddress {
  let path: string;

  switch (runtime) {
    case 'local':
      // simplify: @(path) is already canonical
      if (runtimeAddr.startsWith('@(') && runtimeAddr.endsWith(')')) {
        return runtimeAddr as CanonicalAddress;
      }
      path = runtimeAddr;
      break;

    case 'browser':
      // brianln.ai browser: local://name → @(name)
      path = runtimeAddr.replace(/^local:\/\//, '');
      break;

    case 'cloudflare':
      // brianln.ai CF: remote://do/name or remote://worker://name or worker://name
      path = runtimeAddr
        .replace(/^remote:\/\/do\//, '')
        .replace(/^remote:\/\/worker\//, '')
        .replace(/^worker:\/\//, '')
        .replace(/^do:\/\//, '');
      break;

    case 'beam':
      // BEAM: tuple string form → just use the path
      path = runtimeAddr;
      break;

    default:
      path = runtimeAddr;
  }

  return `@(${path})` as CanonicalAddress;
}

/**
 * Convert a canonical address to runtime-specific form.
 */
export function fromCanonical(canonical: CanonicalAddress, runtime: Runtime): string {
  const match = canonical.match(/^@\((.+)\)$/);
  if (!match) {
    throw new Error(`Invalid canonical address: ${canonical}`);
  }
  const path = match[1];

  switch (runtime) {
    case 'local':
      return canonical; // identity
    case 'browser':
      return `local://${path}`;
    case 'cloudflare':
      return `remote://do/${path}`;
    case 'beam':
      return path;
    default:
      return canonical;
  }
}

// ---------------------------------------------------------------------------
// SharedMessage
// ---------------------------------------------------------------------------

/**
 * SharedMessage - the cross-runtime wire format.
 *
 * Every message crossing a runtime boundary is serialized as SharedMessage.
 * Internal messages within a single runtime stay in that runtime's native format.
 */
export interface SharedMessage {
  /** Globally unique message ID (crypto.randomUUID()) */
  readonly id: string;
  /** Sender canonical address */
  readonly from: CanonicalAddress;
  /** Recipient canonical address */
  readonly to: CanonicalAddress;
  /** Message type discriminator */
  readonly type: string;
  /** JSON-serializable payload */
  readonly payload: unknown;
  /** Message pattern */
  readonly pattern: 'tell' | 'ask';
  /** For ask/reply correlation */
  readonly correlationId: string | null;
  /** Epoch milliseconds */
  readonly timestamp: number;
  /** Extensible context (trace IDs, routing hints, etc.) */
  readonly metadata: Record<string, unknown>;
  /** Time-to-live in milliseconds (null = no expiry) */
  readonly ttl: number | null;
  /** Base64-encoded HMAC signature (null = unsigned) */
  readonly signature: string | null;
}

// ---------------------------------------------------------------------------
// Zod Validators
// ---------------------------------------------------------------------------

export const canonicalAddressSchema = z.string().regex(
  /^@\(.+\)$/,
  'Canonical address must be in @(path) format'
) as z.ZodType<CanonicalAddress>;

export const sharedMessageSchema = z.object({
  id: z.string().uuid(),
  from: canonicalAddressSchema,
  to: canonicalAddressSchema,
  type: z.string().min(1),
  payload: z.unknown(),
  pattern: z.enum(['tell', 'ask']),
  correlationId: z.string().uuid().nullable(),
  timestamp: z.number().int().positive(),
  metadata: z.record(z.unknown()).default({}),
  ttl: z.number().int().positive().nullable().default(null),
  signature: z.string().nullable().default(null),
}).strict();

// ---------------------------------------------------------------------------
// Converter: simplify Message ↔ SharedMessage
// ---------------------------------------------------------------------------

/**
 * Simplify's internal message shape (subset of fields we need).
 * Full type is in simplify/src/messaging/message.ts
 */
interface SimplifyMessage {
  id: string;
  pattern: 'tell' | 'ask' | 'stream';
  to: `@(${string})`;
  from?: `@(${string})`;
  type: string;
  payload: unknown;
  correlationId?: string;
  timestamp: number;
  metadata?: Record<string, unknown>;
}

export function simplifyToShared(msg: SimplifyMessage): SharedMessage {
  return {
    id: msg.id,
    from: (msg.from ?? '@(unknown)') as CanonicalAddress,
    to: msg.to as CanonicalAddress,
    type: msg.type,
    payload: msg.payload,
    pattern: msg.pattern === 'stream' ? 'tell' : msg.pattern,
    correlationId: msg.correlationId ?? null,
    timestamp: msg.timestamp,
    metadata: msg.metadata ?? {},
    ttl: null,
    signature: null,
  };
}

export function sharedToSimplify(msg: SharedMessage): SimplifyMessage {
  return {
    id: msg.id,
    pattern: msg.pattern,
    to: msg.to,
    from: msg.from,
    type: msg.type,
    payload: msg.payload,
    correlationId: msg.correlationId ?? undefined,
    timestamp: msg.timestamp,
    metadata: Object.keys(msg.metadata).length > 0 ? msg.metadata : undefined,
  };
}

// ---------------------------------------------------------------------------
// Converter: brianln.ai ActorMessage ↔ SharedMessage
// ---------------------------------------------------------------------------

/**
 * brianln.ai's internal message shape.
 * Full type is in brianln.ai/src/actors/types.ts
 */
interface BrianActorMessage {
  type: string;
  payload?: unknown;
  correlationId?: string;
  replyTo?: string;
  timestamp?: number;
}

/** brianln.ai address types */
type BrianActorAddress =
  | `local://${string}`
  | `worker://${string}`
  | `remote://do/${string}`
  | `remote://worker/${string}`;

export function brianToShared(
  msg: BrianActorMessage,
  from: BrianActorAddress,
  to: BrianActorAddress,
): SharedMessage {
  const fromRuntime = from.startsWith('local://') ? 'browser' as const : 'cloudflare' as const;
  const toRuntime = to.startsWith('local://') ? 'browser' as const : 'cloudflare' as const;

  return {
    id: crypto.randomUUID(),
    from: toCanonical(from, fromRuntime),
    to: toCanonical(to, toRuntime),
    type: msg.type,
    payload: msg.payload ?? null,
    pattern: msg.correlationId ? 'ask' : 'tell',
    correlationId: msg.correlationId ?? null,
    timestamp: msg.timestamp ?? Date.now(),
    metadata: msg.replyTo ? { replyTo: msg.replyTo } : {},
    ttl: null,
    signature: null,
  };
}

export function sharedToBrian(msg: SharedMessage, targetRuntime: Runtime = 'browser'): {
  message: BrianActorMessage;
  address: string;
} {
  const message: BrianActorMessage = {
    type: msg.type,
    payload: msg.payload,
    correlationId: msg.correlationId ?? undefined,
    replyTo: (msg.metadata.replyTo as string) ?? undefined,
    timestamp: msg.timestamp,
  };

  return {
    message,
    address: fromCanonical(msg.to, targetRuntime),
  };
}
