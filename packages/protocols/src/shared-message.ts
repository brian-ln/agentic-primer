/**
 * SharedMessage - Cross-Runtime Wire Format
 *
 * Provides refined TypeScript types on top of generated schema types,
 * plus converter functions for each runtime.
 *
 * The generated types (domain.types.ts) define SharedMessage and CanonicalAddress
 * from the JSON Schema. This module re-exports them with template literal
 * refinements and adds converter functions.
 *
 * Spec: ~/knowledge/specs/formal-model/layer-5-projections.md Sec 5.1
 */

import { z } from 'zod';
import type {
  SharedMessage as GeneratedSharedMessage,
  ConnectionState,
  HealthStatus,
  ContentType,
} from './domain.types.js';

// ---------------------------------------------------------------------------
// Refined Canonical Address (template literal)
// ---------------------------------------------------------------------------

/**
 * Canonical address format: @(path)
 * Runtime-agnostic. Each runtime translates to/from its native format.
 *
 * Refines the generated CanonicalAddress (plain string) with a template literal.
 */
export type CanonicalAddress = `@(${string})`;

/**
 * Runtime identifier for address translation
 */
export type Runtime = 'local' | 'cloudflare' | 'browser' | 'beam';

/**
 * Convert a runtime-specific address to canonical form.
 */
export function toCanonical(runtimeAddr: string, runtime: Runtime): CanonicalAddress {
  let path: string;

  switch (runtime) {
    case 'local':
      if (runtimeAddr.startsWith('@(') && runtimeAddr.endsWith(')')) {
        return runtimeAddr as CanonicalAddress;
      }
      path = runtimeAddr;
      break;

    case 'browser':
      path = runtimeAddr.replace(/^local:\/\//, '');
      break;

    case 'cloudflare':
      path = runtimeAddr
        .replace(/^remote:\/\/do\//, '')
        .replace(/^remote:\/\/worker\//, '')
        .replace(/^worker:\/\//, '')
        .replace(/^do:\/\//, '');
      break;

    case 'beam':
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
      return canonical;
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
// Refined SharedMessage (uses template literal CanonicalAddress)
// ---------------------------------------------------------------------------

/**
 * SharedMessage - the cross-runtime wire format with refined types.
 * Extends the generated type with template literal CanonicalAddress.
 */
export interface SharedMessage extends Omit<GeneratedSharedMessage, 'from' | 'to' | 'payload' | 'metadata'> {
  readonly from: CanonicalAddress;
  readonly to: CanonicalAddress;
  readonly payload: unknown;
  readonly metadata: Record<string, unknown>;
}

// Re-export infrastructure types from generated
export type { ConnectionState, HealthStatus, ContentType };

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

interface BrianActorMessage {
  type: string;
  payload?: unknown;
  correlationId?: string;
  replyTo?: string;
  timestamp?: number;
}

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
