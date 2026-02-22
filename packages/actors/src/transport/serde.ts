/**
 * Serialization implementations.
 * Ported from brianln.ai/src/actors/transports/Serde.ts.
 * Uses web platform APIs only (TextEncoder/TextDecoder).
 */

import type { ISerde } from '../interfaces.ts';

/**
 * JSON serialization (default).
 */
export class JsonSerde implements ISerde {
  serialize(value: unknown): Uint8Array {
    const json = JSON.stringify(value);
    return new TextEncoder().encode(json);
  }

  deserialize(data: Uint8Array): unknown {
    const json = new TextDecoder().decode(data);
    return JSON.parse(json);
  }

  get contentType(): string {
    return 'application/json';
  }
}

/**
 * Passthrough serde for binary payloads.
 *
 * serialize: wraps value in Uint8Array if not already; throws for non-binary.
 * deserialize: returns the raw bytes unchanged.
 *
 * Used by the binary channel fast-path — binary WebSocket frames bypass JSON
 * deserialization entirely and are delivered directly to actors as Uint8Array.
 */
export class PassthroughSerde implements ISerde {
  serialize(value: unknown): Uint8Array {
    if (value instanceof Uint8Array) return value;
    if (value instanceof ArrayBuffer) return new Uint8Array(value);
    throw new Error('PassthroughSerde: value must be Uint8Array or ArrayBuffer');
  }

  deserialize(data: Uint8Array): unknown {
    return data;
  }

  get contentType(): string {
    return 'application/octet-stream';
  }
}
