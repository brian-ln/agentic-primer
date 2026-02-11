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
