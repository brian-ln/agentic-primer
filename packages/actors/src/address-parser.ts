/**
 * Address Parser - Path-based addressing utilities
 *
 * Parses hierarchical path addresses for actor routing.
 * Ported from simplify/src/messaging/address-parser.ts
 */

import type { Address } from './message.ts';
import { parseAddress } from './message.ts';

export interface ParsedAddressInfo {
  address: Address;
  raw: string;
  segments: string[];
}

/** Parse an address into path segments. */
export function parseAddressInfo(addr: Address): ParsedAddressInfo {
  const raw = parseAddress(addr);
  const segments = raw.split('/').filter(s => s.length > 0);
  return { address: addr, raw, segments };
}

/** Check if address contains a hierarchical path (has /). */
export function isHierarchicalPath(addr: Address): boolean {
  const raw = parseAddress(addr);
  return raw.includes('/') || raw.length > 0;
}
