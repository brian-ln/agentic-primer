/**
 * Cloudflare-specific type definitions, address parsing, and environment helpers.
 *
 * Provides address schemes for Durable Object and Worker-to-Worker communication,
 * plus alarm scheduling types for the DO actor system.
 */

/** Address format for Durable Object actors: do://{doName}/{actorPath} */
export type DOAddress = `do://${string}/${string}`;

/** Address format for Worker actors: worker://{serviceName}/{actorPath} */
export type WorkerAddress = `worker://${string}/${string}`;

/** Parsed components of a DO address */
export interface ParsedDOAddress {
  doName: string;
  actorPath: string;
}

/** Parsed components of a Worker address */
export interface ParsedWorkerAddress {
  serviceName: string;
  actorPath: string;
}

/**
 * Parse a DO address into its components.
 * @param addr - Address in format `do://{doName}/{actorPath}`
 * @returns Parsed DO name and actor path
 * @throws Error if address format is invalid
 */
export function parseDOAddress(addr: string): ParsedDOAddress {
  const match = addr.match(/^do:\/\/([^/]+)\/(.+)$/);
  if (!match) {
    throw new Error(`Invalid DO address format: ${addr}. Expected do://{doName}/{actorPath}`);
  }
  return { doName: match[1], actorPath: match[2] };
}

/**
 * Parse a Worker address into its components.
 * @param addr - Address in format `worker://{serviceName}/{actorPath}`
 * @returns Parsed service name and actor path
 * @throws Error if address format is invalid
 */
export function parseWorkerAddress(addr: string): ParsedWorkerAddress {
  const match = addr.match(/^worker:\/\/([^/]+)\/(.+)$/);
  if (!match) {
    throw new Error(`Invalid Worker address format: ${addr}. Expected worker://{serviceName}/{actorPath}`);
  }
  return { serviceName: match[1], actorPath: match[2] };
}

/**
 * Environment type helper for typed Cloudflare bindings.
 * Extend this interface in your worker to get typed env access.
 */
export interface CloudflareEnv {
  [key: string]:
    | DurableObjectNamespace
    | Fetcher
    | string
    | undefined;
}

/**
 * Alarm schedule configuration for DO actor system.
 * Defines how alarms map to actor messages with optional recurrence.
 */
export interface AlarmSchedule {
  /** Target actor address (local path within the DO) */
  targetActor: string;
  /** Message type to deliver when alarm fires */
  messageType: string;
  /** Optional payload for the alarm message */
  payload?: unknown;
  /** Next alarm fire time as Unix timestamp in ms */
  nextRunAt: number;
  /** Optional recurring interval in ms. If set, alarm reschedules after firing. */
  interval?: number;
}
