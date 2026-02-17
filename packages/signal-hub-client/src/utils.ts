/**
 * Utility functions for Signal Hub client
 */

import type { CanonicalAddress } from '@agentic-primer/protocols';

/**
 * Generate a browser actor address
 */
export function generateBrowserAddress(name?: string): CanonicalAddress {
  const id = name || `actor-${crypto.randomUUID()}`;
  return `@(browser/${id})`;
}

/**
 * Validate canonical address format
 */
export function isValidAddress(address: string): address is CanonicalAddress {
  return /^@\(.+\)$/.test(address);
}

/**
 * Calculate exponential backoff delay
 */
export function calculateBackoff(
  attempt: number,
  initialDelay: number,
  maxDelay: number
): number {
  const delay = initialDelay * Math.pow(2, attempt);
  return Math.min(delay, maxDelay);
}

/**
 * Create a deferred promise
 */
export interface Deferred<T> {
  promise: Promise<T>;
  resolve: (value: T) => void;
  reject: (error: Error) => void;
}

export function createDeferred<T>(): Deferred<T> {
  let resolve!: (value: T) => void;
  let reject!: (error: Error) => void;

  const promise = new Promise<T>((res, rej) => {
    resolve = res;
    reject = rej;
  });

  return { promise, resolve, reject };
}

/**
 * Wait for a specified duration
 */
export function wait(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Check if a value is a plain object
 */
export function isPlainObject(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}
