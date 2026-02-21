/**
 * token-store.ts
 *
 * Read/write/clear token files at ~/.config/bln/auth-<clientId>.json
 * Uses node:fs and node:os — this package runs in Node/Bun, NOT in CF Workers.
 */

import { existsSync, mkdirSync, readFileSync, unlinkSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import type { StoredToken } from './types.js';

function tokenPath(clientId: string): string {
  const base = join(homedir(), '.config', 'bln');
  mkdirSync(base, { recursive: true });
  return join(base, `auth-${clientId}.json`);
}

/** Read stored token for clientId. Returns null if missing or unreadable. */
export function readToken(clientId: string): StoredToken | null {
  const path = tokenPath(clientId);
  if (!existsSync(path)) return null;
  try {
    const raw = readFileSync(path, 'utf8');
    return JSON.parse(raw) as StoredToken;
  } catch {
    return null;
  }
}

/** Persist a token to disk. */
export function writeToken(token: StoredToken): void {
  const path = tokenPath(token.client_id);
  writeFileSync(path, JSON.stringify(token, null, 2), 'utf8');
}

/** Delete stored token for clientId. */
export function deleteToken(clientId: string): void {
  const path = tokenPath(clientId);
  if (existsSync(path)) {
    unlinkSync(path);
  }
}
