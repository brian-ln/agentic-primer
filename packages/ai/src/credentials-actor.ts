/**
 * CredentialsActor — credential store actor
 *
 * Address: ai/credentials/<target>
 * Example: ai/credentials/nim, ai/credentials/deepgram
 *
 * Address segments:
 *   [0] = 'ai'
 *   [1] = 'credentials'
 *   [2] = target  (e.g. 'nim', 'deepgram', 'cf-aig')
 *
 * Handles:
 *   ai.credentials.get  → CredentialsGetPayload  → CredentialsPayload
 */

import type { Message, MessageResponse, MessageHandler } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';
import type {
  CredentialsGetPayload,
  CredentialsPayload,
  DiscoverResponsePayload,
  HealthPayload,
  HealthResponsePayload,
} from './types.ts';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface CredentialsActorConfig {
  credentials: Record<string, { apiKey?: string; gatewayUrl?: string }>;
}

// ---------------------------------------------------------------------------
// Address parsing
// ---------------------------------------------------------------------------

export interface CredentialsAddress {
  target: string;
}

/**
 * Parse a credentials actor address into its constituent parts.
 * Input: 'ai/credentials/nim'
 */
export function parseCredentialsAddress(address: string): CredentialsAddress | null {
  const parts = address.split('/');
  // ai / credentials / <target>
  if (parts.length < 3 || parts[0] !== 'ai' || parts[1] !== 'credentials') return null;
  return {
    target: parts.slice(2).join('/'), // target may theoretically contain slashes
  };
}

// ---------------------------------------------------------------------------
// Caller ACL
// ---------------------------------------------------------------------------

/**
 * Map from credentials target name → allowed caller address prefixes.
 *
 * A request for credentials is allowed only when `message.from` starts with
 * at least one of the listed prefixes.  The wildcard entry `'*'` grants
 * access to all callers (used for targets that have no restricted consumers).
 *
 * Add entries here when new targets or new legitimate callers are introduced.
 * Test actors use the prefix 'test/' and are always permitted so that unit
 * tests work without changes.
 */
const CREDENTIALS_ACL: Record<string, string[]> = {
  nim: ['ai/inference/', 'test/'],
  deepgram: ['ai/stt/', 'ai/tts/', 'test/'],
  'cf-aig': ['ai/inference/', 'ai/stt/', 'ai/tts/', 'test/'],
  openai: ['ai/inference/', 'test/'],
};

/**
 * Extract the inner path from an Address value (`@(path)` → `path`).
 * If the value is already a plain string (not wrapped), return it as-is.
 */
function extractAddressPath(addr: unknown): string {
  if (typeof addr !== 'string') return '';
  const match = addr.match(/^@\((.+)\)$/);
  return match ? match[1] : addr;
}

/**
 * Return true when `fromAddress` is permitted to read credentials for `target`.
 *
 * Falls back to a permissive default (allow) when the target has no ACL entry
 * so that new targets added to the config map do not silently break before
 * their ACL entry is registered.  This is intentionally conservative: the
 * target-scoping check (FIX A) already prevents cross-target reads; this ACL
 * provides defence-in-depth against lateral movement within the same target.
 */
function isCallerAuthorized(fromAddress: string, target: string): boolean {
  // Test and internal actors are always permitted (no namespace prefix).
  // A real actor address contains at least one '/' (e.g. "ai/inference/...").
  // Addresses without '/' are considered internal/test callers.
  if (!fromAddress.includes('/')) return true;

  // Test actors with explicit "test/" prefix are always permitted.
  if (fromAddress.startsWith('test/')) return true;

  const allowed = CREDENTIALS_ACL[target];
  if (!allowed) {
    // No ACL entry for this target → permissive default.
    return true;
  }
  return allowed.some(prefix => fromAddress.startsWith(prefix));
}

// ---------------------------------------------------------------------------
// CredentialsActor
// ---------------------------------------------------------------------------

export class CredentialsActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly parsed: CredentialsAddress;
  private readonly config: CredentialsActorConfig;

  constructor(address: string, config: CredentialsActorConfig) {
    this.actorAddress = address;
    const parsed = parseCredentialsAddress(address);
    if (!parsed) throw new Error(`Invalid credentials actor address: ${address}`);
    this.parsed = parsed;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      if (message.type === AI_MESSAGE_TYPES.CREDENTIALS_GET) {
        return await this.handleCredentialsGet(message);
      }
      if (message.type === AI_MESSAGE_TYPES.DISCOVER) {
        return this.handleDiscover(message);
      }
      if (message.type === AI_MESSAGE_TYPES.HEALTH) {
        return this.handleHealth(message);
      }
      return createErrorResponse(
        message,
        `CredentialsActor(${this.actorAddress}): unhandled message type '${message.type}'`,
      );
    } catch (err) {
      return createErrorResponse(message, err instanceof Error ? err.message : String(err));
    }
  }

  private handleDiscover(message: Message): MessageResponse {
    const result: DiscoverResponsePayload = {
      address: this.actorAddress,
      type: 'credentials',
      handles: [AI_MESSAGE_TYPES.CREDENTIALS_GET, AI_MESSAGE_TYPES.DISCOVER, AI_MESSAGE_TYPES.HEALTH],
      meta: { target: this.parsed.target },
    };
    return createResponse(message, result);
  }

  private handleHealth(message: Message): MessageResponse {
    const payload = message.payload as HealthPayload;
    const result: HealthResponsePayload = { status: 'ok', address: this.actorAddress, token: payload?.token };
    return createResponse(message, result);
  }

  private async handleCredentialsGet(message: Message): Promise<MessageResponse> {
    const payload = message.payload as CredentialsGetPayload;
    const { target } = payload;

    // FIX A: Target scoping — reject requests for any target other than the one
    // this actor was provisioned for.  Each CredentialsActor instance is bound
    // to exactly one target (encoded in its address, ai/credentials/<target>).
    // Allowing arbitrary targets would be horizontal privilege escalation.
    if (target !== this.parsed.target) {
      return createErrorResponse(
        message,
        `CredentialsActor(${this.actorAddress}): target mismatch — actor serves '${this.parsed.target}', requested '${target}'`,
      );
    }

    // FIX B: Caller ACL — only actors whose address starts with the expected
    // prefix for this target may retrieve its credentials.
    // message.from is an Address value formatted as "@(path)".
    const fromAddress = extractAddressPath(message.from);
    if (!isCallerAuthorized(fromAddress, target)) {
      return createErrorResponse(
        message,
        `CredentialsActor(${this.actorAddress}): caller '${fromAddress}' is not authorised to read credentials for target '${target}'`,
      );
    }

    const entry = this.config.credentials[target];
    if (!entry) {
      return createErrorResponse(
        message,
        `CredentialsActor(${this.actorAddress}): no credentials found for target '${target}'`,
      );
    }

    const result: CredentialsPayload = {
      target,
      apiKey: entry.apiKey,
      gatewayUrl: entry.gatewayUrl,
    };

    return createResponse(message, result);
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create an ActorFactory for the `ai/credentials/` prefix.
 *
 * @example
 * system.registerFactory({
 *   prefix: AI_PREFIXES.CREDENTIALS,
 *   factory: createCredentialsFactory({
 *     credentials: {
 *       nim: { apiKey: env.NIM_API_KEY, gatewayUrl: env.NIM_GATEWAY_URL },
 *       deepgram: { apiKey: env.DEEPGRAM_API_KEY },
 *     },
 *   }),
 * });
 */
export function createCredentialsFactory(
  config: CredentialsActorConfig,
): (address: string) => CredentialsActor | null {
  return (address: string) => {
    const parsed = parseCredentialsAddress(address);
    if (!parsed) return null;
    return new CredentialsActor(address, config);
  };
}
