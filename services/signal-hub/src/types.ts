/**
 * Signal Hub Type Definitions
 *
 * Imports from @agentic-primer/protocols and defines internal types
 */

import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols/shared-message';

// Re-export protocol types
export type { SharedMessage, CanonicalAddress };

// ===========================
// Environment Bindings
// ===========================

export interface Env {
  // Durable Object bindings
  SIGNAL_HUB: DurableObjectNamespace;
  /** AgentHub DO — actor mesh for AI capability actors (SessionActor, FluxRelayActor). Optional — only bound when agent-hub route is deployed. */
  AGENT_HUB?: DurableObjectNamespace;

  // Environment variables
  PROTOCOL_VERSION: string;
  MAX_MESSAGE_SIZE: string;
  HEARTBEAT_INTERVAL: string;
  /**
   * Application-level heartbeat interval in seconds.
   * Defaults to 300 (5 min). Cloudflare handles TCP-level keepalive natively
   * via the hibernatable WebSocket API; this interval is for application-layer
   * session state verification only.
   */
  HB_INTERVAL_SECONDS?: string;
  ACTOR_REGISTRY_LIMIT: string;
  DEFAULT_ACTOR_TTL: string;
  MAX_ACTOR_TTL: string;
  BROADCAST_SYNC_THRESHOLD: string;
  JWT_SECRET?: string;
  AUTH_ENABLED: string;
  DEBUG?: string;
}

// ===========================
// Actor Registry
// ===========================

export interface ActorRegistration {
  actorAddress: CanonicalAddress;
  capabilities: string[];
  metadata: Record<string, unknown>;
  connectionId: string; // WebSocket connection ID
  registeredAt: number; // epoch ms
  expiresAt: number; // epoch ms
  version: number; // for conflict resolution
  renewalToken: string; // for hub:renew
}

// ===========================
// Session State
// ===========================

export type ConnectionState = 'connecting' | 'connected' | 'disconnecting' | 'disconnected';

export interface Session {
  sessionId: string;
  actorIdentity: CanonicalAddress | null; // Verified from JWT
  capabilities: string[];
  connectedAt: number;
  lastHeartbeat: number;
  authenticated: boolean;
  paused: boolean; // Backpressure state
  connectionState: ConnectionState; // Track connection lifecycle state
  disconnectedAt?: number; // Track when session was disconnected
  rateLimitBucket: TokenBucket; // Rate limiting per session (100 msg/min)
}

// ===========================
// Topic Subscriptions
// ===========================

export interface TopicSubscription {
  topic: string;
  actorAddress: CanonicalAddress;
  subscribedAt: number;
}

// ===========================
// Authentication
// ===========================

export interface ActorIdentity {
  actorId: string; // Canonical address path (e.g., "browser/client-ui")
  userId: string; // User ID from JWT sub claim
  capabilities: string[];
  expiresAt: number; // epoch ms
}

export interface JWTPayload {
  sub: string; // User ID
  actorId: string; // Actor canonical address path
  capabilities: string[];
  iss: string; // Issuer: "signal-hub"
  exp: number; // Expiration (Unix timestamp)
}

// ===========================
// Error Types
// ===========================

export type HubErrorCode =
  | 'version_mismatch'
  | 'unauthorized'
  | 'rate_limited'
  | 'unknown_actor'
  | 'message_too_large'
  | 'message_expired'
  | 'timeout'
  | 'internal_error'
  | 'invalid_token';

export class HubError extends Error {
  constructor(
    public code: HubErrorCode,
    message: string,
    public details?: Record<string, unknown>
  ) {
    super(message);
    this.name = 'HubError';
  }
}

// ===========================
// Rate Limiting
// ===========================

export interface TokenBucket {
  tokens: number;
  capacity: number;
  refillRate: number; // tokens per second
  lastRefill: number; // epoch ms
}

// ===========================
// Queue Stats
// ===========================

export interface QueueStats {
  pending: number;
  processed: number;
  failed: number;
  paused: boolean;
}

// ===========================
// Metrics
// ===========================

/**
 * Per-minute metrics counters.
 * Counters are cumulative since last reset (alarm tick).
 * Call resetMetrics() to start a new window.
 */
export interface HubMetrics {
  messages_received: number;
  messages_sent: number;
  errors: number;
  connections_opened: number;
  connections_closed: number;
  /** Epoch ms when this metrics window started */
  windowStart: number;
  /** Epoch ms when getMetrics() was called */
  timestamp: number;
}

// ===========================
// Hub Message Type Guards
// ===========================

export function isHubMessage(msg: unknown): msg is SharedMessage {
  return (
    typeof msg === 'object' &&
    msg !== null &&
    'type' in msg &&
    typeof msg.type === 'string' &&
    msg.type.startsWith('hub:')
  );
}

export function getMessageType(msg: SharedMessage): string {
  return msg.type;
}
