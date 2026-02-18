/**
 * Connection Lifecycle Handlers
 *
 * Handles: hub:connect, hub:heartbeat, hub:disconnect
 */

import type { SharedMessage, Session, Env } from '../types';
import { HubError } from '../types';
import { createReply, toCanonicalAddress } from '../utils';
import { validateJWT } from '../auth/jwt';

const SIGNAL_HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');

/**
 * Handle hub:connect message
 */
export async function handleConnect(
  msg: SharedMessage,
  session: Session,
  env: Env
): Promise<SharedMessage> {
  const metadata = msg.metadata as Record<string, unknown>;

  // 1. Validate protocol version
  const clientVersion = metadata.protocolVersion as string;
  if (!clientVersion) {
    throw new HubError('version_mismatch', 'Missing protocolVersion in metadata');
  }

  const serverVersion = env.PROTOCOL_VERSION;
  const [clientMajor] = clientVersion.split('.');
  const [serverMajor] = serverVersion.split('.');

  if (clientMajor !== serverMajor) {
    throw new HubError(
      'version_mismatch',
      `Protocol version mismatch: client=${clientVersion}, server=${serverVersion}`
    );
  }

  // 2. Authenticate if required
  const authEnabled = env.AUTH_ENABLED === 'true';
  const authToken = metadata.authToken as string | undefined;

  if (authEnabled) {
    if (!authToken) {
      throw new HubError('unauthorized', 'Authentication required but no authToken provided');
    }

    if (!env.JWT_SECRET) {
      throw new HubError('internal_error', 'JWT_SECRET not configured');
    }

    // Validate JWT and extract identity
    const identity = await validateJWT(authToken, env.JWT_SECRET);

    // Update session with verified identity
    session.actorIdentity = toCanonicalAddress(identity.actorId);
    session.capabilities = identity.capabilities;
    session.authenticated = true;
  } else {
    // Development mode - use client's from address as identity
    session.actorIdentity = msg.from;
    session.capabilities = (metadata.capabilities as string[]) ?? [];
    session.authenticated = false;
  }

  // 3. Extract capabilities
  const capabilities = metadata.capabilities as string[];
  if (!Array.isArray(capabilities) || capabilities.length === 0) {
    throw new HubError('unauthorized', 'capabilities must be a non-empty array');
  }

  // Update session
  session.connectedAt = Date.now();
  session.lastHeartbeat = Date.now();

  // 4. Send hub:connected response
  const payload = {
    sessionId: session.sessionId,
    serverVersion,
    maxMessageSize: parseInt(env.MAX_MESSAGE_SIZE, 10),
    // HB_INTERVAL_SECONDS is canonical (seconds); fall back to HEARTBEAT_INTERVAL (ms).
    // Default: 300 000 ms (5 min). CF infrastructure handles TCP-level keepalive
    // natively — this interval is for application-level session state verification only.
    heartbeatInterval: env.HB_INTERVAL_SECONDS
      ? parseInt(env.HB_INTERVAL_SECONDS, 10) * 1000
      : parseInt(env.HEARTBEAT_INTERVAL || '300000', 10),
    capabilities: {
      maxActorsPerInstance: parseInt(env.ACTOR_REGISTRY_LIMIT, 10),
      supportsBackpressure: true,
      supportedContentTypes: ['application/json'],
    },
  };

  return createReply('hub:connected', payload, msg, SIGNAL_HUB_ADDRESS, {
    actorIdentity: session.actorIdentity,
  });
}

/**
 * Handle hub:heartbeat message
 */
export function handleHeartbeat(msg: SharedMessage, session: Session): SharedMessage {
  const payload = msg.payload as { timestamp: number };

  // Update session heartbeat
  session.lastHeartbeat = Date.now();

  // Send hub:heartbeat_ack
  const ackPayload = {
    timestamp: payload.timestamp,
    serverTime: Date.now(),
  };

  return createReply('hub:heartbeat_ack', ackPayload, msg, SIGNAL_HUB_ADDRESS);
}

/**
 * Handle hub:disconnect message
 */
export function handleDisconnect(msg: SharedMessage, _session: Session): SharedMessage {
  // No response needed - connection will close
  return createReply('hub:disconnect', { acknowledged: true }, msg, SIGNAL_HUB_ADDRESS);
}
