/**
 * Signal Hub - Cloudflare Workers Entry Point
 *
 * WebSocket-based message router on Cloudflare Durable Objects.
 * Hosts two Durable Objects:
 *
 *   SignalHub  (/ws, /)       — actor registration, discovery, pub/sub message routing
 *   AgentHub   (/agent-ws)    — AI capability actor mesh (SessionActor, FluxRelayActor)
 */

import type { Env } from './types';
import { SignalHub } from './durable-objects/SignalHub';
import { AgentHub } from './durable-objects/AgentHub';
import { validateJWT } from './auth/jwt';

// Export Durable Object classes for Cloudflare Workers
export { SignalHub, AgentHub };

/**
 * Worker fetch handler
 *
 * Routes WebSocket upgrade requests to the appropriate Durable Object:
 *   /ws or /      → SignalHub (actor registry + message routing)
 *   /agent-ws     → AgentHub  (AI session + flux relay actors)
 */
export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);

    // Health check endpoint
    if (url.pathname === '/health') {
      return new Response(
        JSON.stringify({
          status: 'ok',
          service: 'signal-hub',
          version: env.PROTOCOL_VERSION,
          timestamp: Date.now(),
        }),
        {
          headers: { 'Content-Type': 'application/json' },
        }
      );
    }

    // AgentHub — AI session and flux relay actor mesh
    if (url.pathname === '/agent-ws') {
      if (!env.AGENT_HUB) {
        return new Response('AgentHub binding not configured', { status: 503 });
      }

      // FIX C: Auth gate — validate bearer token before forwarding to AgentHub.
      // When AUTH_ENABLED=true, a valid JWT must be provided either via:
      //   Authorization: Bearer <token>   (preferred for HTTP clients)
      //   ?token=<token>                  (fallback for WS upgrade, where
      //                                   custom headers are not always supported)
      // If no valid token is present, return 401 before any actor is provisioned.
      if (env.AUTH_ENABLED === 'true') {
        if (!env.JWT_SECRET) {
          return new Response('JWT_SECRET not configured', { status: 503 });
        }

        // Extract token: prefer Authorization header, fall back to ?token= param.
        const authHeader = request.headers.get('Authorization') ?? '';
        const tokenFromHeader = authHeader.replace(/^[Bb]earer\s+/, '');
        const tokenFromParam = url.searchParams.get('token') ?? '';
        const rawToken = tokenFromHeader || tokenFromParam;

        if (!rawToken) {
          return new Response('Unauthorized: missing bearer token', { status: 401 });
        }

        try {
          await validateJWT(rawToken, env.JWT_SECRET);
        } catch {
          return new Response('Unauthorized: invalid or expired token', { status: 401 });
        }
      }

      // One AgentHub per namespace (namespace-keyed for sharding)
      const namespace = url.searchParams.get('ns') ?? 'default';
      const id = env.AGENT_HUB.idFromName(namespace);
      const stub = env.AGENT_HUB.get(id);
      return stub.fetch(request);
    }

    // SignalHub — actor registry and message routing
    if (url.pathname === '/ws' || url.pathname === '/') {
      // For MVP, use a single instance (hardcoded ID)
      // In production, you might shard by user ID or region
      const id = env.SIGNAL_HUB.idFromName('default');
      const stub = env.SIGNAL_HUB.get(id);
      return stub.fetch(request);
    }

    // 404 for other paths
    return new Response('Not Found', { status: 404 });
  },
};
