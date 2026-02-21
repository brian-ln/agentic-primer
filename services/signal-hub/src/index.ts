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
