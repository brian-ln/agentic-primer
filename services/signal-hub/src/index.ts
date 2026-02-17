/**
 * Signal Hub - Cloudflare Workers Entry Point
 *
 * WebSocket-based message router on Cloudflare Durable Objects
 */

import type { Env } from './types';
import { SignalHub } from './durable-objects/SignalHub';

// Export Durable Object class for Cloudflare Workers
export { SignalHub };

/**
 * Worker fetch handler
 *
 * Routes WebSocket upgrade requests to SignalHub Durable Object
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

    // WebSocket upgrade
    if (url.pathname === '/ws' || url.pathname === '/') {
      // Get or create Durable Object instance
      // For MVP, use a single instance (hardcoded ID)
      // In production, you might shard by user ID or region
      const id = env.SIGNAL_HUB.idFromName('default');
      const stub = env.SIGNAL_HUB.get(id);

      // Forward request to Durable Object
      return stub.fetch(request);
    }

    // 404 for other paths
    return new Response('Not Found', { status: 404 });
  },
};
