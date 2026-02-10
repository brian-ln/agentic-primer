/**
 * Test worker that extends DOActorSystem for provisioner integration testing.
 *
 * This file gets bundled by esbuild and loaded into miniflare.
 * It spawns a simple counter actor to verify:
 * - DOActorSystem lifecycle
 * - Actor message routing
 * - DOPersistence table creation
 * - HTTP actor-message endpoint
 */
import { DurableObject } from 'cloudflare:workers';
import { DOActorSystem } from '../do-actor-system.ts';
import { ActorSystem, address } from '@agentic-primer/actors';
import type { DOActorSystemConfig } from '../do-actor-system.ts';

interface Env {
  ACTOR_DO: DurableObjectNamespace;
}

/**
 * A DOActorSystem that spawns a counter actor and exposes it via HTTP.
 * The counter actor responds to 'increment', 'decrement', and 'get' messages.
 */
export class ActorDO extends DOActorSystem<Env> {
  protected getConfig(): DOActorSystemConfig {
    return { name: 'actor-do-test' };
  }

  protected configure(system: ActorSystem): void {
    // Spawn a counter actor using functional behavior
    system.spawn(
      (state: number, msg) => {
        switch (msg.type) {
          case 'increment': return state + 1;
          case 'decrement': return state - 1;
          case 'get': return state;
          case 'add': return state + (msg.payload as number);
          default: return state;
        }
      },
      0,
      'counter'
    );
  }

  /**
   * Extended fetch that adds a /stats endpoint for testing introspection.
   */
  async fetch(request: Request): Promise<Response> {
    const url = new URL(request.url);

    if (url.pathname === '/stats') {
      const stats = this.actorSystem.getStats();
      return new Response(JSON.stringify(stats), {
        headers: { 'Content-Type': 'application/json' },
      });
    }

    if (url.pathname === '/ask' && request.method === 'POST') {
      try {
        const data = await request.json() as { to: string; type: string; payload?: unknown };
        const result = await this.actorSystem.ask(
          address(data.to),
          data.type,
          data.payload
        );
        return new Response(JSON.stringify({ result }), {
          headers: { 'Content-Type': 'application/json' },
        });
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        return new Response(JSON.stringify({ error: message }), {
          status: 500,
          headers: { 'Content-Type': 'application/json' },
        });
      }
    }

    // Delegate to parent (handles /actor-message, WebSocket upgrades)
    return super.fetch(request);
  }
}

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const id = env.ACTOR_DO.idFromName('singleton');
    const stub = env.ACTOR_DO.get(id);
    return stub.fetch(request);
  },
};
