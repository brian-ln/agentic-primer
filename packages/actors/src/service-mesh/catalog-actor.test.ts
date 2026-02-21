/**
 * Unit tests for CatalogActor<TEndpoint, TConstraints>
 *
 * Tests the message protocol: catalog.resolve, catalog.list, and unknown types.
 * Uses actor.receive() directly as a harness (no ActorSystem needed for unit tests).
 * ActorSystem integration is exercised in the ask() smoke test at the end.
 */

import { describe, test, expect } from 'bun:test';
import { ActorSystem, createMessage, address } from '../index.ts';
import { CatalogActor } from './catalog-actor.ts';
import type { RouteDescriptor, BaseRoutingConstraints } from './types.ts';

// ---------------------------------------------------------------------------
// Shared test data
// ---------------------------------------------------------------------------

interface TestEndpoint {
  url: string;
  region: string;
}

const ACTOR_ADDR = address('service-mesh/catalog/test');

function makeRoute(id: string): RouteDescriptor<TestEndpoint> {
  return {
    address: `@(service-mesh/leaf/${id})`,
    cost: 10,
    capabilities: ['text'],
    isDirect: false,
    endpoint: { url: `https://${id}.example.com`, region: 'us-east' },
  };
}

/** Build a catalog.resolve message targeting the test actor. */
function resolveMsg(key: string, constraints?: BaseRoutingConstraints) {
  return createMessage(ACTOR_ADDR, 'catalog.resolve', { key, constraints }, { pattern: 'ask' });
}

/** Build a catalog.list message targeting the test actor. */
function listMsg() {
  return createMessage(ACTOR_ADDR, 'catalog.list', {}, { pattern: 'ask' });
}

/** Build an unknown-type message. */
function unknownMsg() {
  return createMessage(ACTOR_ADDR, 'catalog.unknown', {}, { pattern: 'ask' });
}

// ---------------------------------------------------------------------------
// 1. catalog.resolve — resolver called with key and constraints, returns routes
// ---------------------------------------------------------------------------

describe('catalog.resolve', () => {
  test('resolver called with key and constraints, returns routes', async () => {
    const route = makeRoute('alpha');
    const constraints: BaseRoutingConstraints = { maxCost: 50, preferDirect: false };

    let capturedKey: string | undefined;
    let capturedConstraints: BaseRoutingConstraints | undefined;

    const actor = new CatalogActor<TestEndpoint, BaseRoutingConstraints>({
      address: ACTOR_ADDR,
      resolver: (key, c) => {
        capturedKey = key;
        capturedConstraints = c;
        return [route];
      },
    });

    const response = await actor.receive(resolveMsg('alpha', constraints));

    expect(response.success).toBe(true);
    expect(capturedKey).toBe('alpha');
    expect(capturedConstraints).toEqual(constraints);
    expect(response.payload).toEqual({ routes: [route], resolved: 'alpha' });
  });

  // -------------------------------------------------------------------------
  // 2. catalog.resolve — unknown key returns empty routes (resolver returns [])
  // -------------------------------------------------------------------------

  test('unknown key returns empty routes array', async () => {
    const actor = new CatalogActor<TestEndpoint>({
      address: ACTOR_ADDR,
      resolver: (_key) => [],
    });

    const response = await actor.receive(resolveMsg('nonexistent'));

    expect(response.success).toBe(true);
    const payload = response.payload as { routes: RouteDescriptor<TestEndpoint>[]; resolved: string };
    expect(payload.routes).toEqual([]);
    expect(payload.resolved).toBe('nonexistent');
  });

  // -------------------------------------------------------------------------
  // 3. catalog.resolve — resolver error returns error response
  // -------------------------------------------------------------------------

  test('resolver throwing returns error response', async () => {
    const actor = new CatalogActor<TestEndpoint>({
      address: ACTOR_ADDR,
      resolver: (_key) => {
        throw new Error('database unavailable');
      },
    });

    const response = await actor.receive(resolveMsg('anything'));

    expect(response.success).toBe(false);
    expect(response.error).toContain('CatalogActor resolve error:');
    expect(response.error).toContain('database unavailable');
  });
});

// ---------------------------------------------------------------------------
// 4. catalog.list — returns keys from listKeys()
// ---------------------------------------------------------------------------

describe('catalog.list', () => {
  test('returns keys from listKeys()', async () => {
    const actor = new CatalogActor<TestEndpoint>({
      address: ACTOR_ADDR,
      resolver: () => [],
      listKeys: () => ['alpha', 'beta', 'gamma'],
    });

    const response = await actor.receive(listMsg());

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ keys: ['alpha', 'beta', 'gamma'] });
  });

  // -------------------------------------------------------------------------
  // 5. catalog.list — returns empty array when no listKeys provided
  // -------------------------------------------------------------------------

  test('returns empty array when listKeys not provided', async () => {
    const actor = new CatalogActor<TestEndpoint>({
      address: ACTOR_ADDR,
      resolver: () => [],
      // listKeys intentionally omitted
    });

    const response = await actor.receive(listMsg());

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ keys: [] });
  });

  test('listKeys error returns error response', async () => {
    const actor = new CatalogActor<TestEndpoint>({
      address: ACTOR_ADDR,
      resolver: () => [],
      listKeys: () => { throw new Error('index corrupt'); },
    });

    const response = await actor.receive(listMsg());

    expect(response.success).toBe(false);
    expect(response.error).toContain('CatalogActor list error:');
    expect(response.error).toContain('index corrupt');
  });
});

// ---------------------------------------------------------------------------
// 6. Unknown message type — returns error response
// ---------------------------------------------------------------------------

describe('unknown message type', () => {
  test('unhandled message type returns error response', async () => {
    const actor = new CatalogActor<TestEndpoint>({
      address: ACTOR_ADDR,
      resolver: () => [],
    });

    const response = await actor.receive(unknownMsg());

    expect(response.success).toBe(false);
    expect(response.error).toContain("unhandled message type 'catalog.unknown'");
  });
});

// ---------------------------------------------------------------------------
// 7. Async resolver — works with Promise-returning resolver
// ---------------------------------------------------------------------------

describe('async resolver', () => {
  test('Promise-returning resolver resolves correctly', async () => {
    const route = makeRoute('async-route');

    const actor = new CatalogActor<TestEndpoint>({
      address: ACTOR_ADDR,
      resolver: async (key) => {
        // Simulate async work (e.g. DB lookup)
        await Promise.resolve();
        return key === 'async-route' ? [route] : [];
      },
    });

    const response = await actor.receive(resolveMsg('async-route'));

    expect(response.success).toBe(true);
    const payload = response.payload as { routes: RouteDescriptor<TestEndpoint>[]; resolved: string };
    expect(payload.routes).toHaveLength(1);
    expect(payload.routes[0]).toEqual(route);
    expect(payload.resolved).toBe('async-route');
  });

  test('async listKeys resolves correctly', async () => {
    const actor = new CatalogActor<TestEndpoint>({
      address: ACTOR_ADDR,
      resolver: () => [],
      listKeys: async () => {
        await Promise.resolve();
        return ['async-key-1', 'async-key-2'];
      },
    });

    const response = await actor.receive(listMsg());

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ keys: ['async-key-1', 'async-key-2'] });
  });
});

// ---------------------------------------------------------------------------
// 8. Constraint passthrough — constraints passed to resolver unchanged
// ---------------------------------------------------------------------------

describe('constraint passthrough', () => {
  test('complex constraints passed to resolver unchanged', async () => {
    const constraints: BaseRoutingConstraints = {
      tags: { env: 'prod', tier: 'premium' },
      excludeTags: { region: ['eu-west', 'ap-southeast'] },
      maxCost: 100,
      capabilities: ['streaming', 'vision'],
      preferDirect: true,
      orderBy: 'cost',
    };

    let received: BaseRoutingConstraints | undefined;

    const actor = new CatalogActor<TestEndpoint, BaseRoutingConstraints>({
      address: ACTOR_ADDR,
      resolver: (_key, c) => {
        received = c;
        return [];
      },
    });

    await actor.receive(resolveMsg('any-key', constraints));

    // Constraints must arrive at the resolver exactly as sent — no mutation
    expect(received).toEqual(constraints);
    expect(received?.tags).toEqual({ env: 'prod', tier: 'premium' });
    expect(received?.excludeTags).toEqual({ region: ['eu-west', 'ap-southeast'] });
    expect(received?.capabilities).toEqual(['streaming', 'vision']);
    expect(received?.maxCost).toBe(100);
    expect(received?.preferDirect).toBe(true);
    expect(received?.orderBy).toBe('cost');
  });

  test('undefined constraints passed through as undefined', async () => {
    let received: BaseRoutingConstraints | undefined = 'sentinel' as any;

    const actor = new CatalogActor<TestEndpoint>({
      address: ACTOR_ADDR,
      resolver: (_key, c) => {
        received = c;
        return [];
      },
    });

    // No constraints supplied
    await actor.receive(resolveMsg('key-no-constraints'));

    expect(received).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// ActorSystem integration smoke test — system.ask() path
// ---------------------------------------------------------------------------

describe('ActorSystem integration', () => {
  test('ask() via ActorSystem returns correct result', async () => {
    const route = makeRoute('system-route');
    const system = new ActorSystem({ name: 'catalog-test-system' });

    const actor = new CatalogActor<TestEndpoint>({
      address: address('catalog/integration-test'),
      resolver: (key) => (key === 'system-route' ? [route] : []),
      listKeys: () => ['system-route'],
    });

    system.register(actor);

    // ask catalog.resolve
    const resolveResult = await system.ask<{ routes: RouteDescriptor<TestEndpoint>[]; resolved: string }>(
      address('catalog/integration-test'),
      'catalog.resolve',
      { key: 'system-route' }
    );

    expect(resolveResult.routes).toHaveLength(1);
    expect(resolveResult.routes[0]).toEqual(route);
    expect(resolveResult.resolved).toBe('system-route');

    // ask catalog.list
    const listResult = await system.ask<{ keys: string[] }>(
      address('catalog/integration-test'),
      'catalog.list',
      {}
    );

    expect(listResult.keys).toEqual(['system-route']);
  });
});
