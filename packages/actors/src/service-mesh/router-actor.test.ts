/**
 * Unit tests for RouterActor<TEndpoint, TConstraints>
 *
 * Tests the message protocol and the catalog → policy → leaf orchestration,
 * including failover, empty results, and error propagation.
 *
 * Uses mock catalog and policy actors (MessageHandler inline implementations).
 * Does NOT use real CatalogActor or PolicyActor — RouterActor is tested in isolation.
 * ActorSystem integration is exercised in the smoke test at the end.
 */

import { describe, test, expect } from 'bun:test';
import {
  ActorSystem,
  createMessage,
  createResponse,
  createErrorResponse,
  address,
  type MessageHandler,
  type Message,
  type MessageResponse,
  type Address,
} from '../index.ts';
import { RouterActor } from './router-actor.ts';
import type {
  BaseRoutingConstraints,
  CatalogResolveResult,
  PolicySelectResult,
  RouteDescriptor,
} from './types.ts';

// ---------------------------------------------------------------------------
// Shared test data helpers
// ---------------------------------------------------------------------------

interface TestEndpoint {
  url: string;
}

const ROUTER_ADDR = address('service-mesh/router/test');
const CATALOG_ADDR = address('service-mesh/catalog/mock');
const POLICY_ADDR = address('service-mesh/policy/mock');
const LEAF_A_ADDR = '@(service-mesh/leaf/alpha)' as Address;
const LEAF_B_ADDR = '@(service-mesh/leaf/beta)' as Address;

function makeRoute(
  id: string,
  overrides: Partial<RouteDescriptor<TestEndpoint>> = {}
): RouteDescriptor<TestEndpoint> {
  return {
    address: `@(service-mesh/leaf/${id})`,
    cost: 10,
    capabilities: ['text'],
    isDirect: false,
    endpoint: { url: `https://${id}.example.com` },
    ...overrides,
  };
}

/** Build a router.route message targeting the test actor. */
function routeMsg(
  key: string,
  constraints?: BaseRoutingConstraints,
  extra: Record<string, unknown> = {}
) {
  return createMessage(
    ROUTER_ADDR,
    'router.route',
    { key, constraints, ...extra },
    { pattern: 'ask' }
  );
}

/** Build an unknown-type message. */
function unknownMsg() {
  return createMessage(ROUTER_ADDR, 'router.unknown', {}, { pattern: 'ask' });
}

// ---------------------------------------------------------------------------
// Mock handler factories
// ---------------------------------------------------------------------------

/** A catalog mock that always returns the given routes. */
function mockCatalog(
  routes: RouteDescriptor<TestEndpoint>[],
  resolvedKey?: string
): MessageHandler & { address: Address } {
  return {
    address: CATALOG_ADDR,
    receive: async (message: Message): Promise<MessageResponse> => {
      const result: CatalogResolveResult<TestEndpoint> = {
        routes,
        resolved: resolvedKey ?? (message.payload as { key: string }).key,
      };
      return createResponse(message, result);
    },
  };
}

/** A catalog mock that returns an error response. */
function mockCatalogError(errorMsg: string): MessageHandler & { address: Address } {
  return {
    address: CATALOG_ADDR,
    receive: async (message: Message): Promise<MessageResponse> => {
      return createErrorResponse(message, errorMsg);
    },
  };
}

/** A policy mock that returns the given ordered address list. */
function mockPolicy(ordered: string[]): MessageHandler & { address: Address } {
  return {
    address: POLICY_ADDR,
    receive: async (message: Message): Promise<MessageResponse> => {
      const result: PolicySelectResult = { ordered };
      return createResponse(message, result);
    },
  };
}

/** A policy mock that returns an error response. */
function mockPolicyError(errorMsg: string): MessageHandler & { address: Address } {
  return {
    address: POLICY_ADDR,
    receive: async (message: Message): Promise<MessageResponse> => {
      return createErrorResponse(message, errorMsg);
    },
  };
}

/** A leaf mock that succeeds and returns the given result. */
function mockLeafSuccess(
  leafAddr: Address,
  result: unknown
): MessageHandler & { address: Address } {
  return {
    address: leafAddr,
    receive: async (message: Message): Promise<MessageResponse> => {
      return createResponse(message, result);
    },
  };
}

/** A leaf mock that fails with the given error message. */
function mockLeafError(
  leafAddr: Address,
  errorMsg: string
): MessageHandler & { address: Address } {
  return {
    address: leafAddr,
    receive: async (message: Message): Promise<MessageResponse> => {
      return createErrorResponse(message, errorMsg);
    },
  };
}

/** Build an ActorSystem and register a set of actors, returning the system. */
function makeSystem(
  ...actors: Array<MessageHandler & { address: Address }>
): ActorSystem {
  const system = new ActorSystem({ name: 'router-test-system' });
  for (const actor of actors) {
    system.register(actor);
  }
  return system;
}

// ---------------------------------------------------------------------------
// Helper: build RouterActor backed by a real ActorSystem with mocks registered
// ---------------------------------------------------------------------------

function makeRouter(
  system: ActorSystem,
  opts: {
    timeout?: number;
    invokeLeaf?: (leafAddr: Address, payload: unknown, sys: ActorSystem, timeout: number) => Promise<unknown>;
  } = {}
): RouterActor<TestEndpoint, BaseRoutingConstraints> {
  return new RouterActor<TestEndpoint, BaseRoutingConstraints>({
    address: ROUTER_ADDR,
    catalogAddress: CATALOG_ADDR,
    policyAddress: POLICY_ADDR,
    system,
    timeout: opts.timeout ?? 5000,
    invokeLeaf: opts.invokeLeaf,
  });
}

// ---------------------------------------------------------------------------
// 1. Basic routing — catalog returns routes, policy orders them, leaf invoked
// ---------------------------------------------------------------------------

describe('basic routing', () => {
  test('catalog → policy → leaf: first leaf succeeds, result returned', async () => {
    const routes = [makeRoute('alpha'), makeRoute('beta')];
    const system = makeSystem(
      mockCatalog(routes),
      mockPolicy([LEAF_A_ADDR, LEAF_B_ADDR]),
      mockLeafSuccess(LEAF_A_ADDR, { answer: 42 })
    );
    const router = makeRouter(system);

    const response = await router.receive(routeMsg('my-service'));

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ answer: 42 });
  });
});

// ---------------------------------------------------------------------------
// 2. Catalog returns empty routes → error response
// ---------------------------------------------------------------------------

describe('catalog returns empty routes', () => {
  test('empty routes array → error "No routes found"', async () => {
    const system = makeSystem(
      mockCatalog([]),
      mockPolicy([LEAF_A_ADDR])
    );
    const router = makeRouter(system);

    const response = await router.receive(routeMsg('missing-service'));

    expect(response.success).toBe(false);
    expect(response.error).toContain('No routes found for key: missing-service');
  });
});

// ---------------------------------------------------------------------------
// 3. Policy orders to empty list → error response
// ---------------------------------------------------------------------------

describe('policy returns empty ordered list', () => {
  test('ordered empty → error "No routes satisfy constraints"', async () => {
    const routes = [makeRoute('alpha')];
    const system = makeSystem(
      mockCatalog(routes),
      mockPolicy([]) // policy filters everything out
    );
    const router = makeRouter(system);

    const response = await router.receive(routeMsg('constrained-service'));

    expect(response.success).toBe(false);
    expect(response.error).toContain('No routes satisfy constraints for key: constrained-service');
  });
});

// ---------------------------------------------------------------------------
// 4. First leaf fails, second leaf succeeds → failover
// ---------------------------------------------------------------------------

describe('failover', () => {
  test('first leaf fails, second succeeds → success from second leaf', async () => {
    const routes = [makeRoute('alpha'), makeRoute('beta')];
    const system = makeSystem(
      mockCatalog(routes),
      mockPolicy([LEAF_A_ADDR, LEAF_B_ADDR]),
      mockLeafError(LEAF_A_ADDR, 'alpha is down'),
      mockLeafSuccess(LEAF_B_ADDR, { result: 'from-beta' })
    );
    const router = makeRouter(system);

    const response = await router.receive(routeMsg('my-service'));

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ result: 'from-beta' });
  });
});

// ---------------------------------------------------------------------------
// 5. All leaves fail → error response
// ---------------------------------------------------------------------------

describe('all leaves fail', () => {
  test('all leaf actors fail → error "All routes failed"', async () => {
    const routes = [makeRoute('alpha'), makeRoute('beta')];
    const system = makeSystem(
      mockCatalog(routes),
      mockPolicy([LEAF_A_ADDR, LEAF_B_ADDR]),
      mockLeafError(LEAF_A_ADDR, 'alpha down'),
      mockLeafError(LEAF_B_ADDR, 'beta down')
    );
    const router = makeRouter(system);

    const response = await router.receive(routeMsg('broken-service'));

    expect(response.success).toBe(false);
    expect(response.error).toContain('All routes failed for key: broken-service');
  });
});

// ---------------------------------------------------------------------------
// 6. Custom invokeLeaf called with correct args
// ---------------------------------------------------------------------------

describe('custom invokeLeaf', () => {
  test('custom invokeLeaf receives leafAddr, payload, system, and timeout', async () => {
    const routes = [makeRoute('alpha')];
    let capturedLeafAddr: Address | undefined;
    let capturedPayload: unknown;
    let capturedSystem: ActorSystem | undefined;
    let capturedTimeout: number | undefined;

    const system = makeSystem(
      mockCatalog(routes),
      mockPolicy([LEAF_A_ADDR])
    );

    const router = makeRouter(system, {
      timeout: 7777,
      invokeLeaf: async (leafAddr, payload, sys, timeout) => {
        capturedLeafAddr = leafAddr;
        capturedPayload = payload;
        capturedSystem = sys;
        capturedTimeout = timeout;
        return { custom: true };
      },
    });

    const response = await router.receive(routeMsg('custom-service', undefined, { extra: 'data' }));

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ custom: true });
    expect(capturedLeafAddr).toBe(LEAF_A_ADDR);
    expect((capturedPayload as { key: string }).key).toBe('custom-service');
    expect((capturedPayload as { extra: string }).extra).toBe('data');
    expect(capturedSystem).toBe(system);
    expect(capturedTimeout).toBe(7777);
  });
});

// ---------------------------------------------------------------------------
// 7. Catalog error response → error propagated
// ---------------------------------------------------------------------------

describe('catalog error response', () => {
  test('catalog actor returns error → error response propagated', async () => {
    const system = makeSystem(
      mockCatalogError('catalog: database unreachable'),
      mockPolicy([LEAF_A_ADDR])
    );
    const router = makeRouter(system);

    const response = await router.receive(routeMsg('any-service'));

    expect(response.success).toBe(false);
    expect(response.error).toContain('catalog');
  });
});

// ---------------------------------------------------------------------------
// 8. Policy error response → error propagated
// ---------------------------------------------------------------------------

describe('policy error response', () => {
  test('policy actor returns error → error response propagated', async () => {
    const routes = [makeRoute('alpha')];
    const system = makeSystem(
      mockCatalog(routes),
      mockPolicyError('policy: rules engine crashed')
    );
    const router = makeRouter(system);

    const response = await router.receive(routeMsg('any-service'));

    expect(response.success).toBe(false);
    expect(response.error).toContain('policy');
  });
});

// ---------------------------------------------------------------------------
// 9. key and constraints forwarded to catalog correctly
// ---------------------------------------------------------------------------

describe('catalog receives correct key and constraints', () => {
  test('key and constraints are forwarded to catalog unchanged', async () => {
    const constraints: BaseRoutingConstraints = {
      maxCost: 50,
      capabilities: ['streaming'],
      preferDirect: true,
    };

    let capturedKey: string | undefined;
    let capturedConstraints: BaseRoutingConstraints | undefined;

    const catalog: MessageHandler & { address: Address } = {
      address: CATALOG_ADDR,
      receive: async (message: Message): Promise<MessageResponse> => {
        const req = message.payload as { key: string; constraints?: BaseRoutingConstraints };
        capturedKey = req.key;
        capturedConstraints = req.constraints;
        const result: CatalogResolveResult<TestEndpoint> = {
          routes: [makeRoute('alpha')],
          resolved: req.key,
        };
        return createResponse(message, result);
      },
    };

    const system = makeSystem(
      catalog,
      mockPolicy([LEAF_A_ADDR]),
      mockLeafSuccess(LEAF_A_ADDR, {})
    );
    const router = makeRouter(system);

    await router.receive(routeMsg('target-service', constraints));

    expect(capturedKey).toBe('target-service');
    expect(capturedConstraints).toEqual(constraints);
  });
});

// ---------------------------------------------------------------------------
// 10. Unknown message type → error response
// ---------------------------------------------------------------------------

describe('unknown message type', () => {
  test('unhandled message type returns error response', async () => {
    const system = makeSystem(
      mockCatalog([]),
      mockPolicy([])
    );
    const router = makeRouter(system);

    const response = await router.receive(unknownMsg());

    expect(response.success).toBe(false);
    expect(response.error).toContain("unhandled message type 'router.unknown'");
  });
});

// ---------------------------------------------------------------------------
// 11. ActorSystem integration — register router + mocks, send router.route
// ---------------------------------------------------------------------------

describe('ActorSystem integration', () => {
  test('system.ask() router.route with registered mocks returns correct result', async () => {
    const routes = [makeRoute('alpha'), makeRoute('beta')];

    const system = makeSystem(
      mockCatalog(routes, 'resolved-service'),
      mockPolicy([LEAF_B_ADDR, LEAF_A_ADDR]), // beta preferred
      mockLeafSuccess(LEAF_B_ADDR, { model: 'beta', tokens: 100 }),
      mockLeafSuccess(LEAF_A_ADDR, { model: 'alpha', tokens: 80 })
    );

    const router = new RouterActor<TestEndpoint, BaseRoutingConstraints>({
      address: ROUTER_ADDR,
      catalogAddress: CATALOG_ADDR,
      policyAddress: POLICY_ADDR,
      system,
      timeout: 5000,
    });
    system.register(router);

    const result = await system.ask<{ model: string; tokens: number }>(
      ROUTER_ADDR,
      'router.route',
      { key: 'my-model', constraints: { preferDirect: false } }
    );

    // Beta is first in ordered list, so beta should win
    expect(result.model).toBe('beta');
    expect(result.tokens).toBe(100);
  });

  test('system.ask() with failover: first leaf unregistered, second returns result', async () => {
    const routes = [makeRoute('alpha'), makeRoute('beta')];

    // LEAF_A_ADDR is NOT registered → system.ask() throws → failover to beta
    const system = makeSystem(
      mockCatalog(routes),
      mockPolicy([LEAF_A_ADDR, LEAF_B_ADDR]),
      // leaf alpha is intentionally not registered
      mockLeafSuccess(LEAF_B_ADDR, { from: 'beta-fallback' })
    );

    const router = new RouterActor<TestEndpoint, BaseRoutingConstraints>({
      address: ROUTER_ADDR,
      catalogAddress: CATALOG_ADDR,
      policyAddress: POLICY_ADDR,
      system,
      timeout: 5000,
    });
    system.register(router);

    const result = await system.ask<{ from: string }>(
      ROUTER_ADDR,
      'router.route',
      { key: 'failover-test' }
    );

    expect(result.from).toBe('beta-fallback');
  });
});
