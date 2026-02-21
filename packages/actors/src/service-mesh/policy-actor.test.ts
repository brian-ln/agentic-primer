/**
 * Unit tests for PolicyActor<TEndpoint, TConstraints>
 *
 * Tests the message protocol and all built-in filtering / ordering logic.
 * Uses actor.receive() directly as a harness for most tests.
 * ActorSystem integration is exercised in the smoke test at the end.
 */

import { describe, test, expect } from 'bun:test';
import { ActorSystem, createMessage, address } from '../index.ts';
import { PolicyActor } from './policy-actor.ts';
import type { RouteDescriptor, BaseRoutingConstraints, PolicySelectResult } from './types.ts';

// ---------------------------------------------------------------------------
// Shared test data helpers
// ---------------------------------------------------------------------------

interface TestEndpoint {
  url: string;
}

const ACTOR_ADDR = address('service-mesh/policy/test');

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

/** Build a policy.select message targeting the test actor. */
function selectMsg(
  routes: RouteDescriptor<TestEndpoint>[],
  constraints?: BaseRoutingConstraints
) {
  return createMessage(
    ACTOR_ADDR,
    'policy.select',
    { routes, constraints },
    { pattern: 'ask' }
  );
}

/** Build an unknown-type message. */
function unknownMsg() {
  return createMessage(ACTOR_ADDR, 'policy.unknown', {}, { pattern: 'ask' });
}

/** Extract the ordered addresses from a successful response payload. */
function orderedFrom(payload: unknown): string[] {
  return (payload as PolicySelectResult).ordered;
}

// ---------------------------------------------------------------------------
// 1. policy.select with no constraints — returns all routes unchanged
// ---------------------------------------------------------------------------

describe('policy.select — no constraints', () => {
  test('returns all routes in original order when no constraints provided', async () => {
    const routes = [makeRoute('alpha'), makeRoute('beta'), makeRoute('gamma')];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(selectMsg(routes));

    expect(response.success).toBe(true);
    expect(orderedFrom(response.payload)).toEqual([
      '@(service-mesh/leaf/alpha)',
      '@(service-mesh/leaf/beta)',
      '@(service-mesh/leaf/gamma)',
    ]);
  });
});

// ---------------------------------------------------------------------------
// 2. maxCost filtering — routes over limit excluded
// ---------------------------------------------------------------------------

describe('maxCost filtering', () => {
  test('excludes routes where cost > maxCost', async () => {
    const routes = [
      makeRoute('cheap', { cost: 5 }),
      makeRoute('mid', { cost: 15 }),
      makeRoute('pricey', { cost: 25 }),
    ];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(selectMsg(routes, { maxCost: 15 }));

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);
    expect(ordered).toContain('@(service-mesh/leaf/cheap)');
    expect(ordered).toContain('@(service-mesh/leaf/mid)');
    expect(ordered).not.toContain('@(service-mesh/leaf/pricey)');
  });

  test('keeps routes with undefined cost (unknown cost is not excluded)', async () => {
    const routes = [
      makeRoute('known', { cost: 10 }),
      makeRoute('unknown-cost', { cost: undefined }),
    ];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(selectMsg(routes, { maxCost: 5 }));

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);
    // cost=10 exceeds maxCost=5 → excluded; undefined cost → kept
    expect(ordered).not.toContain('@(service-mesh/leaf/known)');
    expect(ordered).toContain('@(service-mesh/leaf/unknown-cost)');
  });
});

// ---------------------------------------------------------------------------
// 3. capabilities filtering — route without required capability excluded
// ---------------------------------------------------------------------------

describe('capabilities filtering', () => {
  test('excludes route missing a required capability', async () => {
    const routes = [
      makeRoute('has-both', { capabilities: ['text', 'vision'] }),
      makeRoute('text-only', { capabilities: ['text'] }),
      makeRoute('no-caps', { capabilities: [] }),
    ];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(
      selectMsg(routes, { capabilities: ['text', 'vision'] })
    );

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);
    expect(ordered).toContain('@(service-mesh/leaf/has-both)');
    expect(ordered).not.toContain('@(service-mesh/leaf/text-only)');
    expect(ordered).not.toContain('@(service-mesh/leaf/no-caps)');
  });

  test('excludes route with no capabilities array when capabilities required', async () => {
    const routes = [
      makeRoute('has-cap', { capabilities: ['streaming'] }),
      makeRoute('no-caps-field', { capabilities: undefined }),
    ];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(
      selectMsg(routes, { capabilities: ['streaming'] })
    );

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);
    expect(ordered).toContain('@(service-mesh/leaf/has-cap)');
    expect(ordered).not.toContain('@(service-mesh/leaf/no-caps-field)');
  });

  test('does not filter when capabilities constraint is empty array', async () => {
    const routes = [makeRoute('a'), makeRoute('b')];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(selectMsg(routes, { capabilities: [] }));

    expect(response.success).toBe(true);
    expect(orderedFrom(response.payload)).toHaveLength(2);
  });
});

// ---------------------------------------------------------------------------
// 4. preferDirect ordering — isDirect routes sorted first
// ---------------------------------------------------------------------------

describe('preferDirect ordering', () => {
  test('isDirect=true routes sorted before isDirect=false when preferDirect is set', async () => {
    const routes = [
      makeRoute('indirect-1', { isDirect: false }),
      makeRoute('direct-1', { isDirect: true }),
      makeRoute('indirect-2', { isDirect: false }),
      makeRoute('direct-2', { isDirect: true }),
    ];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(selectMsg(routes, { preferDirect: true }));

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);
    const directIndices = [
      ordered.indexOf('@(service-mesh/leaf/direct-1)'),
      ordered.indexOf('@(service-mesh/leaf/direct-2)'),
    ];
    const indirectIndices = [
      ordered.indexOf('@(service-mesh/leaf/indirect-1)'),
      ordered.indexOf('@(service-mesh/leaf/indirect-2)'),
    ];
    expect(Math.max(...directIndices)).toBeLessThan(Math.min(...indirectIndices));
  });

  test('preferDirect=false leaves order unchanged', async () => {
    const routes = [
      makeRoute('indirect', { isDirect: false }),
      makeRoute('direct', { isDirect: true }),
    ];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(selectMsg(routes, { preferDirect: false }));

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);
    expect(ordered[0]).toBe('@(service-mesh/leaf/indirect)');
    expect(ordered[1]).toBe('@(service-mesh/leaf/direct)');
  });
});

// ---------------------------------------------------------------------------
// 5. orderBy: 'cost' — sorted ascending, undefined cost goes last
// ---------------------------------------------------------------------------

describe("orderBy: 'cost'", () => {
  test('sorts routes ascending by cost; undefined cost goes last', async () => {
    const routes = [
      makeRoute('mid', { cost: 20 }),
      makeRoute('cheap', { cost: 5 }),
      makeRoute('no-cost', { cost: undefined }),
      makeRoute('pricey', { cost: 50 }),
    ];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(selectMsg(routes, { orderBy: 'cost' }));

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);
    expect(ordered).toEqual([
      '@(service-mesh/leaf/cheap)',
      '@(service-mesh/leaf/mid)',
      '@(service-mesh/leaf/pricey)',
      '@(service-mesh/leaf/no-cost)',
    ]);
  });
});

// ---------------------------------------------------------------------------
// 6. Custom scorer — high score route sorts first
// ---------------------------------------------------------------------------

describe('custom scorer', () => {
  test('route with highest scorer score is returned first', async () => {
    const routes = [
      makeRoute('low', { cost: 10 }),
      makeRoute('high', { cost: 5 }),
      makeRoute('mid', { cost: 8 }),
    ];

    // Score inversely proportional to address length (just to test injection)
    const scorer = (route: RouteDescriptor<TestEndpoint>) =>
      route.address === '@(service-mesh/leaf/high)' ? 100 : 1;

    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR, scorer });

    const response = await actor.receive(selectMsg(routes));

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);
    expect(ordered[0]).toBe('@(service-mesh/leaf/high)');
  });

  test('scorer is passed constraints', async () => {
    const routes = [makeRoute('a', { cost: 10 }), makeRoute('b', { cost: 5 })];
    let capturedConstraints: BaseRoutingConstraints | undefined;

    const scorer = (
      _route: RouteDescriptor<TestEndpoint>,
      constraints?: BaseRoutingConstraints
    ) => {
      capturedConstraints = constraints;
      return 0;
    };

    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR, scorer });
    const constraints: BaseRoutingConstraints = { maxCost: 50 };

    await actor.receive(selectMsg(routes, constraints));

    expect(capturedConstraints).toEqual(constraints);
  });
});

// ---------------------------------------------------------------------------
// 7. Custom preFilter — custom filter applied, routes filtered out
// ---------------------------------------------------------------------------

describe('custom preFilter', () => {
  test('preFilter removes routes before built-in filters run', async () => {
    const routes = [
      makeRoute('keep', { cost: 5 }),
      makeRoute('remove', { cost: 3 }),
    ];

    const preFilter = (rs: RouteDescriptor<TestEndpoint>[]) =>
      rs.filter((r) => r.address !== '@(service-mesh/leaf/remove)');

    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR, preFilter });

    const response = await actor.receive(selectMsg(routes));

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);
    expect(ordered).toEqual(['@(service-mesh/leaf/keep)']);
  });

  test('preFilter receives constraints', async () => {
    const routes = [makeRoute('a'), makeRoute('b')];
    let capturedConstraints: BaseRoutingConstraints | undefined;

    const preFilter = (
      rs: RouteDescriptor<TestEndpoint>[],
      constraints?: BaseRoutingConstraints
    ) => {
      capturedConstraints = constraints;
      return rs;
    };

    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR, preFilter });
    const constraints: BaseRoutingConstraints = { preferDirect: true };

    await actor.receive(selectMsg(routes, constraints));

    expect(capturedConstraints).toEqual(constraints);
  });
});

// ---------------------------------------------------------------------------
// 8. Empty routes input → empty ordered array
// ---------------------------------------------------------------------------

describe('empty routes input', () => {
  test('returns empty ordered array, not an error', async () => {
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(selectMsg([], { maxCost: 100 }));

    expect(response.success).toBe(true);
    expect(orderedFrom(response.payload)).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// 9. Unknown message type → error response
// ---------------------------------------------------------------------------

describe('unknown message type', () => {
  test('unhandled message type returns error response', async () => {
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(unknownMsg());

    expect(response.success).toBe(false);
    expect(response.error).toContain("unhandled message type 'policy.unknown'");
  });
});

// ---------------------------------------------------------------------------
// 10. All routes filtered out → empty ordered array (not an error)
// ---------------------------------------------------------------------------

describe('all routes filtered out', () => {
  test('returns empty ordered array when all routes fail maxCost', async () => {
    const routes = [
      makeRoute('pricey-1', { cost: 100 }),
      makeRoute('pricey-2', { cost: 200 }),
    ];
    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR });

    const response = await actor.receive(selectMsg(routes, { maxCost: 10 }));

    expect(response.success).toBe(true);
    expect(orderedFrom(response.payload)).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// 11. preFilter + maxCost — both applied (preFilter first)
// ---------------------------------------------------------------------------

describe('preFilter + maxCost combination', () => {
  test('preFilter runs before maxCost; both exclusions respected', async () => {
    const routes = [
      makeRoute('keep-cheap', { cost: 5 }),
      makeRoute('remove-by-prefilter', { cost: 3 }),
      makeRoute('remove-by-cost', { cost: 50 }),
    ];

    let preFilterSawAddresses: string[] = [];

    const preFilter = (rs: RouteDescriptor<TestEndpoint>[]) => {
      preFilterSawAddresses = rs.map((r) => r.address);
      return rs.filter((r) => r.address !== '@(service-mesh/leaf/remove-by-prefilter)');
    };

    const actor = new PolicyActor<TestEndpoint>({ address: ACTOR_ADDR, preFilter });

    const response = await actor.receive(selectMsg(routes, { maxCost: 10 }));

    expect(response.success).toBe(true);
    const ordered = orderedFrom(response.payload);

    // Only keep-cheap survives both filters
    expect(ordered).toEqual(['@(service-mesh/leaf/keep-cheap)']);

    // preFilter saw all three routes (before maxCost was applied)
    expect(preFilterSawAddresses).toContain('@(service-mesh/leaf/remove-by-prefilter)');
    expect(preFilterSawAddresses).toContain('@(service-mesh/leaf/remove-by-cost)');
  });
});

// ---------------------------------------------------------------------------
// 12. ActorSystem integration smoke test
// ---------------------------------------------------------------------------

describe('ActorSystem integration', () => {
  test('system.register() + system.ask() returns correct PolicySelectResult', async () => {
    const routes = [
      makeRoute('direct-cheap', { isDirect: true, cost: 5 }),
      makeRoute('indirect-mid', { isDirect: false, cost: 15 }),
      makeRoute('direct-expensive', { isDirect: true, cost: 40 }),
    ];

    const system = new ActorSystem({ name: 'policy-test-system' });

    const actor = new PolicyActor<TestEndpoint>({
      address: address('policy/integration-test'),
    });

    system.register(actor);

    // preferDirect: sort direct first, then orderBy cost: ascending cost sort
    // across the whole list.
    //
    // Ordering pipeline:
    //   preferDirect pass → [direct-cheap(5), direct-expensive(40), indirect-mid(15)]
    //   orderBy cost pass → [direct-cheap(5), indirect-mid(15), direct-expensive(40)]
    //
    // The cost sort is a final stable pass over all routes, so a cheap indirect
    // route ends up before a pricey direct one.
    const result = await system.ask<PolicySelectResult>(
      address('policy/integration-test'),
      'policy.select',
      {
        routes,
        constraints: { preferDirect: true, orderBy: 'cost' },
      }
    );

    expect(result.ordered).toBeDefined();
    expect(Array.isArray(result.ordered)).toBe(true);
    expect(result.ordered).toHaveLength(3);

    // direct-cheap (cost=5) must be first — lowest cost overall
    expect(result.ordered[0]).toBe('@(service-mesh/leaf/direct-cheap)');
    // indirect-mid (cost=15) is cheaper than direct-expensive (cost=40)
    expect(result.ordered[1]).toBe('@(service-mesh/leaf/indirect-mid)');
    expect(result.ordered[2]).toBe('@(service-mesh/leaf/direct-expensive)');
  });
});
