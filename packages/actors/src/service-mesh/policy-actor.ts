/**
 * Generic PolicyActor<TEndpoint, TConstraints>
 *
 * A domain-agnostic actor that filters and orders RouteDescriptor lists
 * according to BaseRoutingConstraints. Domain-specific logic is injected
 * via optional preFilter and scorer callbacks in the config.
 *
 * Handles:
 *   'policy.select' → PolicySelectResult  ({ ordered: string[] })
 */

import type { Message, MessageResponse, MessageHandler, Address } from '../message.ts';
import { createResponse, createErrorResponse } from '../message.ts';
import type {
  RouteDescriptor,
  BaseRoutingConstraints,
  PolicySelectRequest,
  PolicySelectResult,
} from './types.ts';

export interface PolicyActorConfig<
  TEndpoint,
  TConstraints extends BaseRoutingConstraints = BaseRoutingConstraints,
> {
  /** The actor's address in @(...) format. */
  address: Address;
  /**
   * Optional custom filter — applied before built-in BaseRoutingConstraints
   * filtering. Use this for domain-specific tag matching or other logic that
   * cannot be expressed in BaseRoutingConstraints alone.
   */
  preFilter?: (
    routes: RouteDescriptor<TEndpoint>[],
    constraints?: TConstraints
  ) => RouteDescriptor<TEndpoint>[];
  /**
   * Optional custom scorer — assigns a numeric score to each route after
   * filtering (higher score = better). Applied before built-in sort passes.
   */
  scorer?: (route: RouteDescriptor<TEndpoint>, constraints?: TConstraints) => number;
}

export class PolicyActor<
  TEndpoint,
  TConstraints extends BaseRoutingConstraints = BaseRoutingConstraints,
> implements MessageHandler {
  readonly address: Address;
  private readonly config: PolicyActorConfig<TEndpoint, TConstraints>;

  constructor(config: PolicyActorConfig<TEndpoint, TConstraints>) {
    this.address = config.address;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    switch (message.type) {
      case 'policy.select': {
        const req = message.payload as PolicySelectRequest<TEndpoint, TConstraints>;
        try {
          const ordered = this.select(req.routes, req.constraints);
          const result: PolicySelectResult = { ordered };
          return createResponse(message, result);
        } catch (err) {
          const msg = err instanceof Error ? err.message : String(err);
          return createErrorResponse(message, `PolicyActor select error: ${msg}`);
        }
      }

      default:
        return createErrorResponse(
          message,
          `PolicyActor: unhandled message type '${message.type}'`
        );
    }
  }

  // ---------------------------------------------------------------------------
  // Core selection logic — pure, synchronous, fully testable in isolation
  // ---------------------------------------------------------------------------

  private select(
    routes: RouteDescriptor<TEndpoint>[],
    constraints?: TConstraints
  ): string[] {
    let working = routes.slice(); // never mutate the input array

    // Step 1 — optional domain-specific pre-filter (runs before built-in filters)
    if (this.config.preFilter) {
      working = this.config.preFilter(working, constraints);
    }

    // Step 2 — built-in BaseRoutingConstraints filtering
    if (constraints) {
      working = this.applyBaseFilters(working, constraints);
    }

    // Step 3 — scoring + ordering
    working = this.applyOrdering(working, constraints);

    return working.map((r) => r.address);
  }

  // ---------------------------------------------------------------------------
  // Filtering
  // ---------------------------------------------------------------------------

  private applyBaseFilters(
    routes: RouteDescriptor<TEndpoint>[],
    constraints: TConstraints
  ): RouteDescriptor<TEndpoint>[] {
    return routes.filter((route) => {
      // maxCost: exclude routes whose cost exceeds the limit
      if (
        constraints.maxCost !== undefined &&
        route.cost !== undefined &&
        route.cost > constraints.maxCost
      ) {
        return false;
      }

      // capabilities: ALL listed capabilities must be present on the route.
      // If the route has no capabilities array but capabilities are required → exclude.
      if (constraints.capabilities && constraints.capabilities.length > 0) {
        const routeCaps = route.capabilities ?? [];
        for (const required of constraints.capabilities) {
          if (!routeCaps.includes(required)) {
            return false;
          }
        }
      }

      // tags / excludeTags: BaseRoutingConstraints carries these fields, but
      // RouteDescriptor has no built-in tags field. Tag-based filtering is
      // domain-specific and must be handled by a preFilter callback or a
      // domain-specific subclass. Skip here.

      return true;
    });
  }

  // ---------------------------------------------------------------------------
  // Ordering
  // ---------------------------------------------------------------------------

  private applyOrdering(
    routes: RouteDescriptor<TEndpoint>[],
    constraints?: TConstraints
  ): RouteDescriptor<TEndpoint>[] {
    if (routes.length === 0) return routes;

    let working = routes.slice();

    // Step A — custom scorer (higher score = better → sort descending)
    if (this.config.scorer) {
      const scorer = this.config.scorer;
      const scores = new Map<RouteDescriptor<TEndpoint>, number>(
        working.map((r) => [r, scorer(r, constraints)])
      );
      working = stableSort(working, (a, b) => scores.get(b)! - scores.get(a)!);
    }

    // Step B — preferDirect: isDirect=true routes go before isDirect=false (stable)
    if (constraints?.preferDirect) {
      working = stableSort(working, (a, b) => {
        if (a.isDirect === b.isDirect) return 0;
        return a.isDirect ? -1 : 1;
      });
    }

    // Step C — orderBy
    if (constraints?.orderBy === 'cost') {
      working = stableSort(working, (a, b) => {
        // undefined cost goes last
        if (a.cost === undefined && b.cost === undefined) return 0;
        if (a.cost === undefined) return 1;
        if (b.cost === undefined) return -1;
        return a.cost - b.cost;
      });
    }
    // orderBy === 'priority': preserve current order (no-op)

    return working;
  }
}

// ---------------------------------------------------------------------------
// Utility: stable sort that does not mutate the input array
// ---------------------------------------------------------------------------

function stableSort<T>(arr: T[], compareFn: (a: T, b: T) => number): T[] {
  // Attach original indices to make the sort stable across all JS engines
  return arr
    .map((item, idx) => ({ item, idx }))
    .sort((a, b) => {
      const cmp = compareFn(a.item, b.item);
      return cmp !== 0 ? cmp : a.idx - b.idx;
    })
    .map(({ item }) => item);
}
