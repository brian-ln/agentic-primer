/**
 * Service mesh shared types — catalog/policy/router triad.
 *
 * This module defines the domain-agnostic contract between the three actors
 * that make up the service mesh pattern:
 *
 *   CatalogActor  — resolves a routing key to a list of RouteDescriptor<TEndpoint>
 *   PolicyActor   — filters and orders descriptors by BaseRoutingConstraints
 *   RouterActor   — orchestrates catalog → policy → leaf with failover
 *
 * All types are fully generic. Domain-specific fields belong in the consuming
 * package (e.g. TEndpoint = AIRouteEndpoint, TConstraints extends BaseRoutingConstraints).
 * No AI-specific or domain-specific fields are permitted here.
 */

/**
 * A resolved route descriptor — one concrete endpoint that can serve a request.
 * TEndpoint carries domain-specific fields.
 *
 * address MUST be in @(...) format — the actor system requires this.
 */
export interface RouteDescriptor<TEndpoint = unknown> {
  address: string;          // @(domain/leaf/...) format
  cost?: number;            // relative cost (lower = cheaper); undefined = unknown
  capabilities?: string[];  // capability tags for filtering
  isDirect: boolean;        // true = local/direct route, skips remote transport
  endpoint: TEndpoint;      // domain-specific fields
}

/**
 * Base constraints that RoutingPolicyActor understands out of the box.
 * Domain-specific constraints EXTEND this interface.
 */
export interface BaseRoutingConstraints {
  tags?: Record<string, string>;          // pin to routes matching these tag values
  excludeTags?: Record<string, string[]>; // exclude routes matching these tag values
  maxCost?: number;                        // filter out routes where cost > maxCost
  capabilities?: string[];                // ALL listed capabilities must be present
  preferDirect?: boolean;                  // sort isDirect routes first
  orderBy?: 'cost' | 'priority' | (string & {}); // extensible — derived types may add more strategies
}

/** Result returned by the catalog actor on resolve. */
export interface CatalogResolveResult<TEndpoint> {
  routes: RouteDescriptor<TEndpoint>[];
  resolved: string; // canonical key after alias expansion (may differ from input key)
}

/** Request sent to the catalog actor. */
export interface CatalogResolveRequest<TConstraints = BaseRoutingConstraints> {
  key: string;
  constraints?: TConstraints;
}

/** Request sent to the policy actor. */
export interface PolicySelectRequest<TEndpoint, TConstraints = BaseRoutingConstraints> {
  routes: RouteDescriptor<TEndpoint>[];
  constraints?: TConstraints;
}

/** Result returned by the policy actor — ordered actor addresses, best first. */
export interface PolicySelectResult {
  ordered: string[]; // @(...) addresses in priority order
}

/** Request format for the router actor. */
export interface RouterRequest<TConstraints = BaseRoutingConstraints> {
  key: string;             // model name, service name, etc. — routing key
  constraints?: TConstraints;
  [key: string]: unknown;  // domain-specific payload fields pass through
}
