/**
 * Service mesh actor triad — domain-agnostic catalog/policy/router pattern.
 *
 * The triad implements a generic service mesh:
 *   CatalogActor — resolves a key to RouteDescriptor<TEndpoint>[]
 *   PolicyActor  — filters and orders routes by BaseRoutingConstraints
 *   RouterActor  — orchestrates catalog → policy → leaf with failover
 *
 * All three are fully generic. Domain-specific behavior is injected via
 * constructor config (resolvers, filters, addresses, message types).
 */
export * from './types.ts';
export { CatalogActor, type RouteResolver, type CatalogActorConfig } from './catalog-actor.ts';
export { PolicyActor, type PolicyActorConfig } from './policy-actor.ts';
export { RouterActor, type RouterActorConfig } from './router-actor.ts';
