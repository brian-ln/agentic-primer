/**
 * Generic RouterActor<TEndpoint, TConstraints>
 *
 * A domain-agnostic actor that orchestrates the service mesh triad:
 *   catalog → policy → leaf (with automatic failover)
 *
 * Handles:
 *   'router.route' → result from the winning leaf actor
 *
 * Algorithm:
 *   1. ask catalogAddress 'catalog.resolve' → CatalogResolveResult<TEndpoint>
 *      If empty routes → error response
 *   2. ask policyAddress 'policy.select' → PolicySelectResult
 *      If empty ordered list → error response
 *   3. Try leaf addresses in order; return on first success
 *      If all fail → error response
 */

import type { Message, MessageResponse, MessageHandler, Address } from '../message.ts';
import { createResponse, createErrorResponse } from '../message.ts';
import type { ActorSystem } from '../actor-system.ts';
import type {
  BaseRoutingConstraints,
  CatalogResolveRequest,
  CatalogResolveResult,
  PolicySelectRequest,
  PolicySelectResult,
  RouterRequest,
} from './types.ts';

export interface RouterActorConfig<
  TEndpoint,
  TConstraints extends BaseRoutingConstraints = BaseRoutingConstraints,
> {
  /** The actor's address */
  address: Address;
  /** Address of the catalog actor — must be registered in the same ActorSystem */
  catalogAddress: Address;
  /** Address of the policy actor — must be registered in the same ActorSystem */
  policyAddress: Address;
  /** The ActorSystem — needed to .ask() catalog + policy + leaf actors */
  system: ActorSystem;
  /** Timeout in ms for each downstream actor call. Default: 30_000 */
  timeout?: number;
  /**
   * Custom leaf invocation — sends the request payload to the winning leaf actor address.
   * If not provided, RouterActor calls system.ask(leafAddr, 'route.execute', payload, timeout).
   * Domain-specific subclasses override this to use their own message type.
   */
  invokeLeaf?: (
    leafAddr: Address,
    payload: RouterRequest<TConstraints>,
    system: ActorSystem,
    timeout: number
  ) => Promise<unknown>;
}

const DEFAULT_TIMEOUT_MS = 30_000;

export class RouterActor<
  TEndpoint = unknown,
  TConstraints extends BaseRoutingConstraints = BaseRoutingConstraints,
> implements MessageHandler {
  readonly address: Address;
  private readonly catalogAddress: Address;
  private readonly policyAddress: Address;
  private readonly system: ActorSystem;
  private readonly timeout: number;
  private readonly invokeLeaf: (
    leafAddr: Address,
    payload: RouterRequest<TConstraints>,
    system: ActorSystem,
    timeout: number
  ) => Promise<unknown>;

  constructor(config: RouterActorConfig<TEndpoint, TConstraints>) {
    this.address = config.address;
    this.catalogAddress = config.catalogAddress;
    this.policyAddress = config.policyAddress;
    this.system = config.system;
    this.timeout = config.timeout ?? DEFAULT_TIMEOUT_MS;
    this.invokeLeaf =
      config.invokeLeaf ??
      ((leafAddr, payload, system, timeout) =>
        system.ask(leafAddr, 'route.execute', payload, timeout));
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type !== 'router.route') {
      return createErrorResponse(
        message,
        `RouterActor: unhandled message type '${message.type}'`
      );
    }

    const payload = message.payload as RouterRequest<TConstraints>;
    const { key, constraints } = payload;

    try {
      // Step 1 — resolve routes from catalog
      const catalogReq: CatalogResolveRequest<TConstraints> = { key, constraints };
      let catalog: CatalogResolveResult<TEndpoint>;

      try {
        catalog = await this.system.ask<CatalogResolveResult<TEndpoint>>(
          this.catalogAddress,
          'catalog.resolve',
          catalogReq,
          this.timeout
        );
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        return createErrorResponse(message, `RouterActor catalog error: ${msg}`);
      }

      if (!catalog.routes || catalog.routes.length === 0) {
        return createErrorResponse(message, `No routes found for key: ${key}`);
      }

      // Step 2 — select and order routes via policy
      const policyReq: PolicySelectRequest<TEndpoint, TConstraints> = {
        routes: catalog.routes,
        constraints,
      };
      let policy: PolicySelectResult;

      try {
        policy = await this.system.ask<PolicySelectResult>(
          this.policyAddress,
          'policy.select',
          policyReq,
          this.timeout
        );
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        return createErrorResponse(message, `RouterActor policy error: ${msg}`);
      }

      if (!policy.ordered || policy.ordered.length === 0) {
        return createErrorResponse(
          message,
          `No routes satisfy constraints for key: ${key}`
        );
      }

      // Step 3 — try leaf actors in priority order (failover)
      for (const leafAddrStr of policy.ordered) {
        const leafAddr = leafAddrStr as Address;
        try {
          const result = await this.invokeLeaf(leafAddr, payload, this.system, this.timeout);
          return createResponse(message, result);
        } catch {
          // Leaf failed — continue to next address
        }
      }

      // All leaf addresses exhausted
      return createErrorResponse(message, `All routes failed for key: ${key}`);
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err);
      return createErrorResponse(message, `RouterActor fatal error: ${msg}`);
    }
  }
}
