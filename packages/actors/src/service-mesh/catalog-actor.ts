/**
 * Generic CatalogActor<TEndpoint, TConstraints>
 *
 * A domain-agnostic actor that resolves routing keys to RouteDescriptor lists.
 * All domain-specific logic is injected via the RouteResolver callback.
 *
 * Handles:
 *   'catalog.resolve' → CatalogResolveResult<TEndpoint>
 *   'catalog.list'    → { keys: string[] }
 */

import type { Message, MessageResponse, MessageHandler, Address } from '../message.ts';
import { createResponse, createErrorResponse } from '../message.ts';
import type {
  RouteDescriptor,
  BaseRoutingConstraints,
  CatalogResolveRequest,
  CatalogResolveResult,
} from './types.ts';

/**
 * Resolves a string key (name, alias, or ID) to RouteDescriptor<TEndpoint>[].
 * Domain-specific implementations supply this callback.
 * May be sync or async.
 */
export type RouteResolver<TEndpoint, TConstraints = BaseRoutingConstraints> = (
  key: string,
  constraints?: TConstraints
) => RouteDescriptor<TEndpoint>[] | Promise<RouteDescriptor<TEndpoint>[]>;

export interface CatalogActorConfig<TEndpoint, TConstraints = BaseRoutingConstraints> {
  /** The actor's address in @(...) format. */
  address: Address;
  /** Domain-specific route resolution logic. */
  resolver: RouteResolver<TEndpoint, TConstraints>;
  /** Optional: list all available keys (for catalog.list message). */
  listKeys?: () => string[] | Promise<string[]>;
}

export class CatalogActor<TEndpoint, TConstraints = BaseRoutingConstraints>
  implements MessageHandler {
  readonly address: Address;
  private readonly config: CatalogActorConfig<TEndpoint, TConstraints>;

  constructor(config: CatalogActorConfig<TEndpoint, TConstraints>) {
    this.address = config.address;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    switch (message.type) {
      case 'catalog.resolve': {
        const req = message.payload as CatalogResolveRequest<TConstraints>;
        try {
          const routes = await this.config.resolver(req.key, req.constraints);
          const result: CatalogResolveResult<TEndpoint> = {
            routes,
            resolved: req.key,
          };
          return createResponse(message, result);
        } catch (err) {
          const msg = err instanceof Error ? err.message : String(err);
          return createErrorResponse(message, `CatalogActor resolve error: ${msg}`);
        }
      }

      case 'catalog.list': {
        try {
          const keys = this.config.listKeys ? await this.config.listKeys() : [];
          return createResponse(message, { keys });
        } catch (err) {
          const msg = err instanceof Error ? err.message : String(err);
          return createErrorResponse(message, `CatalogActor list error: ${msg}`);
        }
      }

      default:
        return createErrorResponse(
          message,
          `CatalogActor: unhandled message type '${message.type}'`
        );
    }
  }
}
