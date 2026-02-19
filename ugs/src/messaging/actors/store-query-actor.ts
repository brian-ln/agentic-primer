#!/usr/bin/env bun
/**
 * Store Query Actor - Wraps GraphStore to handle query messages
 *
 * Provides actor interface for querying the graph store directly.
 * Used as fallback for unlabeled patterns in query system.
 */

import type GraphStore from '@src/graph.ts';
import type { Message, MessageResponse } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';

/**
 * StoreQueryActor - Handles query messages against GraphStore
 */
export class StoreQueryActor {
  constructor(
    private store: GraphStore,
    public ports: any  // Expose store's ports for subscriptions
  ) {
    // Share store's ports so subscriptions can access state changes
    this.ports = store.ports;
  }

  /**
   * Handle query messages
   */
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type !== 'query') {
      return createErrorResponse(
        message,
        `StoreQueryActor only handles 'query' messages, got: ${message.type}`
      );
    }

    try {
      const { filter = {}, limit = 1000 } = message.payload;
      const results: any[] = [];

      // Simple filtering: iterate nodes and match properties
      for (const [nodeId, node] of this.store.nodes) {
        let matches = true;

        // Check each filter property
        for (const [key, value] of Object.entries(filter)) {
          const nodeValue = node.properties.get(key) || (node as any)[key];
          if (nodeValue !== value) {
            matches = false;
            break;
          }
        }

        if (matches) {
          results.push(node);
          if (results.length >= limit) break;
        }
      }

      return createResponse(message, results);
    } catch (error: any) {
      return createErrorResponse(message, `Query error: ${error.message}`);
    }
  }
}
