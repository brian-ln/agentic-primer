#!/usr/bin/env bun
/**
 * RelationshipActor - Graph-addressable knowledge relationships
 *
 * Manages edges between knowledge nodes. Relationships are first-class
 * entities with their own properties (type, strength, evidence).
 *
 * Examples:
 *   @(relationships/rel-123)
 *   @(relationships/supports/abc-def)
 *
 * Relationship types:
 *   - supports: Evidence/reasoning supports conclusion
 *   - contradicts: Evidence contradicts claim
 *   - requires: Depends on other knowledge
 *   - extends: Builds upon other knowledge
 *   - questions: Raises doubt about claim
 */

import { Actor } from '../actor.ts';
import type { MessageRouter } from '../router.ts';
import type { Message, MessageResponse, Address } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { LibSQLKnowledgeStore } from '../../storage/LibSQLKnowledgeStore.ts';

export type RelationshipType =
  | 'supports'      // Evidence/reasoning supports conclusion
  | 'contradicts'   // Evidence contradicts claim
  | 'requires'      // Depends on other knowledge
  | 'extends'       // Builds upon other knowledge
  | 'questions'     // Raises doubt about claim
  | 'related-to';   // General relationship

export interface Relationship {
  id: string;
  type: RelationshipType;
  from: Address;          // Source node (e.g., @(knowledge/decisions/abc))
  to: Address;            // Target node (e.g., @(knowledge/learnings/def))
  strength?: number;      // 0.0-1.0, how strong is this relationship
  evidence?: string;      // Why this relationship exists
  created: number;
  metadata?: Record<string, any>;
}

/**
 * RelationshipActor - Manages knowledge graph edges
 */
export class RelationshipActor extends Actor {
  private storage: LibSQLKnowledgeStore;

  constructor(id: string, router: MessageRouter, dbPath?: string) {
    super(id, router);
    this.storage = new LibSQLKnowledgeStore(dbPath);
  }

  /**
   * Handle incoming messages
   */
  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      switch (type) {
        case 'create':
          return await this.handleCreate(message, payload);

        case 'upsert':
          return await this.handleUpsert(message, payload);

        case 'get':
          return await this.handleGet(message, payload);

        case 'query':
          return await this.handleQuery(message, payload);

        case 'traverse':
          return await this.handleTraverse(message, payload);

        case 'delete':
          return await this.handleDelete(message, payload);

        default:
          return createErrorResponse(message, `Unknown message type: ${type}`);
      }
    } catch (error: any) {
      return createErrorResponse(message, error.message);
    }
  }

  /**
   * Create new relationship
   */
  private async handleCreate(message: Message, payload: any): Promise<MessageResponse> {
    const { type, from, to, strength, evidence, metadata } = payload;

    // Validate required fields
    if (!type || !from || !to) {
      return createErrorResponse(message, 'Missing required fields: type, from, to');
    }

    // Validate relationship type
    const validTypes: RelationshipType[] = ['supports', 'contradicts', 'requires', 'extends', 'questions', 'related-to'];
    if (!validTypes.includes(type)) {
      return createErrorResponse(message, `Invalid relationship type: ${type}`);
    }

    // Validate strength if provided
    if (strength !== undefined && (strength < 0 || strength > 1)) {
      return createErrorResponse(message, 'Strength must be between 0 and 1');
    }

    // Generate ID
    const id = this.generateId();
    const timestamp = Date.now();

    // Create relationship
    const relationship: Relationship = {
      id,
      type: type as RelationshipType,
      from: from as Address,
      to: to as Address,
      strength,
      evidence,
      created: timestamp,
      metadata
    };

    await this.storage.createRelationship(relationship);

    // Return address
    const relationshipAddress = `@(relationships/${type}/${id})` as Address;

    return createResponse(message, {
      address: relationshipAddress,
      relationship
    });
  }

  /**
   * Upsert relationship (create if not exists, update if exists)
   */
  private async handleUpsert(message: Message, payload: any): Promise<MessageResponse> {
    const { type, from, to, strength, evidence, metadata, mergeStrategy = 'shallow' } = payload;

    // Validate required fields
    if (!type || !from || !to) {
      return createErrorResponse(message, 'Missing required fields: type, from, to');
    }

    // Validate relationship type
    const validTypes: RelationshipType[] = ['supports', 'contradicts', 'requires', 'extends', 'questions', 'related-to'];
    if (!validTypes.includes(type)) {
      return createErrorResponse(message, `Invalid relationship type: ${type}`);
    }

    // Validate strength if provided
    if (strength !== undefined && (strength < 0 || strength > 1)) {
      return createErrorResponse(message, 'Strength must be between 0 and 1');
    }

    // Check if relationship already exists
    const existingRels = await this.storage.queryRelationships({
      from: from as Address,
      to: to as Address,
      type,
    }, 1);

    const timestamp = Date.now();

    if (existingRels.length > 0) {
      // Update existing relationship
      const existing = existingRels[0];

      // Merge properties based on strategy
      let updatedProperties: any = {};

      switch (mergeStrategy) {
        case 'replace':
          // Replace all properties
          updatedProperties = {
            strength,
            evidence,
            ...metadata,
          };
          break;

        case 'deep':
          // Deep merge properties
          updatedProperties = {
            ...existing,
            ...(strength !== undefined ? { strength } : {}),
            ...(evidence !== undefined ? { evidence } : {}),
            metadata: {
              ...(existing.metadata || {}),
              ...(metadata || {}),
            },
          };
          break;

        case 'shallow':
        default:
          // Shallow merge (default)
          updatedProperties = {
            ...existing,
            ...(strength !== undefined ? { strength } : {}),
            ...(evidence !== undefined ? { evidence } : {}),
            ...(metadata ? { metadata: { ...existing.metadata, ...metadata } } : {}),
          };
          break;
      }

      // Update in storage
      const updated: Relationship = {
        ...existing,
        ...updatedProperties,
        created: existing.created, // Preserve original creation time
      };

      await this.storage.updateRelationship(updated);

      const relationshipAddress = `@(relationships/${type}/${existing.id})` as Address;

      return createResponse(message, {
        address: relationshipAddress,
        relationship: updated,
        operation: 'update',
      });
    } else {
      // Create new relationship
      const id = this.generateId();

      const relationship: Relationship = {
        id,
        type: type as RelationshipType,
        from: from as Address,
        to: to as Address,
        strength,
        evidence,
        created: timestamp,
        metadata
      };

      await this.storage.createRelationship(relationship);

      const relationshipAddress = `@(relationships/${type}/${id})` as Address;

      return createResponse(message, {
        address: relationshipAddress,
        relationship,
        operation: 'create',
      });
    }
  }

  /**
   * Get relationship by ID
   */
  private async handleGet(message: Message, payload: any): Promise<MessageResponse> {
    const { id } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    const relationship = await this.storage.getRelationship(id);

    if (!relationship) {
      return createErrorResponse(message, `Relationship not found: ${id}`);
    }

    return createResponse(message, { relationship });
  }

  /**
   * Query relationships
   */
  private async handleQuery(message: Message, payload: any): Promise<MessageResponse> {
    const { filter = {}, limit = 100 } = payload;

    const results = await this.storage.queryRelationships(filter, limit);

    return createResponse(message, {
      count: results.length,
      relationships: results
    });
  }

  /**
   * Traverse graph from a starting node
   */
  private async handleTraverse(message: Message, payload: any): Promise<MessageResponse> {
    const {
      start,
      direction = 'outbound',  // outbound | inbound | both
      relationshipType,         // optional filter
      depth = 1,
      maxResults = 100
    } = payload;

    if (!start) {
      return createErrorResponse(message, 'Missing required field: start');
    }

    const visited = new Set<string>();
    const results: Array<{
      node: Address;
      relationship: Relationship;
      depth: number;
    }> = [];

    // BFS traversal
    const queue: Array<{ node: Address; depth: number }> = [{ node: start, depth: 0 }];

    while (queue.length > 0 && results.length < maxResults) {
      const current = queue.shift()!;

      if (current.depth >= depth) continue;
      if (visited.has(current.node)) continue;

      visited.add(current.node);

      // Get relationships based on direction
      let relationships: Relationship[] = [];

      if (direction === 'outbound' || direction === 'both') {
        const outbound = await this.getFromNode(current.node);
        relationships = relationships.concat(outbound);
      }

      if (direction === 'inbound' || direction === 'both') {
        const inbound = await this.getToNode(current.node);
        relationships = relationships.concat(inbound);
      }

      // Filter by type if specified
      if (relationshipType) {
        relationships = relationships.filter(r => r.type === relationshipType);
      }

      // Add to results and queue
      for (const rel of relationships) {
        const nextNode = direction === 'inbound' ? rel.from : rel.to;

        if (!visited.has(nextNode)) {
          results.push({
            node: nextNode,
            relationship: rel,
            depth: current.depth + 1
          });

          queue.push({
            node: nextNode,
            depth: current.depth + 1
          });
        }
      }
    }

    return createResponse(message, {
      start,
      depth,
      count: results.length,
      paths: results
    });
  }

  /**
   * Delete relationship
   */
  private async handleDelete(message: Message, payload: any): Promise<MessageResponse> {
    const { id } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    const relationship = await this.storage.getRelationship(id);

    if (!relationship) {
      return createErrorResponse(message, `Relationship not found: ${id}`);
    }

    await this.storage.deleteRelationship(id);

    return createResponse(message, {
      deleted: true,
      relationship
    });
  }

  /**
   * Helper: Get relationships from a node
   */
  private async getFromNode(node: Address): Promise<Relationship[]> {
    return await this.storage.getRelationshipsFrom(node);
  }

  /**
   * Helper: Get relationships to a node
   */
  private async getToNode(node: Address): Promise<Relationship[]> {
    return await this.storage.getRelationshipsTo(node);
  }


  /**
   * Generate unique ID for relationships
   */
  private generateId(): string {
    return `rel_${Date.now()}_${Math.random().toString(36).slice(2, 8)}`;
  }

  /**
   * Get statistics
   */
  async getStats() {
    // Query all relationships from storage
    const allRels = await this.storage.queryRelationships({}, 10000);

    // Compute type statistics
    const typeStats: Record<string, number> = {};
    const types: RelationshipType[] = ['supports', 'contradicts', 'requires', 'extends', 'questions', 'related-to'];
    types.forEach(type => {
      typeStats[type] = allRels.filter(r => r.type === type).length;
    });

    // Count unique nodes
    const fromNodes = new Set(allRels.map(r => r.from));
    const toNodes = new Set(allRels.map(r => r.to));

    return {
      total: allRels.length,
      by_type: typeStats,
      nodes_with_relationships: {
        from: fromNodes.size,
        to: toNodes.size
      }
    };
  }
}

export default RelationshipActor;
