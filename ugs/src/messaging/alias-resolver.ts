#!/usr/bin/env bun
/**
 * Alias Resolver - Graph-Based Alias Resolution for Path-Based Addressing
 *
 * Stores and resolves path aliases as graph relationships, enabling:
 * - Multiple paths pointing to same actor
 * - Dynamic alias creation/deletion
 * - Context injection via alias metadata
 * - Priority-based resolution
 * - Queryable alias relationships
 *
 * **Alias Storage:**
 * Aliases are stored as graph nodes with `resolves_to` relationships:
 *
 * ```
 * Alias Node:
 * {
 *   id: 'alias-services-llm',
 *   type: 'alias',
 *   properties: {
 *     alias_path: 'services/llm',
 *     canonical_path: 'domain/inference',
 *     priority: 1,
 *     context: { role: 'llm-service' }
 *   }
 * }
 *
 * Relationship:
 * {
 *   from: 'alias-services-llm',
 *   to: 'actor-inference',
 *   type: 'resolves_to'
 * }
 * ```
 *
 * Phase: 7 (Advanced Features)
 * Design: docs/PATH_ADDRESSING_DESIGN.md, PHASE_7_DESIGN.md
 */

import type GraphStore from '../graph.ts';
import { validatePath } from '@agentic-primer/actors';

/**
 * Resolved path with context.
 */
export interface ResolvedPath {
  /** Canonical path (resolved from alias) */
  path: string;
  /** Context metadata from alias */
  context: Record<string, any>;
  /** Resolution priority (higher = preferred) */
  priority: number;
  /** Whether this was an alias resolution (false = direct path) */
  wasAlias: boolean;
}

/**
 * Alias metadata.
 */
export interface Alias {
  /** Alias ID */
  id: string;
  /** Alias path (what to resolve from) */
  aliasPath: string;
  /** Canonical path (what to resolve to) */
  canonicalPath: string;
  /** Resolution priority (higher wins on conflicts) */
  priority: number;
  /** Context injection metadata */
  context: Record<string, any>;
  /** Description */
  description?: string;
  /** Creation timestamp */
  created: number;
}

/**
 * Alias resolution options.
 */
export interface ResolveOptions {
  /** Maximum resolution depth (prevent cycles) */
  maxDepth?: number;
  /** Track resolution path (for debugging) */
  trackPath?: boolean;
}

/**
 * Alias resolution error.
 */
export class AliasError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'AliasError';
  }
}

/**
 * Alias Resolver - Graph-Based Alias Management
 *
 * Manages path aliases stored as graph relationships.
 *
 * ## Features
 *
 * - **Dynamic Aliases:** Create/delete at runtime
 * - **Multiple Aliases:** Multiple paths to same actor
 * - **Context Injection:** Metadata attached to aliases
 * - **Priority Resolution:** Higher priority wins on conflicts
 * - **Cycle Detection:** Prevents infinite alias loops
 * - **Queryable:** Find all aliases, reverse lookups
 *
 * ## Usage
 *
 * ```typescript
 * const resolver = new AliasResolver(graphStore);
 *
 * // Create alias
 * await resolver.createAlias('services/llm', 'domain/inference', {
 *   priority: 1,
 *   context: { role: 'llm-service' }
 * });
 *
 * // Resolve alias
 * const resolved = await resolver.resolve('services/llm');
 * // => { path: 'domain/inference', context: { role: 'llm-service' }, ... }
 *
 * // List all aliases
 * const aliases = await resolver.listAliases();
 *
 * // Delete alias
 * await resolver.deleteAlias('services/llm');
 * ```
 *
 * @see PathResolver for path validation
 * @see MessageRouter for routing integration
 */
export class AliasResolver {
  private graph: GraphStore;

  /**
   * Create an alias resolver.
   *
   * @param graph - Graph store for alias storage
   */
  constructor(graph: GraphStore) {
    this.graph = graph;
  }

  /**
   * Create an alias.
   *
   * Creates an alias node and `resolves_to` relationship in the graph.
   *
   * @param aliasPath - Alias path (e.g., "services/llm")
   * @param canonicalPath - Canonical path (e.g., "domain/inference")
   * @param options - Alias options
   * @throws AliasError if alias already exists or paths are invalid
   *
   * @example
   * ```typescript
   * await resolver.createAlias('services/llm', 'domain/inference', {
   *   priority: 1,
   *   context: { role: 'llm-service', version: 'stable' },
   *   description: 'LLM service alias'
   * });
   * ```
   */
  async createAlias(
    aliasPath: string,
    canonicalPath: string,
    options: {
      priority?: number;
      context?: Record<string, any>;
      description?: string;
    } = {}
  ): Promise<void> {
    // Validate paths
    if (!validatePath(aliasPath)) {
      throw new AliasError(`Invalid alias path: ${aliasPath}`);
    }

    if (!validatePath(canonicalPath)) {
      throw new AliasError(`Invalid canonical path: ${canonicalPath}`);
    }

    // Check if alias already exists
    const existing = await this.getAliasNode(aliasPath);
    if (existing) {
      throw new AliasError(`Alias already exists: ${aliasPath}`);
    }

    // Detect potential cycles
    await this.checkForCycles(aliasPath, canonicalPath);

    // Create alias node
    const aliasId = `alias-${aliasPath.replace(/\//g, '-')}`;
    this.graph.addNode(aliasId, 'alias', {
      alias_path: aliasPath,
      canonical_path: canonicalPath,
      priority: options.priority ?? 0,
      context: options.context ?? {},
      description: options.description,
    });

    // Create resolves_to relationship
    // Note: We don't need to link to actual actor node, just store canonical path
    // Router will resolve canonical path separately
    const edgeId = `${aliasId}-resolves`;
    this.graph.addEdge(edgeId, aliasId, canonicalPath, 'resolves_to', {
      created: Date.now(),
    });
  }

  /**
   * Resolve an alias to canonical path.
   *
   * Looks up alias in graph and returns canonical path with context.
   * If path is not an alias, returns it unchanged.
   *
   * @param path - Path to resolve (alias or canonical)
   * @param options - Resolution options
   * @returns Resolved path with context
   *
   * @example
   * ```typescript
   * const resolved = await resolver.resolve('services/llm');
   * // => {
   * //   path: 'domain/inference',
   * //   context: { role: 'llm-service' },
   * //   priority: 1,
   * //   wasAlias: true
   * // }
   * ```
   */
  async resolve(
    path: string,
    options: ResolveOptions = {}
  ): Promise<ResolvedPath> {
    const maxDepth = options.maxDepth ?? 10;
    const visited = new Set<string>();

    let currentPath = path;
    let depth = 0;
    let context: Record<string, any> = {};
    let priority = 0;
    let wasAlias = false;

    while (depth < maxDepth) {
      // Check for cycles
      if (visited.has(currentPath)) {
        throw new AliasError(
          `Alias cycle detected: ${Array.from(visited).join(' → ')} → ${currentPath}`
        );
      }
      visited.add(currentPath);

      // Try to resolve as alias
      const aliasNode = await this.getAliasNode(currentPath);

      if (!aliasNode) {
        // Not an alias, return current path
        return {
          path: currentPath,
          context,
          priority,
          wasAlias,
        };
      }

      // Resolve alias
      const canonicalPath = aliasNode.properties.get('canonical_path') as string;
      const aliasContext = aliasNode.properties.get('context') as Record<string, any> || {};
      const aliasPriority = aliasNode.properties.get('priority') as number || 0;

      // Merge context (deeper contexts override shallower)
      context = { ...context, ...aliasContext };
      priority = Math.max(priority, aliasPriority);
      wasAlias = true;

      currentPath = canonicalPath;
      depth++;
    }

    // Max depth reached
    throw new AliasError(
      `Alias resolution exceeded max depth (${maxDepth}): ${path}`
    );
  }

  /**
   * Delete an alias.
   *
   * Removes alias node and relationships from graph.
   *
   * @param aliasPath - Alias path to delete
   * @returns True if alias was deleted, false if not found
   *
   * @example
   * ```typescript
   * const deleted = await resolver.deleteAlias('services/llm');
   * // => true
   * ```
   */
  async deleteAlias(aliasPath: string): Promise<boolean> {
    const aliasNode = await this.getAliasNode(aliasPath);

    if (!aliasNode) {
      return false;
    }

    // Delete node (this also removes connected edges)
    await this.graph.deleteNode(aliasNode.id);
    return true;
  }

  /**
   * List all aliases.
   *
   * Returns all alias nodes in the graph.
   *
   * @returns Array of aliases
   *
   * @example
   * ```typescript
   * const aliases = await resolver.listAliases();
   * // => [
   * //   { id: '...', aliasPath: 'services/llm', canonicalPath: 'domain/inference', ... },
   * //   ...
   * // ]
   * ```
   */
  async listAliases(): Promise<Alias[]> {
    const aliasNodes = this.graph.getByType('alias');
    return aliasNodes.map(node => this.nodeToAlias(node));
  }

  /**
   * Find aliases pointing to a canonical path (reverse lookup).
   *
   * @param canonicalPath - Canonical path to search for
   * @returns Array of aliases pointing to this path
   *
   * @example
   * ```typescript
   * const aliases = await resolver.findAliasesFor('domain/inference');
   * // => [
   * //   { aliasPath: 'services/llm', ... },
   * //   { aliasPath: 'ai/claude', ... }
   * // ]
   * ```
   */
  async findAliasesFor(canonicalPath: string): Promise<Alias[]> {
    const allAliases = await this.listAliases();
    return allAliases.filter(alias => alias.canonicalPath === canonicalPath);
  }

  /**
   * Check if a path is an alias.
   *
   * @param path - Path to check
   * @returns True if path is an alias
   */
  async isAlias(path: string): Promise<boolean> {
    const node = await this.getAliasNode(path);
    return node !== null;
  }

  /**
   * Get alias node by path.
   *
   * @param aliasPath - Alias path to lookup
   * @returns Alias node, or null if not found
   * @internal
   */
  private async getAliasNode(aliasPath: string): Promise<any | null> {
    const aliasNodes = this.graph.getByType('alias');

    for (const node of aliasNodes) {
      const nodePath = node.properties.get('alias_path');
      if (nodePath === aliasPath) {
        return node;
      }
    }

    return null;
  }

  /**
   * Check for potential alias cycles.
   *
   * Detects if creating an alias would create a cycle.
   *
   * @param aliasPath - Proposed alias path
   * @param canonicalPath - Proposed canonical path
   * @throws AliasError if cycle would be created
   * @internal
   */
  private async checkForCycles(
    aliasPath: string,
    canonicalPath: string
  ): Promise<void> {
    // Check if canonical path is itself an alias that resolves to alias path
    try {
      const resolved = await this.resolve(canonicalPath, { maxDepth: 10 });

      if (resolved.path === aliasPath) {
        throw new AliasError(
          `Creating alias would create cycle: ${aliasPath} → ${canonicalPath} → ${aliasPath}`
        );
      }
    } catch (err) {
      // If resolution fails, it's likely not a cycle (or already a cycle)
      // Re-throw if it's an actual cycle error
      if (err instanceof AliasError && err.message.includes('cycle')) {
        throw err;
      }
    }
  }

  /**
   * Convert node to Alias interface.
   *
   * @param node - Alias node
   * @returns Alias object
   * @internal
   */
  private nodeToAlias(node: any): Alias {
    return {
      id: node.id,
      aliasPath: node.properties.get('alias_path') as string,
      canonicalPath: node.properties.get('canonical_path') as string,
      priority: node.properties.get('priority') as number || 0,
      context: node.properties.get('context') as Record<string, any> || {},
      description: node.properties.get('description') as string | undefined,
      created: node.created,
    };
  }
}

export default AliasResolver;
