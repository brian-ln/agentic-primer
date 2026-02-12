#!/usr/bin/env bun
/**
 * Message Router - Routes messages to graph nodes and actors
 *
 * Bridges message-based communication with UGS program execution.
 *
 * ## Routing Strategies
 *
 * 1. **Direct Actor Registration** - Actors registered via registerActor()
 * 2. **Hierarchical Routing** - Paths containing '/' route through supervision tree
 * 3. **Flat ID Routing** - Legacy flat IDs lookup in graph store
 *
 * ## Dual Routing Mode (Phase 6: Migration Support)
 *
 * Router supports both flat IDs and hierarchical paths for backward compatibility:
 *
 * **Hierarchical Path (Canonical):**
 * ```typescript
 * router.ask(createMessage(address('domain/inference'), ...));
 * // Routes: domain → inference (via supervision tree)
 * ```
 *
 * **Flat ID (Legacy, with alias resolution):**
 * ```typescript
 * router.ask(createMessage(address('services/inference'), ...));
 * // 1. Detects flat ID format
 * // 2. Resolves alias: 'inference' → 'domain/inference'
 * // 3. Routes as hierarchical path
 * // 4. Logs deprecation warning
 * ```
 *
 * ## Hierarchical Routing (Path-Based Addressing)
 *
 * Messages with paths (e.g., `@(domain/inference)`) route hierarchically:
 * - Extract root segment from path
 * - Delegate to root supervisor
 * - Supervisor forwards through supervision tree
 *
 * **Protocol Integration:**
 * Path-based routing aligns with protocol Address.namespace:
 * ```typescript
 * import { toProtocolAddress } from '../protocol';
 *
 * const addr = address('domain/inference');
 * const protocol = toProtocolAddress(addr);
 * // protocol.namespace => 'domain' (supervision tree path)
 * // protocol.id => 'inference' (leaf actor)
 * ```
 *
 * See: src/protocol/README.md for protocol integration details
 * See: docs/DUAL_ROUTING_MIGRATION.md for migration guide
 */

import type GraphStore from '../graph.ts';
import type { ProgramManager } from '../entities/program.ts';
import {
  type Message,
  type MessageResponse,
  type Address,
  type TokenStreamEvent,
  type AsyncStreamMessage,
  type StreamAsyncOptions,
  type ITransport,
  type ISerde,
  address,
  parseAddress,
  createMessage,
  createResponse,
  createErrorResponse,
  generateCorrelationId,
  parseAddressInfo,
  isHierarchicalPath,
  matchAdvancedPattern as matchPattern,
  hasWildcards,
} from '@agentic-primer/actors';
import { simplifyToShared, sharedToSimplify } from '@agentic-primer/protocols';
import { type StreamOptions } from './message.ts';
import { PathCache, type PathCacheConfig } from '@agentic-primer/actors';
import { AliasResolver } from './alias-resolver.ts';

// ---------------------------------------------------------------------------
// BridgeRoute: Cross-system routing via transport + serde
// ---------------------------------------------------------------------------

/**
 * A bridge route maps an address prefix to a transport and serialization pair.
 *
 * When the router encounters a destination address starting with `prefix`,
 * it converts the simplify Message to SharedMessage, serializes it with
 * `serde`, and dispatches through `transport`.
 *
 * Example:
 * ```typescript
 * router.registerBridge({
 *   prefix: 'remote/',
 *   transport: wsTransport,
 *   serde: new JsonSerde(),
 * });
 *
 * // This message will be bridged:
 * router.tell(createMessage(address('remote/signal-hub'), 'ping', {}));
 * ```
 */
export interface BridgeRoute {
  /** Address prefix that triggers this bridge (e.g., "remote/", "worker://") */
  prefix: string;
  /** Transport for cross-runtime delivery */
  transport: ITransport;
  /** Serializer for wire format */
  serde: ISerde;
}

export class MessageRouter {
  private store: GraphStore;
  private programManager: ProgramManager;
  private pendingRequests: Map<string, {
    resolve: (response: MessageResponse) => void;
    reject: (error: Error) => void;
    timeout: NodeJS.Timeout;
  }> = new Map();
  private actorRegistry: Map<string, any> = new Map(); // Map of id -> Actor instances

  /** Enable/disable deprecation warnings for flat ID usage */
  private enableDeprecationWarnings: boolean = true;

  /** Track flat ID usage for metrics */
  private flatIdUsageCount: number = 0;
  private pathUsageCount: number = 0;

  /** Path cache for hierarchical routing optimization */
  private pathCache: PathCache;

  /** Alias resolver for path alias resolution */
  private aliasResolver: AliasResolver;

  /** Bridge routes for cross-system message dispatch */
  private bridges: Map<string, BridgeRoute> = new Map();

  constructor(store: GraphStore, programManager: ProgramManager, cacheConfig?: PathCacheConfig) {
    this.store = store;
    this.programManager = programManager;
    this.pathCache = new PathCache(cacheConfig);
    this.aliasResolver = new AliasResolver(store);
  }

  // ---------------------------------------------------------------------------
  // Bridge Route Management
  // ---------------------------------------------------------------------------

  /**
   * Register a bridge route for cross-system message dispatch.
   *
   * Messages addressed to targets starting with `bridge.prefix` will be
   * serialized to SharedMessage format and dispatched via the transport.
   *
   * The transport's onReceive handler is wired up to deliver inbound messages
   * back into the local actor system.
   *
   * @param bridge - Bridge route configuration
   */
  registerBridge(bridge: BridgeRoute): void {
    this.bridges.set(bridge.prefix, bridge);

    // Wire up inbound: transport receives → deserialize → route locally
    bridge.transport.onReceive((_sender: string, rawMessage: unknown) => {
      try {
        // Deserialize if we got Uint8Array, otherwise treat as already-parsed
        let sharedMsg: any;
        if (rawMessage instanceof Uint8Array) {
          sharedMsg = bridge.serde.deserialize(rawMessage);
        } else {
          sharedMsg = rawMessage;
        }

        // Convert SharedMessage to simplify Message
        const localMsg = sharedToSimplify(sharedMsg);
        const msg = createMessage(
          localMsg.to,
          localMsg.type,
          localMsg.payload,
          {
            pattern: localMsg.pattern,
            from: localMsg.from,
            correlationId: localMsg.correlationId,
            metadata: localMsg.metadata,
          },
        );

        // Route locally (fire-and-forget for inbound bridged messages)
        this.tell(msg);
      } catch (error) {
        console.error(
          `[BridgeRoute] Failed to process inbound message from ${_sender}:`,
          error instanceof Error ? error.message : error,
        );
      }
    });
  }

  /**
   * Unregister a bridge route.
   *
   * @param prefix - The bridge prefix to remove
   */
  unregisterBridge(prefix: string): void {
    this.bridges.delete(prefix);
  }

  /**
   * List registered bridge prefixes.
   */
  listBridges(): string[] {
    return Array.from(this.bridges.keys());
  }

  /**
   * Configure deprecation warnings.
   *
   * @param enabled - Whether to log warnings for flat ID usage
   */
  setDeprecationWarnings(enabled: boolean): void {
    this.enableDeprecationWarnings = enabled;
  }

  /**
   * Get routing statistics.
   *
   * Useful for monitoring migration progress from flat IDs to paths.
   *
   * @returns Routing usage stats
   */
  getRoutingStats(): {
    flatIdUsage: number;
    pathUsage: number;
    totalRoutes: number;
    migrationProgress: number; // Percentage using paths
    cacheMetrics: ReturnType<PathCache['getMetrics']>;
    bridges: string[];
  } {
    const total = this.flatIdUsageCount + this.pathUsageCount;
    const migrationProgress = total > 0 ? (this.pathUsageCount / total) * 100 : 0;

    return {
      flatIdUsage: this.flatIdUsageCount,
      pathUsage: this.pathUsageCount,
      totalRoutes: total,
      migrationProgress: Math.round(migrationProgress * 100) / 100, // 2 decimal places
      cacheMetrics: this.pathCache.getMetrics(),
      bridges: this.listBridges(),
    };
  }

  /**
   * Get alias resolver for managing path aliases.
   *
   * Allows creating, resolving, and deleting aliases at runtime.
   *
   * @returns Alias resolver instance
   */
  getAliasResolver(): AliasResolver {
    return this.aliasResolver;
  }

  /**
   * Register an actor that's not in the graph store (e.g., tool actors)
   *
   * Supports two signatures:
   * - registerActor(id, actor) - explicit ID
   * - registerActor(actor) - uses actor.address as ID
   */
  registerActor(idOrActor: string | any, actor?: any): void {
    let id: string;
    let actorInstance: any;

    if (typeof idOrActor === 'string') {
      // registerActor(id, actor)
      id = idOrActor;
      actorInstance = actor!;
    } else {
      // registerActor(actor) - extract ID from actor.address
      actorInstance = idOrActor;
      id = actorInstance.address?.id || actorInstance.address || actorInstance.id;
      if (!id) {
        throw new Error('Actor must have address.id, address, or id property');
      }
    }

    this.actorRegistry.set(id, actorInstance);
    // Cache the actor for faster lookups
    this.pathCache.set(id, actorInstance);
  }

  /**
   * Unregister an actor
   */
  unregisterActor(id: string): void {
    this.actorRegistry.delete(id);
    // Invalidate cache entry
    this.pathCache.invalidate(id);
  }

  /**
   * Get an actor by path (for subscription system)
   */
  getActor(path: string): any | undefined {
    return this.actorRegistry.get(path);
  }

  /**
   * List all registered actors (for debugging)
   */
  listActors(): string[] {
    return Array.from(this.actorRegistry.keys());
  }

  /**
   * Cache an actor reference at a specific path.
   *
   * Used by supervisors to cache resolved child actors.
   *
   * @param path - Full path to cache (e.g., "domain/inference")
   * @param actor - Actor reference to cache
   * @internal
   */
  cacheActor(path: string, actor: any): void {
    this.pathCache.set(path, actor);
  }

  /**
   * Invalidate path cache entry.
   *
   * Used by supervisors when children are removed.
   *
   * @param path - Path to invalidate
   * @internal
   */
  invalidatePath(path: string): void {
    this.pathCache.invalidate(path);
  }

  /**
   * Send a message (fire-and-forget pattern)
   */
  async tell(message: Message): Promise<void> {
    try {
      await this.route(message);
    } catch (error: any) {
      // Log error but don't throw for tell pattern
      console.error(`Failed to deliver message ${message.id}:`, error.message);
    }
  }

  /**
   * Send a message and wait for response (request-response pattern)
   */
  async ask<T = any>(message: Message, timeoutMs: number = 30000): Promise<MessageResponse<T>> {
    if (!message.correlationId) {
      throw new Error('ask() requires message with correlationId');
    }

    return new Promise<MessageResponse<T>>(async (resolve, reject) => {
      // Set up timeout
      const timeout = setTimeout(() => {
        this.pendingRequests.delete(message.correlationId!);
        reject(new Error(`Message ${message.id} timed out after ${timeoutMs}ms`));
      }, timeoutMs);

      // Store pending request
      this.pendingRequests.set(message.correlationId, { resolve, reject, timeout });

      // Route the message
      try {
        const response = await this.route(message);
        // For ask pattern, resolve immediately with response
        clearTimeout(timeout);
        this.pendingRequests.delete(message.correlationId!);
        resolve(response as MessageResponse<T>);
      } catch (error: any) {
        clearTimeout(timeout);
        this.pendingRequests.delete(message.correlationId!);
        reject(error);
      }
    });
  }

  /**
   * Stream a message with real-time token updates (callback-based)
   * Uses actor's stream() method if available, otherwise throws error
   */
  async streamAsk<T, R>(
    to: Address,
    type: string,
    payload: T,
    options: StreamOptions<R>
  ): Promise<void> {
    const targetId = parseAddress(to);

    // Check if there's a registered actor with stream support
    if (this.actorRegistry.has(targetId)) {
      const actor = this.actorRegistry.get(targetId)!;

      if (typeof actor.stream !== 'function') {
        throw new Error(`Actor ${targetId} does not support streaming`);
      }

      // Call actor's stream method
      await actor.stream(payload, options.onChunk);
      return;
    }

    // Check if node exists in graph
    const node = this.store.get(targetId);
    if (!node) {
      throw new Error(`Node not found: ${targetId}`);
    }

    throw new Error(`Node ${targetId} does not support streaming`);
  }

  /**
   * Stream a message with AsyncIterator support
   * Provides backpressure handling, cancellation, and memory safety
   */
  async *streamAsync<T, R>(
    to: Address,
    type: string,
    payload: T,
    options: StreamAsyncOptions = {}
  ): AsyncIterableIterator<AsyncStreamMessage<R>> {
    const targetId = parseAddress(to);
    const bufferSize = options.bufferSize ?? 100;
    const timeout = options.timeout ?? 60000;

    // Check if there's a registered actor with streamAsync support
    let actor: any;
    if (this.actorRegistry.has(targetId)) {
      actor = this.actorRegistry.get(targetId)!;
    } else {
      const node = this.store.get(targetId);
      if (!node) {
        throw new Error(`Node not found: ${targetId}`);
      }
      throw new Error(`Node ${targetId} does not support async streaming`);
    }

    if (typeof actor.streamAsync !== 'function') {
      throw new Error(`Actor ${targetId} does not support async streaming`);
    }

    // Create buffer for backpressure handling
    const buffer: AsyncStreamMessage<R>[] = [];
    let isPaused = false;
    let isDone = false;
    let error: Error | null = null;
    let resolveNext: ((value: IteratorResult<AsyncStreamMessage<R>>) => void) | null = null;

    // Track if cancelled
    let isCancelled = false;
    const cancelledPromise = new Promise<void>((resolve) => {
      if (options.signal) {
        options.signal.addEventListener('abort', () => {
          isCancelled = true;
          error = new Error(options.signal!.reason || 'Stream cancelled');
          isDone = true;
          resolve();
          if (resolveNext) {
            resolveNext({ done: true, value: undefined });
            resolveNext = null;
          }
        });
      }
    });

    // Timeout handler
    const timeoutPromise = new Promise<void>((resolve) => {
      setTimeout(() => {
        if (!isDone && !isCancelled) {
          error = new Error(`Stream timeout after ${timeout}ms`);
          isDone = true;
          resolve();
          if (resolveNext) {
            resolveNext({ done: true, value: undefined });
            resolveNext = null;
          }
        }
      }, timeout);
    });

    // Start the actor's stream in background
    const streamPromise = (async () => {
      try {
        const iterator = actor.streamAsync(payload);
        for await (const item of iterator) {
          if (isCancelled || isDone) break;

          // Add to buffer
          buffer.push(item);

          // Backpressure: pause if buffer is full
          if (buffer.length >= bufferSize) {
            isPaused = true;
          }

          // Wake up waiting consumer
          if (resolveNext) {
            const msg = buffer.shift()!;
            resolveNext({ done: false, value: msg });
            resolveNext = null;
            isPaused = buffer.length >= bufferSize;
          }
        }
      } catch (err: any) {
        error = err;
      } finally {
        isDone = true;
        if (resolveNext) {
          if (error) {
            resolveNext({ done: true, value: undefined });
          } else {
            resolveNext({ done: true, value: undefined });
          }
          resolveNext = null;
        }
      }
    })();

    // AsyncIterator implementation
    try {
      while (true) {
        // Check cancellation
        if (isCancelled) {
          if (error) throw error;
          return;
        }

        // If buffer has items, yield immediately
        if (buffer.length > 0) {
          const msg = buffer.shift()!;
          isPaused = buffer.length >= bufferSize;

          // Check for error messages
          if (msg.type === 'error') {
            throw new Error(msg.error || 'Stream error');
          }

          yield msg;

          // Check for end message
          if (msg.type === 'end') {
            return;
          }

          continue;
        }

        // If stream is done and buffer is empty
        if (isDone) {
          if (error) throw error;
          return;
        }

        // Wait for next item
        await new Promise<void>((resolve) => {
          resolveNext = (result) => {
            if (result.done) {
              resolve();
            } else {
              // Put it back in buffer for next iteration
              buffer.unshift(result.value);
              resolve();
            }
          };
        });
      }
    } finally {
      // Cleanup: signal cancellation to source
      isCancelled = true;
      isDone = true;
    }
  }

  /**
   * Route a message to its destination (dual routing mode).
   *
   * Supports both flat ID routing (legacy) and hierarchical path routing (canonical).
   *
   * ## Dual Routing Strategy (Phase 6: Migration Support)
   *
   * 1. **Check registered actors** - Direct actor registry lookup (fast path)
   * 2. **Detect address format** - Flat ID or hierarchical path?
   * 3. **For hierarchical paths** - Route through supervision tree
   * 4. **For flat IDs (legacy):**
   *    - Attempt alias resolution (flat ID → canonical path)
   *    - If resolved: route as hierarchical path
   *    - If unresolved: fallback to flat ID routing (graph store)
   *    - Log deprecation warning
   *
   * ## Routing Strategy
   *
   * 1. **Check registered actors** - Direct actor registry lookup (e.g., tool actors)
   * 2. **Check if path contains `/`** - If yes, use hierarchical routing
   * 3. **Otherwise, use flat ID routing** - Lookup in graph store (legacy)
   *
   * ## Hierarchical Routing
   *
   * For paths containing `/`, message is routed through supervision tree:
   * - Root supervisor receives message
   * - Delegates to child based on first path segment
   * - Child delegates further until leaf actor reached
   *
   * ## Flat ID Routing (Legacy)
   *
   * For flat IDs (no `/`), message is routed directly:
   * - Attempt alias resolution to canonical path
   * - If resolved, route as hierarchical path
   * - If unresolved, lookup actor in graph store
   * - Invoke program or query document
   *
   * @param message - Message to route
   * @returns Message response
   */
  private async route(message: Message): Promise<MessageResponse> {
    const targetId = parseAddress(message.to);

    // First check if there's a registered actor (e.g., tool actors, supervisors)
    // This is the fast path for both flat IDs and paths
    if (this.actorRegistry.has(targetId)) {
      const actor = this.actorRegistry.get(targetId)!;
      return await actor.receive(message);
    }

    // Check bridge routes: detect remote addresses and dispatch via transport
    const bridgeResult = await this.tryBridgeRoute(message, targetId);
    if (bridgeResult !== null) {
      return bridgeResult;
    }

    // All addresses use hierarchical routing (paths-only mode)
    this.pathUsageCount++;
    return await this.hierarchicalRoute(message);
  }

  /**
   * Attempt to route a message through a registered bridge.
   *
   * Matches the target address against bridge prefixes. If a match is found,
   * converts the simplify Message to SharedMessage, serializes it, and
   * dispatches via the bridge transport.
   *
   * @param message - The message to bridge
   * @param targetId - The parsed target address
   * @returns MessageResponse if bridged, null if no bridge matched
   */
  private async tryBridgeRoute(
    message: Message,
    targetId: string,
  ): Promise<MessageResponse | null> {
    for (const [prefix, bridge] of this.bridges) {
      if (targetId.startsWith(prefix)) {
        try {
          // Convert simplify Message to SharedMessage wire format
          const sharedMsg = simplifyToShared({
            id: message.id,
            pattern: message.pattern === 'stream' ? 'tell' : message.pattern,
            to: message.to,
            from: message.from,
            type: message.type,
            payload: message.payload,
            correlationId: message.correlationId,
            timestamp: message.timestamp,
            metadata: message.metadata,
          });

          // Serialize to bytes
          const bytes = bridge.serde.serialize(sharedMsg);

          // Dispatch through transport
          await bridge.transport.send(targetId, bytes);

          return createResponse(message, { bridged: true, prefix });
        } catch (error) {
          return createErrorResponse(
            message,
            `Bridge dispatch failed for prefix "${prefix}": ${
              error instanceof Error ? error.message : String(error)
            }`,
          );
        }
      }
    }

    return null; // No bridge matched
  }

  /**
   * Route flat ID with alias resolution (dual routing support).
   *
   * Attempts to resolve flat ID to canonical path, then routes hierarchically.
   * Falls back to legacy flat ID routing if resolution fails.
   *
   * @param message - Original message with flat ID address
   * @param flatId - Flat ID extracted from address
   * @returns Message response
   */
  private async flatIdRouteWithAlias(
    message: Message,
    flatId: string
  ): Promise<MessageResponse> {
    // PATHS-ONLY MODE: No alias resolution, use legacy flat ID routing (graph store lookup)
    // All new code should use hierarchical paths like @(domain/tasks) instead of @(tasks)
    if (this.enableDeprecationWarnings) {
      console.warn(
        `[Router] Flat ID "${flatId}" detected. ` +
        `Paths-only mode active. Using legacy graph store lookup. ` +
        `Please migrate to hierarchical paths.`
      );
    }

    return await this.flatRoute(message);
  }

  /**
   * Route message hierarchically through supervision tree.
   *
   * Supports:
   * - Alias resolution (e.g., services/llm → domain/inference)
   * - Pattern matching (e.g., workflows/star/tasks with wildcards)
   * - Path caching for performance
   * - Supervision tree delegation
   *
   * @param message - Message with hierarchical path
   * @returns Message response from supervisor delegation
   */
  private async hierarchicalRoute(message: Message): Promise<MessageResponse> {
    let targetPath = parseAddress(message.to);

    // Phase 1a: Resolve aliases before routing (gracefully handle missing graph methods)
    try {
      const resolvedAlias = await this.aliasResolver.resolve(targetPath);
      if (resolvedAlias.wasAlias) {
        targetPath = resolvedAlias.path;
        // Note: Alias context injection could be added here if needed
      }
    } catch (error) {
      // Alias resolution failed (e.g., graph doesn't support getByType)
      // Continue with original path - this is fine, means no aliases configured
    }

    // Phase 1b: Check if path contains wildcards (pattern matching)
    if (hasWildcards(targetPath)) {
      return await this.routePattern(message, targetPath);
    }

    // Check cache first (fast path)
    const cachedActor = this.pathCache.get(targetPath);
    if (cachedActor) {
      return await cachedActor.receive(message);
    }

    const segments = targetPath.split('/').filter(s => s.length > 0);

    if (segments.length === 0) {
      return createErrorResponse(message, 'Invalid empty path');
    }

    // Get root segment (first part of path)
    const rootSegment = segments[0];

    // Lookup root supervisor in actor registry
    const rootActor = this.actorRegistry.get(rootSegment);

    if (!rootActor) {
      // Fallback: If single segment, try graph store lookup (programs/documents)
      if (segments.length === 1) {
        const node = this.store.get(targetPath);
        if (node && (node.type === 'program' || node.type === 'session' || node.type === 'information' || node.type === 'human')) {
          return await this.flatRoute(message);
        }
      }

      return createErrorResponse(
        message,
        `Root supervisor not found: ${rootSegment}. Register supervisors with router.registerActor()`
      );
    }

    // Delegate to root supervisor (it will handle further delegation)
    // Note: We don't cache yet - supervisor will call back into router to cache resolved path
    return await rootActor.receive(message);
  }

  /**
   * Route message with pattern matching (Phase 1b).
   *
   * Expands wildcard patterns against registered actors and routes to matches.
   * Returns error if no matches or multiple ambiguous matches.
   *
   * @param message - Original message
   * @param pattern - Path pattern with wildcards (e.g., "workflows/star/tasks")
   * @returns Message response
   */
  private async routePattern(message: Message, pattern: string): Promise<MessageResponse> {
    // Collect all registered actor paths
    const candidatePaths: string[] = [];

    // Add all registered actors
    for (const actorId of this.actorRegistry.keys()) {
      candidatePaths.push(actorId);
    }

    // Add cached paths (may include supervisor-delegated paths)
    // Note: This is safe because cache stores full paths
    const cacheMetrics = this.pathCache.getMetrics();
    // PathCache doesn't expose keys, so we rely on actorRegistry for now
    // TODO: Consider adding PathCache.keys() method if pattern performance is critical

    // Match pattern against candidate paths
    const matches = candidatePaths.filter(candidatePath =>
      matchPattern(candidatePath, pattern)
    );

    if (matches.length === 0) {
      return createErrorResponse(
        message,
        `No actors found matching pattern: ${pattern}`
      );
    }

    if (matches.length > 1) {
      return createErrorResponse(
        message,
        `Ambiguous pattern: ${pattern} matches multiple actors: ${matches.join(', ')}`
      );
    }

    // Single match - route to it
    const matchedPath = matches[0];
    const actor = this.actorRegistry.get(matchedPath);

    if (!actor) {
      return createErrorResponse(
        message,
        `Internal error: matched path ${matchedPath} not found in registry`
      );
    }

    // Cache the pattern → resolved path mapping for performance
    this.pathCache.set(pattern, actor);

    return await actor.receive(message);
  }

  /**
   * Route message using flat ID (legacy).
   *
   * Looks up actor in graph store and invokes program or queries document.
   *
   * @param message - Message with flat ID address
   * @returns Message response from program/document
   */
  private async flatRoute(message: Message): Promise<MessageResponse> {
    const targetId = parseAddress(message.to);

    // Get the target node from graph
    const node = this.store.get(targetId);
    if (!node) {
      // Handle system messages (no sender)
      if (!message.from) {
        return {
          id: message.id + '_response',
          correlationId: message.correlationId || message.id,
          from: message.to,
          to: message.to, // System message, respond to self
          success: false,
          error: `Node not found: ${targetId}`,
          timestamp: Date.now(),
        };
      }
      return createErrorResponse(message, `Node not found: ${targetId}`);
    }

    // Check if target is a program (executable actor)
    if (node.type === 'program') {
      return await this.invokeProgram(targetId, message);
    }

    // Check if target is a document (query-only actor)
    if (node.type === 'session' || node.type === 'information' || node.type === 'human') {
      return await this.queryDocument(targetId, message);
    }

    // Unknown node type
    return createErrorResponse(
      message,
      `Node ${targetId} (type: ${node.type}) is not addressable`
    );
  }

  /**
   * Invoke a program node with actor context
   */
  private async invokeProgram(id: string, message: Message): Promise<MessageResponse> {
    try {
      // Get the program implementation
      const node = this.store.get(id);
      if (!node || node.type !== 'program') {
        throw new Error(`Program not found: ${id}`);
      }

      const impl = node.properties.get('impl') as string;
      const startTime = Date.now();

      // Create actor context with ask/tell capabilities
      const actorContext = {
        ask: async (to: string, type: string, payload: any): Promise<any> => {
          // Support both @(id) and plain id formats
          let targetAddr: Address;
          if (to.startsWith('@(') && to.endsWith(')')) {
            targetAddr = to as Address;
          } else {
            targetAddr = address(to);
          }

          const askMsg = createMessage(targetAddr, type, payload, {
            pattern: 'ask',
            from: message.to, // From the program being executed
            correlationId: generateCorrelationId(),
          });
          const response = await this.ask(askMsg);
          if (!response.success) {
            throw new Error(response.error || 'Actor call failed');
          }
          return response.payload;
        },
        tell: async (to: string, type: string, payload: any): Promise<void> => {
          let targetAddr: Address;
          if (to.startsWith('@(') && to.endsWith(')')) {
            targetAddr = to as Address;
          } else {
            targetAddr = address(to);
          }

          const tellMsg = createMessage(targetAddr, type, payload, {
            pattern: 'tell',
            from: message.to,
          });
          await this.tell(tellMsg);
        },
      };

      // Execute program with actor context
      // Wrap in async function to support await in program code
      const fn = new Function('input', `return (async function() { ${impl} }).call(this);`);
      const output = await fn.call(actorContext, {
        message: message.payload,
        type: message.type,
        metadata: message.metadata,
      });

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: message.to,
        to: message.from || message.to,
        success: true,
        payload: output,
        timestamp: Date.now(),
      };
    } catch (error: any) {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: message.to,
        to: message.from || message.to,
        success: false,
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }

  /**
   * Query a document node (returns node data)
   */
  private async queryDocument(id: string, message: Message): Promise<MessageResponse> {
    try {
      const node = this.store.get(id);
      if (!node) {
        return {
          id: message.id + '_response',
          correlationId: message.correlationId || message.id,
          from: message.to,
          to: message.from || message.to,
          success: false,
          error: `Document not found: ${id}`,
          timestamp: Date.now(),
        };
      }

      // Return node properties as document data
      const data = {
        id: node.id,
        type: node.type,
        properties: Object.fromEntries(node.properties.entries()),
        created: node.created,
        modified: node.modified,
      };

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: message.to,
        to: message.from || message.to,
        success: true,
        payload: data,
        timestamp: Date.now(),
      };
    } catch (error: any) {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: message.to,
        to: message.from || message.to,
        success: false,
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }

  /**
   * Get router statistics
   */
  getStats() {
    return {
      pendingRequests: this.pendingRequests.size,
    };
  }
}

export default MessageRouter;
