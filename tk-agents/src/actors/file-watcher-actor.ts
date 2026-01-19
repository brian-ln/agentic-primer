/**
 * FileWatcherActor - Graph-resident actor for file monitoring
 *
 * This actor watches individual files for changes, broadcasts events to
 * subscribers, maintains searchable event indices, and manages its lifecycle
 * through suspend/resume mechanisms.
 *
 * Key Features:
 * - Graph node with unique address in ActorSystem
 * - Message-passing interface (no direct property access)
 * - Pub/sub event broadcasting
 * - Searchable event history
 * - Automatic suspend/resume for resource efficiency
 *
 * @see FILE_WATCHER_ACTOR_DESIGN.md for complete architecture
 * @see file-watcher.spec.md for formal specification
 * @see file-watcher.model.lisp for formal model
 */

import type { Graph } from "../graph";
import type { ActorSystem, ActorMessage } from "./actor-system";
import fs from "node:fs";
import path from "node:path";

// =============================================================================
// TYPE DEFINITIONS
// =============================================================================

/**
 * FileWatcherActor configuration
 */
export interface FileWatcherConfig {
  id: string;                    // Unique ID: "fw_1", "fw_2", ...
  filePath: string;              // Absolute path to file
  fileType: FileType;            // File type classification
  graph: Graph;                  // Graph database instance
  actorSystem: ActorSystem;      // Actor system for message routing
}

/**
 * File type classification
 */
export type FileType = "session" | "agent" | "task_graph" | "knowledge" | "other";

/**
 * Actor status
 */
export type WatcherStatus = "watching" | "suspended" | "stopped";

/**
 * Graph node properties for FileWatcher
 */
export interface FileWatcherNode {
  id: string;
  type: "file_watcher";
  filePath: string;
  fileType: FileType;
  status: WatcherStatus;
  createdAt: string;             // ISO-8601
  lastActivity: string;          // ISO-8601
  eventCount: number;
  subscriberCount: number;
  actorAddress: string;
}

/**
 * Event subscription with optional filter
 */
export interface Subscription {
  subscriptionId: string;        // "sub_1", "sub_2", ...
  subscriberId: string;          // Subscriber actor ID
  callback: string;              // Actor address for event delivery
  filter?: SubscriptionFilter;
}

/**
 * Subscription filter criteria
 */
export interface SubscriptionFilter {
  eventType?: string[];          // Filter by event types
  after?: string;                // ISO-8601 timestamp
  before?: string;               // ISO-8601 timestamp
  pattern?: string;              // Regex pattern for content
}

/**
 * File change event
 */
export interface FileChangeEvent {
  eventId: string;               // "evt_1", "evt_2", ...
  watcherId: string;
  timestamp: string;             // ISO-8601
  eventType: "file_change" | "file_created" | "file_deleted";
  filePath: string;
  fileType: FileType;
  changes?: FileChanges;
}

/**
 * File change details
 */
export interface FileChanges {
  bytesAdded: number;
  newLines: string[];
  parsedEvents?: StreamEvent[];
}

/**
 * Parsed stream event (e.g., from JSONL)
 */
export interface StreamEvent {
  type: "llm_response" | "tool_result" | "tool_use" | "text_delta";
  timestamp: string;
  content: any;
  metadata?: Record<string, any>;
}

/**
 * Searchable event index
 */
export interface EventIndex {
  byType: Map<string, FileChangeEvent[]>;
  byTimestamp: FileChangeEvent[];
  byContent: Map<string, FileChangeEvent[]>;
}

/**
 * Search query parameters
 */
export interface SearchQuery {
  eventType?: string[];
  after?: string;
  before?: string;
  pattern?: string;
  limit?: number;
}

/**
 * Watch message payload
 */
export interface WatchPayload {
  filePath: string;
  fileType: FileType;
  options?: {
    debounceMs?: number;         // Default: 100
    includeInitial?: boolean;    // Default: false
  };
}

/**
 * Watch response
 */
export interface WatchResponse {
  success: boolean;
  watcherId: string;
  status: "watching";
  currentSize: number;
  error?: string;
}

/**
 * Subscribe payload
 */
export interface SubscribePayload {
  subscriberId: string;
  filter?: SubscriptionFilter;
  callback?: string;
}

/**
 * Subscribe response
 */
export interface SubscribeResponse {
  success: boolean;
  subscriptionId: string;
  subscriberCount: number;
}

/**
 * Search payload
 */
export interface SearchPayload {
  query: SearchQuery;
}

/**
 * Search response
 */
export interface SearchResponse {
  success: boolean;
  events: FileChangeEvent[];
  totalCount: number;
  hasMore: boolean;
}

/**
 * Suspend response
 */
export interface SuspendResponse {
  success: boolean;
  status: "suspended";
  eventIndexSize: number;
}

/**
 * Resume response
 */
export interface ResumeResponse {
  success: boolean;
  status: "watching";
  eventsRestored: number;
  catchupEvents?: number;
}

// =============================================================================
// FILE WATCHER ACTOR IMPLEMENTATION
// =============================================================================

/**
 * FileWatcherActor - Watches a single file for changes
 *
 * This actor is a graph node with message-passing interface. All operations
 * are performed via ActorSystem.send() messages.
 *
 * Message Types:
 * - watch: Start watching file
 * - unwatch: Stop watching file
 * - subscribe: Subscribe to file events
 * - unsubscribe: Unsubscribe from events
 * - search: Search event history
 * - get_events: Get recent events
 * - suspend: Pause watching
 * - resume: Resume watching
 * - get: Read node properties
 * - update: Update node properties
 * - delete: Remove watcher
 *
 * Lifecycle States:
 * - stopped: No active watching
 * - watching: Active file monitoring
 * - suspended: Paused (resources freed)
 * - deleted: Terminal state
 */
export class FileWatcherActor {
  // Properties (stored in graph)
  private id: string;
  private filePath: string;
  private fileType: FileType;

  // Dependencies
  private graph: Graph;
  private actorSystem: ActorSystem;

  // State (not stored in graph)
  private watcher?: fs.FSWatcher;
  private status: WatcherStatus = "stopped";
  private lastSize: number = 0;
  private lastActivity: Date = new Date();
  private eventCount: number = 0;

  private subscriptions: Subscription[] = [];
  private subscriptionCounter: number = 0;

  private eventIndex: EventIndex = {
    byType: new Map(),
    byTimestamp: [],
    byContent: new Map(),
  };
  private eventCounter: number = 0;
  private maxIndexSize: number = 1000;

  private suspendTimer?: Timer;
  private suspendTimeout: number = 5 * 60 * 1000; // 5 minutes
  private debounceMs: number = 100;

  constructor(config: FileWatcherConfig) {
    this.id = config.id;
    this.filePath = config.filePath;
    this.fileType = config.fileType;
    this.graph = config.graph;
    this.actorSystem = config.actorSystem;
  }

  /**
   * Message handler - all communication goes through this method
   */
  async handleMessage(message: ActorMessage): Promise<any> {
    try {
      switch (message.type) {
        case "watch":
          return await this.handleWatch(message.payload as WatchPayload);
        case "unwatch":
          return await this.handleUnwatch();
        case "subscribe":
          return await this.handleSubscribe(message.payload as SubscribePayload);
        case "unsubscribe":
          return await this.handleUnsubscribe(message.payload);
        case "search":
          return await this.handleSearch(message.payload as SearchPayload);
        case "get_events":
          return await this.handleGetEvents(message.payload);
        case "suspend":
          return await this.handleSuspend();
        case "resume":
          return await this.handleResume();
        case "get":
          return await this.handleGet(message.payload);
        case "update":
          return await this.handleUpdate(message.payload);
        case "delete":
          return await this.handleDelete();
        default:
          throw new Error(`Unknown message type: ${message.type}`);
      }
    } catch (error: any) {
      console.error(`FileWatcherActor ${this.id} error:`, error);
      return {
        success: false,
        error: error.message || String(error),
      };
    }
  }

  // ===========================================================================
  // MESSAGE HANDLERS
  // ===========================================================================

  /**
   * Handle "watch" message - Start watching file
   */
  private async handleWatch(payload: WatchPayload): Promise<WatchResponse> {
    // Validate preconditions
    if (this.status !== "stopped" && this.status !== "suspended") {
      throw new Error(`Cannot watch: already watching (status=${this.status})`);
    }

    if (!fs.existsSync(payload.filePath)) {
      throw new Error(`File not found: ${payload.filePath}`);
    }

    // Update configuration
    this.filePath = payload.filePath;
    this.fileType = payload.fileType;
    this.debounceMs = payload.options?.debounceMs ?? 100;

    // Get current file size
    const stats = await fs.promises.stat(this.filePath);
    this.lastSize = stats.size;

    // Create FS watcher
    this.createFSWatcher();

    // Update status
    this.status = "watching";
    await this.updateGraphStatus("watching");

    // Start suspend timer
    this.startSuspendTimer();

    // Emit initial event if requested
    if (payload.options?.includeInitial) {
      await this.emitInitialEvent();
    }

    return {
      success: true,
      watcherId: this.id,
      status: "watching",
      currentSize: this.lastSize,
    };
  }

  /**
   * Handle "unwatch" message - Stop watching file
   */
  private async handleUnwatch(): Promise<any> {
    if (this.status !== "watching") {
      throw new Error(`Cannot unwatch: not watching (status=${this.status})`);
    }

    // Close FS watcher
    this.closeFSWatcher();

    // Update status
    this.status = "stopped";
    await this.updateGraphStatus("stopped");

    // Clear suspend timer
    this.stopSuspendTimer();

    return {
      success: true,
      status: "stopped",
      finalEventCount: this.eventCount,
    };
  }

  /**
   * Handle "subscribe" message - Subscribe to file events
   */
  private async handleSubscribe(payload: SubscribePayload): Promise<SubscribeResponse> {
    // Auto-resume if suspended
    if (this.status === "suspended") {
      await this.handleResume();
    }

    // Create subscription
    const subscriptionId = `sub_${++this.subscriptionCounter}`;
    const subscription: Subscription = {
      subscriptionId,
      subscriberId: payload.subscriberId,
      callback: payload.callback ?? payload.subscriberId,
      filter: payload.filter,
    };

    this.subscriptions.push(subscription);

    // Update graph
    await this.updateGraphProperties({
      subscriberCount: this.subscriptions.length,
    });

    // Create edge in graph
    await this.graph.addEdge(payload.subscriberId, this.id, "subscribes_to");

    return {
      success: true,
      subscriptionId,
      subscriberCount: this.subscriptions.length,
    };
  }

  /**
   * Handle "unsubscribe" message - Cancel subscription
   */
  private async handleUnsubscribe(payload: { subscriptionId: string }): Promise<any> {
    const sub = this.subscriptions.find(s => s.subscriptionId === payload.subscriptionId);

    if (!sub) {
      throw new Error(`Invalid subscription ID: ${payload.subscriptionId}`);
    }

    // Remove subscription
    this.subscriptions = this.subscriptions.filter(
      s => s.subscriptionId !== payload.subscriptionId
    );

    // Update graph
    await this.updateGraphProperties({
      subscriberCount: this.subscriptions.length,
    });

    // Remove edge from graph
    await this.graph.removeEdge(sub.subscriberId, this.id, "subscribes_to");

    // Consider auto-suspend if no subscribers
    if (this.subscriptions.length === 0) {
      this.startSuspendTimer();
    }

    return {
      success: true,
      subscriberCount: this.subscriptions.length,
    };
  }

  /**
   * Handle "search" message - Search event history
   */
  private async handleSearch(payload: SearchPayload): Promise<SearchResponse> {
    const { query } = payload;
    let results: FileChangeEvent[] = [];

    // Search by event type
    if (query.eventType && query.eventType.length > 0) {
      for (const type of query.eventType) {
        const events = this.eventIndex.byType.get(type) || [];
        results.push(...events);
      }
    } else {
      // All events
      results = [...this.eventIndex.byTimestamp];
    }

    // Filter by timestamp range
    if (query.after) {
      results = results.filter(e => e.timestamp >= query.after!);
    }
    if (query.before) {
      results = results.filter(e => e.timestamp <= query.before!);
    }

    // Filter by content pattern
    if (query.pattern) {
      const regex = new RegExp(query.pattern);
      results = results.filter(e => {
        return e.changes?.newLines?.some(line => regex.test(line));
      });
    }

    // Sort by timestamp (newest first)
    results.sort((a, b) => b.timestamp.localeCompare(a.timestamp));

    // Paginate
    const limit = query.limit ?? 50;
    const totalCount = results.length;
    const paginatedResults = results.slice(0, limit);

    return {
      success: true,
      events: paginatedResults,
      totalCount,
      hasMore: totalCount > limit,
    };
  }

  /**
   * Handle "get_events" message - Get recent events
   */
  private async handleGetEvents(payload: { limit?: number; offset?: number }): Promise<any> {
    const limit = payload.limit ?? 100;
    const offset = payload.offset ?? 0;

    const events = this.eventIndex.byTimestamp
      .slice()
      .reverse() // Newest first
      .slice(offset, offset + limit);

    return {
      success: true,
      events,
      totalCount: this.eventIndex.byTimestamp.length,
    };
  }

  /**
   * Handle "suspend" message - Pause watching
   */
  private async handleSuspend(): Promise<SuspendResponse> {
    if (this.status !== "watching") {
      throw new Error(`Cannot suspend: not watching (status=${this.status})`);
    }

    // Close FS watcher
    this.closeFSWatcher();

    // Persist event index
    const indexData = await this.persistEventIndex();

    // Update status
    this.status = "suspended";
    await this.updateGraphStatus("suspended");

    // Clear suspend timer
    this.stopSuspendTimer();

    return {
      success: true,
      status: "suspended",
      eventIndexSize: indexData.length,
    };
  }

  /**
   * Handle "resume" message - Resume watching
   */
  private async handleResume(): Promise<ResumeResponse> {
    if (this.status !== "suspended") {
      throw new Error(`Cannot resume: not suspended (status=${this.status})`);
    }

    // Load event index
    const eventsRestored = await this.loadEventIndex();

    // Recreate FS watcher
    this.createFSWatcher();

    // Detect changes during suspension
    const catchupEvents = await this.detectCatchupEvents();

    // Update status
    this.status = "watching";
    await this.updateGraphStatus("watching");

    // Start suspend timer
    this.startSuspendTimer();

    return {
      success: true,
      status: "watching",
      eventsRestored,
      catchupEvents,
    };
  }

  /**
   * Handle "get" message - Read node properties
   */
  private async handleGet(payload: { properties?: string[] }): Promise<any> {
    const node = await this.graph.getNode(this.id);

    if (payload.properties) {
      const filtered: any = {};
      for (const prop of payload.properties) {
        filtered[prop] = (node as any)[prop];
      }
      return { success: true, node: filtered };
    }

    return { success: true, node };
  }

  /**
   * Handle "update" message - Update node properties
   */
  private async handleUpdate(payload: { properties: Partial<FileWatcherNode> }): Promise<any> {
    await this.updateGraphProperties(payload.properties);
    const node = await this.graph.getNode(this.id);
    return { success: true, node };
  }

  /**
   * Handle "delete" message - Remove watcher
   */
  private async handleDelete(): Promise<any> {
    // Close FS watcher
    this.closeFSWatcher();

    // Notify subscribers
    await this.notifyDeletion();

    // Remove all subscriptions
    this.subscriptions = [];

    // Delete graph node and edges
    await this.graph.deleteNode(this.id);

    // Unregister from ActorSystem
    this.actorSystem.unregister(this.id);

    return {
      success: true,
      deletedId: this.id,
    };
  }

  // ===========================================================================
  // FILE WATCHING LOGIC
  // ===========================================================================

  /**
   * Create FS watcher
   */
  private createFSWatcher() {
    let debounceTimer: Timer | undefined;

    this.watcher = fs.watch(this.filePath, (eventType) => {
      // Debounce rapid changes
      if (debounceTimer) {
        clearTimeout(debounceTimer);
      }

      debounceTimer = setTimeout(async () => {
        await this.handleFileChange(eventType);
      }, this.debounceMs);
    });
  }

  /**
   * Close FS watcher
   */
  private closeFSWatcher() {
    if (this.watcher) {
      this.watcher.close();
      this.watcher = undefined;
    }
  }

  /**
   * Handle file change
   */
  private async handleFileChange(eventType: string) {
    try {
      // Check if file still exists
      if (!fs.existsSync(this.filePath)) {
        console.log(`File deleted: ${this.filePath}`);
        await this.handleDelete();
        return;
      }

      // Read new content
      const stats = await fs.promises.stat(this.filePath);
      const currentSize = stats.size;

      if (currentSize < this.lastSize) {
        // File truncated - reset
        this.lastSize = 0;
      }

      if (currentSize === this.lastSize) {
        // No new content
        return;
      }

      // Read new bytes
      const bytesToRead = currentSize - this.lastSize;
      const buffer = Buffer.alloc(bytesToRead);
      const fd = await fs.promises.open(this.filePath, "r");
      await fd.read(buffer, 0, bytesToRead, this.lastSize);
      await fd.close();

      const newContent = buffer.toString("utf-8");
      this.lastSize = currentSize;

      // Parse new lines
      const newLines = newContent.split("\n").filter(line => line.trim().length > 0);

      // Parse JSONL events if applicable
      let parsedEvents: StreamEvent[] | undefined;
      if (this.filePath.endsWith(".jsonl")) {
        parsedEvents = this.parseJSONL(newLines);
      }

      // Create file change event
      const event: FileChangeEvent = {
        eventId: `evt_${++this.eventCounter}`,
        watcherId: this.id,
        timestamp: new Date().toISOString(),
        eventType: "file_change",
        filePath: this.filePath,
        fileType: this.fileType,
        changes: {
          bytesAdded: bytesToRead,
          newLines,
          parsedEvents,
        },
      };

      // Add to index
      this.addToIndex(event);

      // Update metadata
      this.lastActivity = new Date();
      this.eventCount++;
      await this.updateGraphProperties({
        lastActivity: this.lastActivity.toISOString(),
        eventCount: this.eventCount,
      });

      // Broadcast to subscribers
      await this.broadcastEvent(event);

      // Reset suspend timer
      this.resetSuspendTimer();
    } catch (error) {
      console.error(`Error handling file change for ${this.filePath}:`, error);
    }
  }

  /**
   * Parse JSONL lines into StreamEvents
   */
  private parseJSONL(lines: string[]): StreamEvent[] {
    const events: StreamEvent[] = [];

    for (const line of lines) {
      try {
        const parsed = JSON.parse(line);
        if (parsed.type && parsed.timestamp) {
          events.push(parsed as StreamEvent);
        }
      } catch (error) {
        // Skip invalid JSON
      }
    }

    return events;
  }

  /**
   * Emit initial event for current file content
   */
  private async emitInitialEvent() {
    const content = await fs.promises.readFile(this.filePath, "utf-8");
    const lines = content.split("\n").filter(line => line.trim().length > 0);

    const event: FileChangeEvent = {
      eventId: `evt_${++this.eventCounter}`,
      watcherId: this.id,
      timestamp: new Date().toISOString(),
      eventType: "file_created",
      filePath: this.filePath,
      fileType: this.fileType,
      changes: {
        bytesAdded: content.length,
        newLines: lines,
        parsedEvents: this.filePath.endsWith(".jsonl") ? this.parseJSONL(lines) : undefined,
      },
    };

    this.addToIndex(event);
    await this.broadcastEvent(event);
  }

  // ===========================================================================
  // EVENT BROADCASTING
  // ===========================================================================

  /**
   * Broadcast event to all subscribers
   */
  private async broadcastEvent(event: FileChangeEvent) {
    for (const sub of this.subscriptions) {
      if (this.matchesFilter(event, sub.filter)) {
        await this.actorSystem.send(sub.callback, "file_event", { event });
      }
    }
  }

  /**
   * Check if event matches subscription filter
   */
  private matchesFilter(event: FileChangeEvent, filter?: SubscriptionFilter): boolean {
    if (!filter) return true;

    // Filter by event type
    if (filter.eventType && !filter.eventType.includes(event.eventType)) {
      return false;
    }

    // Filter by timestamp
    if (filter.after && event.timestamp < filter.after) {
      return false;
    }
    if (filter.before && event.timestamp > filter.before) {
      return false;
    }

    // Filter by content pattern
    if (filter.pattern && event.changes?.newLines) {
      const regex = new RegExp(filter.pattern);
      if (!event.changes.newLines.some(line => regex.test(line))) {
        return false;
      }
    }

    return true;
  }

  /**
   * Notify subscribers of watcher deletion
   */
  private async notifyDeletion() {
    const deletionEvent: FileChangeEvent = {
      eventId: `evt_${++this.eventCounter}`,
      watcherId: this.id,
      timestamp: new Date().toISOString(),
      eventType: "file_deleted",
      filePath: this.filePath,
      fileType: this.fileType,
    };

    for (const sub of this.subscriptions) {
      await this.actorSystem.send(sub.callback, "watcher_deleted", {
        event: deletionEvent,
      });
    }
  }

  // ===========================================================================
  // EVENT INDEX MANAGEMENT
  // ===========================================================================

  /**
   * Add event to searchable index
   */
  private addToIndex(event: FileChangeEvent) {
    // Index by type
    const typeKey = event.eventType;
    if (!this.eventIndex.byType.has(typeKey)) {
      this.eventIndex.byType.set(typeKey, []);
    }
    this.eventIndex.byType.get(typeKey)!.push(event);

    // Index chronologically
    this.eventIndex.byTimestamp.push(event);

    // Index by content (simple keyword extraction)
    if (event.changes?.parsedEvents) {
      for (const parsed of event.changes.parsedEvents) {
        const keywords = this.extractKeywords(parsed);
        for (const kw of keywords) {
          if (!this.eventIndex.byContent.has(kw)) {
            this.eventIndex.byContent.set(kw, []);
          }
          this.eventIndex.byContent.get(kw)!.push(event);
        }
      }
    }

    // Evict old events if over limit
    if (this.eventIndex.byTimestamp.length > this.maxIndexSize) {
      this.evictOldestEvent();
    }
  }

  /**
   * Extract keywords from stream event
   */
  private extractKeywords(event: StreamEvent): string[] {
    const keywords: string[] = [event.type];

    // Extract from content
    if (typeof event.content === "string") {
      const words = event.content.toLowerCase().match(/\b\w+\b/g) || [];
      keywords.push(...words.slice(0, 10)); // Top 10 words
    }

    return keywords;
  }

  /**
   * Evict oldest event from index
   */
  private evictOldestEvent() {
    const oldest = this.eventIndex.byTimestamp.shift();
    if (!oldest) return;

    // Remove from type index
    const typeEvents = this.eventIndex.byType.get(oldest.eventType);
    if (typeEvents) {
      const idx = typeEvents.indexOf(oldest);
      if (idx !== -1) {
        typeEvents.splice(idx, 1);
      }
    }

    // Optionally persist evicted event
    // await this.persistEvictedEvent(oldest);
  }

  /**
   * Persist event index to graph
   */
  private async persistEventIndex(): Promise<string> {
    const indexData = {
      byType: Object.fromEntries(this.eventIndex.byType),
      byTimestamp: this.eventIndex.byTimestamp,
      byContent: Object.fromEntries(this.eventIndex.byContent),
    };

    const serialized = JSON.stringify(indexData);

    await this.updateGraphProperties({
      eventIndexSnapshot: serialized,
    } as any);

    return serialized;
  }

  /**
   * Load event index from graph
   */
  private async loadEventIndex(): Promise<number> {
    const node = await this.graph.getNode(this.id);
    const indexData = JSON.parse((node as any).eventIndexSnapshot || "{}");

    this.eventIndex = {
      byType: new Map(Object.entries(indexData.byType || {})),
      byTimestamp: indexData.byTimestamp || [],
      byContent: new Map(Object.entries(indexData.byContent || {})),
    };

    return this.eventIndex.byTimestamp.length;
  }

  /**
   * Detect changes that occurred during suspension
   */
  private async detectCatchupEvents(): Promise<number> {
    // Compare current file size with last known size
    const stats = await fs.promises.stat(this.filePath);
    const currentSize = stats.size;

    if (currentSize > this.lastSize) {
      // File grew during suspension - emit catch-up event
      const bytesToRead = currentSize - this.lastSize;
      const buffer = Buffer.alloc(bytesToRead);
      const fd = await fs.promises.open(this.filePath, "r");
      await fd.read(buffer, 0, bytesToRead, this.lastSize);
      await fd.close();

      const newContent = buffer.toString("utf-8");
      const newLines = newContent.split("\n").filter(line => line.trim().length > 0);

      const event: FileChangeEvent = {
        eventId: `evt_${++this.eventCounter}`,
        watcherId: this.id,
        timestamp: new Date().toISOString(),
        eventType: "file_change",
        filePath: this.filePath,
        fileType: this.fileType,
        changes: {
          bytesAdded: bytesToRead,
          newLines,
          parsedEvents: this.filePath.endsWith(".jsonl") ? this.parseJSONL(newLines) : undefined,
        },
      };

      this.addToIndex(event);
      await this.broadcastEvent(event);

      this.lastSize = currentSize;
      return 1;
    }

    return 0;
  }

  // ===========================================================================
  // LIFECYCLE MANAGEMENT
  // ===========================================================================

  /**
   * Start auto-suspend timer
   */
  private startSuspendTimer() {
    this.suspendTimer = setTimeout(async () => {
      await this.checkAutoSuspend();
    }, this.suspendTimeout);
  }

  /**
   * Stop auto-suspend timer
   */
  private stopSuspendTimer() {
    if (this.suspendTimer) {
      clearTimeout(this.suspendTimer);
      this.suspendTimer = undefined;
    }
  }

  /**
   * Reset auto-suspend timer
   */
  private resetSuspendTimer() {
    this.stopSuspendTimer();
    this.startSuspendTimer();
  }

  /**
   * Check if auto-suspend conditions are met
   */
  private async checkAutoSuspend() {
    const inactiveTime = Date.now() - this.lastActivity.getTime();

    if (
      this.status === "watching" &&
      inactiveTime > this.suspendTimeout &&
      this.subscriptions.length === 0
    ) {
      console.log(`Auto-suspending ${this.id} due to inactivity`);
      await this.handleSuspend();
    }
  }

  // ===========================================================================
  // GRAPH OPERATIONS
  // ===========================================================================

  /**
   * Update graph node status
   */
  private async updateGraphStatus(status: WatcherStatus) {
    await this.updateGraphProperties({ status });
  }

  /**
   * Update graph node properties
   */
  private async updateGraphProperties(properties: Partial<FileWatcherNode>) {
    await this.graph.updateNode(this.id, properties);
  }

  // ===========================================================================
  // PUBLIC INTERFACE (for testing)
  // ===========================================================================

  /**
   * Get current status (for testing)
   */
  public getStatus(): WatcherStatus {
    return this.status;
  }

  /**
   * Get actor ID
   */
  public getId(): string {
    return this.id;
  }
}
