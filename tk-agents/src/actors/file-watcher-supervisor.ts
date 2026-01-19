/**
 * FileWatcherSupervisor - Monitors directories and auto-spawns FileWatcherActors
 *
 * This supervisor actor watches project directories for new files and automatically
 * spawns FileWatcherActors when files matching spawn rules are created.
 *
 * Key Responsibilities:
 * - Monitor directories for file creation/deletion
 * - Auto-spawn FileWatcherActors for matching files
 * - Manage spawned watchers (suspend, resume, terminate)
 * - Track spawned watchers in graph
 *
 * @see FILE_WATCHER_ACTOR_DESIGN.md for architecture details
 */

import type { Graph } from "../graph";
import type { ActorSystem, ActorMessage } from "./actor-system";
import { FileWatcherActor, type FileType } from "./file-watcher-actor";
import fs from "node:fs";
import path from "node:path";

// =============================================================================
// TYPE DEFINITIONS
// =============================================================================

/**
 * Supervisor configuration
 */
export interface SupervisorConfig {
  id: string;                    // "supervisor_1", "supervisor_2", ...
  graph: Graph;
  actorSystem: ActorSystem;
}

/**
 * Spawn rule for auto-creating watchers
 */
export interface SpawnRule {
  pattern: RegExp;               // File pattern to match
  fileType: FileType;            // Type to assign to watcher
  autoSuspendAfter?: number;     // Auto-suspend after N ms (default: 5 min)
}

/**
 * Monitor payload
 */
export interface MonitorPayload {
  directory: string;             // Directory to monitor
  recursive?: boolean;           // Monitor subdirectories (default: true)
  rules: SpawnRule[];            // Spawn rules for auto-creating watchers
}

/**
 * Spawn watcher payload
 */
export interface SpawnWatcherPayload {
  filePath: string;
  fileType: FileType;
  parentId?: string;             // Parent node to link watcher to
  autoSuspendAfter?: number;
}

/**
 * Supervisor node properties
 */
export interface SupervisorNode {
  id: string;
  type: "file_watcher_supervisor";
  watchedDirectories: string[];
  spawnedWatchers: string[];
  status: "active" | "paused";
  createdAt: string;
  lastActivity: string;
}

// =============================================================================
// FILE WATCHER SUPERVISOR IMPLEMENTATION
// =============================================================================

/**
 * FileWatcherSupervisor - Auto-spawns FileWatcherActors for new files
 *
 * Message Types:
 * - monitor: Start monitoring directory
 * - stop_monitoring: Stop monitoring directory
 * - spawn_watcher: Manually spawn watcher
 * - terminate_watcher: Terminate spawned watcher
 * - list_watchers: Get list of spawned watchers
 * - pause: Pause supervisor
 * - resume: Resume supervisor
 * - delete: Remove supervisor
 */
export class FileWatcherSupervisor {
  private id: string;
  private graph: Graph;
  private actorSystem: ActorSystem;

  // State
  private directoryWatchers: Map<string, fs.FSWatcher> = new Map();
  private spawnedWatchers: string[] = [];
  private spawnRules: Map<string, SpawnRule[]> = new Map(); // directory -> rules
  private watcherCounter: number = 0;
  private status: "active" | "paused" = "active";

  constructor(config: SupervisorConfig) {
    this.id = config.id;
    this.graph = config.graph;
    this.actorSystem = config.actorSystem;
  }

  /**
   * Message handler
   */
  async handleMessage(message: ActorMessage): Promise<any> {
    try {
      switch (message.type) {
        case "monitor":
          return await this.handleMonitor(message.payload as MonitorPayload);
        case "stop_monitoring":
          return await this.handleStopMonitoring(message.payload);
        case "spawn_watcher":
          return await this.handleSpawnWatcher(message.payload as SpawnWatcherPayload);
        case "terminate_watcher":
          return await this.handleTerminateWatcher(message.payload);
        case "list_watchers":
          return await this.handleListWatchers();
        case "pause":
          return await this.handlePause();
        case "resume":
          return await this.handleResume();
        case "delete":
          return await this.handleDelete();
        default:
          throw new Error(`Unknown message type: ${message.type}`);
      }
    } catch (error: any) {
      console.error(`FileWatcherSupervisor ${this.id} error:`, error);
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
   * Handle "monitor" - Start monitoring directory
   */
  private async handleMonitor(payload: MonitorPayload): Promise<any> {
    const { directory, recursive = true, rules } = payload;

    if (!fs.existsSync(directory)) {
      throw new Error(`Directory not found: ${directory}`);
    }

    // Create directory watcher
    const watcher = fs.watch(directory, { recursive }, (eventType, filename) => {
      if (this.status === "paused") return;

      if (eventType === "rename" && filename) {
        const fullPath = path.join(directory, filename);

        // Check if file exists (created vs deleted)
        if (fs.existsSync(fullPath)) {
          this.handleFileCreated(fullPath, directory);
        } else {
          this.handleFileDeleted(fullPath);
        }
      }
    });

    this.directoryWatchers.set(directory, watcher);
    this.spawnRules.set(directory, rules);

    // Update graph
    await this.updateGraphProperties({
      watchedDirectories: Array.from(this.directoryWatchers.keys()),
    });

    console.log(`Supervisor ${this.id} monitoring: ${directory}`);

    return {
      success: true,
      monitoring: directory,
      rulesCount: rules.length,
    };
  }

  /**
   * Handle "stop_monitoring" - Stop monitoring directory
   */
  private async handleStopMonitoring(payload: { directory: string }): Promise<any> {
    const watcher = this.directoryWatchers.get(payload.directory);

    if (!watcher) {
      throw new Error(`Not monitoring directory: ${payload.directory}`);
    }

    // Close watcher
    watcher.close();
    this.directoryWatchers.delete(payload.directory);
    this.spawnRules.delete(payload.directory);

    // Update graph
    await this.updateGraphProperties({
      watchedDirectories: Array.from(this.directoryWatchers.keys()),
    });

    console.log(`Supervisor ${this.id} stopped monitoring: ${payload.directory}`);

    return {
      success: true,
      directory: payload.directory,
    };
  }

  /**
   * Handle "spawn_watcher" - Manually spawn watcher
   */
  private async handleSpawnWatcher(payload: SpawnWatcherPayload): Promise<any> {
    const watcherId = await this.spawnWatcher({
      filePath: payload.filePath,
      fileType: payload.fileType,
      parentId: payload.parentId,
      autoSuspendAfter: payload.autoSuspendAfter,
    });

    return {
      success: true,
      watcherId,
    };
  }

  /**
   * Handle "terminate_watcher" - Terminate spawned watcher
   */
  private async handleTerminateWatcher(payload: { watcherId: string }): Promise<any> {
    if (!this.spawnedWatchers.includes(payload.watcherId)) {
      throw new Error(`Watcher ${payload.watcherId} not spawned by this supervisor`);
    }

    // Send delete message to watcher
    await this.actorSystem.send(payload.watcherId, "delete", {});

    // Remove from tracking
    this.spawnedWatchers = this.spawnedWatchers.filter(id => id !== payload.watcherId);

    // Update graph
    await this.updateGraphProperties({
      spawnedWatchers: this.spawnedWatchers,
    });

    console.log(`Supervisor ${this.id} terminated watcher: ${payload.watcherId}`);

    return {
      success: true,
      watcherId: payload.watcherId,
    };
  }

  /**
   * Handle "list_watchers" - Get list of spawned watchers
   */
  private async handleListWatchers(): Promise<any> {
    return {
      success: true,
      watchers: this.spawnedWatchers,
      count: this.spawnedWatchers.length,
    };
  }

  /**
   * Handle "pause" - Pause supervisor
   */
  private async handlePause(): Promise<any> {
    this.status = "paused";

    await this.updateGraphProperties({
      status: "paused",
    });

    console.log(`Supervisor ${this.id} paused`);

    return {
      success: true,
      status: "paused",
    };
  }

  /**
   * Handle "resume" - Resume supervisor
   */
  private async handleResume(): Promise<any> {
    this.status = "active";

    await this.updateGraphProperties({
      status: "active",
    });

    console.log(`Supervisor ${this.id} resumed`);

    return {
      success: true,
      status: "active",
    };
  }

  /**
   * Handle "delete" - Remove supervisor
   */
  private async handleDelete(): Promise<any> {
    // Stop all directory watchers
    for (const watcher of this.directoryWatchers.values()) {
      watcher.close();
    }
    this.directoryWatchers.clear();

    // Terminate all spawned watchers
    for (const watcherId of this.spawnedWatchers) {
      await this.actorSystem.send(watcherId, "delete", {});
    }
    this.spawnedWatchers = [];

    // Delete graph node
    await this.graph.deleteNode(this.id);

    // Unregister from ActorSystem
    this.actorSystem.unregister(this.id);

    console.log(`Supervisor ${this.id} deleted`);

    return {
      success: true,
      deletedId: this.id,
    };
  }

  // ===========================================================================
  // FILE EVENT HANDLERS
  // ===========================================================================

  /**
   * Handle file creation
   */
  private async handleFileCreated(filePath: string, directory: string) {
    console.log(`File created: ${filePath}`);

    // Find matching spawn rule
    const rules = this.spawnRules.get(directory) || [];
    const rule = this.findMatchingRule(filePath, rules);

    if (!rule) {
      console.log(`No spawn rule for: ${filePath}`);
      return;
    }

    // Check if watcher already exists
    const existingWatcher = await this.findWatcherByPath(filePath);
    if (existingWatcher) {
      console.log(`Watcher already exists for: ${filePath}`);
      return;
    }

    // Spawn new FileWatcherActor
    const watcherId = await this.spawnWatcher({
      filePath,
      fileType: rule.fileType,
      autoSuspendAfter: rule.autoSuspendAfter,
    });

    console.log(`Spawned watcher ${watcherId} for: ${filePath}`);

    // Set auto-suspend timer if configured
    if (rule.autoSuspendAfter) {
      setTimeout(async () => {
        await this.checkAndSuspend(watcherId, rule.autoSuspendAfter!);
      }, rule.autoSuspendAfter);
    }
  }

  /**
   * Handle file deletion
   */
  private async handleFileDeleted(filePath: string) {
    console.log(`File deleted: ${filePath}`);

    // Find watcher for this file
    const watcherId = await this.findWatcherByPath(filePath);
    if (!watcherId) {
      return;
    }

    // Terminate watcher
    await this.actorSystem.send(watcherId, "delete", {});

    // Remove from tracking
    this.spawnedWatchers = this.spawnedWatchers.filter(id => id !== watcherId);

    await this.updateGraphProperties({
      spawnedWatchers: this.spawnedWatchers,
    });

    console.log(`Terminated watcher ${watcherId} for deleted file: ${filePath}`);
  }

  // ===========================================================================
  // WATCHER MANAGEMENT
  // ===========================================================================

  /**
   * Spawn a new FileWatcherActor
   */
  private async spawnWatcher(config: SpawnWatcherPayload): Promise<string> {
    const watcherId = `fw_${++this.watcherCounter}`;

    // Create graph node
    await this.graph.createNode({
      id: watcherId,
      type: "file_watcher",
      filePath: config.filePath,
      fileType: config.fileType,
      status: "watching",
      createdAt: new Date().toISOString(),
      lastActivity: new Date().toISOString(),
      eventCount: 0,
      subscriberCount: 0,
      actorAddress: watcherId,
    });

    // Instantiate FileWatcherActor
    const actor = new FileWatcherActor({
      id: watcherId,
      filePath: config.filePath,
      fileType: config.fileType,
      graph: this.graph,
      actorSystem: this.actorSystem,
    });

    // Register in ActorSystem
    this.actorSystem.register(watcherId, actor);

    // Start watching
    await this.actorSystem.send(watcherId, "watch", {
      filePath: config.filePath,
      fileType: config.fileType,
    });

    // Create spawned_by edge
    await this.graph.addEdge(watcherId, this.id, "spawned_by");

    // If parentId provided, create has_watcher edge
    if (config.parentId) {
      await this.graph.addEdge(config.parentId, watcherId, "has_watcher");
    }

    // Track spawned watcher
    this.spawnedWatchers.push(watcherId);

    await this.updateGraphProperties({
      spawnedWatchers: this.spawnedWatchers,
      lastActivity: new Date().toISOString(),
    });

    return watcherId;
  }

  /**
   * Find matching spawn rule for file
   */
  private findMatchingRule(filePath: string, rules: SpawnRule[]): SpawnRule | undefined {
    const filename = path.basename(filePath);

    for (const rule of rules) {
      if (rule.pattern.test(filename) || rule.pattern.test(filePath)) {
        return rule;
      }
    }

    return undefined;
  }

  /**
   * Find watcher by file path
   */
  private async findWatcherByPath(filePath: string): Promise<string | undefined> {
    // Query graph for watcher with matching filePath
    for (const watcherId of this.spawnedWatchers) {
      try {
        const node = await this.graph.getNode(watcherId);
        if ((node as any).filePath === filePath) {
          return watcherId;
        }
      } catch (error) {
        // Node might have been deleted
        continue;
      }
    }

    return undefined;
  }

  /**
   * Check if watcher should be auto-suspended
   */
  private async checkAndSuspend(watcherId: string, inactivityThreshold: number) {
    try {
      // Get watcher metadata
      const node = await this.graph.getNode(watcherId);
      const lastActivity = new Date((node as any).lastActivity);
      const inactiveMs = Date.now() - lastActivity.getTime();

      // Suspend if inactive and no subscribers
      if (inactiveMs > inactivityThreshold && (node as any).subscriberCount === 0) {
        await this.actorSystem.send(watcherId, "suspend", {});
        console.log(`Auto-suspended ${watcherId} due to inactivity`);
      }
    } catch (error) {
      console.error(`Error checking auto-suspend for ${watcherId}:`, error);
    }
  }

  // ===========================================================================
  // GRAPH OPERATIONS
  // ===========================================================================

  /**
   * Update graph node properties
   */
  private async updateGraphProperties(properties: Partial<SupervisorNode>) {
    await this.graph.updateNode(this.id, properties);
  }

  // ===========================================================================
  // PUBLIC INTERFACE (for testing)
  // ===========================================================================

  /**
   * Get supervisor ID
   */
  public getId(): string {
    return this.id;
  }

  /**
   * Get spawned watchers
   */
  public getSpawnedWatchers(): string[] {
    return [...this.spawnedWatchers];
  }

  /**
   * Get monitored directories
   */
  public getMonitoredDirectories(): string[] {
    return Array.from(this.directoryWatchers.keys());
  }
}
