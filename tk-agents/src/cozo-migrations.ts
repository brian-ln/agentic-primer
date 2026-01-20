/**
 * CozoDB Schema Migrations
 *
 * Provides utilities for:
 * - Schema initialization and validation
 * - Schema version management
 * - Migration rollback and recovery
 * - Data import/export for schema changes
 *
 * Migration Strategy:
 * 1. Schema versions are tracked in a metadata relation
 * 2. Migrations are idempotent (can be run multiple times safely)
 * 3. Each migration has up/down operations
 * 4. Migrations can be tested in isolation before applying
 *
 * Based on: DUAL_WRITE_IMPLEMENTATION.md, CozoReconciler pattern
 */

import type { CozoClient } from "./cozo-client";
import type { CozoWasmClient } from "./cozo-wasm-client";
import { EventLog, type Event } from "./persistence/event-log";
import { TaskMutations, updateTaskStatus } from "./cozo-schema";

/**
 * CozoDB client union type
 */
type AnyCozoClient = CozoClient | CozoWasmClient;

/**
 * Schema version metadata
 */
export interface SchemaVersion {
  version: number;
  name: string;
  appliedAt: string;
  description?: string;
}

/**
 * Migration definition
 */
export interface Migration {
  version: number;
  name: string;
  description: string;
  up: (client: AnyCozoClient) => Promise<void>;
  down: (client: AnyCozoClient) => Promise<void>;
}

/**
 * Migration result
 */
export interface MigrationResult {
  success: boolean;
  version: number;
  name: string;
  error?: string;
  duration: number;
}

/**
 * Schema Migration Manager
 *
 * Handles schema versioning, migrations, and validation.
 */
export class SchemaMigrationManager {
  constructor(private client: AnyCozoClient) {}

  /**
   * Initialize schema metadata relation
   *
   * Creates a relation to track applied migrations.
   */
  async initializeMetadata(): Promise<void> {
    try {
      await this.client.run(`
        :create schema_migrations {
          version: Int,
          name: String,
          applied_at: String,
          description: String,
        }
      `);
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      if (!errorMsg.includes("already exists") && !errorMsg.includes("conflicts with an existing one")) {
        throw error;
      }
    }
  }

  /**
   * Get current schema version
   */
  async getCurrentVersion(): Promise<number> {
    try {
      const result = await this.client.run(`
        ?[version] := *schema_migrations{version}
        :order -version
        :limit 1
      `);

      if (result.rows.length === 0) {
        return 0; // No migrations applied yet
      }

      return result.rows[0][0] as number;
    } catch {
      return 0; // Metadata relation doesn't exist yet
    }
  }

  /**
   * Get all applied migrations
   */
  async getAppliedMigrations(): Promise<SchemaVersion[]> {
    try {
      const result = await this.client.run(`
        ?[version, name, applied_at, description] :=
          *schema_migrations{version, name, applied_at, description}
        :order version
      `);

      return result.rows.map((row) => ({
        version: row[0] as number,
        name: row[1] as string,
        appliedAt: row[2] as string,
        description: row[3] as string | undefined,
      }));
    } catch {
      return [];
    }
  }

  /**
   * Record migration as applied
   */
  private async recordMigration(migration: Migration): Promise<void> {
    await this.client.run(
      `?[version, name, applied_at, description] <- [[$version, $name, $applied_at, $description]]
       :put schema_migrations {version, name, applied_at, description}`,
      {
        version: migration.version,
        name: migration.name,
        applied_at: new Date().toISOString(),
        description: migration.description || null,
      }
    );
  }

  /**
   * Remove migration record
   */
  private async removeMigrationRecord(version: number): Promise<void> {
    await this.client.run(
      `?[version, name, applied_at, description] :=
         *schema_migrations{version, name, applied_at, description},
         version == $version
       :rm schema_migrations {version, name, applied_at, description}`,
      { version }
    );
  }

  /**
   * Apply a single migration
   */
  async applyMigration(migration: Migration): Promise<MigrationResult> {
    const startTime = Date.now();

    try {
      // Check if migration already applied
      const currentVersion = await this.getCurrentVersion();
      if (currentVersion >= migration.version) {
        return {
          success: false,
          version: migration.version,
          name: migration.name,
          error: `Migration ${migration.version} already applied (current: ${currentVersion})`,
          duration: Date.now() - startTime,
        };
      }

      // Apply migration
      await migration.up(this.client);

      // Record migration
      await this.recordMigration(migration);

      return {
        success: true,
        version: migration.version,
        name: migration.name,
        duration: Date.now() - startTime,
      };
    } catch (error) {
      return {
        success: false,
        version: migration.version,
        name: migration.name,
        error: error instanceof Error ? error.message : String(error),
        duration: Date.now() - startTime,
      };
    }
  }

  /**
   * Rollback a migration
   */
  async rollbackMigration(migration: Migration): Promise<MigrationResult> {
    const startTime = Date.now();

    try {
      // Check if migration is applied
      const currentVersion = await this.getCurrentVersion();
      if (currentVersion < migration.version) {
        return {
          success: false,
          version: migration.version,
          name: migration.name,
          error: `Migration ${migration.version} not applied (current: ${currentVersion})`,
          duration: Date.now() - startTime,
        };
      }

      // Rollback migration
      await migration.down(this.client);

      // Remove migration record
      await this.removeMigrationRecord(migration.version);

      return {
        success: true,
        version: migration.version,
        name: migration.name,
        duration: Date.now() - startTime,
      };
    } catch (error) {
      return {
        success: false,
        version: migration.version,
        name: migration.name,
        error: error instanceof Error ? error.message : String(error),
        duration: Date.now() - startTime,
      };
    }
  }

  /**
   * Apply all pending migrations
   */
  async migrate(migrations: Migration[]): Promise<MigrationResult[]> {
    const currentVersion = await this.getCurrentVersion();
    const pendingMigrations = migrations.filter((m) => m.version > currentVersion);

    // Sort by version
    pendingMigrations.sort((a, b) => a.version - b.version);

    const results: MigrationResult[] = [];

    for (const migration of pendingMigrations) {
      console.log(`Applying migration ${migration.version}: ${migration.name}...`);
      const result = await this.applyMigration(migration);
      results.push(result);

      if (!result.success) {
        console.error(`Migration ${migration.version} failed: ${result.error}`);
        break; // Stop on first failure
      }

      console.log(`Migration ${migration.version} completed in ${result.duration}ms`);
    }

    return results;
  }

  /**
   * Rollback to a specific version
   */
  async rollbackTo(targetVersion: number, migrations: Migration[]): Promise<MigrationResult[]> {
    const currentVersion = await this.getCurrentVersion();

    if (currentVersion <= targetVersion) {
      console.log(`Already at version ${currentVersion}, no rollback needed`);
      return [];
    }

    // Find migrations to rollback (in reverse order)
    const toRollback = migrations
      .filter((m) => m.version > targetVersion && m.version <= currentVersion)
      .sort((a, b) => b.version - a.version); // Descending order

    const results: MigrationResult[] = [];

    for (const migration of toRollback) {
      console.log(`Rolling back migration ${migration.version}: ${migration.name}...`);
      const result = await this.rollbackMigration(migration);
      results.push(result);

      if (!result.success) {
        console.error(`Rollback of ${migration.version} failed: ${result.error}`);
        break; // Stop on first failure
      }

      console.log(`Rollback ${migration.version} completed in ${result.duration}ms`);
    }

    return results;
  }
}

/**
 * EventLog Replay for CozoDB Recovery
 *
 * Rebuilds CozoDB from EventLog when schema changes or corruption occurs.
 */
export class CozoReconciler {
  constructor(
    private eventLog: EventLog,
    private client: AnyCozoClient
  ) {}

  /**
   * Rebuild CozoDB from EventLog (full replay)
   *
   * Use cases:
   * - CozoDB database corruption
   * - Schema migration
   * - Accumulated sync failures
   */
  async rebuildFromEventLog(): Promise<{ successCount: number; errorCount: number }> {
    console.log("Rebuilding CozoDB from EventLog...");

    // Clear existing CozoDB data
    try {
      await this.client.run(`:clear work`);
      await this.client.run(`:clear dependencies`);
      await this.client.run(`:clear task_labels`);
    } catch (error) {
      console.error("Failed to clear CozoDB relations:", error);
      throw error;
    }

    // Replay all events
    let successCount = 0;
    let errorCount = 0;

    await this.eventLog.replay(async (event) => {
      try {
        switch (event.type) {
          case "task_created":
            await this.replayTaskCreated(event);
            break;
          case "task_start":
          case "task_complete":
          case "task_block":
            await this.replayTaskUpdated(event);
            break;
          case "edge_created":
            await this.replayEdgeCreated(event);
            break;
          case "task_deleted":
            await this.replayTaskDeleted(event);
            break;
          case "cozo_write_failed":
            // Skip - these are metadata events
            break;
          default:
            console.warn(`Unknown event type during replay: ${event.type}`);
        }
        successCount++;
      } catch (error) {
        console.error(`Failed to replay event ${event.type} for ${event.nodeId}:`, error);
        errorCount++;
      }
    });

    console.log(`Replay complete: ${successCount} events applied, ${errorCount} errors`);

    return { successCount, errorCount };
  }

  /**
   * Replay only failed CozoDB writes
   *
   * Use case: Retry recent failures without full rebuild
   */
  async replayFailedWrites(): Promise<{ successCount: number; errorCount: number }> {
    const failedWrites = this.eventLog.getEventsByType("cozo_write_failed");

    console.log(`Replaying ${failedWrites.length} failed CozoDB writes...`);

    let successCount = 0;
    let errorCount = 0;

    for (const failureEvent of failedWrites) {
      // Find the original event by nodeId and looking backwards in time
      const allNodeEvents = this.eventLog.getEventsByNode(failureEvent.nodeId);
      const failureIndex = allNodeEvents.indexOf(failureEvent);

      if (failureIndex > 0) {
        const originalEvent = allNodeEvents[failureIndex - 1];

        try {
          // Replay the original operation
          switch (originalEvent.type) {
            case "task_created":
              await this.replayTaskCreated(originalEvent);
              break;
            case "task_start":
            case "task_complete":
            case "task_block":
              await this.replayTaskUpdated(originalEvent);
              break;
            case "edge_created":
              await this.replayEdgeCreated(originalEvent);
              break;
          }
          console.log(`Successfully replayed failed write for ${failureEvent.nodeId}`);
          successCount++;
        } catch (error) {
          console.error(`Retry failed for ${failureEvent.nodeId}:`, error);
          errorCount++;
        }
      }
    }

    console.log(`Replay complete: ${successCount} succeeded, ${errorCount} failed`);

    return { successCount, errorCount };
  }

  private async replayTaskCreated(event: Event): Promise<void> {
    const data = event.data as {
      goal: string;
      state: string;
      priority?: number;
      labels?: string[];
    };

    const title = data.goal.slice(0, 100);

    // Use schema mutation template
    await this.client.run(
      TaskMutations.PUT_TASK({
        id: event.nodeId,
        type: "task",
        status: data.state,
        priority: data.priority ?? null,
        title,
      })
    );

    // Replay labels using schema mutation
    if (data.labels && data.labels.length > 0) {
      for (const label of data.labels) {
        await this.client.run(TaskMutations.ADD_LABEL(event.nodeId, label));
      }
    }
  }

  private async replayTaskUpdated(event: Event): Promise<void> {
    const data = event.data as { newState: string };

    // Use schema helper for status updates (handles two-phase update)
    await updateTaskStatus(this.client, event.nodeId, data.newState);
  }

  private async replayEdgeCreated(event: Event): Promise<void> {
    const data = event.data as { fromId: string; toId: string };

    // Use schema mutation template
    await this.client.run(TaskMutations.ADD_DEPENDENCY(data.fromId, data.toId));
  }

  private async replayTaskDeleted(event: Event): Promise<void> {
    // Use schema mutation templates for deletion
    await this.client.run(TaskMutations.DELETE_TASK(event.nodeId));
    await this.client.run(TaskMutations.DELETE_TASK_DEPENDENCIES(event.nodeId));
    await this.client.run(TaskMutations.DELETE_TASK_LABELS(event.nodeId));
  }
}

/**
 * Built-in migrations
 */
export const MIGRATIONS: Migration[] = [
  {
    version: 1,
    name: "initial_schema",
    description: "Create initial schema with work, dependencies, and task_labels relations",
    up: async (client) => {
      // Create work relation (id is the primary key)
      try {
        await client.run(`
          :create work {
            id: String
            =>
            type: String,
            status: String,
            priority: Int?,
            title: String,
          }
        `);
      } catch (e: any) {
        if (!e.message.includes("already exists") && !e.message.includes("conflicts")) throw e;
      }

      // Create dependencies relation (task, dep is the compound key)
      try {
        await client.run(`
          :create dependencies {
            task: String,
            dep: String,
          }
        `);
      } catch (e: any) {
        if (!e.message.includes("already exists") && !e.message.includes("conflicts")) throw e;
      }

      // Create task_labels relation (task, label is the compound key)
      try {
        await client.run(`
          :create task_labels {
            task: String,
            label: String,
          }
        `);
      } catch (e: any) {
        if (!e.message.includes("already exists") && !e.message.includes("conflicts")) throw e;
      }
    },
    down: async (client) => {
      await client.run(`:remove work`);
      await client.run(`:remove dependencies`);
      await client.run(`:remove task_labels`);
    },
  },

  {
    version: 2,
    name: "add_knowledge_relation",
    description: "Add knowledge relation for knowledge nodes",
    up: async (client) => {
      try {
        await client.run(`
          :create knowledge {
            id: String
            =>
            type: String,
            title: String,
            content: String,
            source: String?,
            tags: String?,
          }
        `);
      } catch (e: any) {
        if (!e.message.includes("already exists") && !e.message.includes("conflicts")) throw e;
      }
    },
    down: async (client) => {
      await client.run(`:remove knowledge`);
    },
  },

  {
    version: 3,
    name: "add_edges_relation",
    description: "Add generic edges relation for all edge types",
    up: async (client) => {
      try {
        await client.run(`
          :create edges {
            id: String
            =>
            from_id: String,
            to_id: String,
            edge_type: String,
            properties: String?,
          }
        `);
      } catch (e: any) {
        if (!e.message.includes("already exists") && !e.message.includes("conflicts")) throw e;
      }
    },
    down: async (client) => {
      await client.run(`:remove edges`);
    },
  },
];

/**
 * Example usage
 */
if (import.meta.main) {
  console.log("🔧 CozoDB Schema Migrations\n");
  console.log("=".repeat(60) + "\n");

  // This would need a real CozoClient instance
  // For demonstration, we'll show the API
  console.log("Usage Example:");
  console.log(`
import { SchemaMigrationManager, MIGRATIONS } from "./cozo-migrations";
import { CozoClient } from "./cozo-client";

const client = new CozoClient("http://127.0.0.1:9070");
const migrator = new SchemaMigrationManager(client);

// Initialize metadata
await migrator.initializeMetadata();

// Check current version
const version = await migrator.getCurrentVersion();
console.log(\`Current schema version: \${version}\`);

// Apply all pending migrations
const results = await migrator.migrate(MIGRATIONS);
console.log("Migration results:", results);

// Rollback to version 1
await migrator.rollbackTo(1, MIGRATIONS);
  `);

  console.log("\nAvailable migrations:");
  for (const migration of MIGRATIONS) {
    console.log(`  v${migration.version}: ${migration.name} - ${migration.description}`);
  }

  console.log("\n✅ Migrations module loaded successfully!\n");
}
