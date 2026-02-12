#!/usr/bin/env bun
/**
 * Capability Configuration Examples
 *
 * Demonstrates different capability scoping patterns for actors.
 */

import { ActorFactory, CapabilityProfiles } from '../src/messaging/capabilities/factory.ts';
import type { MessageRouter } from '../src/messaging/router.ts';

/**
 * Example configurations for different actor types
 */
export function createCapabilityExamples(router: MessageRouter) {
  const factory = new ActorFactory(router);

  // ============================================================================
  // Workflows Namespace - Full Access
  // ============================================================================

  /**
   * Workflow executor - Full access to workflow data and filesystem
   */
  const workflowExecutor = factory.createActor(WorkflowExecutorActor, {
    id: 'workflow-executor',
    namespace: '/workflows',
    capabilities: CapabilityProfiles.fullAccess(
      '/workflows',
      ['tasks', 'workflows', 'executions', 'state'],
      ['/workflows/data', '/workflows/state', '/tmp']
    ),
  });

  /**
   * Task manager - Read/write tasks, read-only workflows
   */
  const taskManager = factory.createActor(TaskManagerActor, {
    id: 'task-manager',
    namespace: '/workflows',
    capabilities: {
      namespace: '/workflows',
      storage: {
        allowedTables: ['tasks', 'workflows'],
        operations: ['read', 'write', 'subscribe'],
      },
      fs: {
        allowedPaths: ['/workflows/tasks'],
        operations: ['read', 'write'],
      },
    },
  });

  // ============================================================================
  // Session Knowledge Namespace - Isolated Storage
  // ============================================================================

  /**
   * Session indexer - Full access to session database, read-only logs
   */
  const sessionIndexer = factory.createActor(SessionIndexerActor, {
    id: 'session-indexer',
    namespace: '/session-knowledge',
    capabilities: {
      namespace: '/session-knowledge',
      storage: {
        allowedTables: ['sessions', 'messages', 'embeddings', 'session_decisions'],
        operations: ['read', 'write', 'delete', 'subscribe'],
      },
      fs: {
        allowedPaths: ['/session-logs'],
        operations: ['read'],
      },
    },
  });

  /**
   * Query engine - Read-only access to session data
   */
  const queryEngine = factory.createActor(QueryEngineActor, {
    id: 'query-engine',
    namespace: '/session-knowledge',
    capabilities: CapabilityProfiles.readOnly(
      '/session-knowledge',
      ['sessions', 'messages', 'embeddings', 'session_decisions', 'session_learnings'],
      []
    ),
  });

  // ============================================================================
  // Domain Logic Namespace - Restricted Access
  // ============================================================================

  /**
   * Business logic actor - Read-only storage, no filesystem
   */
  const businessLogic = factory.createActor(BusinessLogicActor, {
    id: 'business-logic',
    namespace: '/domain',
    capabilities: CapabilityProfiles.storageOnly('/domain', ['tasks', 'workflows'], ['read']),
  });

  /**
   * Pure computation actor - No capabilities
   */
  const computeActor = factory.createActor(ComputeActor, {
    id: 'compute-actor',
    namespace: '/domain',
    capabilities: CapabilityProfiles.noCapabilities('/domain'),
  });

  // ============================================================================
  // Admin Namespace - Schema Management
  // ============================================================================

  /**
   * Migration runner - Admin access to all tables
   */
  const migrationRunner = factory.createActor(MigrationRunnerActor, {
    id: 'migration-runner',
    namespace: '/admin',
    capabilities: {
      namespace: '/workflows',
      storage: {
        allowedTables: ['*'], // All tables
        operations: ['read', 'write', 'delete', 'admin'], // Schema changes allowed
      },
    },
  });

  return {
    workflowExecutor,
    taskManager,
    sessionIndexer,
    queryEngine,
    businessLogic,
    computeActor,
    migrationRunner,
  };
}

// ============================================================================
// Example Actor Implementations
// ============================================================================

import { Actor } from '../src/messaging/actor.ts';
import type { Message, MessageResponse } from '../src/messaging/message.ts';
import { createResponse } from '../src/messaging/message.ts';
import type { StorageCapability } from '../src/messaging/capabilities/storage.ts';
import type { FileSystemCapability } from '../src/messaging/capabilities/filesystem.ts';

/**
 * WorkflowExecutorActor - Full access example
 */
class WorkflowExecutorActor extends Actor {
  storage!: StorageCapability; // Attached by factory
  fs!: FileSystemCapability; // Attached by factory

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'execute-workflow') {
      // Can access all configured tables
      const workflows = await this.storage.query(
        'SELECT * FROM workflows WHERE id = ?',
        [message.payload.workflowId]
      );

      // Can read/write to configured paths
      const state = await this.fs.read(`/workflows/state/${message.payload.workflowId}.json`);

      return createResponse(message, {
        success: true,
        payload: { workflow: workflows[0], state },
      });
    }

    return createResponse(message, { success: false, error: 'Unknown message type' });
  }
}

/**
 * TaskManagerActor - Scoped access example
 */
class TaskManagerActor extends Actor {
  storage!: StorageCapability;
  fs!: FileSystemCapability;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'get-tasks') {
      // ✅ Allowed - tasks table in allowedTables
      const tasks = await this.storage.query('SELECT * FROM tasks WHERE status = ?', ['open']);

      return createResponse(message, { success: true, payload: { tasks } });
    }

    if (message.type === 'update-task') {
      // ✅ Allowed - tasks table, write operation permitted
      await this.storage.execute('UPDATE tasks SET status = ? WHERE id = ?', [
        message.payload.status,
        message.payload.taskId,
      ]);

      return createResponse(message, { success: true });
    }

    if (message.type === 'get-users') {
      try {
        // ❌ Denied - users table not in allowedTables
        const users = await this.storage.query('SELECT * FROM users');
      } catch (error: any) {
        this.logError('Capability violation', { error: error.message });
        return createResponse(message, { success: false, error: error.message });
      }
    }

    return createResponse(message, { success: false, error: 'Unknown message type' });
  }
}

/**
 * SessionIndexerActor - Storage + read-only filesystem
 */
class SessionIndexerActor extends Actor {
  storage!: StorageCapability;
  fs!: FileSystemCapability;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'index-session') {
      // Read session log from filesystem
      const logContent = await this.fs.read(
        `/session-logs/${message.payload.sessionId}.jsonl`
      );

      // Write to database
      await this.storage.execute(
        'INSERT INTO sessions (id, content, indexed_at) VALUES (?, ?, ?)',
        [message.payload.sessionId, logContent, Date.now()]
      );

      return createResponse(message, { success: true });
    }

    return createResponse(message, { success: false, error: 'Unknown message type' });
  }
}

/**
 * QueryEngineActor - Read-only storage
 */
class QueryEngineActor extends Actor {
  storage!: StorageCapability;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'query-decisions') {
      // ✅ Allowed - read operation
      const decisions = await this.storage.query(
        'SELECT * FROM session_decisions ORDER BY timestamp DESC LIMIT ?',
        [message.payload.limit || 10]
      );

      return createResponse(message, { success: true, payload: { decisions } });
    }

    if (message.type === 'delete-decision') {
      try {
        // ❌ Denied - delete operation not permitted
        await this.storage.delete('DELETE FROM session_decisions WHERE id = ?', [
          message.payload.id,
        ]);
      } catch (error: any) {
        this.logError('Capability violation', { error: error.message });
        return createResponse(message, { success: false, error: error.message });
      }
    }

    return createResponse(message, { success: false, error: 'Unknown message type' });
  }
}

/**
 * BusinessLogicActor - Read-only storage, no filesystem
 */
class BusinessLogicActor extends Actor {
  storage!: StorageCapability;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'validate-workflow') {
      // ✅ Allowed - read operation
      const tasks = await this.storage.query('SELECT * FROM tasks WHERE workflow_id = ?', [
        message.payload.workflowId,
      ]);

      // Pure business logic (no I/O)
      const isValid = this.validateWorkflow(tasks);

      return createResponse(message, { success: true, payload: { isValid } });
    }

    return createResponse(message, { success: false, error: 'Unknown message type' });
  }

  private validateWorkflow(tasks: any[]): boolean {
    // Pure function - no capabilities needed
    return tasks.length > 0 && tasks.every(t => t.status === 'done');
  }
}

/**
 * ComputeActor - No capabilities, pure computation
 */
class ComputeActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'compute-sum') {
      // Pure computation - no I/O
      const sum = message.payload.numbers.reduce((a: number, b: number) => a + b, 0);

      return createResponse(message, { success: true, payload: { sum } });
    }

    return createResponse(message, { success: false, error: 'Unknown message type' });
  }
}

/**
 * MigrationRunnerActor - Admin access for schema changes
 */
class MigrationRunnerActor extends Actor {
  storage!: StorageCapability;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'run-migration') {
      // ✅ Allowed - admin operation
      await this.storage.execute('ALTER TABLE tasks ADD COLUMN priority INTEGER DEFAULT 0');

      this.logInfo('Migration completed', { migration: message.payload.name });

      return createResponse(message, { success: true });
    }

    return createResponse(message, { success: false, error: 'Unknown message type' });
  }
}
