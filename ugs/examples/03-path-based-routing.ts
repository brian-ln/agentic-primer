#!/usr/bin/env bun
/**
 * Example 3: Path-Based Routing
 *
 * Demonstrates hierarchical actor organization:
 * - Path-based addressing (/workflows/tasks/task-123)
 * - Namespaced system actors
 * - Access control through routing
 *
 * Run: bun run examples/03-path-based-routing.ts
 */

import { Actor } from '../src/messaging/actor.ts';
import { MessageRouter } from '../src/messaging/router.ts';
import { GraphStore } from '../src/graph.ts';
import { address, type Message, type MessageResponse } from '../src/messaging/message.ts';
import { createResponse } from '../src/messaging/message.ts';
import { StorageActor } from '../src/system-actors/storage.ts';
import { HTTPClientActor } from '../src/system-actors/http-client.ts';

// Workflow task actor
class WorkflowTaskActor extends Actor {
  constructor(id: string, router: MessageRouter, private taskId: string) {
    super(id, router);
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'execute') {
      console.log(`\n[Task ${this.taskId}] Executing workflow task`);

      // Access workflow-namespaced storage
      await this.ask(
        address('/workflows/system/storage'),
        'storage.set',
        {
          key: `task.${this.taskId}.status`,
          value: 'running'
        }
      );
      console.log(`✓ Stored in workflow storage`);

      // Access workflow-namespaced HTTP client
      const response = await this.ask(
        address('/workflows/system/http'),
        'http.get',
        { url: 'https://httpbin.org/json' }
      );

      if (response.success) {
        console.log(`✓ HTTP request successful`);
      }

      return createResponse(message, {
        taskId: this.taskId,
        status: 'completed'
      });
    }

    return createResponse(message, { error: 'Unknown message type' });
  }
}

// Domain entity actor (cannot access workflow resources)
class DomainEntityActor extends Actor {
  constructor(id: string, router: MessageRouter, private entityId: string) {
    super(id, router);
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'process') {
      console.log(`\n[Entity ${this.entityId}] Processing domain logic`);

      // Access domain-namespaced storage (different from workflow storage)
      await this.ask(
        address('/domain/system/storage'),
        'storage.set',
        {
          key: `entity.${this.entityId}.data`,
          value: { id: this.entityId, updated: Date.now() }
        }
      );
      console.log(`✓ Stored in domain storage`);

      // Try to access workflow storage (will fail - different namespace)
      try {
        await this.ask(
          address('/workflows/system/storage'),
          'storage.get',
          { key: 'task.123.status' }
        );
        console.log(`❌ Should not access workflow storage!`);
      } catch (error) {
        console.log(`✓ Correctly isolated from workflow namespace`);
      }

      return createResponse(message, {
        entityId: this.entityId,
        isolated: true
      });
    }

    return createResponse(message, { error: 'Unknown message type' });
  }
}

// Main execution
async function main() {
  console.log('=== Path-Based Routing Example ===\n');

  // Setup
  const store = new GraphStore();
  const router = new MessageRouter(store);

  // Register workflow-namespaced system actors
  console.log('Registering workflow system actors at /workflows/system/*');
  const workflowStorage = new StorageActor('workflow-storage', router, {
    allowedKeys: ['task.*', 'workflow.*'],
    maxSize: 10 * 1024 * 1024
  });
  router.registerActor('/workflows/system/storage', workflowStorage);

  const workflowHttp = new HTTPClientActor('workflow-http', router, {
    methods: ['GET', 'POST'],
    allowedHosts: ['httpbin.org', 'api.example.com'],
    rateLimit: { requests: 100, window: 60000 },
    timeout: 5000
  });
  router.registerActor('/workflows/system/http', workflowHttp);

  // Register domain-namespaced system actors (different config!)
  console.log('Registering domain system actors at /domain/system/*');
  const domainStorage = new StorageActor('domain-storage', router, {
    allowedKeys: ['entity.*', 'user.*'],
    maxSize: 50 * 1024 * 1024 // Different size limit
  });
  router.registerActor('/domain/system/storage', domainStorage);

  // Register workflow task actors
  console.log('\nRegistering workflow tasks at /workflows/tasks/*');
  const task1 = new WorkflowTaskActor('task-1', router, '123');
  router.registerActor('/workflows/tasks/task-123', task1);

  const task2 = new WorkflowTaskActor('task-2', router, '456');
  router.registerActor('/workflows/tasks/task-456', task2);

  // Register domain entity actors
  console.log('Registering domain entities at /domain/entities/*');
  const entity1 = new DomainEntityActor('entity-1', router, 'user-789');
  router.registerActor('/domain/entities/user-789', entity1);

  // Execute workflow task
  console.log('\n--- Workflow Task Execution ---');
  await task1.ask(
    address('/workflows/tasks/task-123'),
    'execute',
    {}
  );

  // Execute domain entity
  console.log('\n--- Domain Entity Processing ---');
  await entity1.ask(
    address('/domain/entities/user-789'),
    'process',
    {}
  );

  // Demonstrate namespace isolation
  console.log('\n--- Namespace Isolation ---');
  console.log('\nWorkflow storage contains:');
  const workflowData = await workflowStorage.ask(
    address('/workflows/system/storage'),
    'storage.get',
    { key: 'task.123.status' }
  );
  console.log('  task.123.status:', workflowData.payload.value);

  console.log('\nDomain storage contains:');
  const domainData = await domainStorage.ask(
    address('/domain/system/storage'),
    'storage.get',
    { key: 'entity.user-789.data' }
  );
  console.log('  entity.user-789.data:', domainData.payload.value);

  console.log('\n--- Key Benefits ---');
  console.log('✓ Human-readable paths: /workflows/tasks/task-123');
  console.log('✓ Natural namespacing: workflows vs domain');
  console.log('✓ Isolated capabilities: different storage configs');
  console.log('✓ Access control via routing: domain cannot access workflow storage');

  console.log('\n=== Example Complete ===');
  process.exit(0);
}

main().catch(console.error);
