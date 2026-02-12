#!/usr/bin/env bun
/**
 * Example 1: System Actors Basics
 *
 * Demonstrates core system actors:
 * - SchedulerActor for time-based operations
 * - StorageActor for persistent data
 * - HTTPClientActor for HTTP requests
 *
 * Run: bun run examples/01-system-actors-basics.ts
 */

import { Actor } from '../src/messaging/actor.ts';
import { MessageRouter } from '../src/messaging/router.ts';
import { GraphStore } from '../src/graph.ts';
import { address, createMessage, type Message, type MessageResponse } from '../src/messaging/message.ts';
import { createResponse } from '../src/messaging/message.ts';
import { SchedulerActor } from '../src/system-actors/scheduler.ts';
import { StorageActor } from '../src/system-actors/storage.ts';
import { HTTPClientActor } from '../src/system-actors/http-client.ts';

// Example actor using system actors
class TaskProcessorActor extends Actor {
  private taskCount = 0;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'process-task') {
      this.taskCount++;
      console.log(`\n[TaskProcessor] Processing task #${this.taskCount}`);

      // 1. Store task state
      await this.ask(
        address('/system/storage'),
        'storage.set',
        {
          key: `task.${this.taskCount}`,
          value: {
            id: this.taskCount,
            status: 'processing',
            timestamp: Date.now()
          }
        }
      );
      console.log(`✓ Stored task state`);

      // 2. Fetch external data
      const apiResponse = await this.ask(
        address('/system/http'),
        'http.get',
        { url: 'https://httpbin.org/uuid' }
      );

      if (apiResponse.success) {
        console.log(`✓ Fetched UUID: ${apiResponse.payload.body.uuid}`);
      }

      // 3. Schedule delayed completion
      await this.schedule(2000, 'complete-task');
      console.log(`✓ Scheduled completion in 2 seconds`);

      return createResponse(message, {
        taskId: this.taskCount,
        status: 'scheduled'
      });
    }

    if (message.type === 'complete-task') {
      console.log(`\n[TaskProcessor] Completing task #${this.taskCount}`);

      // Update task state
      await this.ask(
        address('/system/storage'),
        'storage.set',
        {
          key: `task.${this.taskCount}`,
          value: {
            id: this.taskCount,
            status: 'completed',
            timestamp: Date.now()
          }
        }
      );
      console.log(`✓ Task completed and stored`);

      return createResponse(message, { completed: true });
    }

    return createResponse(message, { error: 'Unknown message type' });
  }
}

// Main execution
async function main() {
  console.log('=== System Actors Example ===\n');

  // Setup
  const store = new GraphStore();
  const router = new MessageRouter(store);

  // Register system actors
  const scheduler = new SchedulerActor(router, { clock: 'real' });
  router.registerActor('/system/scheduler', scheduler);

  const storage = new StorageActor('storage', router, {
    allowedKeys: ['*'], // Allow all keys for demo
    maxSize: 10 * 1024 * 1024 // 10MB
  });
  router.registerActor('/system/storage', storage);

  const http = new HTTPClientActor('http', router, {
    methods: ['GET', 'POST'],
    allowedHosts: ['httpbin.org'],
    rateLimit: { requests: 10, window: 60000 },
    timeout: 5000
  });
  router.registerActor('/system/http', http);

  // Register task processor
  const taskProcessor = new TaskProcessorActor('task-processor', router);
  router.registerActor('/tasks/processor', taskProcessor);

  // Process a task
  console.log('Starting task processing...');
  const result = await taskProcessor.ask(
    address('/tasks/processor'),
    'process-task',
    {}
  );

  console.log(`\nTask initiated:`, result.payload);

  // Wait for completion (2s delay + buffer)
  await new Promise(resolve => setTimeout(resolve, 2500));

  // Retrieve final state
  const finalState = await storage.ask(
    address('/system/storage'),
    'storage.get',
    { key: 'task.1' }
  );

  if (finalState.success) {
    console.log(`\nFinal task state:`, finalState.payload.value);
  } else {
    console.log(`\nFinal task state: (not found or incomplete)`);
  }

  console.log('\n=== Example Complete ===');
  process.exit(0);
}

main().catch(console.error);
