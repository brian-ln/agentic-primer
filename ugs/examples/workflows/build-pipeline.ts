#!/usr/bin/env bun
/**
 * Example: Build Pipeline Workflow
 *
 * Demonstrates linear pipeline: compile → link → test → deploy
 * Classic CI/CD workflow pattern
 */

import { buildWorkflow } from '../../src/messaging/actors/workflow-builder.ts';
import { WorkflowOrchestrator } from '../../src/messaging/actors/workflow-orchestrator.ts';
import { TaskActor } from '../../src/messaging/actors/task.ts';
import { RelationshipActor } from '../../src/messaging/actors/relationship.ts';
import { MessageRouter } from '../../src/messaging/router.ts';
import GraphStore from '../../src/graph.ts';
import { ProgramManager } from '../../src/entities/program.ts';
import { address } from '../../src/messaging/message.ts';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

/**
 * Define the workflow
 */
const workflow = buildWorkflow('build-pipeline', 'CI/CD Build Pipeline')
  .describe('Automated build, test, and deployment pipeline')
  .task('compile', {
    title: 'Compile TypeScript',
    description: 'Transpile TS to JS with type checking',
    priority: 'P0'
  })
  .task('link', {
    title: 'Link Dependencies',
    description: 'Bundle and link external dependencies',
    dependsOn: ['compile'],
    priority: 'P0'
  })
  .task('test', {
    title: 'Run Test Suite',
    description: 'Execute unit and integration tests',
    dependsOn: ['link'],
    priority: 'P0'
  })
  .task('deploy', {
    title: 'Deploy to Production',
    description: 'Push artifacts to production environment',
    dependsOn: ['test'],
    priority: 'P1'
  })
  .build();

/**
 * Execute the workflow
 */
async function main() {
  console.log('Build Pipeline Workflow\n');
  console.log(buildWorkflow('build-pipeline', 'CI/CD Build Pipeline')
    .describe('Automated build, test, and deployment pipeline')
    .task('compile', {
      title: 'Compile TypeScript',
      description: 'Transpile TS to JS with type checking',
      priority: 'P0'
    })
    .task('link', {
      title: 'Link Dependencies',
      description: 'Bundle and link external dependencies',
      dependsOn: ['compile'],
      priority: 'P0'
    })
    .task('test', {
      title: 'Run Test Suite',
      description: 'Execute unit and integration tests',
      dependsOn: ['link'],
      priority: 'P0'
    })
    .task('deploy', {
      title: 'Deploy to Production',
      description: 'Push artifacts to production environment',
      dependsOn: ['test'],
      priority: 'P1'
    })
    .visualize()
  );
  console.log('\n');

  // Create temp directory for demo
  const testDir = await mkdtemp(join(tmpdir(), 'workflow-example-'));

  try {
    // Setup actor system
    const store = new GraphStore(testDir);
    await store.initialize();
    const programManager = new ProgramManager(store);
    const router = new MessageRouter(store, programManager);

  // Create actors
  const taskActor = new TaskActor('tasks', router, store);
  const relationshipActor = new RelationshipActor('relationships', router);
  const orchestrator = new WorkflowOrchestrator('orchestrator', router, store);

  // Register actors
  router.registerActor('domain/tasks', taskActor);
  router.registerActor('domain/relationships', relationshipActor);
  router.registerActor('domain/orchestrator', orchestrator);

  // Define workflow
  console.log('Defining workflow...');
  const defineResp = await orchestrator.receive({
    id: 'msg-1',
    to: address('domain/orchestrator'),
    from: address('domain/system'),
    type: 'define-workflow',
    payload: workflow,
    timestamp: Date.now(),
    pattern: 'ask'
  });

  if (defineResp.error) {
    console.error('Failed to define workflow:', defineResp.error);
    return;
  }

  console.log('Workflow defined successfully\n');

  // Execute workflow
  console.log('Executing workflow...');
  const execResp = await orchestrator.receive({
    id: 'msg-2',
    to: address('domain/orchestrator'),
    from: address('domain/system'),
    type: 'execute-workflow',
    payload: { workflowId: 'build-pipeline' },
    timestamp: Date.now(),
    pattern: 'ask'
  });

  if (execResp.error) {
    console.error('Failed to execute workflow:', execResp.error);
    return;
  }

  const { executionId, taskIds } = execResp.payload!;
  console.log(`Workflow execution started: ${executionId}`);
  console.log('Task IDs:', taskIds);
  console.log('\n');

  // Simulate task execution
  console.log('Simulating task execution...\n');

  for (const [taskName, taskId] of Object.entries(taskIds as Record<string, string>)) {
    console.log(`→ Starting ${taskName} (${taskId})`);

    // Get task
    const getResp = await taskActor.receive({
      id: `msg-get-${taskId}`,
      to: address('domain/tasks'),
      from: address('domain/system'),
      type: 'get',
      payload: { id: taskId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    if (getResp.error) {
      console.error(`Failed to get task ${taskId}:`, getResp.error);
      continue;
    }

    const task = getResp.payload?.task;
    console.log(`  Status: ${task.lifecycle}`);

    // If running, complete it
    if (task.lifecycle === 'in_progress') {
      console.log(`  Completing ${taskName}...`);

      const completeResp = await taskActor.receive({
        id: `msg-complete-${taskId}`,
        to: address('domain/tasks'),
        from: address('domain/system'),
        type: 'complete',
        payload: {
          id: taskId,
          result: { status: 'success', timestamp: Date.now() }
        },
        timestamp: Date.now(),
        pattern: 'ask'
      });

      if (completeResp.error) {
        console.error(`Failed to complete task ${taskId}:`, completeResp.error);
        continue;
      }

      console.log(`  ✓ ${taskName} completed`);

      // Notify orchestrator
      await orchestrator.receive({
        id: `msg-notify-${taskId}`,
        to: address('domain/orchestrator'),
        from: address('domain/tasks'),
        type: 'task-completed',
        payload: { taskId },
        timestamp: Date.now(),
        pattern: 'tell'
      });

      console.log(`  Propagating completion...\n`);

      // Small delay for orchestrator to process
      await new Promise(resolve => setTimeout(resolve, 100));
    }
  }

  // Get final execution state
  console.log('Fetching final execution state...');
  const execStateResp = await orchestrator.receive({
    id: 'msg-final',
    to: address('domain/orchestrator'),
    from: address('domain/system'),
    type: 'get-execution',
    payload: { id: executionId },
    timestamp: Date.now(),
    pattern: 'ask'
  });

  if (execStateResp.error) {
    console.error('Failed to get execution state:', execStateResp.error);
    return;
  }

  const execution = execStateResp.payload?.execution;
  console.log('\nFinal Execution State:');
  console.log(`  Status: ${execution.status}`);
  console.log(`  Started: ${new Date(execution.startedAt).toISOString()}`);
  if (execution.completedAt) {
    console.log(`  Completed: ${new Date(execution.completedAt).toISOString()}`);
    console.log(`  Duration: ${execution.completedAt - execution.startedAt}ms`);
  }
  console.log('\nTask States:');
  for (const [taskId, state] of Object.entries(execution.taskStates)) {
    console.log(`  ${taskId}: ${state}`);
  }

  console.log('\n✓ Build pipeline workflow completed successfully!');
  } finally {
    // Clean up temp directory
    await rm(testDir, { recursive: true, force: true });
  }
}

if (import.meta.main) {
  main().catch(console.error);
}

export { workflow };
