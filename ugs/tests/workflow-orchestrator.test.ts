#!/usr/bin/env bun
/**
 * Tests for WorkflowOrchestrator
 *
 * Tests:
 * - Workflow definition and execution
 * - Dependency resolution
 * - Auto-execution of ready tasks
 * - Linear, parallel, and diamond patterns
 * - Error handling and cycle detection
 */

import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { WorkflowOrchestrator } from '../src/messaging/actors/workflow-orchestrator.ts';
import { TaskActor } from '../src/messaging/actors/task.ts';
import { RelationshipActor } from '../src/messaging/actors/relationship.ts';
import { MessageRouter } from '../src/messaging/router.ts';
import GraphStore from '../src/graph.ts';
import { ProgramManager } from '../src/entities/program.ts';
import { address } from '@agentic-primer/actors';
import { buildWorkflow, linearPipeline, diamond } from '../src/messaging/actors/workflow-builder.ts';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

describe('WorkflowOrchestrator', () => {
  let store: GraphStore;
  let router: MessageRouter;
  let taskActor: TaskActor;
  let relationshipActor: RelationshipActor;
  let orchestrator: WorkflowOrchestrator;
  let testDir: string;

  beforeEach(async () => {
    // Create temp directory for test
    testDir = await mkdtemp(join(tmpdir(), 'workflow-test-'));
    store = new GraphStore(testDir);
    await store.initialize();
    const programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    taskActor = new TaskActor('tasks', router, store);
    relationshipActor = new RelationshipActor('relationships', router);
    orchestrator = new WorkflowOrchestrator('orchestrator', router, store);

    router.registerActor('tasks', taskActor);
    router.registerActor('relationships', relationshipActor);
    router.registerActor('orchestrator', orchestrator);
  });

  afterEach(async () => {
    // Clean up temp directory
    if (testDir) {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  test('should define a workflow', async () => {
    const workflow = buildWorkflow('test-workflow', 'Test Workflow')
      .task('task-1', { title: 'Task 1' })
      .task('task-2', { title: 'Task 2', dependsOn: ['task-1'] })
      .build();

    const resp = await orchestrator.receive({
      id: 'msg-1',
      to: address('orchestrator'),
      from: address('system'),
      type: 'define-workflow',
      payload: workflow,
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(resp.error).toBeUndefined();
    expect(resp.payload?.workflow.id).toBe('test-workflow');
  });

  test('should execute linear pipeline workflow', async () => {
    const workflow = linearPipeline('linear-test', 'Linear Test', [
      { id: 'step-1', title: 'Step 1' },
      { id: 'step-2', title: 'Step 2' },
      { id: 'step-3', title: 'Step 3' }
    ]);

    // Define workflow
    await orchestrator.receive({
      id: 'msg-1',
      to: address('orchestrator'),
      from: address('system'),
      type: 'define-workflow',
      payload: workflow,
      timestamp: Date.now(),
      pattern: 'ask'
    });

    // Execute workflow
    const execResp = await orchestrator.receive({
      id: 'msg-2',
      to: address('orchestrator'),
      from: address('system'),
      type: 'execute-workflow',
      payload: { workflowId: 'linear-test' },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(execResp.error).toBeUndefined();
    expect(execResp.payload?.executionId).toBeDefined();

    const { executionId, taskIds } = execResp.payload!;

    // First task should be running
    const task1Id = taskIds['step-1'];
    const task1 = await taskActor.receive({
      id: 'msg-get-1',
      to: address('tasks'),
      from: address('system'),
      type: 'get',
      payload: { id: task1Id },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(task1.payload?.task.lifecycle).toBe('in_progress');

    // Second task should be pending
    const task2Id = taskIds['step-2'];
    const task2 = await taskActor.receive({
      id: 'msg-get-2',
      to: address('tasks'),
      from: address('system'),
      type: 'get',
      payload: { id: task2Id },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(task2.payload?.task.lifecycle).toBe('pending');
  });

  test('should auto-start dependent task after completion', async () => {
    const workflow = buildWorkflow('auto-start-test', 'Auto Start Test')
      .task('task-a', { title: 'Task A' })
      .task('task-b', { title: 'Task B', dependsOn: ['task-a'] })
      .build();

    // Define and execute
    await orchestrator.receive({
      id: 'msg-1',
      to: address('orchestrator'),
      from: address('system'),
      type: 'define-workflow',
      payload: workflow,
      timestamp: Date.now(),
      pattern: 'ask'
    });

    const execResp = await orchestrator.receive({
      id: 'msg-2',
      to: address('orchestrator'),
      from: address('system'),
      type: 'execute-workflow',
      payload: { workflowId: 'auto-start-test' },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    const { taskIds } = execResp.payload!;
    const taskAId = taskIds['task-a'];
    const taskBId = taskIds['task-b'];

    // Complete task A
    await taskActor.receive({
      id: 'msg-complete',
      to: address('tasks'),
      from: address('system'),
      type: 'complete',
      payload: { id: taskAId, result: { success: true } },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    // Notify orchestrator
    await orchestrator.receive({
      id: 'msg-notify',
      to: address('orchestrator'),
      from: address('tasks'),
      type: 'task-completed',
      payload: { taskId: taskAId },
      timestamp: Date.now(),
      pattern: 'tell'
    });

    // Task B should now be running (auto-started)
    const taskB = await taskActor.receive({
      id: 'msg-get-b',
      to: address('tasks'),
      from: address('system'),
      type: 'get',
      payload: { id: taskBId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(taskB.payload?.task.lifecycle).toBe('in_progress');
  });

  test('should handle diamond pattern (fan-out + fan-in)', async () => {
    const workflow = diamond(
      'diamond-test',
      'Diamond Test',
      { id: 'start', title: 'Start' },
      { id: 'left', title: 'Left Branch' },
      { id: 'right', title: 'Right Branch' },
      { id: 'end', title: 'End' }
    );

    // Define and execute
    await orchestrator.receive({
      id: 'msg-1',
      to: address('orchestrator'),
      from: address('system'),
      type: 'define-workflow',
      payload: workflow,
      timestamp: Date.now(),
      pattern: 'ask'
    });

    const execResp = await orchestrator.receive({
      id: 'msg-2',
      to: address('orchestrator'),
      from: address('system'),
      type: 'execute-workflow',
      payload: { workflowId: 'diamond-test' },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    const { taskIds } = execResp.payload!;

    // Complete start task
    const startId = taskIds['start'];
    await taskActor.receive({
      id: 'msg-complete-start',
      to: address('tasks'),
      from: address('system'),
      type: 'complete',
      payload: { id: startId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    await orchestrator.receive({
      id: 'msg-notify-start',
      to: address('orchestrator'),
      from: address('tasks'),
      type: 'task-completed',
      payload: { taskId: startId },
      timestamp: Date.now(),
      pattern: 'tell'
    });

    // Both left and right should now be running
    const leftId = taskIds['left'];
    const rightId = taskIds['right'];

    const left = await taskActor.receive({
      id: 'msg-get-left',
      to: address('tasks'),
      from: address('system'),
      type: 'get',
      payload: { id: leftId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    const right = await taskActor.receive({
      id: 'msg-get-right',
      to: address('tasks'),
      from: address('system'),
      type: 'get',
      payload: { id: rightId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(left.payload?.task.lifecycle).toBe('in_progress');
    expect(right.payload?.task.lifecycle).toBe('in_progress');

    // End should still be pending (waiting for both)
    const endId = taskIds['end'];
    const end = await taskActor.receive({
      id: 'msg-get-end',
      to: address('tasks'),
      from: address('system'),
      type: 'get',
      payload: { id: endId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(end.payload?.task.lifecycle).toBe('pending');

    // Complete left branch
    await taskActor.receive({
      id: 'msg-complete-left',
      to: address('tasks'),
      from: address('system'),
      type: 'complete',
      payload: { id: leftId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    await orchestrator.receive({
      id: 'msg-notify-left',
      to: address('orchestrator'),
      from: address('tasks'),
      type: 'task-completed',
      payload: { taskId: leftId },
      timestamp: Date.now(),
      pattern: 'tell'
    });

    // End should still be pending (waiting for right)
    const endAfterLeft = await taskActor.receive({
      id: 'msg-get-end-2',
      to: address('tasks'),
      from: address('system'),
      type: 'get',
      payload: { id: endId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(endAfterLeft.payload?.task.lifecycle).toBe('pending');

    // Complete right branch
    await taskActor.receive({
      id: 'msg-complete-right',
      to: address('tasks'),
      from: address('system'),
      type: 'complete',
      payload: { id: rightId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    await orchestrator.receive({
      id: 'msg-notify-right',
      to: address('orchestrator'),
      from: address('tasks'),
      type: 'task-completed',
      payload: { taskId: rightId },
      timestamp: Date.now(),
      pattern: 'tell'
    });

    // Now end should be running
    const endAfterRight = await taskActor.receive({
      id: 'msg-get-end-3',
      to: address('tasks'),
      from: address('system'),
      type: 'get',
      payload: { id: endId },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(endAfterRight.payload?.task.lifecycle).toBe('in_progress');
  });

  test('should detect cycles in workflow definition', () => {
    expect(() => {
      buildWorkflow('cycle-test', 'Cycle Test')
        .task('task-a', { title: 'Task A', dependsOn: ['task-b'] })
        .task('task-b', { title: 'Task B', dependsOn: ['task-a'] })
        .build();
    }).toThrow('Cycle detected');
  });

  test('should list workflows and executions', async () => {
    const workflow1 = buildWorkflow('wf-1', 'Workflow 1')
      .task('task-1', { title: 'Task 1' })
      .build();

    const workflow2 = buildWorkflow('wf-2', 'Workflow 2')
      .task('task-2', { title: 'Task 2' })
      .build();

    // Define workflows
    await orchestrator.receive({
      id: 'msg-1',
      to: address('orchestrator'),
      from: address('system'),
      type: 'define-workflow',
      payload: workflow1,
      timestamp: Date.now(),
      pattern: 'ask'
    });

    await orchestrator.receive({
      id: 'msg-2',
      to: address('orchestrator'),
      from: address('system'),
      type: 'define-workflow',
      payload: workflow2,
      timestamp: Date.now(),
      pattern: 'ask'
    });

    // List workflows
    const listResp = await orchestrator.receive({
      id: 'msg-3',
      to: address('orchestrator'),
      from: address('system'),
      type: 'list-workflows',
      payload: {},
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(listResp.payload?.workflows.length).toBe(2);
  });

  test('should get workflow by ID', async () => {
    const workflow = buildWorkflow('get-test', 'Get Test')
      .task('task-1', { title: 'Task 1' })
      .build();

    await orchestrator.receive({
      id: 'msg-1',
      to: address('orchestrator'),
      from: address('system'),
      type: 'define-workflow',
      payload: workflow,
      timestamp: Date.now(),
      pattern: 'ask'
    });

    const getResp = await orchestrator.receive({
      id: 'msg-2',
      to: address('orchestrator'),
      from: address('system'),
      type: 'get-workflow',
      payload: { id: 'get-test' },
      timestamp: Date.now(),
      pattern: 'ask'
    });

    expect(getResp.payload?.workflow.id).toBe('get-test');
    expect(getResp.payload?.workflow.name).toBe('Get Test');
  });
});
