import { test, expect, describe, beforeEach } from 'bun:test';
import { TaskManager, Task, TaskLifecycle, TaskPriority } from './task.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

describe('TaskManager', () => {
  let store: GraphStore;
  let manager: TaskManager;

  beforeEach(async () => {
    // Create a fresh in-memory store for each test with unique path
    testCounter++;
    store = new GraphStore(`/tmp/ugs-task-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`);
    await store.initialize();
    manager = new TaskManager(store);
  });

  describe('createTask', () => {
    test('creates a task in pending lifecycle', async () => {
      const task = await manager.createTask('test-task', 'Test Task Title');

      expect(task.id).toBe('test-task');
      expect(task.config.title).toBe('Test Task Title');
      expect(task.lifecycle).toBe('pending');
      expect(task.version).toBe(1);
      expect(task.type).toBe('program');
      expect(task.programType).toBe('task');
    });

    test('creates a task with all options', async () => {
      const task = await manager.createTask('full-task', 'Full Task', {
        spec: {
          inputs: ['topic: quantum computing'],
          outputs: ['summary document'],
          constraints: ['max 2000 words'],
          successCriteria: [
            { type: 'file_exists', path: 'quantum-research.md' },
            { type: 'word_count', min: 1000 }
          ]
        },
        priority: 'P1',
        description: 'A comprehensive task with full specification'
      });

      expect(task.config.title).toBe('Full Task');
      expect(task.config.spec?.inputs).toEqual(['topic: quantum computing']);
      expect(task.config.spec?.outputs).toEqual(['summary document']);
      expect(task.config.spec?.constraints).toEqual(['max 2000 words']);
      expect(task.config.spec?.successCriteria?.length).toBe(2);
      expect(task.config.priority).toBe('P1');
      expect(task.config.description).toBe('A comprehensive task with full specification');
    });

    test('creates a task with assignee in assigned lifecycle', async () => {
      const task = await manager.createTask('assigned-task', 'Assigned Task', {
        assignee: '@(researcher)'
      });

      expect(task.lifecycle).toBe('assigned');
      expect(task.config.assignee).toBe('@(researcher)');
    });

    test('throws error if task already exists', async () => {
      await manager.createTask('dup', 'Duplicate Task');

      await expect(manager.createTask('dup', 'Another Task')).rejects.toThrow('Task already exists: dup');
    });

    test('emits TASK_CREATED event', async () => {
      await manager.createTask('event-test', 'Event Test Task');

      const events = manager.getTaskEvents();
      expect(events.length).toBeGreaterThanOrEqual(1);

      const createEvent = events.find(e => e.type === 'TASK_CREATED' && e.taskId === 'event-test');
      expect(createEvent).toBeDefined();
      expect(createEvent!.data.lifecycle).toBe('pending');
    });

    test('emits both TASK_CREATED and TASK_ASSIGNED events when assignee provided', async () => {
      await manager.createTask('assigned-evt', 'Assigned Event Task', {
        assignee: '@(agent-1)'
      });

      const events = manager.getTaskEventHistory('assigned-evt');
      const createEvent = events.find(e => e.type === 'TASK_CREATED');
      const assignEvent = events.find(e => e.type === 'TASK_ASSIGNED');

      expect(createEvent).toBeDefined();
      expect(assignEvent).toBeDefined();
      expect(assignEvent!.data.assignee).toBe('@(agent-1)');
    });
  });

  describe('getTask', () => {
    test('returns null for non-existent task', () => {
      const task = manager.getTask('nonexistent');
      expect(task).toBeNull();
    });

    test('retrieves an existing task', async () => {
      await manager.createTask('get-test', 'Get Test Task', { priority: 'P2' });

      const task = manager.getTask('get-test');
      expect(task).not.toBeNull();
      expect(task!.id).toBe('get-test');
      expect(task!.config.title).toBe('Get Test Task');
      expect(task!.config.priority).toBe('P2');
    });
  });

  describe('updateTask', () => {
    test('updates a pending task', async () => {
      await manager.createTask('update-test', 'Original Title');

      const updated = await manager.updateTask('update-test', {
        title: 'Updated Title',
        priority: 'P0'
      });

      expect(updated.config.title).toBe('Updated Title');
      expect(updated.config.priority).toBe('P0');
      expect(updated.version).toBe(2);
    });

    test('updates an assigned task', async () => {
      await manager.createTask('update-assigned', 'Task', { assignee: '@(agent)' });

      const updated = await manager.updateTask('update-assigned', {
        description: 'New description'
      });

      expect(updated.config.description).toBe('New description');
    });

    test('throws error when updating non-existent task', async () => {
      await expect(manager.updateTask('nonexistent', { title: 'New Title' }))
        .rejects.toThrow('Task not found: nonexistent');
    });

    test('throws error when updating in_progress task', async () => {
      await manager.createTask('in-prog', 'Task', { assignee: '@(agent)' });
      await manager.startTask('in-prog');

      await expect(manager.updateTask('in-prog', { title: 'New Title' }))
        .rejects.toThrow('Cannot update task in in_progress lifecycle');
    });

    test('throws error when updating completed task', async () => {
      await manager.createTask('completed', 'Task', { assignee: '@(agent)' });
      await manager.startTask('completed');
      await manager.completeTask('completed');

      await expect(manager.updateTask('completed', { title: 'New Title' }))
        .rejects.toThrow('Cannot update task in completed lifecycle');
    });

    test('emits TASK_UPDATED event', async () => {
      await manager.createTask('update-evt', 'Task');
      await manager.updateTask('update-evt', { priority: 'P1' });

      const events = manager.getTaskEventHistory('update-evt');
      const updateEvent = events.find(e => e.type === 'TASK_UPDATED');
      expect(updateEvent).toBeDefined();
      expect(updateEvent!.data.newVersion).toBe(2);
    });
  });

  describe('assignTask', () => {
    test('assigns a pending task', async () => {
      await manager.createTask('assign-test', 'Task to Assign');

      const assigned = await manager.assignTask('assign-test', '@(researcher)');

      expect(assigned.lifecycle).toBe('assigned');
      expect(assigned.config.assignee).toBe('@(researcher)');
    });

    test('reassigns an already assigned task', async () => {
      await manager.createTask('reassign', 'Task', { assignee: '@(agent-1)' });

      const reassigned = await manager.assignTask('reassign', '@(agent-2)');

      expect(reassigned.config.assignee).toBe('@(agent-2)');
    });

    test('throws error when assigning in_progress task', async () => {
      await manager.createTask('in-prog-assign', 'Task', { assignee: '@(agent)' });
      await manager.startTask('in-prog-assign');

      await expect(manager.assignTask('in-prog-assign', '@(other)'))
        .rejects.toThrow('Cannot assign task in in_progress lifecycle');
    });

    test('emits TASK_ASSIGNED event', async () => {
      await manager.createTask('assign-evt', 'Task');
      await manager.assignTask('assign-evt', '@(agent)');

      const events = manager.getTaskEventHistory('assign-evt');
      const assignEvent = events.find(e => e.type === 'TASK_ASSIGNED');
      expect(assignEvent).toBeDefined();
      expect(assignEvent!.data.assignee).toBe('@(agent)');
      expect(assignEvent!.data.newLifecycle).toBe('assigned');
    });
  });

  describe('unassignTask', () => {
    test('unassigns an assigned task', async () => {
      await manager.createTask('unassign-test', 'Task', { assignee: '@(agent)' });

      const unassigned = await manager.unassignTask('unassign-test');

      expect(unassigned.lifecycle).toBe('pending');
      expect(unassigned.config.assignee).toBeFalsy();
    });

    test('unassigns an in_progress task', async () => {
      await manager.createTask('unassign-prog', 'Task', { assignee: '@(agent)' });
      await manager.startTask('unassign-prog');

      const unassigned = await manager.unassignTask('unassign-prog');

      expect(unassigned.lifecycle).toBe('pending');
    });

    test('throws error when unassigning completed task', async () => {
      await manager.createTask('unassign-done', 'Task', { assignee: '@(agent)' });
      await manager.startTask('unassign-done');
      await manager.completeTask('unassign-done');

      await expect(manager.unassignTask('unassign-done'))
        .rejects.toThrow('Cannot unassign task in completed lifecycle');
    });

    test('throws error when unassigning failed task', async () => {
      await manager.createTask('unassign-fail', 'Task', { assignee: '@(agent)' });
      await manager.startTask('unassign-fail');
      await manager.failTask('unassign-fail', 'Error');

      await expect(manager.unassignTask('unassign-fail'))
        .rejects.toThrow('Cannot unassign task in failed lifecycle');
    });

    test('emits TASK_UNASSIGNED event', async () => {
      await manager.createTask('unassign-evt', 'Task', { assignee: '@(agent)' });
      await manager.unassignTask('unassign-evt');

      const events = manager.getTaskEventHistory('unassign-evt');
      const unassignEvent = events.find(e => e.type === 'TASK_UNASSIGNED');
      expect(unassignEvent).toBeDefined();
      expect(unassignEvent!.data.previousAssignee).toBe('@(agent)');
      expect(unassignEvent!.data.newLifecycle).toBe('pending');
    });
  });

  describe('startTask', () => {
    test('starts an assigned task', async () => {
      await manager.createTask('start-test', 'Task', { assignee: '@(agent)' });

      const started = await manager.startTask('start-test');

      expect(started.lifecycle).toBe('in_progress');
    });

    test('throws error when starting pending task', async () => {
      await manager.createTask('start-pending', 'Task');

      await expect(manager.startTask('start-pending'))
        .rejects.toThrow('Cannot start task in pending lifecycle');
    });

    test('throws error when starting already in_progress task', async () => {
      await manager.createTask('start-prog', 'Task', { assignee: '@(agent)' });
      await manager.startTask('start-prog');

      await expect(manager.startTask('start-prog'))
        .rejects.toThrow('Cannot start task in in_progress lifecycle');
    });

    test('emits TASK_STARTED event', async () => {
      await manager.createTask('start-evt', 'Task', { assignee: '@(agent)' });
      await manager.startTask('start-evt');

      const events = manager.getTaskEventHistory('start-evt');
      const startEvent = events.find(e => e.type === 'TASK_STARTED');
      expect(startEvent).toBeDefined();
      expect(startEvent!.data.newLifecycle).toBe('in_progress');
    });
  });

  describe('completeTask', () => {
    test('completes an in_progress task', async () => {
      await manager.createTask('complete-test', 'Task', { assignee: '@(agent)' });
      await manager.startTask('complete-test');

      const completed = await manager.completeTask('complete-test');

      expect(completed.lifecycle).toBe('completed');
    });

    test('completes a task with result', async () => {
      await manager.createTask('complete-result', 'Task', { assignee: '@(agent)' });
      await manager.startTask('complete-result');

      const completed = await manager.completeTask('complete-result', { output: 'research.md', wordCount: 1500 });

      expect(completed.lifecycle).toBe('completed');
      expect(completed.config.result).toEqual({ output: 'research.md', wordCount: 1500 });
    });

    test('throws error when completing pending task', async () => {
      await manager.createTask('complete-pending', 'Task');

      await expect(manager.completeTask('complete-pending'))
        .rejects.toThrow('Cannot complete task in pending lifecycle');
    });

    test('throws error when completing assigned task', async () => {
      await manager.createTask('complete-assigned', 'Task', { assignee: '@(agent)' });

      await expect(manager.completeTask('complete-assigned'))
        .rejects.toThrow('Cannot complete task in assigned lifecycle');
    });

    test('emits TASK_COMPLETED event', async () => {
      await manager.createTask('complete-evt', 'Task', { assignee: '@(agent)' });
      await manager.startTask('complete-evt');
      await manager.completeTask('complete-evt', { success: true });

      const events = manager.getTaskEventHistory('complete-evt');
      const completeEvent = events.find(e => e.type === 'TASK_COMPLETED');
      expect(completeEvent).toBeDefined();
      expect(completeEvent!.data.newLifecycle).toBe('completed');
      expect(completeEvent!.data.result).toEqual({ success: true });
    });
  });

  describe('failTask', () => {
    test('fails an in_progress task', async () => {
      await manager.createTask('fail-test', 'Task', { assignee: '@(agent)' });
      await manager.startTask('fail-test');

      const failed = await manager.failTask('fail-test', 'Resource not available');

      expect(failed.lifecycle).toBe('failed');
      expect(failed.config.failureReason).toBe('Resource not available');
    });

    test('throws error when failing pending task', async () => {
      await manager.createTask('fail-pending', 'Task');

      await expect(manager.failTask('fail-pending', 'Error'))
        .rejects.toThrow('Cannot fail task in pending lifecycle');
    });

    test('throws error when failing assigned task', async () => {
      await manager.createTask('fail-assigned', 'Task', { assignee: '@(agent)' });

      await expect(manager.failTask('fail-assigned', 'Error'))
        .rejects.toThrow('Cannot fail task in assigned lifecycle');
    });

    test('emits TASK_FAILED event', async () => {
      await manager.createTask('fail-evt', 'Task', { assignee: '@(agent)' });
      await manager.startTask('fail-evt');
      await manager.failTask('fail-evt', 'Test failure');

      const events = manager.getTaskEventHistory('fail-evt');
      const failEvent = events.find(e => e.type === 'TASK_FAILED');
      expect(failEvent).toBeDefined();
      expect(failEvent!.data.newLifecycle).toBe('failed');
      expect(failEvent!.data.reason).toBe('Test failure');
    });
  });

  describe('listTasks', () => {
    test('lists all tasks', async () => {
      await manager.createTask('list-1', 'Task 1');
      await manager.createTask('list-2', 'Task 2');
      await manager.createTask('list-3', 'Task 3');

      const tasks = manager.listTasks();

      expect(tasks.length).toBe(3);
    });

    test('filters tasks by lifecycle', async () => {
      await manager.createTask('pending-1', 'Pending Task');
      await manager.createTask('assigned-1', 'Assigned Task', { assignee: '@(agent)' });
      await manager.createTask('prog-1', 'In Progress Task', { assignee: '@(agent)' });
      await manager.startTask('prog-1');

      const pending = manager.listTasks({ lifecycle: 'pending' });
      const assigned = manager.listTasks({ lifecycle: 'assigned' });
      const inProgress = manager.listTasks({ lifecycle: 'in_progress' });

      expect(pending.length).toBe(1);
      expect(pending[0].id).toBe('pending-1');
      expect(assigned.length).toBe(1);
      expect(assigned[0].id).toBe('assigned-1');
      expect(inProgress.length).toBe(1);
      expect(inProgress[0].id).toBe('prog-1');
    });

    test('filters tasks by assignee', async () => {
      await manager.createTask('agent1-task', 'Task', { assignee: '@(agent-1)' });
      await manager.createTask('agent2-task', 'Task', { assignee: '@(agent-2)' });
      await manager.createTask('unassigned', 'Task');

      const agent1Tasks = manager.listTasks({ assignee: '@(agent-1)' });
      const agent2Tasks = manager.listTasks({ assignee: '@(agent-2)' });

      expect(agent1Tasks.length).toBe(1);
      expect(agent1Tasks[0].id).toBe('agent1-task');
      expect(agent2Tasks.length).toBe(1);
      expect(agent2Tasks[0].id).toBe('agent2-task');
    });

    test('filters tasks by priority', async () => {
      await manager.createTask('p0-task', 'P0 Task', { priority: 'P0' });
      await manager.createTask('p1-task', 'P1 Task', { priority: 'P1' });
      await manager.createTask('p2-task', 'P2 Task', { priority: 'P2' });

      const p0Tasks = manager.listTasks({ priority: 'P0' });
      const p1Tasks = manager.listTasks({ priority: 'P1' });

      expect(p0Tasks.length).toBe(1);
      expect(p0Tasks[0].id).toBe('p0-task');
      expect(p1Tasks.length).toBe(1);
      expect(p1Tasks[0].id).toBe('p1-task');
    });

    test('combines filters', async () => {
      await manager.createTask('match', 'Match Task', { assignee: '@(agent)', priority: 'P1' });
      await manager.createTask('wrong-priority', 'Wrong', { assignee: '@(agent)', priority: 'P2' });
      await manager.createTask('wrong-assignee', 'Wrong', { assignee: '@(other)', priority: 'P1' });

      const filtered = manager.listTasks({ assignee: '@(agent)', priority: 'P1' });

      expect(filtered.length).toBe(1);
      expect(filtered[0].id).toBe('match');
    });
  });

  describe('lifecycle machine enforcement', () => {
    test('pending -> assigned transition is valid', async () => {
      await manager.createTask('sm-1', 'Task');
      const assigned = await manager.assignTask('sm-1', '@(agent)');
      expect(assigned.lifecycle).toBe('assigned');
    });

    test('assigned -> in_progress transition is valid', async () => {
      await manager.createTask('sm-2', 'Task', { assignee: '@(agent)' });
      const started = await manager.startTask('sm-2');
      expect(started.lifecycle).toBe('in_progress');
    });

    test('in_progress -> completed transition is valid', async () => {
      await manager.createTask('sm-3', 'Task', { assignee: '@(agent)' });
      await manager.startTask('sm-3');
      const completed = await manager.completeTask('sm-3');
      expect(completed.lifecycle).toBe('completed');
    });

    test('in_progress -> failed transition is valid', async () => {
      await manager.createTask('sm-4', 'Task', { assignee: '@(agent)' });
      await manager.startTask('sm-4');
      const failed = await manager.failTask('sm-4', 'Error');
      expect(failed.lifecycle).toBe('failed');
    });

    test('assigned -> pending (unassign) transition is valid', async () => {
      await manager.createTask('sm-5', 'Task', { assignee: '@(agent)' });
      const unassigned = await manager.unassignTask('sm-5');
      expect(unassigned.lifecycle).toBe('pending');
    });

    test('in_progress -> pending (unassign) transition is valid', async () => {
      await manager.createTask('sm-6', 'Task', { assignee: '@(agent)' });
      await manager.startTask('sm-6');
      const unassigned = await manager.unassignTask('sm-6');
      expect(unassigned.lifecycle).toBe('pending');
    });

    test('pending -> in_progress transition is invalid', async () => {
      await manager.createTask('sm-7', 'Task');
      await expect(manager.startTask('sm-7'))
        .rejects.toThrow('Cannot start task in pending lifecycle');
    });

    test('pending -> completed transition is invalid', async () => {
      await manager.createTask('sm-8', 'Task');
      await expect(manager.completeTask('sm-8'))
        .rejects.toThrow('Cannot complete task in pending lifecycle');
    });

    test('assigned -> completed transition is invalid', async () => {
      await manager.createTask('sm-9', 'Task', { assignee: '@(agent)' });
      await expect(manager.completeTask('sm-9'))
        .rejects.toThrow('Cannot complete task in assigned lifecycle');
    });

    test('completed tasks are terminal (cannot unassign)', async () => {
      await manager.createTask('sm-10', 'Task', { assignee: '@(agent)' });
      await manager.startTask('sm-10');
      await manager.completeTask('sm-10');
      await expect(manager.unassignTask('sm-10'))
        .rejects.toThrow('Cannot unassign task in completed lifecycle');
    });

    test('failed tasks are terminal (cannot unassign)', async () => {
      await manager.createTask('sm-11', 'Task', { assignee: '@(agent)' });
      await manager.startTask('sm-11');
      await manager.failTask('sm-11', 'Error');
      await expect(manager.unassignTask('sm-11'))
        .rejects.toThrow('Cannot unassign task in failed lifecycle');
    });
  });
});
