import { test, expect, describe, beforeEach } from 'bun:test';
import { TaskManager, TaskLifecycle, TaskState, ConflictError, TaskEventEntry } from './task.ts';
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

      expect(manager.createTask('dup', 'Another Task')).rejects.toThrow('Task already exists: dup');
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
      expect(manager.updateTask('nonexistent', { title: 'New Title' }))
        .rejects.toThrow('Task not found: nonexistent');
    });

    test('throws error when updating in_progress task', async () => {
      await manager.createTask('in-prog', 'Task', { assignee: '@(agent)' });
      await manager.startTask('in-prog');

      expect(manager.updateTask('in-prog', { title: 'New Title' }))
        .rejects.toThrow('Cannot update task in in_progress lifecycle');
    });

    test('throws error when updating completed task', async () => {
      await manager.createTask('completed', 'Task', { assignee: '@(agent)' });
      await manager.startTask('completed');
      await manager.completeTask('completed');

      expect(manager.updateTask('completed', { title: 'New Title' }))
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

      expect(manager.assignTask('in-prog-assign', '@(other)'))
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

      expect(manager.unassignTask('unassign-done'))
        .rejects.toThrow('Cannot unassign task in completed lifecycle');
    });

    test('throws error when unassigning failed task', async () => {
      await manager.createTask('unassign-fail', 'Task', { assignee: '@(agent)' });
      await manager.startTask('unassign-fail');
      await manager.failTask('unassign-fail', 'Error');

      expect(manager.unassignTask('unassign-fail'))
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

      expect(manager.startTask('start-pending'))
        .rejects.toThrow('Cannot start task in pending lifecycle');
    });

    test('throws error when starting already in_progress task', async () => {
      await manager.createTask('start-prog', 'Task', { assignee: '@(agent)' });
      await manager.startTask('start-prog');

      expect(manager.startTask('start-prog'))
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

      expect(manager.completeTask('complete-pending'))
        .rejects.toThrow('Cannot complete task in pending lifecycle');
    });

    test('throws error when completing assigned task', async () => {
      await manager.createTask('complete-assigned', 'Task', { assignee: '@(agent)' });

      expect(manager.completeTask('complete-assigned'))
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

      expect(manager.failTask('fail-pending', 'Error'))
        .rejects.toThrow('Cannot fail task in pending lifecycle');
    });

    test('throws error when failing assigned task', async () => {
      await manager.createTask('fail-assigned', 'Task', { assignee: '@(agent)' });

      expect(manager.failTask('fail-assigned', 'Error'))
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
      expect(manager.startTask('sm-7'))
        .rejects.toThrow('Cannot start task in pending lifecycle');
    });

    test('pending -> completed transition is invalid', async () => {
      await manager.createTask('sm-8', 'Task');
      expect(manager.completeTask('sm-8'))
        .rejects.toThrow('Cannot complete task in pending lifecycle');
    });

    test('assigned -> completed transition is invalid', async () => {
      await manager.createTask('sm-9', 'Task', { assignee: '@(agent)' });
      expect(manager.completeTask('sm-9'))
        .rejects.toThrow('Cannot complete task in assigned lifecycle');
    });

    test('completed tasks are terminal (cannot unassign)', async () => {
      await manager.createTask('sm-10', 'Task', { assignee: '@(agent)' });
      await manager.startTask('sm-10');
      await manager.completeTask('sm-10');
      expect(manager.unassignTask('sm-10'))
        .rejects.toThrow('Cannot unassign task in completed lifecycle');
    });

    test('failed tasks are terminal (cannot unassign)', async () => {
      await manager.createTask('sm-11', 'Task', { assignee: '@(agent)' });
      await manager.startTask('sm-11');
      await manager.failTask('sm-11', 'Error');
      expect(manager.unassignTask('sm-11'))
        .rejects.toThrow('Cannot unassign task in failed lifecycle');
    });
  });

  describe('backward-compat aliases (state/data)', () => {
    test('task.state is an alias for task.lifecycle', async () => {
      const task = await manager.createTask('alias-1', 'Alias Task');
      expect(task.state).toBe('pending');
      expect(task.state).toBe(task.lifecycle);
    });

    test('task.data is an alias for task.config', async () => {
      const task = await manager.createTask('alias-2', 'Alias Task', { priority: 'P1' });
      expect(task.data).toBe(task.config);
      expect(task.data.title).toBe('Alias Task');
      expect(task.data.priority).toBe('P1');
    });

    test('TaskState type alias works (same as TaskLifecycle)', () => {
      const s: TaskState = 'pending';
      const l: TaskLifecycle = s;
      expect(l).toBe('pending');
    });

    test('getTask returns task with state/data aliases', async () => {
      await manager.createTask('alias-get', 'Get Task');
      const task = manager.getTask('alias-get')!;
      expect(task.state).toBe(task.lifecycle);
      expect(task.data).toBe(task.config);
    });

    test('listTasks returns tasks with state/data aliases', async () => {
      await manager.createTask('alias-list-1', 'Task 1');
      await manager.createTask('alias-list-2', 'Task 2');
      const tasks = manager.listTasks();
      for (const t of tasks) {
        expect(t.state).toBe(t.lifecycle);
        expect(t.data).toBe(t.config);
      }
    });

    test('listTasks supports filter.state as alias for filter.lifecycle', async () => {
      await manager.createTask('alias-filter-1', 'Pending Task');
      await manager.createTask('alias-filter-2', 'Assigned Task', { assignee: '@(agent)' });

      const pending = manager.listTasks({ state: 'pending' });
      expect(pending.length).toBe(1);
      expect(pending[0].id).toBe('alias-filter-1');
    });
  });

  describe('addDependency / removeDependency / getDependencies', () => {
    test('addDependency creates a DEPENDS_ON edge', async () => {
      await manager.createTask('dep-a', 'Task A');
      await manager.createTask('dep-b', 'Task B');

      await manager.addDependency('dep-b', 'dep-a');

      const { dependsOn } = manager.getDependencies('dep-b');
      expect(dependsOn.length).toBe(1);
      expect(dependsOn[0].id).toBe('dep-a');
    });

    test('addDependency emits TASK_DEP_ADDED event', async () => {
      await manager.createTask('dep-evt-a', 'Task A');
      await manager.createTask('dep-evt-b', 'Task B');
      await manager.addDependency('dep-evt-b', 'dep-evt-a');

      const events = manager.getTaskEventHistory('dep-evt-b');
      const depEvent = events.find(e => e.type === 'TASK_DEP_ADDED');
      expect(depEvent).toBeDefined();
      expect(depEvent!.data.dependsOnId).toBe('dep-evt-a');
    });

    test('addDependency throws if task does not exist', async () => {
      await manager.createTask('dep-exists', 'Exists');
      expect(manager.addDependency('dep-exists', 'nonexistent'))
        .rejects.toThrow('Task not found: nonexistent');
    });

    test('addDependency throws on direct circular dependency', async () => {
      await manager.createTask('circ-a', 'A');
      await manager.createTask('circ-b', 'B');
      await manager.addDependency('circ-b', 'circ-a'); // b depends on a
      expect(manager.addDependency('circ-a', 'circ-b')) // a depends on b => circular
        .rejects.toThrow('Circular dependency');
    });

    test('addDependency is idempotent', async () => {
      await manager.createTask('idem-a', 'A');
      await manager.createTask('idem-b', 'B');
      await manager.addDependency('idem-b', 'idem-a');
      await manager.addDependency('idem-b', 'idem-a'); // second call should not throw
      const { dependsOn } = manager.getDependencies('idem-b');
      expect(dependsOn.length).toBe(1);
    });

    test('removeDependency removes the edge', async () => {
      await manager.createTask('rm-a', 'A');
      await manager.createTask('rm-b', 'B');
      await manager.addDependency('rm-b', 'rm-a');
      await manager.removeDependency('rm-b', 'rm-a');

      const { dependsOn } = manager.getDependencies('rm-b');
      expect(dependsOn.length).toBe(0);
    });

    test('removeDependency emits TASK_DEP_REMOVED event', async () => {
      await manager.createTask('rmevt-a', 'A');
      await manager.createTask('rmevt-b', 'B');
      await manager.addDependency('rmevt-b', 'rmevt-a');
      await manager.removeDependency('rmevt-b', 'rmevt-a');

      const events = manager.getTaskEventHistory('rmevt-b');
      const rmEvent = events.find(e => e.type === 'TASK_DEP_REMOVED');
      expect(rmEvent).toBeDefined();
      expect(rmEvent!.data.dependsOnId).toBe('rmevt-a');
    });

    test('getDependencies: blockers only includes incomplete deps', async () => {
      await manager.createTask('blk-a', 'A', { assignee: '@(agent)' });
      await manager.createTask('blk-b', 'B', { assignee: '@(agent)' });
      await manager.createTask('blk-c', 'C');

      await manager.addDependency('blk-c', 'blk-a');
      await manager.addDependency('blk-c', 'blk-b');

      // Complete blk-a
      await manager.startTask('blk-a');
      await manager.completeTask('blk-a');

      const { dependsOn, blockers } = manager.getDependencies('blk-c');
      expect(dependsOn.length).toBe(2);
      expect(blockers.length).toBe(1);
      expect(blockers[0].id).toBe('blk-b');
    });
  });

  describe('listReady / listBlocked', () => {
    test('listReady returns pending tasks with no incomplete deps', async () => {
      await manager.createTask('ready-a', 'A');
      await manager.createTask('ready-b', 'B');
      await manager.createTask('ready-c', 'C');
      await manager.addDependency('ready-c', 'ready-a'); // c blocked by a

      const ready = manager.listReady();
      const readyIds = ready.map(t => t.id).sort();
      expect(readyIds).toContain('ready-a');
      expect(readyIds).toContain('ready-b');
      expect(readyIds).not.toContain('ready-c');
    });

    test('listBlocked returns pending tasks with incomplete deps', async () => {
      await manager.createTask('bl-a', 'A');
      await manager.createTask('bl-b', 'B');
      await manager.addDependency('bl-b', 'bl-a');

      const blocked = manager.listBlocked();
      expect(blocked.length).toBe(1);
      expect(blocked[0].id).toBe('bl-b');
    });

    test('completing a dep moves a task from blocked to ready', async () => {
      await manager.createTask('move-a', 'A', { assignee: '@(agent)' });
      await manager.createTask('move-b', 'B');
      await manager.addDependency('move-b', 'move-a');

      expect(manager.listBlocked().map(t => t.id)).toContain('move-b');
      expect(manager.listReady().map(t => t.id)).not.toContain('move-b');

      await manager.startTask('move-a');
      await manager.completeTask('move-a');

      expect(manager.listBlocked().map(t => t.id)).not.toContain('move-b');
      expect(manager.listReady().map(t => t.id)).toContain('move-b');
    });

    test('listReady excludes non-pending tasks', async () => {
      await manager.createTask('nonpend-a', 'A', { assignee: '@(agent)' });
      // a is assigned, not pending — should not appear in ready list
      const ready = manager.listReady();
      expect(ready.map(t => t.id)).not.toContain('nonpend-a');
    });
  });

  describe('patchTask', () => {
    test('patches title only', async () => {
      await manager.createTask('patch-1', 'Original Title');
      const patched = await manager.patchTask('patch-1', { title: 'New Title' });
      expect(patched.config.title).toBe('New Title');
      expect(patched.version).toBe(2);
    });

    test('patches multiple fields', async () => {
      await manager.createTask('patch-2', 'Task', { priority: 'P2' });
      const patched = await manager.patchTask('patch-2', { priority: 'P0', description: 'Updated' });
      expect(patched.config.priority).toBe('P0');
      expect(patched.config.description).toBe('Updated');
    });

    test('absent fields remain unchanged', async () => {
      await manager.createTask('patch-3', 'Task', { priority: 'P1', description: 'Keep me' });
      const patched = await manager.patchTask('patch-3', { title: 'New' });
      expect(patched.config.priority).toBe('P1');
      expect(patched.config.description).toBe('Keep me');
    });

    test('patches lifecycle', async () => {
      await manager.createTask('patch-4', 'Task');
      const patched = await manager.patchTask('patch-4', { lifecycle: 'assigned' });
      expect(patched.lifecycle).toBe('assigned');
    });

    test('seq check passes when seq matches version', async () => {
      await manager.createTask('patch-seq-ok', 'Task');
      const patched = await manager.patchTask('patch-seq-ok', { title: 'New' }, 1);
      expect(patched.version).toBe(2);
    });

    test('seq check throws ConflictError when seq mismatches', async () => {
      await manager.createTask('patch-seq-bad', 'Task');
      expect(manager.patchTask('patch-seq-bad', { title: 'New' }, 99))
        .rejects.toThrow('Conflict');
    });

    test('seq conflict throws ConflictError instance', async () => {
      await manager.createTask('patch-seq-type', 'Task');
      try {
        await manager.patchTask('patch-seq-type', { title: 'New' }, 99);
        throw new Error('should have thrown');
      } catch (err) {
        expect(err instanceof ConflictError).toBe(true);
      }
    });

    test('throws if task not found', async () => {
      expect(manager.patchTask('nonexistent', { title: 'X' }))
        .rejects.toThrow('Task not found: nonexistent');
    });

    test('emits TASK_UPDATED event', async () => {
      await manager.createTask('patch-evt', 'Task');
      await manager.patchTask('patch-evt', { priority: 'P0' });
      const events = manager.getTaskEventHistory('patch-evt');
      const updateEvent = events.find(e => e.type === 'TASK_UPDATED');
      expect(updateEvent).toBeDefined();
    });
  });

  describe('applyBatch', () => {
    test('creates new tasks', async () => {
      const result = await manager.applyBatch([
        { id: 'batch-1', title: 'Batch Task 1' },
        { id: 'batch-2', title: 'Batch Task 2' },
      ]);
      expect(result.applied.some(item => item.id === 'batch-1')).toBe(true);
      expect(result.applied.some(item => item.id === 'batch-2')).toBe(true);
      expect(result.errors.length).toBe(0);
      expect(result.conflicts.length).toBe(0);

      expect(manager.getTask('batch-1')!.config.title).toBe('Batch Task 1');
    });

    test('patches existing tasks', async () => {
      await manager.createTask('batch-existing', 'Original');
      const result = await manager.applyBatch([
        { id: 'batch-existing', title: 'Updated' },
      ]);
      expect(result.applied.some(item => item.id === 'batch-existing')).toBe(true);
      expect(manager.getTask('batch-existing')!.config.title).toBe('Updated');
    });

    test('wires dependsOn edges in phase 2', async () => {
      await manager.createTask('batch-dep-a', 'A');
      const result = await manager.applyBatch([
        { id: 'batch-dep-b', title: 'B', dependsOn: ['batch-dep-a'] },
      ]);
      expect(result.applied.some(item => item.id === 'batch-dep-b')).toBe(true);
      const { dependsOn } = manager.getDependencies('batch-dep-b');
      expect(dependsOn.map(t => t.id)).toContain('batch-dep-a');
    });

    test('per-record seq conflicts recorded, others succeed', async () => {
      await manager.createTask('conflict-task', 'Task'); // version=1
      const result = await manager.applyBatch([
        { id: 'new-task', title: 'New' },
        { id: 'conflict-task', title: 'Updated', seq: 99 }, // wrong seq
      ]);
      expect(result.applied.some(item => item.id === 'new-task')).toBe(true);
      expect(result.conflicts.some(c => c.id === 'conflict-task')).toBe(true);
    });

    test('per-record errors recorded, others succeed', async () => {
      // Creating a task that already exists in the same batch causes an error on second attempt
      await manager.createTask('already-exists', 'Exists');
      const result = await manager.applyBatch([
        { id: 'brand-new', title: 'New' },
        { id: 'already-exists', dependsOn: ['nonexistent-dep'] }, // dep error
      ]);
      expect(result.applied.some(item => item.id === 'brand-new')).toBe(true);
      // already-exists should be patched (it exists) but dep add may fail
    });

    test('creates task with title defaulting to id if no title', async () => {
      const result = await manager.applyBatch([
        { id: 'no-title-task' },
      ]);
      expect(result.applied.some(item => item.id === 'no-title-task')).toBe(true);
      const task = manager.getTask('no-title-task')!;
      expect(task.config.title).toBe('no-title-task');
    });

    test('applied array returns { id, action, seq } objects', async () => {
      await manager.createTask('apply-existing', 'Existing Task');
      const result = await manager.applyBatch([
        { id: 'apply-new', title: 'New Task' },
        { id: 'apply-existing', title: 'Updated Task' },
      ]);

      const newItem = result.applied.find(item => item.id === 'apply-new');
      expect(newItem).toBeDefined();
      expect(newItem!.action).toBe('created');
      expect(typeof newItem!.seq).toBe('number');
      expect(newItem!.seq).toBeGreaterThan(0);

      const updatedItem = result.applied.find(item => item.id === 'apply-existing');
      expect(updatedItem).toBeDefined();
      expect(updatedItem!.action).toBe('updated');
      expect(typeof updatedItem!.seq).toBe('number');
      expect(updatedItem!.seq).toBeGreaterThan(0);
    });

    test('applied result includes summary with applied/conflicts/errors counts', async () => {
      await manager.createTask('summary-existing', 'Task'); // version=1
      const result = await manager.applyBatch([
        { id: 'summary-new', title: 'New' },
        { id: 'summary-existing', title: 'Updated', seq: 99 }, // conflict
      ]);

      expect(result.summary).toBeDefined();
      expect(result.summary.applied).toBe(result.applied.length);
      expect(result.summary.conflicts).toBe(result.conflicts.length);
      expect(result.summary.errors).toBe(result.errors.length);
    });

    test('applyBatch with lifecycle: completed on a new task results in completed task', async () => {
      const result = await manager.applyBatch([
        { id: 'lifecycle-completed', title: 'Completed Task', lifecycle: 'completed' },
      ]);

      expect(result.errors.length).toBe(0);
      const item = result.applied.find(i => i.id === 'lifecycle-completed');
      expect(item).toBeDefined();
      expect(item!.action).toBe('created');

      const task = manager.getTask('lifecycle-completed');
      expect(task).not.toBeNull();
      expect(task!.lifecycle).toBe('completed');
    });

    test('applyBatch with lifecycle: in_progress on a new task results in in_progress task', async () => {
      const result = await manager.applyBatch([
        { id: 'lifecycle-inprogress', title: 'In Progress Task', lifecycle: 'in_progress' },
      ]);

      expect(result.errors.length).toBe(0);
      const item = result.applied.find(i => i.id === 'lifecycle-inprogress');
      expect(item).toBeDefined();
      expect(item!.action).toBe('created');

      const task = manager.getTask('lifecycle-inprogress');
      expect(task).not.toBeNull();
      expect(task!.lifecycle).toBe('in_progress');
    });
  });

  describe('TASK_UNBLOCKED event', () => {
    test('emits TASK_UNBLOCKED when completing a blocking task unblocks another', async () => {
      await manager.createTask('unblk-blocker', 'Blocker', { assignee: '@(agent)' });
      await manager.createTask('unblk-waiter', 'Waiter');
      await manager.addDependency('unblk-waiter', 'unblk-blocker');

      await manager.startTask('unblk-blocker');
      await manager.completeTask('unblk-blocker');

      const events = manager.getTaskEventHistory('unblk-waiter');
      const unblockedEvent = events.find(e => e.type === 'TASK_UNBLOCKED');
      expect(unblockedEvent).toBeDefined();
      expect(unblockedEvent!.data.unblockingTaskId).toBe('unblk-blocker');
    });

    test('emits TASK_UNBLOCKED when failing a blocking task unblocks another', async () => {
      await manager.createTask('unblk-fail-blocker', 'Blocker', { assignee: '@(agent)' });
      await manager.createTask('unblk-fail-waiter', 'Waiter');
      await manager.addDependency('unblk-fail-waiter', 'unblk-fail-blocker');

      await manager.startTask('unblk-fail-blocker');
      await manager.failTask('unblk-fail-blocker', 'Error');

      const events = manager.getTaskEventHistory('unblk-fail-waiter');
      const unblockedEvent = events.find(e => e.type === 'TASK_UNBLOCKED');
      expect(unblockedEvent).toBeDefined();
    });

    test('does not emit TASK_UNBLOCKED if task still has other blockers', async () => {
      await manager.createTask('multi-blk-a', 'A', { assignee: '@(agent)' });
      await manager.createTask('multi-blk-b', 'B');
      await manager.createTask('multi-blk-waiter', 'Waiter');
      await manager.addDependency('multi-blk-waiter', 'multi-blk-a');
      await manager.addDependency('multi-blk-waiter', 'multi-blk-b');

      // Only complete a — b is still pending
      await manager.startTask('multi-blk-a');
      await manager.completeTask('multi-blk-a');

      const events = manager.getTaskEventHistory('multi-blk-waiter');
      const unblockedEvent = events.find(e => e.type === 'TASK_UNBLOCKED');
      expect(unblockedEvent).toBeUndefined();
    });

    test('emits TASK_UNBLOCKED only after all blockers are done', async () => {
      await manager.createTask('all-blk-a', 'A', { assignee: '@(agent)' });
      await manager.createTask('all-blk-b', 'B', { assignee: '@(agent)' });
      await manager.createTask('all-blk-waiter', 'Waiter');
      await manager.addDependency('all-blk-waiter', 'all-blk-a');
      await manager.addDependency('all-blk-waiter', 'all-blk-b');

      await manager.startTask('all-blk-a');
      await manager.completeTask('all-blk-a');

      // Still blocked by b
      let events = manager.getTaskEventHistory('all-blk-waiter');
      expect(events.find(e => e.type === 'TASK_UNBLOCKED')).toBeUndefined();

      await manager.startTask('all-blk-b');
      await manager.completeTask('all-blk-b');

      // Now unblocked
      events = manager.getTaskEventHistory('all-blk-waiter');
      expect(events.find(e => e.type === 'TASK_UNBLOCKED')).toBeDefined();
    });
  });

  describe('appendNote', () => {
    test('appends a note and returns a TaskEventEntry', async () => {
      await manager.createTask('note-1', 'Task for notes');
      const entry = await manager.appendNote('note-1', 'First note text');

      expect(entry.id).toBe('evt:note-1:0');
      expect(entry.taskId).toBe('note-1');
      expect(entry.kind).toBe('note');
      expect(entry.text).toBe('First note text');
      expect(entry.seq).toBe(0);
      expect(typeof entry.timestamp).toBe('string');
    });

    test('appends a note with author', async () => {
      await manager.createTask('note-author', 'Task');
      const entry = await manager.appendNote('note-author', 'Note text', 'alice');

      expect(entry.author).toBe('alice');
    });

    test('sequential notes get increasing seq numbers', async () => {
      await manager.createTask('note-seq', 'Task');
      const e0 = await manager.appendNote('note-seq', 'First');
      const e1 = await manager.appendNote('note-seq', 'Second');
      const e2 = await manager.appendNote('note-seq', 'Third');

      expect(e0.seq).toBe(0);
      expect(e1.seq).toBe(1);
      expect(e2.seq).toBe(2);
    });

    test('throws if task not found', async () => {
      expect(manager.appendNote('nonexistent', 'Note'))
        .rejects.toThrow('Task not found: nonexistent');
    });

    test('emits TASK_NOTE_APPENDED event', async () => {
      await manager.createTask('note-evt', 'Task');
      await manager.appendNote('note-evt', 'Some note');

      const events = manager.getTaskEventHistory('note-evt');
      const noteEvent = events.find(e => e.type === 'TASK_NOTE_APPENDED');
      expect(noteEvent).toBeDefined();
      expect(noteEvent!.data.text).toBe('Some note');
    });
  });

  describe('recordDecision', () => {
    test('records a decision and returns a TaskEventEntry', async () => {
      await manager.createTask('dec-1', 'Decision task');
      const entry = await manager.recordDecision('dec-1', 'Use TypeScript');

      expect(entry.id).toBe('evt:dec-1:0');
      expect(entry.taskId).toBe('dec-1');
      expect(entry.kind).toBe('decision');
      expect(entry.text).toBe('Use TypeScript');
      expect(entry.seq).toBe(0);
    });

    test('records a decision with context and rationale', async () => {
      await manager.createTask('dec-opts', 'Task');
      const entry = await manager.recordDecision('dec-opts', 'Use TypeScript', {
        context: 'Choosing a language',
        rationale: 'Type safety required',
        author: 'bob',
      });

      expect(entry.context).toBe('Choosing a language');
      expect(entry.rationale).toBe('Type safety required');
      expect(entry.author).toBe('bob');
    });

    test('throws if task not found', async () => {
      expect(manager.recordDecision('nonexistent', 'Decision'))
        .rejects.toThrow('Task not found: nonexistent');
    });

    test('emits TASK_DECISION_RECORDED event', async () => {
      await manager.createTask('dec-evt', 'Task');
      await manager.recordDecision('dec-evt', 'My decision');

      const events = manager.getTaskEventHistory('dec-evt');
      const decEvent = events.find(e => e.type === 'TASK_DECISION_RECORDED');
      expect(decEvent).toBeDefined();
      expect(decEvent!.data.decision).toBe('My decision');
    });
  });

  describe('linkArtifact', () => {
    test('links a file artifact', async () => {
      await manager.createTask('link-1', 'Link task');
      const entry = await manager.linkArtifact('link-1', 'src/main.ts', 'file');

      expect(entry.id).toBe('evt:link-1:0');
      expect(entry.kind).toBe('link');
      expect(entry.text).toBe('src/main.ts');
      expect(entry.linkKind).toBe('file');
      expect(entry.seq).toBe(0);
    });

    test('links a commit artifact', async () => {
      await manager.createTask('link-commit', 'Task');
      const entry = await manager.linkArtifact('link-commit', 'abc123', 'commit');

      expect(entry.linkKind).toBe('commit');
    });

    test('links a url artifact', async () => {
      await manager.createTask('link-url', 'Task');
      const entry = await manager.linkArtifact('link-url', 'https://example.com', 'url');

      expect(entry.linkKind).toBe('url');
    });

    test('links an issue artifact', async () => {
      await manager.createTask('link-issue', 'Task');
      const entry = await manager.linkArtifact('link-issue', '#42', 'issue');

      expect(entry.linkKind).toBe('issue');
    });

    test('throws if task not found', async () => {
      expect(manager.linkArtifact('nonexistent', 'file.ts', 'file'))
        .rejects.toThrow('Task not found: nonexistent');
    });

    test('emits TASK_ARTIFACT_LINKED event', async () => {
      await manager.createTask('link-evt', 'Task');
      await manager.linkArtifact('link-evt', 'file.ts', 'file');

      const events = manager.getTaskEventHistory('link-evt');
      const linkEvent = events.find(e => e.type === 'TASK_ARTIFACT_LINKED');
      expect(linkEvent).toBeDefined();
      expect(linkEvent!.data.artifact).toBe('file.ts');
      expect(linkEvent!.data.kind).toBe('file');
    });
  });

  describe('appendLog', () => {
    test('appends a log entry', async () => {
      await manager.createTask('log-1', 'Log task');
      const entry = await manager.appendLog('log-1', 'stdout: process started');

      expect(entry.id).toBe('evt:log-1:0');
      expect(entry.kind).toBe('log');
      expect(entry.text).toBe('stdout: process started');
      expect(entry.seq).toBe(0);
    });

    test('throws if task not found', async () => {
      expect(manager.appendLog('nonexistent', 'log line'))
        .rejects.toThrow('Task not found: nonexistent');
    });

    test('emits TASK_LOG_APPENDED event', async () => {
      await manager.createTask('log-evt', 'Task');
      await manager.appendLog('log-evt', 'log output');

      const events = manager.getTaskEventHistory('log-evt');
      const logEvent = events.find(e => e.type === 'TASK_LOG_APPENDED');
      expect(logEvent).toBeDefined();
      expect(logEvent!.data.text).toBe('log output');
    });
  });

  describe('getEventLog', () => {
    test('returns empty array for task with no events', async () => {
      await manager.createTask('elog-empty', 'Task');
      const log = manager.getEventLog('elog-empty');
      expect(log).toEqual([]);
    });

    test('returns events ordered by seq', async () => {
      await manager.createTask('elog-order', 'Task');
      await manager.appendNote('elog-order', 'First');
      await manager.recordDecision('elog-order', 'Second');
      await manager.linkArtifact('elog-order', 'file.ts', 'file');

      const log = manager.getEventLog('elog-order');
      expect(log.length).toBe(3);
      expect(log[0].seq).toBe(0);
      expect(log[1].seq).toBe(1);
      expect(log[2].seq).toBe(2);
      expect(log[0].kind).toBe('note');
      expect(log[1].kind).toBe('decision');
      expect(log[2].kind).toBe('link');
    });

    test('returns all event kinds', async () => {
      await manager.createTask('elog-kinds', 'Task');
      await manager.appendNote('elog-kinds', 'A note');
      await manager.recordDecision('elog-kinds', 'A decision');
      await manager.linkArtifact('elog-kinds', 'f.ts', 'file');
      await manager.appendLog('elog-kinds', 'A log');

      const log = manager.getEventLog('elog-kinds');
      expect(log.length).toBe(4);
      const kinds = log.map(e => e.kind);
      expect(kinds).toContain('note');
      expect(kinds).toContain('decision');
      expect(kinds).toContain('link');
      expect(kinds).toContain('log');
    });
  });

  describe('getDecisions', () => {
    test('returns only decision events', async () => {
      await manager.createTask('gdec-1', 'Task');
      await manager.appendNote('gdec-1', 'A note');
      await manager.recordDecision('gdec-1', 'Decision A');
      await manager.appendLog('gdec-1', 'A log');
      await manager.recordDecision('gdec-1', 'Decision B');

      const decisions = manager.getDecisions('gdec-1');
      expect(decisions.length).toBe(2);
      expect(decisions[0].text).toBe('Decision A');
      expect(decisions[1].text).toBe('Decision B');
    });

    test('returns empty array when no decisions', async () => {
      await manager.createTask('gdec-empty', 'Task');
      await manager.appendNote('gdec-empty', 'Just a note');

      const decisions = manager.getDecisions('gdec-empty');
      expect(decisions).toEqual([]);
    });
  });

  describe('primeTask', () => {
    test('returns task, events, and decisions', async () => {
      await manager.createTask('prime-1', 'Prime Task', { priority: 'P1' });
      await manager.appendNote('prime-1', 'Note 1');
      await manager.recordDecision('prime-1', 'Decision 1', { rationale: 'Because' });
      await manager.linkArtifact('prime-1', 'f.ts', 'file');

      const result = manager.primeTask('prime-1');

      expect(result.task).toBeDefined();
      expect(result.task.id).toBe('prime-1');
      expect(result.task.config.title).toBe('Prime Task');

      expect(result.events.length).toBe(3);
      expect(result.events[0].kind).toBe('note');
      expect(result.events[1].kind).toBe('decision');
      expect(result.events[2].kind).toBe('link');

      expect(result.decisions.length).toBe(1);
      expect(result.decisions[0].text).toBe('Decision 1');
      expect(result.decisions[0].rationale).toBe('Because');
    });

    test('throws if task not found', () => {
      expect(() => manager.primeTask('nonexistent'))
        .toThrow('Task not found: nonexistent');
    });

    test('returns empty events and decisions for task with no content', async () => {
      await manager.createTask('prime-empty', 'Empty Task');
      const result = manager.primeTask('prime-empty');

      expect(result.events).toEqual([]);
      expect(result.decisions).toEqual([]);
    });
  });

  describe('append-only invariant', () => {
    test('TaskManager has no deleteEvent or updateEvent method', () => {
      // Events are append-only: no delete or update methods should exist
      expect(typeof (manager as any).deleteEvent).toBe('undefined');
      expect(typeof (manager as any).updateEvent).toBe('undefined');
      expect(typeof (manager as any).deleteTaskEvent).toBe('undefined');
    });

    test('seq numbers are always increasing and non-overlapping', async () => {
      await manager.createTask('seq-check', 'Task');
      const entries: TaskEventEntry[] = [];
      entries.push(await manager.appendNote('seq-check', 'n1'));
      entries.push(await manager.recordDecision('seq-check', 'd1'));
      entries.push(await manager.appendLog('seq-check', 'l1'));
      entries.push(await manager.linkArtifact('seq-check', 'f.ts', 'file'));

      const seqs = entries.map(e => e.seq);
      expect(seqs).toEqual([0, 1, 2, 3]);
    });
  });

  describe('exportTasks', () => {
    test('exports all tasks as valid JSONL (each line is parseable JSON)', async () => {
      await manager.createTask('exp-1', 'Task One', { priority: 'P1' });
      await manager.createTask('exp-2', 'Task Two', { description: 'Details here' });
      await manager.createTask('exp-3', 'Task Three', { assignee: '@(agent)' });

      const records = manager.exportTasks('all');

      expect(records.length).toBe(3);
      // Serialize to JSONL and verify each line round-trips
      const lines = records.map(r => JSON.stringify(r));
      for (const line of lines) {
        const parsed = JSON.parse(line);
        expect(parsed).toHaveProperty('id');
        expect(parsed).toHaveProperty('title');
        expect(parsed).toHaveProperty('lifecycle');
      }
    });

    test('export with filter=open excludes completed and failed tasks', async () => {
      await manager.createTask('open-pending', 'Pending Task');
      await manager.createTask('open-assigned', 'Assigned Task', { assignee: '@(a)' });
      // Create and complete a task
      await manager.createTask('done-task', 'Done Task', { assignee: '@(a)' });
      await manager.startTask('done-task');
      await manager.completeTask('done-task');
      // Create and fail a task
      await manager.createTask('failed-task', 'Failed Task', { assignee: '@(a)' });
      await manager.startTask('failed-task');
      await manager.failTask('failed-task', 'crashed');

      const records = manager.exportTasks('open');

      const ids = records.map(r => r.id);
      expect(ids).toContain('open-pending');
      expect(ids).toContain('open-assigned');
      expect(ids).not.toContain('done-task');
      expect(ids).not.toContain('failed-task');
    });

    test('export with prefix only includes tasks whose ID starts with the prefix', async () => {
      await manager.createTask('FM-001', 'FM Task One');
      await manager.createTask('FM-002', 'FM Task Two');
      await manager.createTask('WG-001', 'WG Task');

      const records = manager.exportTasks('all', 'FM');

      const ids = records.map(r => r.id);
      expect(ids).toContain('FM-001');
      expect(ids).toContain('FM-002');
      expect(ids).not.toContain('WG-001');
    });

    test('round-trip: export then apply to fresh store produces identical task list', async () => {
      // Setup source store with tasks and dependencies
      await manager.createTask('rt-a', 'Task A', { priority: 'P2', description: 'Alpha' });
      await manager.createTask('rt-b', 'Task B', { priority: 'P1' });
      await manager.addDependency('rt-b', 'rt-a');

      const records = manager.exportTasks('all');

      // Apply to a fresh store
      const freshStore = new GraphStore(
        `/tmp/ugs-roundtrip-${Date.now()}-${Math.random().toString(36).slice(2)}`
      );
      await freshStore.initialize();
      const freshManager = new TaskManager(freshStore);

      // Convert ExportRecord array to ApplyRecord array
      const applyRecords = records.map(r => ({
        id: r.id,
        title: r.title,
        lifecycle: r.lifecycle,
        priority: r.priority,
        description: r.description,
        assignee: r.assignee,
        dependsOn: r['depends-on'],
      }));

      const result = await freshManager.applyBatch(applyRecords);
      expect(result.errors).toHaveLength(0);
      expect(result.conflicts).toHaveLength(0);

      const freshTasks = freshManager.listTasks();
      expect(freshTasks.length).toBe(2);

      const rtA = freshManager.getTask('rt-a');
      const rtB = freshManager.getTask('rt-b');
      expect(rtA).not.toBeNull();
      expect(rtB).not.toBeNull();
      expect(rtA!.config.priority).toBe('P2');
      expect(rtA!.config.description).toBe('Alpha');

      // Verify dependency was preserved
      const { dependsOn } = freshManager.getDependencies('rt-b');
      expect(dependsOn.map(t => t.id)).toContain('rt-a');
    });

    test('export with no tasks produces empty array (not an error)', async () => {
      const records = manager.exportTasks('all');
      expect(records).toEqual([]);
    });
  });
});
