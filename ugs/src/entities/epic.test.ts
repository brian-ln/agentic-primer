import { test, expect, describe, beforeEach } from 'bun:test';
import { EpicManager } from './epic.ts';
import { TaskManager } from './task.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

function makeStore(): GraphStore {
  testCounter++;
  return new GraphStore(
    `/tmp/ugs-epic-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`
  );
}

describe('EpicManager', () => {
  let store: GraphStore;
  let manager: EpicManager;

  beforeEach(async () => {
    store = makeStore();
    await store.initialize();
    manager = new EpicManager(store);
  });

  // ===== CRUD =====

  describe('createEpic', () => {
    test('creates an epic in planning lifecycle', async () => {
      const epic = await manager.createEpic('epic-1', { title: 'Auth System Overhaul' });

      expect(epic.id).toBe('epic-1');
      expect(epic.lifecycle).toBe('planning');
      expect(epic.config.title).toBe('Auth System Overhaul');
      expect(epic.version).toBe(1);
      expect(epic.seq).toBe(1);
    });

    test('creates an epic with all config fields', async () => {
      const epic = await manager.createEpic('epic-full', {
        title: 'Full Epic',
        description: 'A fully configured epic',
        owner: 'alice',
      });

      expect(epic.config.title).toBe('Full Epic');
      expect(epic.config.description).toBe('A fully configured epic');
      expect(epic.config.owner).toBe('alice');
    });

    test('throws error if epic already exists', async () => {
      await manager.createEpic('epic-dup', { title: 'Duplicate' });
      await expect(manager.createEpic('epic-dup', { title: 'Another' })).rejects.toThrow(
        'Epic already exists: epic-dup'
      );
    });

    test('created and modified are ISO strings', async () => {
      const epic = await manager.createEpic('epic-dates', { title: 'Date Epic' });
      expect(typeof epic.created).toBe('string');
      expect(typeof epic.modified).toBe('string');
      // Should be valid ISO 8601
      expect(() => new Date(epic.created)).not.toThrow();
      expect(() => new Date(epic.modified)).not.toThrow();
    });
  });

  describe('getEpic', () => {
    test('returns null for non-existent epic', () => {
      const epic = manager.getEpic('nonexistent');
      expect(epic).toBeNull();
    });

    test('retrieves an existing epic', async () => {
      await manager.createEpic('epic-get', { title: 'Get Epic', owner: 'bob' });
      const epic = manager.getEpic('epic-get');
      expect(epic).not.toBeNull();
      expect(epic!.id).toBe('epic-get');
      expect(epic!.config.title).toBe('Get Epic');
      expect(epic!.config.owner).toBe('bob');
    });
  });

  describe('listEpics', () => {
    test('lists all epics', async () => {
      await manager.createEpic('list-1', { title: 'Epic 1' });
      await manager.createEpic('list-2', { title: 'Epic 2' });
      await manager.createEpic('list-3', { title: 'Epic 3' });

      const epics = manager.listEpics();
      expect(epics.length).toBe(3);
    });

    test('filters epics by lifecycle', async () => {
      await manager.createEpic('plan-1', { title: 'Planning Epic' });
      await manager.createEpic('active-1', { title: 'Active Epic' });
      await manager.activateEpic('active-1');

      const planning = manager.listEpics({ lifecycle: 'planning' });
      const active = manager.listEpics({ lifecycle: 'active' });

      expect(planning.length).toBe(1);
      expect(planning[0].id).toBe('plan-1');
      expect(active.length).toBe(1);
      expect(active[0].id).toBe('active-1');
    });

    test('returns empty array when no epics match filter', async () => {
      await manager.createEpic('plan-only', { title: 'Planning' });
      const completed = manager.listEpics({ lifecycle: 'completed' });
      expect(completed).toEqual([]);
    });
  });

  describe('updateEpic', () => {
    test('updates title only', async () => {
      await manager.createEpic('upd-1', { title: 'Original Title' });
      const updated = await manager.updateEpic('upd-1', { title: 'New Title' });
      expect(updated.config.title).toBe('New Title');
      expect(updated.version).toBe(2);
    });

    test('updates description and owner', async () => {
      await manager.createEpic('upd-2', { title: 'Epic' });
      const updated = await manager.updateEpic('upd-2', {
        description: 'New description',
        owner: 'carol',
      });
      expect(updated.config.description).toBe('New description');
      expect(updated.config.owner).toBe('carol');
    });

    test('unset fields remain unchanged', async () => {
      await manager.createEpic('upd-3', { title: 'Epic', owner: 'alice' });
      const updated = await manager.updateEpic('upd-3', { title: 'Updated Title' });
      expect(updated.config.owner).toBe('alice');
    });

    test('throws if epic not found', async () => {
      await expect(manager.updateEpic('nonexistent', { title: 'X' })).rejects.toThrow(
        'Epic not found: nonexistent'
      );
    });
  });

  // ===== LIFECYCLE TRANSITIONS =====

  describe('activateEpic', () => {
    test('transitions from planning to active', async () => {
      await manager.createEpic('act-1', { title: 'Epic' });
      const activated = await manager.activateEpic('act-1');
      expect(activated.lifecycle).toBe('active');
    });

    test('throws if not in planning lifecycle', async () => {
      await manager.createEpic('act-bad', { title: 'Epic' });
      await manager.activateEpic('act-bad');
      await expect(manager.activateEpic('act-bad')).rejects.toThrow(
        'Cannot activate epic in active lifecycle'
      );
    });

    test('throws if epic not found', async () => {
      await expect(manager.activateEpic('nonexistent')).rejects.toThrow('Epic not found: nonexistent');
    });
  });

  describe('completeEpic', () => {
    test('transitions from active to completed', async () => {
      await manager.createEpic('comp-1', { title: 'Epic' });
      await manager.activateEpic('comp-1');
      const completed = await manager.completeEpic('comp-1');
      expect(completed.lifecycle).toBe('completed');
    });

    test('throws if not in active lifecycle', async () => {
      await manager.createEpic('comp-bad', { title: 'Epic' });
      await expect(manager.completeEpic('comp-bad')).rejects.toThrow(
        'Cannot complete epic in planning lifecycle'
      );
    });

    test('throws if epic not found', async () => {
      await expect(manager.completeEpic('nonexistent')).rejects.toThrow('Epic not found: nonexistent');
    });
  });

  describe('cancelEpic', () => {
    test('cancels a planning epic', async () => {
      await manager.createEpic('cancel-1', { title: 'Epic' });
      const cancelled = await manager.cancelEpic('cancel-1');
      expect(cancelled.lifecycle).toBe('cancelled');
    });

    test('cancels an active epic', async () => {
      await manager.createEpic('cancel-2', { title: 'Epic' });
      await manager.activateEpic('cancel-2');
      const cancelled = await manager.cancelEpic('cancel-2');
      expect(cancelled.lifecycle).toBe('cancelled');
    });

    test('throws if epic is completed', async () => {
      await manager.createEpic('cancel-bad', { title: 'Epic' });
      await manager.activateEpic('cancel-bad');
      await manager.completeEpic('cancel-bad');
      await expect(manager.cancelEpic('cancel-bad')).rejects.toThrow(
        'Cannot cancel epic in completed lifecycle'
      );
    });

    test('throws if epic not found', async () => {
      await expect(manager.cancelEpic('nonexistent')).rejects.toThrow('Epic not found: nonexistent');
    });
  });

  // ===== LIFECYCLE STATE MACHINE =====

  describe('lifecycle machine enforcement', () => {
    test('planning -> active is valid', async () => {
      await manager.createEpic('sm-1', { title: 'Epic' });
      const activated = await manager.activateEpic('sm-1');
      expect(activated.lifecycle).toBe('active');
    });

    test('active -> completed is valid', async () => {
      await manager.createEpic('sm-2', { title: 'Epic' });
      await manager.activateEpic('sm-2');
      const completed = await manager.completeEpic('sm-2');
      expect(completed.lifecycle).toBe('completed');
    });

    test('planning -> cancelled is valid', async () => {
      await manager.createEpic('sm-3', { title: 'Epic' });
      const cancelled = await manager.cancelEpic('sm-3');
      expect(cancelled.lifecycle).toBe('cancelled');
    });

    test('active -> cancelled is valid', async () => {
      await manager.createEpic('sm-4', { title: 'Epic' });
      await manager.activateEpic('sm-4');
      const cancelled = await manager.cancelEpic('sm-4');
      expect(cancelled.lifecycle).toBe('cancelled');
    });

    test('completed -> activate is invalid', async () => {
      await manager.createEpic('sm-5', { title: 'Epic' });
      await manager.activateEpic('sm-5');
      await manager.completeEpic('sm-5');
      await expect(manager.activateEpic('sm-5')).rejects.toThrow(
        'Cannot activate epic in completed lifecycle'
      );
    });

    test('completed -> cancel is invalid', async () => {
      await manager.createEpic('sm-6', { title: 'Epic' });
      await manager.activateEpic('sm-6');
      await manager.completeEpic('sm-6');
      await expect(manager.cancelEpic('sm-6')).rejects.toThrow(
        'Cannot cancel epic in completed lifecycle'
      );
    });
  });

  // ===== TASK MEMBERSHIP =====

  describe('addTaskToEpic / removeTaskFromEpic / getEpicTasks', () => {
    let taskManager: TaskManager;

    beforeEach(() => {
      taskManager = new TaskManager(store);
    });

    test('adds a task to an epic', async () => {
      await manager.createEpic('mem-epic-1', { title: 'Epic' });
      await taskManager.createTask('mem-task-1', 'Task 1');

      await manager.addTaskToEpic('mem-epic-1', 'mem-task-1');

      const taskIds = manager.getEpicTaskIds('mem-epic-1');
      expect(taskIds).toContain('mem-task-1');
    });

    test('addTaskToEpic is idempotent', async () => {
      await manager.createEpic('idem-epic', { title: 'Epic' });
      await taskManager.createTask('idem-task', 'Task');

      await manager.addTaskToEpic('idem-epic', 'idem-task');
      await manager.addTaskToEpic('idem-epic', 'idem-task'); // second call should not throw

      const taskIds = manager.getEpicTaskIds('idem-epic');
      expect(taskIds.filter(id => id === 'idem-task').length).toBe(1);
    });

    test('removeTaskFromEpic removes the task', async () => {
      await manager.createEpic('rm-epic', { title: 'Epic' });
      await taskManager.createTask('rm-task', 'Task');

      await manager.addTaskToEpic('rm-epic', 'rm-task');
      await manager.removeTaskFromEpic('rm-epic', 'rm-task');

      const taskIds = manager.getEpicTaskIds('rm-epic');
      expect(taskIds).not.toContain('rm-task');
    });

    test('getEpicTasks returns task objects with lifecycle', async () => {
      await manager.createEpic('tasks-epic', { title: 'Epic' });
      await taskManager.createTask('tasks-t1', 'Task 1');
      await taskManager.createTask('tasks-t2', 'Task 2', { assignee: '@(agent)' });

      await manager.addTaskToEpic('tasks-epic', 'tasks-t1');
      await manager.addTaskToEpic('tasks-epic', 'tasks-t2');

      const tasks = manager.getEpicTasks('tasks-epic');
      expect(tasks.length).toBe(2);

      const t1 = tasks.find(t => t.id === 'tasks-t1');
      const t2 = tasks.find(t => t.id === 'tasks-t2');

      expect(t1).toBeDefined();
      expect(t1!.lifecycle).toBe('pending');
      expect(t1!.title).toBe('Task 1');

      expect(t2).toBeDefined();
      expect(t2!.lifecycle).toBe('assigned');
    });

    test('addTaskToEpic throws if epic not found', async () => {
      await taskManager.createTask('orphan-task', 'Task');
      await expect(manager.addTaskToEpic('nonexistent-epic', 'orphan-task')).rejects.toThrow(
        'Epic not found: nonexistent-epic'
      );
    });

    test('addTaskToEpic throws if task not found', async () => {
      await manager.createEpic('epic-no-task', { title: 'Epic' });
      await expect(manager.addTaskToEpic('epic-no-task', 'nonexistent-task')).rejects.toThrow(
        'Task not found: nonexistent-task'
      );
    });

    test('multiple tasks can be added to one epic', async () => {
      await manager.createEpic('multi-epic', { title: 'Epic' });
      for (let i = 1; i <= 5; i++) {
        await taskManager.createTask(`multi-task-${i}`, `Task ${i}`);
        await manager.addTaskToEpic('multi-epic', `multi-task-${i}`);
      }

      const taskIds = manager.getEpicTaskIds('multi-epic');
      expect(taskIds.length).toBe(5);
    });
  });

  // ===== STATS =====

  describe('getEpicStats', () => {
    let taskManager: TaskManager;

    beforeEach(() => {
      taskManager = new TaskManager(store);
    });

    test('returns zero stats for empty epic', async () => {
      await manager.createEpic('stats-empty', { title: 'Empty Epic' });
      const stats = manager.getEpicStats('stats-empty');

      expect(stats.total).toBe(0);
      expect(stats.completed).toBe(0);
      expect(stats.pending).toBe(0);
      expect(stats.in_progress).toBe(0);
    });

    test('counts tasks by lifecycle', async () => {
      await manager.createEpic('stats-epic', { title: 'Stats Epic' });

      // Create tasks in various states
      await taskManager.createTask('st-pending', 'Pending Task');
      await taskManager.createTask('st-assigned', 'Assigned Task', { assignee: '@(agent)' });
      await taskManager.createTask('st-done', 'Done Task', { assignee: '@(agent)' });
      await taskManager.startTask('st-done');
      await taskManager.completeTask('st-done');

      await manager.addTaskToEpic('stats-epic', 'st-pending');
      await manager.addTaskToEpic('stats-epic', 'st-assigned');
      await manager.addTaskToEpic('stats-epic', 'st-done');

      const stats = manager.getEpicStats('stats-epic');
      expect(stats.total).toBe(3);
      expect(stats.completed).toBe(1);
      // assigned counts as in_progress
      expect(stats.in_progress).toBe(1);
      expect(stats.pending).toBe(1);
    });
  });

  // ===== BACKWARD-COMPAT ALIASES =====

  describe('backward-compat aliases (state/data)', () => {
    test('epic.state is an alias for epic.lifecycle', async () => {
      const epic = await manager.createEpic('alias-1', { title: 'Alias Epic' });
      expect(epic.state).toBe('planning');
      expect(epic.state).toBe(epic.lifecycle);
    });

    test('epic.data is an alias for epic.config', async () => {
      const epic = await manager.createEpic('alias-2', { title: 'Alias Epic', owner: 'dave' });
      expect(epic.data).toBe(epic.config);
      expect(epic.data.title).toBe('Alias Epic');
      expect(epic.data.owner).toBe('dave');
    });

    test('getEpic returns epic with state/data aliases', async () => {
      await manager.createEpic('alias-get', { title: 'Get Epic' });
      const epic = manager.getEpic('alias-get')!;
      expect(epic.state).toBe(epic.lifecycle);
      expect(epic.data).toBe(epic.config);
    });

    test('listEpics returns epics with state/data aliases', async () => {
      await manager.createEpic('alias-list-1', { title: 'Epic 1' });
      await manager.createEpic('alias-list-2', { title: 'Epic 2' });
      const epics = manager.listEpics();
      for (const e of epics) {
        expect(e.state).toBe(e.lifecycle);
        expect(e.data).toBe(e.config);
      }
    });

    test('state reflects lifecycle changes', async () => {
      await manager.createEpic('alias-sm', { title: 'SM Epic' });
      await manager.activateEpic('alias-sm');
      const epic = manager.getEpic('alias-sm')!;
      expect(epic.state).toBe('active');
      expect(epic.lifecycle).toBe('active');
    });
  });
});
