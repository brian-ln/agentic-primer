import { test, expect, describe, beforeEach } from 'bun:test';
import { ProjectManager } from './project.ts';
import { EpicManager } from './epic.ts';
import { TaskManager } from './task.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

function makeStore(): GraphStore {
  testCounter++;
  return new GraphStore(
    `/tmp/ugs-project-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`
  );
}

describe('ProjectManager', () => {
  let store: GraphStore;
  let manager: ProjectManager;

  beforeEach(async () => {
    store = makeStore();
    await store.initialize();
    manager = new ProjectManager(store);
  });

  // ===== CRUD =====

  describe('createProject', () => {
    test('creates a project in draft status', async () => {
      const project = await manager.createProject('proj-1', { title: 'Q1 Work Graph Migration' });

      expect(project.id).toBe('proj-1');
      expect(project.status).toBe('draft');
      expect(project.config.title).toBe('Q1 Work Graph Migration');
      expect(project.version).toBe(1);
      expect(project.seq).toBe(1);
    });

    test('creates a project with all config fields', async () => {
      const project = await manager.createProject('proj-full', {
        title: 'Full Project',
        description: 'A fully configured project',
        owner: 'alice',
      });

      expect(project.config.title).toBe('Full Project');
      expect(project.config.description).toBe('A fully configured project');
      expect(project.config.owner).toBe('alice');
    });

    test('throws error if project already exists', async () => {
      await manager.createProject('proj-dup', { title: 'Duplicate' });
      await expect(manager.createProject('proj-dup', { title: 'Another' })).rejects.toThrow(
        'Project already exists: proj-dup'
      );
    });

    test('created and modified are ISO strings', async () => {
      const project = await manager.createProject('proj-dates', { title: 'Date Project' });
      expect(typeof project.created).toBe('string');
      expect(typeof project.modified).toBe('string');
      expect(() => new Date(project.created)).not.toThrow();
      expect(() => new Date(project.modified)).not.toThrow();
    });
  });

  describe('getProject', () => {
    test('returns null for non-existent project', () => {
      const project = manager.getProject('nonexistent');
      expect(project).toBeNull();
    });

    test('retrieves an existing project', async () => {
      await manager.createProject('proj-get', { title: 'Get Project', owner: 'bob' });
      const project = manager.getProject('proj-get');
      expect(project).not.toBeNull();
      expect(project!.id).toBe('proj-get');
      expect(project!.config.title).toBe('Get Project');
      expect(project!.config.owner).toBe('bob');
    });
  });

  describe('listProjects', () => {
    test('lists all projects', async () => {
      await manager.createProject('list-1', { title: 'Project 1' });
      await manager.createProject('list-2', { title: 'Project 2' });
      await manager.createProject('list-3', { title: 'Project 3' });

      const projects = manager.listProjects();
      expect(projects.length).toBe(3);
    });

    test('filters projects by status', async () => {
      await manager.createProject('draft-1', { title: 'Draft Project' });
      await manager.createProject('active-1', { title: 'Active Project' });
      await manager.activateProject('active-1');

      const drafts = manager.listProjects({ status: 'draft' });
      const actives = manager.listProjects({ status: 'active' });

      expect(drafts.length).toBe(1);
      expect(drafts[0].id).toBe('draft-1');
      expect(actives.length).toBe(1);
      expect(actives[0].id).toBe('active-1');
    });

    test('returns empty array when no projects match filter', async () => {
      await manager.createProject('draft-only', { title: 'Draft' });
      const completed = manager.listProjects({ status: 'completed' });
      expect(completed).toEqual([]);
    });
  });

  describe('updateProject', () => {
    test('updates title only', async () => {
      await manager.createProject('upd-1', { title: 'Original Title' });
      const updated = await manager.updateProject('upd-1', { title: 'New Title' });
      expect(updated.config.title).toBe('New Title');
      expect(updated.version).toBe(2);
    });

    test('updates description and owner', async () => {
      await manager.createProject('upd-2', { title: 'Project' });
      const updated = await manager.updateProject('upd-2', {
        description: 'New description',
        owner: 'carol',
      });
      expect(updated.config.description).toBe('New description');
      expect(updated.config.owner).toBe('carol');
    });

    test('unset fields remain unchanged', async () => {
      await manager.createProject('upd-3', { title: 'Project', owner: 'alice' });
      const updated = await manager.updateProject('upd-3', { title: 'Updated Title' });
      expect(updated.config.owner).toBe('alice');
    });

    test('throws if project not found', async () => {
      await expect(manager.updateProject('nonexistent', { title: 'X' })).rejects.toThrow(
        'Project not found: nonexistent'
      );
    });
  });

  // ===== STATUS TRANSITIONS =====

  describe('activateProject', () => {
    test('transitions from draft to active', async () => {
      await manager.createProject('act-1', { title: 'Project' });
      const activated = await manager.activateProject('act-1');
      expect(activated.status).toBe('active');
    });

    test('throws if not in draft status', async () => {
      await manager.createProject('act-bad', { title: 'Project' });
      await manager.activateProject('act-bad');
      await expect(manager.activateProject('act-bad')).rejects.toThrow(
        'Cannot activate project in active status'
      );
    });

    test('throws if project not found', async () => {
      await expect(manager.activateProject('nonexistent')).rejects.toThrow(
        'Project not found: nonexistent'
      );
    });
  });

  describe('completeProject', () => {
    test('transitions from active to completed', async () => {
      await manager.createProject('comp-1', { title: 'Project' });
      await manager.activateProject('comp-1');
      const completed = await manager.completeProject('comp-1');
      expect(completed.status).toBe('completed');
    });

    test('throws if not in active status', async () => {
      await manager.createProject('comp-bad', { title: 'Project' });
      await expect(manager.completeProject('comp-bad')).rejects.toThrow(
        'Cannot complete project in draft status'
      );
    });

    test('throws if project not found', async () => {
      await expect(manager.completeProject('nonexistent')).rejects.toThrow(
        'Project not found: nonexistent'
      );
    });
  });

  describe('archiveProject', () => {
    test('archives a draft project', async () => {
      await manager.createProject('arch-1', { title: 'Project' });
      const archived = await manager.archiveProject('arch-1');
      expect(archived.status).toBe('archived');
    });

    test('archives an active project', async () => {
      await manager.createProject('arch-2', { title: 'Project' });
      await manager.activateProject('arch-2');
      const archived = await manager.archiveProject('arch-2');
      expect(archived.status).toBe('archived');
    });

    test('archives a completed project', async () => {
      await manager.createProject('arch-3', { title: 'Project' });
      await manager.activateProject('arch-3');
      await manager.completeProject('arch-3');
      const archived = await manager.archiveProject('arch-3');
      expect(archived.status).toBe('archived');
    });

    test('throws if already archived', async () => {
      await manager.createProject('arch-bad', { title: 'Project' });
      await manager.archiveProject('arch-bad');
      await expect(manager.archiveProject('arch-bad')).rejects.toThrow(
        'already archived'
      );
    });

    test('throws if project not found', async () => {
      await expect(manager.archiveProject('nonexistent')).rejects.toThrow(
        'Project not found: nonexistent'
      );
    });
  });

  // ===== STATUS STATE MACHINE =====

  describe('status machine enforcement', () => {
    test('draft -> active is valid', async () => {
      await manager.createProject('sm-1', { title: 'Project' });
      const activated = await manager.activateProject('sm-1');
      expect(activated.status).toBe('active');
    });

    test('active -> completed is valid', async () => {
      await manager.createProject('sm-2', { title: 'Project' });
      await manager.activateProject('sm-2');
      const completed = await manager.completeProject('sm-2');
      expect(completed.status).toBe('completed');
    });

    test('draft -> archived is valid', async () => {
      await manager.createProject('sm-3', { title: 'Project' });
      const archived = await manager.archiveProject('sm-3');
      expect(archived.status).toBe('archived');
    });

    test('draft -> completed is invalid', async () => {
      await manager.createProject('sm-4', { title: 'Project' });
      await expect(manager.completeProject('sm-4')).rejects.toThrow(
        'Cannot complete project in draft status'
      );
    });

    test('completed -> activate is invalid', async () => {
      await manager.createProject('sm-5', { title: 'Project' });
      await manager.activateProject('sm-5');
      await manager.completeProject('sm-5');
      await expect(manager.activateProject('sm-5')).rejects.toThrow(
        'Cannot activate project in completed status'
      );
    });
  });

  // ===== EPIC MEMBERSHIP =====

  describe('addEpicToProject / removeEpicFromProject / getProjectEpics', () => {
    let epicManager: EpicManager;

    beforeEach(() => {
      epicManager = new EpicManager(store);
    });

    test('adds an epic to a project', async () => {
      await manager.createProject('mem-proj-1', { title: 'Project' });
      await epicManager.createEpic('mem-epic-1', { title: 'Epic 1' });

      await manager.addEpicToProject('mem-proj-1', 'mem-epic-1');

      const epicIds = manager.getProjectEpicIds('mem-proj-1');
      expect(epicIds).toContain('mem-epic-1');
    });

    test('addEpicToProject is idempotent', async () => {
      await manager.createProject('idem-proj', { title: 'Project' });
      await epicManager.createEpic('idem-epic', { title: 'Epic' });

      await manager.addEpicToProject('idem-proj', 'idem-epic');
      await manager.addEpicToProject('idem-proj', 'idem-epic'); // second call should not throw

      const epicIds = manager.getProjectEpicIds('idem-proj');
      expect(epicIds.filter(id => id === 'idem-epic').length).toBe(1);
    });

    test('removeEpicFromProject removes the epic', async () => {
      await manager.createProject('rm-proj', { title: 'Project' });
      await epicManager.createEpic('rm-epic', { title: 'Epic' });

      await manager.addEpicToProject('rm-proj', 'rm-epic');
      await manager.removeEpicFromProject('rm-proj', 'rm-epic');

      const epicIds = manager.getProjectEpicIds('rm-proj');
      expect(epicIds).not.toContain('rm-epic');
    });

    test('getProjectEpics returns epic objects with lifecycle', async () => {
      await manager.createProject('epics-proj', { title: 'Project' });
      await epicManager.createEpic('epics-e1', { title: 'Epic 1' });
      await epicManager.createEpic('epics-e2', { title: 'Epic 2' });
      await epicManager.activateEpic('epics-e2');

      await manager.addEpicToProject('epics-proj', 'epics-e1');
      await manager.addEpicToProject('epics-proj', 'epics-e2');

      const epics = manager.getProjectEpics('epics-proj');
      expect(epics.length).toBe(2);

      const e1 = epics.find(e => e.id === 'epics-e1');
      const e2 = epics.find(e => e.id === 'epics-e2');

      expect(e1).toBeDefined();
      expect(e1!.lifecycle).toBe('planning');
      expect(e1!.title).toBe('Epic 1');

      expect(e2).toBeDefined();
      expect(e2!.lifecycle).toBe('active');
    });

    test('addEpicToProject throws if project not found', async () => {
      await epicManager.createEpic('orphan-epic', { title: 'Epic' });
      await expect(manager.addEpicToProject('nonexistent-proj', 'orphan-epic')).rejects.toThrow(
        'Project not found: nonexistent-proj'
      );
    });

    test('addEpicToProject throws if epic not found', async () => {
      await manager.createProject('proj-no-epic', { title: 'Project' });
      await expect(manager.addEpicToProject('proj-no-epic', 'nonexistent-epic')).rejects.toThrow(
        'Epic not found: nonexistent-epic'
      );
    });

    test('multiple epics can be added to one project', async () => {
      await manager.createProject('multi-proj', { title: 'Project' });
      for (let i = 1; i <= 4; i++) {
        await epicManager.createEpic(`multi-epic-${i}`, { title: `Epic ${i}` });
        await manager.addEpicToProject('multi-proj', `multi-epic-${i}`);
      }

      const epicIds = manager.getProjectEpicIds('multi-proj');
      expect(epicIds.length).toBe(4);
    });
  });

  // ===== STATS =====

  describe('getProjectStats', () => {
    let epicManager: EpicManager;
    let taskManager: TaskManager;

    beforeEach(() => {
      epicManager = new EpicManager(store);
      taskManager = new TaskManager(store);
    });

    test('returns zero stats for empty project', async () => {
      await manager.createProject('stats-empty', { title: 'Empty Project' });
      const stats = manager.getProjectStats('stats-empty');

      expect(stats.epics).toBe(0);
      expect(stats.tasks).toBe(0);
      expect(stats.completed).toBe(0);
    });

    test('counts epics correctly', async () => {
      await manager.createProject('stats-proj', { title: 'Stats Project' });
      await epicManager.createEpic('stats-e1', { title: 'Epic 1' });
      await epicManager.createEpic('stats-e2', { title: 'Epic 2' });

      await manager.addEpicToProject('stats-proj', 'stats-e1');
      await manager.addEpicToProject('stats-proj', 'stats-e2');

      const stats = manager.getProjectStats('stats-proj');
      expect(stats.epics).toBe(2);
    });

    test('rolls up tasks from all epics', async () => {
      await manager.createProject('rollup-proj', { title: 'Rollup Project' });

      // Epic 1 with 2 tasks (1 completed)
      await epicManager.createEpic('rollup-e1', { title: 'Epic 1' });
      await taskManager.createTask('rollup-t1', 'Task 1', { assignee: '@(agent)' });
      await taskManager.startTask('rollup-t1');
      await taskManager.completeTask('rollup-t1');
      await taskManager.createTask('rollup-t2', 'Task 2');
      await epicManager.addTaskToEpic('rollup-e1', 'rollup-t1');
      await epicManager.addTaskToEpic('rollup-e1', 'rollup-t2');

      // Epic 2 with 1 task (not completed)
      await epicManager.createEpic('rollup-e2', { title: 'Epic 2' });
      await taskManager.createTask('rollup-t3', 'Task 3');
      await epicManager.addTaskToEpic('rollup-e2', 'rollup-t3');

      await manager.addEpicToProject('rollup-proj', 'rollup-e1');
      await manager.addEpicToProject('rollup-proj', 'rollup-e2');

      const stats = manager.getProjectStats('rollup-proj');
      expect(stats.epics).toBe(2);
      expect(stats.tasks).toBe(3);
      expect(stats.completed).toBe(1);
    });

    test('project without epics has zero task counts', async () => {
      await manager.createProject('no-epic-proj', { title: 'No Epic Project' });
      const stats = manager.getProjectStats('no-epic-proj');
      expect(stats.epics).toBe(0);
      expect(stats.tasks).toBe(0);
      expect(stats.completed).toBe(0);
    });
  });

  // ===== BACKWARD-COMPAT ALIASES =====

  describe('backward-compat aliases (data)', () => {
    test('project.data is an alias for project.config', async () => {
      const project = await manager.createProject('alias-1', { title: 'Alias Project', owner: 'dave' });
      expect(project.data).toBe(project.config);
      expect(project.data.title).toBe('Alias Project');
      expect(project.data.owner).toBe('dave');
    });

    test('getProject returns project with data alias', async () => {
      await manager.createProject('alias-get', { title: 'Get Project' });
      const project = manager.getProject('alias-get')!;
      expect(project.data).toBe(project.config);
    });

    test('listProjects returns projects with data alias', async () => {
      await manager.createProject('alias-list-1', { title: 'Project 1' });
      await manager.createProject('alias-list-2', { title: 'Project 2' });
      const projects = manager.listProjects();
      for (const p of projects) {
        expect(p.data).toBe(p.config);
      }
    });

    test('status reflects status transitions', async () => {
      await manager.createProject('alias-sm', { title: 'SM Project' });
      await manager.activateProject('alias-sm');
      const project = manager.getProject('alias-sm')!;
      expect(project.status).toBe('active');
    });
  });
});
