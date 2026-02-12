import { test, expect, describe, beforeEach } from 'bun:test';
import { ProgramManager, Program, ProgramState } from './program.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

describe('ProgramManager', () => {
  let store: GraphStore;
  let manager: ProgramManager;

  beforeEach(async () => {
    // Create a fresh in-memory store for each test with unique path
    testCounter++;
    store = new GraphStore(`/tmp/ugs-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`);
    await store.initialize();
    manager = new ProgramManager(store);
  });

  describe('createProgram', () => {
    test('creates a program in draft state', async () => {
      const program = await manager.createProgram('test-prog', 'return input * 2;');

      expect(program.id).toBe('test-prog');
      expect(program.impl).toBe('return input * 2;');
      expect(program.state).toBe('draft');
      expect(program.version).toBe(1);
      expect(program.name).toBe('test-prog'); // Default name is the ID
    });

    test('creates a program with custom options', async () => {
      const program = await manager.createProgram('calc', 'return input.a + input.b;', {
        name: 'Calculator',
        description: 'Adds two numbers',
        inputSchema: { type: 'object', properties: { a: { type: 'number' }, b: { type: 'number' } } },
        outputSchema: { type: 'number' }
      });

      expect(program.name).toBe('Calculator');
      expect(program.description).toBe('Adds two numbers');
      expect(program.inputSchema).toEqual({ type: 'object', properties: { a: { type: 'number' }, b: { type: 'number' } } });
      expect(program.outputSchema).toEqual({ type: 'number' });
    });

    test('throws error if program already exists', async () => {
      await manager.createProgram('dup', 'return 1;');

      await expect(manager.createProgram('dup', 'return 2;')).rejects.toThrow('Program already exists: dup');
    });

    test('emits PROGRAM_CREATED event', async () => {
      await manager.createProgram('event-test', 'return null;');

      const events = manager.getProgramEvents();
      expect(events.length).toBeGreaterThanOrEqual(1);

      const createEvent = events.find(e => e.type === 'PROGRAM_CREATED' && e.programId === 'event-test');
      expect(createEvent).toBeDefined();
      expect(createEvent!.data.state).toBe('draft');
    });
  });

  describe('getProgram', () => {
    test('returns null for non-existent program', () => {
      const program = manager.getProgram('nonexistent');
      expect(program).toBeNull();
    });

    test('retrieves an existing program', async () => {
      await manager.createProgram('get-test', 'return "hello";', { name: 'Get Test' });

      const program = manager.getProgram('get-test');
      expect(program).not.toBeNull();
      expect(program!.id).toBe('get-test');
      expect(program!.name).toBe('Get Test');
      expect(program!.impl).toBe('return "hello";');
    });
  });

  describe('updateProgram', () => {
    test('updates a draft program', async () => {
      await manager.createProgram('update-test', 'return 1;');

      const updated = await manager.updateProgram('update-test', {
        impl: 'return 2;',
        name: 'Updated Program'
      });

      expect(updated.impl).toBe('return 2;');
      expect(updated.name).toBe('Updated Program');
      expect(updated.version).toBe(2);
    });

    test('throws error when updating non-existent program', async () => {
      await expect(manager.updateProgram('nonexistent', { impl: 'new code' }))
        .rejects.toThrow('Program not found: nonexistent');
    });

    test('throws error when updating published program', async () => {
      await manager.createProgram('pub-test', 'return 1;');
      await manager.publishProgram('pub-test');

      await expect(manager.updateProgram('pub-test', { impl: 'return 2;' }))
        .rejects.toThrow('Cannot update program in published state');
    });

    test('emits PROGRAM_UPDATED event', async () => {
      await manager.createProgram('update-evt', 'return 1;');
      await manager.updateProgram('update-evt', { impl: 'return 2;' });

      const events = manager.getProgramEventHistory('update-evt');
      const updateEvent = events.find(e => e.type === 'PROGRAM_UPDATED');
      expect(updateEvent).toBeDefined();
      expect(updateEvent!.data.newVersion).toBe(2);
    });
  });

  describe('publishProgram', () => {
    test('publishes a draft program', async () => {
      await manager.createProgram('pub-test', 'return "published";');

      const published = await manager.publishProgram('pub-test');

      expect(published.state).toBe('published');
    });

    test('throws error when publishing non-existent program', async () => {
      await expect(manager.publishProgram('nonexistent'))
        .rejects.toThrow('Program not found: nonexistent');
    });

    test('throws error when publishing already published program', async () => {
      await manager.createProgram('double-pub', 'return 1;');
      await manager.publishProgram('double-pub');

      await expect(manager.publishProgram('double-pub'))
        .rejects.toThrow('Cannot publish program in published state');
    });

    test('emits PROGRAM_PUBLISHED event', async () => {
      await manager.createProgram('pub-evt', 'return 1;');
      await manager.publishProgram('pub-evt');

      const events = manager.getProgramEventHistory('pub-evt');
      const pubEvent = events.find(e => e.type === 'PROGRAM_PUBLISHED');
      expect(pubEvent).toBeDefined();
      expect(pubEvent!.data.newState).toBe('published');
    });
  });

  describe('deprecateProgram', () => {
    test('deprecates a published program', async () => {
      await manager.createProgram('dep-test', 'return "deprecated";');
      await manager.publishProgram('dep-test');

      const deprecated = await manager.deprecateProgram('dep-test');

      expect(deprecated.state).toBe('deprecated');
    });

    test('throws error when deprecating draft program', async () => {
      await manager.createProgram('draft-dep', 'return 1;');

      await expect(manager.deprecateProgram('draft-dep'))
        .rejects.toThrow('Cannot deprecate program in draft state');
    });

    test('emits PROGRAM_DEPRECATED event', async () => {
      await manager.createProgram('dep-evt', 'return 1;');
      await manager.publishProgram('dep-evt');
      await manager.deprecateProgram('dep-evt');

      const events = manager.getProgramEventHistory('dep-evt');
      const depEvent = events.find(e => e.type === 'PROGRAM_DEPRECATED');
      expect(depEvent).toBeDefined();
      expect(depEvent!.data.newState).toBe('deprecated');
    });
  });

  describe('invokeProgram', () => {
    test('invokes a published program successfully', async () => {
      await manager.createProgram('invoke-test', 'return input * 2;');
      await manager.publishProgram('invoke-test');

      const result = await manager.invokeProgram('invoke-test', 5);

      expect(result.success).toBe(true);
      expect(result.output).toBe(10);
      expect(result.duration).toBeGreaterThanOrEqual(0);
    });

    test('invokes a program with object input', async () => {
      await manager.createProgram('obj-invoke', 'return input.a + input.b;');
      await manager.publishProgram('obj-invoke');

      const result = await manager.invokeProgram('obj-invoke', { a: 3, b: 4 });

      expect(result.success).toBe(true);
      expect(result.output).toBe(7);
    });

    test('throws error when invoking draft program', async () => {
      await manager.createProgram('draft-invoke', 'return 1;');

      await expect(manager.invokeProgram('draft-invoke'))
        .rejects.toThrow('Cannot invoke program in draft state');
    });

    test('throws error when invoking deprecated program', async () => {
      await manager.createProgram('dep-invoke', 'return 1;');
      await manager.publishProgram('dep-invoke');
      await manager.deprecateProgram('dep-invoke');

      await expect(manager.invokeProgram('dep-invoke'))
        .rejects.toThrow('Cannot invoke program in deprecated state');
    });

    test('returns error result on execution failure', async () => {
      await manager.createProgram('fail-invoke', 'throw new Error("intentional failure");');
      await manager.publishProgram('fail-invoke');

      const result = await manager.invokeProgram('fail-invoke');

      expect(result.success).toBe(false);
      expect(result.error).toBe('intentional failure');
    });

    test('emits PROGRAM_INVOKED event', async () => {
      await manager.createProgram('invoke-evt', 'return 42;');
      await manager.publishProgram('invoke-evt');
      await manager.invokeProgram('invoke-evt', { test: true });

      const events = manager.getProgramEventHistory('invoke-evt');
      const invokeEvent = events.find(e => e.type === 'PROGRAM_INVOKED');
      expect(invokeEvent).toBeDefined();
      expect(invokeEvent!.data.success).toBe(true);
    });
  });

  describe('listPrograms', () => {
    test('lists all programs', async () => {
      await manager.createProgram('list-1', 'return 1;');
      await manager.createProgram('list-2', 'return 2;');
      await manager.createProgram('list-3', 'return 3;');

      const programs = manager.listPrograms();

      expect(programs.length).toBe(3);
    });

    test('filters programs by state', async () => {
      await manager.createProgram('draft-1', 'return 1;');
      await manager.createProgram('draft-2', 'return 2;');
      await manager.createProgram('pub-1', 'return 3;');
      await manager.publishProgram('pub-1');

      const drafts = manager.listPrograms('draft');
      const published = manager.listPrograms('published');

      expect(drafts.length).toBe(2);
      expect(published.length).toBe(1);
      expect(published[0].id).toBe('pub-1');
    });
  });

  describe('state machine enforcement', () => {
    test('draft -> published transition is valid', async () => {
      await manager.createProgram('sm-1', 'return 1;');
      const published = await manager.publishProgram('sm-1');
      expect(published.state).toBe('published');
    });

    test('published -> deprecated transition is valid', async () => {
      await manager.createProgram('sm-2', 'return 1;');
      await manager.publishProgram('sm-2');
      const deprecated = await manager.deprecateProgram('sm-2');
      expect(deprecated.state).toBe('deprecated');
    });

    test('draft -> deprecated transition is invalid', async () => {
      await manager.createProgram('sm-3', 'return 1;');
      await expect(manager.deprecateProgram('sm-3'))
        .rejects.toThrow('Cannot deprecate program in draft state');
    });

    test('deprecated -> published transition is invalid', async () => {
      await manager.createProgram('sm-4', 'return 1;');
      await manager.publishProgram('sm-4');
      await manager.deprecateProgram('sm-4');
      await expect(manager.publishProgram('sm-4'))
        .rejects.toThrow('Cannot publish program in deprecated state');
    });

    test('published -> draft transition is invalid (no update allowed)', async () => {
      await manager.createProgram('sm-5', 'return 1;');
      await manager.publishProgram('sm-5');
      await expect(manager.updateProgram('sm-5', { impl: 'return 2;' }))
        .rejects.toThrow('Cannot update program in published state');
    });
  });
});
