import { test, expect, describe, beforeEach } from 'bun:test';
import { InformationManager, Information, InformationLifecycle, InformationType } from './information.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

describe('InformationManager', () => {
  let store: GraphStore;
  let manager: InformationManager;

  beforeEach(async () => {
    // Create a fresh in-memory store for each test with unique path
    testCounter++;
    store = new GraphStore(`/tmp/ugs-info-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`);
    await store.initialize();
    manager = new InformationManager(store);
  });

  describe('createInformation', () => {
    test('creates information in draft lifecycle', async () => {
      const info = await manager.createInformation('test-info', 'fact', { key: 'value' });

      expect(info.id).toBe('test-info');
      expect(info.config.infoType).toBe('fact');
      expect(info.config.content).toEqual({ key: 'value' });
      expect(info.lifecycle).toBe('draft');
      expect(info.version).toBe(1);
      expect(info.type).toBe('program');
      expect(info.programType).toBe('information');
    });

    test('creates information with all infoTypes', async () => {
      const types: InformationType[] = ['fact', 'schema', 'workflow', 'pattern'];

      for (const infoType of types) {
        const info = await manager.createInformation(`info-${infoType}`, infoType, { type: infoType });
        expect(info.config.infoType).toBe(infoType);
      }
    });

    test('creates information with custom options', async () => {
      const info = await manager.createInformation('custom-info', 'fact', { data: 'test' }, {
        schema: '@(my-schema)',
        sources: ['@(task-1)', 'https://example.com'],
        tags: ['tag1', 'tag2'],
        description: 'Test information'
      });

      expect(info.config.schema).toBe('@(my-schema)');
      expect(info.config.sources).toEqual(['@(task-1)', 'https://example.com']);
      expect(info.config.tags).toEqual(['tag1', 'tag2']);
      expect(info.config.description).toBe('Test information');
    });

    test('throws error if information already exists', async () => {
      await manager.createInformation('dup-info', 'fact', { data: 'first' });

      await expect(manager.createInformation('dup-info', 'fact', { data: 'second' }))
        .rejects.toThrow('Information already exists: dup-info');
    });

    test('emits INFORMATION_CREATED event', async () => {
      await manager.createInformation('event-test', 'fact', { key: 'value' }, {
        tags: ['test-tag']
      });

      const events = manager.getInformationEvents();
      expect(events.length).toBeGreaterThanOrEqual(1);

      const createEvent = events.find(e => e.type === 'INFORMATION_CREATED' && e.informationId === 'event-test');
      expect(createEvent).toBeDefined();
      expect(createEvent!.data.lifecycle).toBe('draft');
      expect(createEvent!.data.infoType).toBe('fact');
      expect(createEvent!.data.tags).toEqual(['test-tag']);
    });
  });

  describe('getInformation', () => {
    test('returns null for non-existent information', () => {
      const info = manager.getInformation('nonexistent');
      expect(info).toBeNull();
    });

    test('retrieves existing information', async () => {
      await manager.createInformation('get-test', 'schema', { fields: ['a', 'b'] }, {
        description: 'A schema'
      });

      const info = manager.getInformation('get-test');
      expect(info).not.toBeNull();
      expect(info!.id).toBe('get-test');
      expect(info!.config.content).toEqual({ fields: ['a', 'b'] });
      expect(info!.config.description).toBe('A schema');
    });
  });

  describe('updateInformation', () => {
    test('updates a draft information', async () => {
      await manager.createInformation('update-test', 'fact', { original: true });

      const updated = await manager.updateInformation('update-test', {
        content: { updated: true },
        tags: ['new-tag'],
        description: 'Updated description'
      });

      expect(updated.config.content).toEqual({ updated: true });
      expect(updated.config.tags).toEqual(['new-tag']);
      expect(updated.config.description).toBe('Updated description');
      expect(updated.version).toBe(2);
    });

    test('throws error when updating non-existent information', async () => {
      await expect(manager.updateInformation('nonexistent', { content: 'test' }))
        .rejects.toThrow('Information not found: nonexistent');
    });

    test('throws error when updating validated information', async () => {
      await manager.createInformation('val-update', 'fact', { data: 'test' });
      await manager.validateInformation('val-update');

      await expect(manager.updateInformation('val-update', { content: 'new' }))
        .rejects.toThrow('Cannot update information in validated lifecycle');
    });

    test('emits INFORMATION_UPDATED event', async () => {
      await manager.createInformation('update-evt', 'fact', { data: 'test' });
      await manager.updateInformation('update-evt', { content: { new: 'data' } });

      const events = manager.getInformationEventHistory('update-evt');
      const updateEvent = events.find(e => e.type === 'INFORMATION_UPDATED');
      expect(updateEvent).toBeDefined();
      expect(updateEvent!.data.newVersion).toBe(2);
    });
  });

  describe('validateInformation', () => {
    test('validates a draft information', async () => {
      await manager.createInformation('val-test', 'fact', { data: 'test' });

      const validated = await manager.validateInformation('val-test');

      expect(validated.lifecycle).toBe('validated');
    });

    test('throws error when validating non-existent information', async () => {
      await expect(manager.validateInformation('nonexistent'))
        .rejects.toThrow('Information not found: nonexistent');
    });

    test('throws error when validating already validated information', async () => {
      await manager.createInformation('double-val', 'fact', { data: 'test' });
      await manager.validateInformation('double-val');

      await expect(manager.validateInformation('double-val'))
        .rejects.toThrow('Cannot validate information in validated lifecycle');
    });

    test('emits INFORMATION_VALIDATED event', async () => {
      await manager.createInformation('val-evt', 'fact', { data: 'test' });
      await manager.validateInformation('val-evt');

      const events = manager.getInformationEventHistory('val-evt');
      const valEvent = events.find(e => e.type === 'INFORMATION_VALIDATED');
      expect(valEvent).toBeDefined();
      expect(valEvent!.data.newLifecycle).toBe('validated');
    });
  });

  describe('activateInformation', () => {
    test('activates validated information', async () => {
      await manager.createInformation('act-test', 'fact', { data: 'test' });
      await manager.validateInformation('act-test');

      const activated = await manager.activateInformation('act-test');

      expect(activated.lifecycle).toBe('active');
    });

    test('activates archived information (reactivation)', async () => {
      await manager.createInformation('react-test', 'fact', { data: 'test' });
      await manager.validateInformation('react-test');
      await manager.activateInformation('react-test');
      await manager.archiveInformation('react-test');

      const reactivated = await manager.activateInformation('react-test');

      expect(reactivated.lifecycle).toBe('active');
    });

    test('throws error when activating draft information', async () => {
      await manager.createInformation('draft-act', 'fact', { data: 'test' });

      await expect(manager.activateInformation('draft-act'))
        .rejects.toThrow('Cannot activate information in draft lifecycle');
    });

    test('emits INFORMATION_ACTIVATED event', async () => {
      await manager.createInformation('act-evt', 'fact', { data: 'test' });
      await manager.validateInformation('act-evt');
      await manager.activateInformation('act-evt');

      const events = manager.getInformationEventHistory('act-evt');
      const actEvent = events.find(e => e.type === 'INFORMATION_ACTIVATED');
      expect(actEvent).toBeDefined();
      expect(actEvent!.data.newLifecycle).toBe('active');
    });
  });

  describe('archiveInformation', () => {
    test('archives active information', async () => {
      await manager.createInformation('arch-test', 'fact', { data: 'test' });
      await manager.validateInformation('arch-test');
      await manager.activateInformation('arch-test');

      const archived = await manager.archiveInformation('arch-test');

      expect(archived.lifecycle).toBe('archived');
    });

    test('throws error when archiving draft information', async () => {
      await manager.createInformation('draft-arch', 'fact', { data: 'test' });

      await expect(manager.archiveInformation('draft-arch'))
        .rejects.toThrow('Cannot archive information in draft lifecycle');
    });

    test('throws error when archiving validated information', async () => {
      await manager.createInformation('val-arch', 'fact', { data: 'test' });
      await manager.validateInformation('val-arch');

      await expect(manager.archiveInformation('val-arch'))
        .rejects.toThrow('Cannot archive information in validated lifecycle');
    });

    test('emits INFORMATION_ARCHIVED event', async () => {
      await manager.createInformation('arch-evt', 'fact', { data: 'test' });
      await manager.validateInformation('arch-evt');
      await manager.activateInformation('arch-evt');
      await manager.archiveInformation('arch-evt');

      const events = manager.getInformationEventHistory('arch-evt');
      const archEvent = events.find(e => e.type === 'INFORMATION_ARCHIVED');
      expect(archEvent).toBeDefined();
      expect(archEvent!.data.newLifecycle).toBe('archived');
    });
  });

  describe('listInformation', () => {
    test('lists all information', async () => {
      await manager.createInformation('list-1', 'fact', { a: 1 });
      await manager.createInformation('list-2', 'schema', { b: 2 });
      await manager.createInformation('list-3', 'workflow', { c: 3 });

      const infos = manager.listInformation();

      expect(infos.length).toBe(3);
    });

    test('filters information by lifecycle', async () => {
      await manager.createInformation('draft-1', 'fact', { d: 1 });
      await manager.createInformation('draft-2', 'fact', { d: 2 });
      await manager.createInformation('val-1', 'fact', { v: 1 });
      await manager.validateInformation('val-1');

      const drafts = manager.listInformation('draft');
      const validated = manager.listInformation('validated');

      expect(drafts.length).toBe(2);
      expect(validated.length).toBe(1);
      expect(validated[0].id).toBe('val-1');
    });
  });

  describe('queryByType', () => {
    test('queries information by infoType', async () => {
      await manager.createInformation('fact-1', 'fact', { f: 1 });
      await manager.createInformation('fact-2', 'fact', { f: 2 });
      await manager.createInformation('schema-1', 'schema', { s: 1 });
      await manager.createInformation('workflow-1', 'workflow', { w: 1 });

      const facts = manager.queryByType('fact');
      const schemas = manager.queryByType('schema');
      const patterns = manager.queryByType('pattern');

      expect(facts.length).toBe(2);
      expect(schemas.length).toBe(1);
      expect(patterns.length).toBe(0);
    });
  });

  describe('queryByTag', () => {
    test('queries information by tag', async () => {
      await manager.createInformation('tagged-1', 'fact', { t: 1 }, { tags: ['alpha', 'beta'] });
      await manager.createInformation('tagged-2', 'fact', { t: 2 }, { tags: ['beta', 'gamma'] });
      await manager.createInformation('tagged-3', 'fact', { t: 3 }, { tags: ['gamma'] });
      await manager.createInformation('untagged', 'fact', { t: 4 });

      const alphas = manager.queryByTag('alpha');
      const betas = manager.queryByTag('beta');
      const gammas = manager.queryByTag('gamma');
      const deltas = manager.queryByTag('delta');

      expect(alphas.length).toBe(1);
      expect(betas.length).toBe(2);
      expect(gammas.length).toBe(2);
      expect(deltas.length).toBe(0);
    });
  });

  describe('lifecycle machine enforcement', () => {
    test('draft -> validated transition is valid', async () => {
      await manager.createInformation('sm-1', 'fact', { data: 1 });
      const validated = await manager.validateInformation('sm-1');
      expect(validated.lifecycle).toBe('validated');
    });

    test('validated -> active transition is valid', async () => {
      await manager.createInformation('sm-2', 'fact', { data: 2 });
      await manager.validateInformation('sm-2');
      const active = await manager.activateInformation('sm-2');
      expect(active.lifecycle).toBe('active');
    });

    test('active -> archived transition is valid', async () => {
      await manager.createInformation('sm-3', 'fact', { data: 3 });
      await manager.validateInformation('sm-3');
      await manager.activateInformation('sm-3');
      const archived = await manager.archiveInformation('sm-3');
      expect(archived.lifecycle).toBe('archived');
    });

    test('archived -> active transition is valid (reactivation)', async () => {
      await manager.createInformation('sm-4', 'fact', { data: 4 });
      await manager.validateInformation('sm-4');
      await manager.activateInformation('sm-4');
      await manager.archiveInformation('sm-4');
      const reactivated = await manager.activateInformation('sm-4');
      expect(reactivated.lifecycle).toBe('active');
    });

    test('draft -> active transition is invalid', async () => {
      await manager.createInformation('sm-5', 'fact', { data: 5 });
      await expect(manager.activateInformation('sm-5'))
        .rejects.toThrow('Cannot activate information in draft lifecycle');
    });

    test('draft -> archived transition is invalid', async () => {
      await manager.createInformation('sm-6', 'fact', { data: 6 });
      await expect(manager.archiveInformation('sm-6'))
        .rejects.toThrow('Cannot archive information in draft lifecycle');
    });

    test('validated -> archived transition is invalid', async () => {
      await manager.createInformation('sm-7', 'fact', { data: 7 });
      await manager.validateInformation('sm-7');
      await expect(manager.archiveInformation('sm-7'))
        .rejects.toThrow('Cannot archive information in validated lifecycle');
    });
  });

  describe('content storage and retrieval', () => {
    test('stores and retrieves complex content', async () => {
      const complexContent = {
        nested: {
          deep: {
            value: 'test'
          }
        },
        array: [1, 2, { three: 3 }],
        boolean: true,
        number: 42.5,
        null: null
      };

      await manager.createInformation('complex', 'fact', complexContent);
      const info = manager.getInformation('complex');

      expect(info!.config.content).toEqual(complexContent);
    });

    test('stores and retrieves string content', async () => {
      await manager.createInformation('string-content', 'fact', 'simple string');
      const info = manager.getInformation('string-content');
      expect(info!.config.content).toBe('simple string');
    });

    test('stores and retrieves array content', async () => {
      await manager.createInformation('array-content', 'fact', [1, 2, 3, 'four']);
      const info = manager.getInformation('array-content');
      expect(info!.config.content).toEqual([1, 2, 3, 'four']);
    });
  });
});
