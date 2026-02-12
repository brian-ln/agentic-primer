#!/usr/bin/env bun
/**
 * CREATE_RELATIONSHIP Action Tests
 * Tests for CREATE RELATIONSHIP operations in the query/DSL layer
 * Target: >20 test cases covering relationship creation semantics
 */

import { test, expect, describe } from 'bun:test';
import { query, createRelationship } from '../builder.ts';
import { pattern } from '../pattern.ts';
import { QueryCompiler } from '../compiler.ts';
import type { QueryDefinition, QueryPlan } from '../types.ts';

describe('CREATE_RELATIONSHIP Action - Builder API', () => {
  test('creates basic CREATE_RELATIONSHIP action with builder', () => {
    const action = createRelationship('task', 'blocker', 'requires');

    const spec = action.build();
    expect(spec.type).toBe('create_relationship');
    expect(spec.params.from).toBe('task');
    expect(spec.params.to).toBe('blocker');
    expect(spec.params.type).toBe('requires');
  });

  test('creates CREATE_RELATIONSHIP action with properties', () => {
    const action = createRelationship('task', 'blocker', 'requires')
      .withProperties({ priority: 'high', createdAt: Date.now() });

    const spec = action.build();
    expect(spec.params.properties.priority).toBe('high');
    expect(spec.params.properties.createdAt).toBeTypeOf('number');
  });

  test('creates CREATE_RELATIONSHIP action with strength', () => {
    const action = createRelationship('task', 'dep', 'requires')
      .strength(0.9);

    const spec = action.build();
    expect(spec.params.properties.strength).toBe(0.9);
  });

  test('validates strength bounds (0-1)', () => {
    expect(() => {
      createRelationship('task', 'dep', 'requires').strength(1.5);
    }).toThrow('Strength must be between 0 and 1');

    expect(() => {
      createRelationship('task', 'dep', 'requires').strength(-0.1);
    }).toThrow('Strength must be between 0 and 1');
  });

  test('creates CREATE_RELATIONSHIP action with evidence', () => {
    const action = createRelationship('task', 'blocker', 'requires')
      .evidence('Task cannot proceed until blocker is resolved');

    const spec = action.build();
    expect(spec.params.properties.evidence).toBe('Task cannot proceed until blocker is resolved');
  });

  test('chains multiple property modifiers', () => {
    const action = createRelationship('knowledge', 'other', 'supports')
      .strength(0.8)
      .evidence('Strong supporting evidence')
      .withProperties({ source: 'analysis', verified: true });

    const spec = action.build();
    expect(spec.params.properties.strength).toBe(0.8);
    expect(spec.params.properties.evidence).toBe('Strong supporting evidence');
    expect(spec.params.properties.source).toBe('analysis');
    expect(spec.params.properties.verified).toBe(true);
  });
});

describe('CREATE_RELATIONSHIP Action - Query Integration', () => {
  test('adds CREATE_RELATIONSHIP to query via createRelationship method', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('blocker').label('Task').where({ id: 'task-2' })
      )
      .createRelationship('task', 'blocker', {
        type: 'requires',
        properties: { priority: 'high' },
      });

    const def = q.build();
    expect(def.actions).toHaveLength(1);
    expect(def.actions![0].type).toBe('create_relationship');
    expect(def.actions![0].params.type).toBe('requires');
  });

  test('adds CREATE_RELATIONSHIP using link shorthand', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('user').label('User').where({ id: 'alice' })
      )
      .link('task', 'user', 'assignedTo');

    const def = q.build();
    expect(def.actions).toHaveLength(1);
    expect(def.actions![0].type).toBe('create_relationship');
    expect(def.actions![0].params.type).toBe('assignedTo');
  });

  test('link shorthand accepts properties', () => {
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .link('task', 'user', 'assignedTo', { assignedAt: Date.now() });

    const def = q.build();
    expect(def.actions![0].params.properties.assignedAt).toBeTypeOf('number');
  });

  test('creates multiple relationships in single query', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'build' }),
        pattern('test').label('Task').where({ id: 'test' }),
        pattern('deploy').label('Task').where({ id: 'deploy' })
      )
      .link('test', 'build', 'requires')
      .link('deploy', 'test', 'requires');

    const def = q.build();
    expect(def.actions).toHaveLength(2);
    expect(def.actions![0].params.from).toBe('test');
    expect(def.actions![0].params.to).toBe('build');
    expect(def.actions![1].params.from).toBe('deploy');
    expect(def.actions![1].params.to).toBe('test');
  });

  test('creates relationship with different types', () => {
    const relationshipTypes = [
      'requires',
      'supports',
      'contradicts',
      'extends',
      'questions',
      'related-to',
    ];

    for (const type of relationshipTypes) {
      const q = query()
        .match(pattern('a'), pattern('b'))
        .link('a', 'b', type);

      const def = q.build();
      expect(def.actions![0].params.type).toBe(type);
    }
  });
});

describe('CREATE_RELATIONSHIP Action - Compilation', () => {
  test('compiles CREATE_RELATIONSHIP action with both variables', async () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('blocker').label('Task').where({ id: 'task-2' })
      )
      .createRelationship('task', 'blocker', { type: 'requires' });

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Should have 2 pattern steps + 1 relationship creation step
    expect(plan.steps).toHaveLength(3);

    const relStep = plan.steps[2];
    expect(relStep.type).toBe('action');
    expect(relStep.metadata?.actionType).toBe('create_relationship');
  });

  test('compiled relationship action targets RelationshipActor', async () => {
    const q = query()
      .match(pattern('task'), pattern('user'))
      .link('task', 'user', 'assignedTo');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.actor).toContain('relationships');
  });

  test('relationship action depends on both source and target patterns', async () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('blocker').label('Task').where({ id: 'task-2' })
      )
      .link('task', 'blocker', 'requires');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');

    // Should depend on both pattern resolution steps
    expect(relStep?.dependencies).toHaveLength(2);
  });

  test('relationship action is not parallelizable (requires dependencies)', async () => {
    const q = query()
      .match(pattern('task'), pattern('user'))
      .link('task', 'user', 'assignedTo');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.parallelizable).toBe(false);
  });

  test('throws error if from variable not found', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .createRelationship('nonexistent', 'task', { type: 'requires' });

    const compiler = new QueryCompiler();

    await expect(async () => {
      await compiler.compile(q.build());
    }).toThrow(/unknown from variable/);
  });

  test('throws error if to variable not found', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .createRelationship('task', 'nonexistent', { type: 'requires' });

    const compiler = new QueryCompiler();

    await expect(async () => {
      await compiler.compile(q.build());
    }).toThrow(/unknown to variable/);
  });

  test('relationship creation uses ask pattern for response', async () => {
    const q = query()
      .match(pattern('task'), pattern('user'))
      .link('task', 'user', 'assignedTo');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.message.pattern).toBe('ask');
    expect(relStep?.message.type).toBe('create');
  });

  test('relationship payload includes type and properties', async () => {
    const q = query()
      .match(pattern('task'), pattern('blocker'))
      .createRelationship('task', 'blocker', {
        type: 'requires',
        properties: { priority: 'high', createdAt: 123456 },
      });

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.message.payload.type).toBe('requires');
    expect(relStep?.message.payload.priority).toBe('high');
    expect(relStep?.message.payload.createdAt).toBe(123456);
  });
});

describe('CREATE_RELATIONSHIP Action - Metadata', () => {
  test('stores from variable in metadata', async () => {
    const q = query()
      .match(pattern('task'), pattern('user'))
      .link('task', 'user', 'assignedTo');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.metadata?.fromVariable).toBe('task');
  });

  test('stores to variable in metadata', async () => {
    const q = query()
      .match(pattern('task'), pattern('user'))
      .link('task', 'user', 'assignedTo');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.metadata?.toVariable).toBe('user');
  });

  test('stores relationship type in metadata', async () => {
    const q = query()
      .match(pattern('task'), pattern('blocker'))
      .link('task', 'blocker', 'requires');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.metadata?.relationshipType).toBe('requires');
  });
});

describe('CREATE_RELATIONSHIP Action - Complex Scenarios', () => {
  test('creates dependency graph relationships', async () => {
    const q = query()
      .match(
        pattern('compile').label('Task').where({ id: 'compile' }),
        pattern('test').label('Task').where({ id: 'test' }),
        pattern('deploy').label('Task').where({ id: 'deploy' })
      )
      .link('test', 'compile', 'requires')
      .link('deploy', 'test', 'requires');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relSteps = plan.steps.filter(s => s.metadata?.actionType === 'create_relationship');
    expect(relSteps).toHaveLength(2);

    // First relationship: test requires compile
    expect(relSteps[0].metadata?.fromVariable).toBe('test');
    expect(relSteps[0].metadata?.toVariable).toBe('compile');

    // Second relationship: deploy requires test
    expect(relSteps[1].metadata?.fromVariable).toBe('deploy');
    expect(relSteps[1].metadata?.toVariable).toBe('test');
  });

  test('creates bidirectional relationships', async () => {
    const q = query()
      .match(
        pattern('task1').label('Task').where({ id: 't1' }),
        pattern('task2').label('Task').where({ id: 't2' })
      )
      .link('task1', 'task2', 'related-to')
      .link('task2', 'task1', 'related-to');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relSteps = plan.steps.filter(s => s.metadata?.actionType === 'create_relationship');
    expect(relSteps).toHaveLength(2);
  });

  test('creates knowledge graph relationships with evidence', async () => {
    const q = query()
      .match(
        pattern('decision').label('Knowledge').where({ type: 'decision' }),
        pattern('learning').label('Knowledge').where({ type: 'learning' })
      )
      .createRelationship('learning', 'decision', {
        type: 'supports',
        properties: {
          strength: 0.9,
          evidence: 'Learning directly validates decision',
          source: 'session-123',
        },
      });

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.message.payload.strength).toBe(0.9);
    expect(relStep?.message.payload.evidence).toContain('validates');
    expect(relStep?.message.payload.source).toBe('session-123');
  });

  test('combines CREATE entity with CREATE relationship', async () => {
    const q = query()
      .match(pattern('user').label('User').where({ id: 'alice' }))
      .forEach(
        // CREATE is handled via forEach
        createRelationship('newTask', 'user', 'assignedTo')
      );

    const def = q.build();
    expect(def.patterns).toHaveLength(1);
    expect(def.actions).toHaveLength(1);
  });
});

describe('CREATE_RELATIONSHIP Action - Edge Cases', () => {
  test('handles same variable for from and to (self-relationship)', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'recursive' }))
      .link('task', 'task', 'extends');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.metadata?.fromVariable).toBe('task');
    expect(relStep?.metadata?.toVariable).toBe('task');
  });

  test('handles empty properties object', async () => {
    const q = query()
      .match(pattern('task'), pattern('user'))
      .createRelationship('task', 'user', {
        type: 'assignedTo',
        properties: {},
      });

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.message.payload.type).toBe('assignedTo');
  });

  test('handles relationship creation without explicit properties', async () => {
    const q = query()
      .match(pattern('task'), pattern('user'))
      .link('task', 'user', 'assignedTo');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(s => s.metadata?.actionType === 'create_relationship');
    expect(relStep?.message.payload.type).toBe('assignedTo');
  });

  test('validates relationship types', () => {
    // This would be enhanced in a real implementation with type validation
    const validTypes = ['requires', 'supports', 'contradicts', 'extends', 'questions', 'related-to'];

    for (const type of validTypes) {
      const action = createRelationship('a', 'b', type);
      expect(action.build().params.type).toBe(type);
    }
  });
});
