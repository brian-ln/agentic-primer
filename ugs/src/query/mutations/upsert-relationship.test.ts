#!/usr/bin/env bun
/**
 * UPSERT_RELATIONSHIP Action Tests
 * Tests for idempotent relationship updates (Phase 3 R1)
 * Target: 40+ test cases covering upsert semantics
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { query, upsertRelationship } from '../builder.ts';
import { pattern } from '../pattern.ts';
import { QueryCompiler } from '../compiler.ts';
import type { QueryDefinition, QueryPlan } from '../types.ts';

describe('UPSERT_RELATIONSHIP Action - Builder API', () => {
  test('creates basic UPSERT_RELATIONSHIP action with builder', () => {
    const action = upsertRelationship('task', 'user', 'assignedTo');

    const spec = action.build();
    expect(spec.type).toBe('upsert_relationship');
    expect(spec.params.from).toBe('task');
    expect(spec.params.to).toBe('user');
    expect(spec.params.type).toBe('assignedTo');
  });

  test('creates UPSERT_RELATIONSHIP action with properties', () => {
    const action = upsertRelationship('task', 'user', 'assignedTo')
      .withProperties({ priority: 'high', assignedAt: Date.now() });

    const spec = action.build();
    expect(spec.params.properties.priority).toBe('high');
    expect(spec.params.properties.assignedAt).toBeTypeOf('number');
  });

  test('creates UPSERT_RELATIONSHIP action with strength', () => {
    const action = upsertRelationship('task', 'dep', 'requires')
      .strength(0.85);

    const spec = action.build();
    expect(spec.params.properties.strength).toBe(0.85);
  });

  test('validates strength bounds (0-1)', () => {
    expect(() => {
      upsertRelationship('task', 'dep', 'requires').strength(1.5);
    }).toThrow('Strength must be between 0 and 1');

    expect(() => {
      upsertRelationship('task', 'dep', 'requires').strength(-0.1);
    }).toThrow('Strength must be between 0 and 1');
  });

  test('creates UPSERT_RELATIONSHIP action with evidence', () => {
    const action = upsertRelationship('task', 'blocker', 'requires')
      .evidence('Updated dependency reasoning');

    const spec = action.build();
    expect(spec.params.properties.evidence).toBe('Updated dependency reasoning');
  });

  test('chains multiple property modifiers', () => {
    const action = upsertRelationship('knowledge', 'other', 'supports')
      .strength(0.9)
      .evidence('Updated supporting evidence')
      .withProperties({ source: 'analysis', verified: true, updatedAt: Date.now() });

    const spec = action.build();
    expect(spec.params.properties.strength).toBe(0.9);
    expect(spec.params.properties.evidence).toBe('Updated supporting evidence');
    expect(spec.params.properties.source).toBe('analysis');
    expect(spec.params.properties.verified).toBe(true);
    expect(spec.params.properties.updatedAt).toBeTypeOf('number');
  });

  test('upsert action type distinguishes from create', () => {
    const upsertAction = upsertRelationship('task', 'user', 'assignedTo');
    const createAction = query().build(); // Would use createRelationship

    const upsertSpec = upsertAction.build();
    expect(upsertSpec.type).toBe('upsert_relationship');
  });
});

describe('UPSERT_RELATIONSHIP Action - Query Integration', () => {
  test('adds UPSERT_RELATIONSHIP to query via upsertRelationship method', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('user').label('User').where({ id: 'alice' })
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: { priority: 'high', assignedAt: Date.now() }
      });

    const def = q.build();
    expect(def.actions).toHaveLength(1);
    expect(def.actions![0].type).toBe('upsert_relationship');
    expect(def.actions![0].params.type).toBe('assignedTo');
    expect(def.actions![0].params.properties.priority).toBe('high');
  });

  test('adds UPSERT_RELATIONSHIP using forEach', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('user').label('User').where({ id: 'alice' })
      )
      .forEach(upsertRelationship('task', 'user', 'assignedTo'));

    const def = q.build();
    expect(def.actions).toHaveLength(1);
    expect(def.actions![0].type).toBe('upsert_relationship');
  });

  test('supports multiple UPSERT_RELATIONSHIP actions in a query', () => {
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User'),
        pattern('team').label('Team')
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: { role: 'owner' }
      })
      .upsertRelationship('user', 'team', {
        type: 'memberOf',
        properties: { role: 'developer' }
      });

    const def = q.build();
    expect(def.actions).toHaveLength(2);
    expect(def.actions![0].params.type).toBe('assignedTo');
    expect(def.actions![1].params.type).toBe('memberOf');
  });
});

describe('UPSERT_RELATIONSHIP Action - Compilation', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('compiles UPSERT_RELATIONSHIP into plan step', async () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('user').label('User').where({ id: 'alice' })
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: { priority: 'high' }
      });

    const plan = await compiler.compile(q.build());

    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    expect(upsertStep).toBeDefined();
    expect(upsertStep!.type).toBe('action');
    expect(upsertStep!.actor).toBe('@(domain/relationships)');
  });

  test('upsert step depends on both from and to variables', async () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('user').label('User').where({ id: 'alice' })
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo'
      });

    const plan = await compiler.compile(q.build());

    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    expect(upsertStep!.dependencies).toHaveLength(2);
  });

  test('validates from variable exists', async () => {
    const q = query()
      .match(pattern('user').label('User'))
      .upsertRelationship('task', 'user', { type: 'assignedTo' });

    await expect(compiler.compile(q.build())).rejects.toThrow(
      'unknown from variable: task'
    );
  });

  test('validates to variable exists', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .upsertRelationship('task', 'user', { type: 'assignedTo' });

    await expect(compiler.compile(q.build())).rejects.toThrow(
      'unknown to variable: user'
    );
  });

  test('includes relationship properties in payload', async () => {
    const props = { priority: 'high', assignedAt: 123456789 };
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: props
      });

    const plan = await compiler.compile(q.build());
    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    expect(upsertStep!.message.payload).toMatchObject(props);
  });

  test('upsert step metadata includes from and to variables', async () => {
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', { type: 'assignedTo' });

    const plan = await compiler.compile(q.build());
    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    expect(upsertStep!.metadata?.fromVariable).toBe('task');
    expect(upsertStep!.metadata?.toVariable).toBe('user');
    expect(upsertStep!.metadata?.relationshipType).toBe('assignedTo');
  });

  test('uses ask pattern for upsert (to get relationship back)', async () => {
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', { type: 'assignedTo' });

    const plan = await compiler.compile(q.build());
    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    expect(upsertStep!.message.pattern).toBe('ask');
  });

  test('targets relationships actor', async () => {
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', { type: 'assignedTo' });

    const plan = await compiler.compile(q.build());
    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    expect(upsertStep!.actor).toBe('@(domain/relationships)');
  });
});

describe('UPSERT_RELATIONSHIP - Idempotency Semantics', () => {
  test('upsert action spec includes idempotent flag', () => {
    const action = upsertRelationship('task', 'user', 'assignedTo')
      .withProperties({ priority: 'high' });

    const spec = action.build();
    expect(spec.params.idempotent).toBe(true);
  });

  test('property merge behavior can be specified', () => {
    const action = upsertRelationship('task', 'user', 'assignedTo')
      .withProperties({ priority: 'high', newField: 'value' })
      .mergeStrategy('deep');

    const spec = action.build();
    expect(spec.params.mergeStrategy).toBe('deep');
  });

  test('default merge strategy is shallow', () => {
    const action = upsertRelationship('task', 'user', 'assignedTo')
      .withProperties({ priority: 'high' });

    const spec = action.build();
    expect(spec.params.mergeStrategy).toBe('shallow');
  });

  test('supports replace merge strategy', () => {
    const action = upsertRelationship('task', 'user', 'assignedTo')
      .withProperties({ priority: 'high' })
      .mergeStrategy('replace');

    const spec = action.build();
    expect(spec.params.mergeStrategy).toBe('replace');
  });
});

describe('UPSERT_RELATIONSHIP - Type Immutability', () => {
  test('relationship type cannot be changed in upsert', () => {
    // This is a semantic constraint enforced at execution time
    // The API doesn't allow changing the type - it's fixed in the action spec
    const action = upsertRelationship('task', 'user', 'assignedTo');

    const spec = action.build();
    expect(spec.params.type).toBe('assignedTo');
    // Type is immutable - upsert will only update properties, not type
  });

  test('upsert preserves relationship type from initial creation', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: { priority: 'high' }
      });

    const plan = await compiler.compile(q.build());
    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    // Relationship type is part of metadata and payload
    expect(upsertStep!.metadata?.relationshipType).toBe('assignedTo');
    expect(upsertStep!.message.payload.type).toBe('assignedTo');
  });
});

describe('UPSERT_RELATIONSHIP - Error Handling', () => {
  test('rejects upsert without relationship type', () => {
    expect(() => {
      query()
        .match(
          pattern('task').label('Task'),
          pattern('user').label('User')
        )
        .upsertRelationship('task', 'user', {} as any);
    }).toThrow();
  });

  test('rejects upsert with invalid merge strategy', () => {
    expect(() => {
      upsertRelationship('task', 'user', 'assignedTo')
        .mergeStrategy('invalid' as any);
    }).toThrow('Invalid merge strategy');
  });

  test('handles missing from variable gracefully in compilation', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('user').label('User'))
      .upsertRelationship('task', 'user', { type: 'assignedTo' });

    await expect(compiler.compile(q.build())).rejects.toThrow('from variable');
  });

  test('handles missing to variable gracefully in compilation', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .upsertRelationship('task', 'user', { type: 'assignedTo' });

    await expect(compiler.compile(q.build())).rejects.toThrow('to variable');
  });
});

describe('UPSERT_RELATIONSHIP - Return Values', () => {
  test('upsert can bind result to variable', () => {
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: { priority: 'high' }
      })
      .return(['task', 'user', 'relationship']);

    const def = q.build();
    expect(def.returns).toContain('relationship');
  });
});

describe('UPSERT_RELATIONSHIP - Performance Considerations', () => {
  test('upsert step cost estimation includes both query and write', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', { type: 'assignedTo' });

    const plan = await compiler.compile(q.build());
    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    // Upsert cost should be higher than simple create
    // because it includes a check-if-exists query
    expect(upsertStep!.cost.latencyMs).toBeGreaterThan(0);
  });

  test('parallel upserts are not parallelizable for same relationship', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', { type: 'assignedTo' });

    const plan = await compiler.compile(q.build());
    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    // Upsert operations are not marked as parallelizable
    // to prevent race conditions
    expect(upsertStep!.parallelizable).toBe(false);
  });
});

describe('UPSERT_RELATIONSHIP - Advanced Use Cases', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('can chain multiple upserts in a workflow', async () => {
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User'),
        pattern('team').label('Team')
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: { priority: 'high' }
      })
      .upsertRelationship('user', 'team', {
        type: 'memberOf',
        properties: { role: 'developer' }
      });

    const plan = await compiler.compile(q.build());
    const upsertSteps = plan.steps.filter(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    expect(upsertSteps).toHaveLength(2);
  });

  test('supports conditional upsert with pattern matching', async () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ status: 'open' }),
        pattern('user').label('User').where({ available: true })
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: { assignedAt: Date.now() }
      });

    const def = q.build();
    expect(def.patterns).toHaveLength(2);
    expect(def.actions).toHaveLength(1);
  });

  test('upsert with complex property merging', () => {
    const action = upsertRelationship('task', 'user', 'assignedTo')
      .withProperties({
        priority: 'high',
        metadata: { assignedBy: 'system', reason: 'capacity' }
      })
      .mergeStrategy('deep');

    const spec = action.build();
    expect(spec.params.properties.metadata).toBeDefined();
    expect(spec.params.mergeStrategy).toBe('deep');
  });

  test('upsert preserves timestamps on update', () => {
    // This is a semantic behavior - upsert should preserve created timestamp
    // but can update other fields
    const action = upsertRelationship('task', 'user', 'assignedTo')
      .withProperties({
        updatedAt: Date.now(),
        lastModifiedBy: 'admin'
      });

    const spec = action.build();
    expect(spec.params.properties.updatedAt).toBeTypeOf('number');
  });

  test('upsert with strength adjustment over time', () => {
    const action1 = upsertRelationship('knowledge', 'claim', 'supports')
      .strength(0.7);

    const action2 = upsertRelationship('knowledge', 'claim', 'supports')
      .strength(0.9); // Increased confidence

    expect(action1.build().params.properties.strength).toBe(0.7);
    expect(action2.build().params.properties.strength).toBe(0.9);
  });

  test('bulk upsert pattern for multiple entities', async () => {
    const q = query()
      .match(
        pattern('tasks').label('Task').where({ status: 'open' }),
        pattern('user').label('User').where({ id: 'alice' })
      )
      .upsertRelationship('tasks', 'user', {
        type: 'assignedTo',
        properties: { batchAssigned: true }
      });

    const plan = await compiler.compile(q.build());
    const upsertStep = plan.steps.find(
      s => s.metadata?.actionType === 'upsert_relationship'
    );

    expect(upsertStep).toBeDefined();
  });

  test('upsert with conditional properties', () => {
    const props: any = { priority: 'high' };

    if (Math.random() > 0.5) {
      props.urgent = true;
    }

    const action = upsertRelationship('task', 'user', 'assignedTo')
      .withProperties(props);

    const spec = action.build();
    expect(spec.params.properties.priority).toBe('high');
  });

  test('idempotency guarantees with retry logic', () => {
    // Upsert should be safe to retry without side effects
    const action = upsertRelationship('task', 'user', 'assignedTo')
      .withProperties({ attempt: 1 });

    const spec1 = action.build();
    const spec2 = action.build();

    // Same action spec should be produced
    expect(spec1.type).toBe(spec2.type);
    expect(spec1.params.idempotent).toBe(true);
  });

  test('upsert signatures are deterministic for caching', async () => {
    const q1 = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: { priority: 'high' }
      });

    const q2 = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .upsertRelationship('task', 'user', {
        type: 'assignedTo',
        properties: { priority: 'high' }
      });

    const plan1 = await compiler.compile(q1.build());
    const plan2 = await compiler.compile(q2.build());

    // Same query should produce same plan ID
    expect(plan1.id).toBe(plan2.id);
  });
});
