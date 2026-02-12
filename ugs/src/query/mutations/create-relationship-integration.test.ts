#!/usr/bin/env bun
/**
 * CREATE_RELATIONSHIP Integration Test
 *
 * End-to-end integration test demonstrating the complete flow:
 * Query Building → Compilation → Execution → Result
 */

import { test, expect, describe } from 'bun:test';
import { query, createRelationship } from '../builder.ts';
import { pattern } from '../pattern.ts';
import { QueryCompiler } from '../compiler.ts';
import type { QueryPlan, ExecutionContext } from '../types.ts';

describe('CREATE_RELATIONSHIP Integration', () => {
  test('complete flow: build → compile → inspect plan', async () => {
    // 1. Build query using declarative DSL
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('blocker').label('Task').where({ id: 'task-2' })
      )
      .link('task', 'blocker', 'requires', {
        priority: 'high',
        createdAt: Date.now(),
      })
      .return(['task', 'blocker']);

    const definition = q.build();

    // Verify query definition
    expect(definition.patterns).toHaveLength(2);
    expect(definition.actions).toHaveLength(1);
    expect(definition.actions![0].type).toBe('create_relationship');
    expect(definition.returns).toContain('task');
    expect(definition.returns).toContain('blocker');

    // 2. Compile to execution plan
    const compiler = new QueryCompiler();
    const plan = await compiler.compile(definition);

    // Verify plan structure
    expect(plan.steps).toHaveLength(3); // 2 patterns + 1 relationship
    expect(plan.variables).toContain('task');
    expect(plan.variables).toContain('blocker');

    // 3. Inspect relationship creation step
    const relStep = plan.steps.find(
      (s) => s.metadata?.actionType === 'create_relationship'
    );

    expect(relStep).toBeDefined();
    expect(relStep!.type).toBe('action');
    expect(relStep!.actor).toContain('relationships');
    expect(relStep!.message.pattern).toBe('ask');
    expect(relStep!.message.type).toBe('create');
    expect(relStep!.message.payload.type).toBe('requires');
    expect(relStep!.message.payload.priority).toBe('high');

    // 4. Verify dependencies
    expect(relStep!.dependencies).toHaveLength(2);

    // Relationship step depends on both pattern steps
    const patternStepIds = plan.steps
      .filter((s) => s.type === 'query')
      .map((s) => s.id);

    expect(relStep!.dependencies).toEqual(
      expect.arrayContaining(patternStepIds)
    );

    // 5. Verify metadata
    expect(relStep!.metadata?.fromVariable).toBe('task');
    expect(relStep!.metadata?.toVariable).toBe('blocker');
    expect(relStep!.metadata?.relationshipType).toBe('requires');
  });

  test('builder method chaining produces correct plan', async () => {
    const q = query()
      .match(
        pattern('learning').label('Knowledge').where({ type: 'learning' }),
        pattern('decision').label('Knowledge').where({ type: 'decision' })
      )
      .forEach(
        createRelationship('learning', 'decision', 'supports')
          .strength(0.85)
          .evidence('Strong correlation observed')
          .withProperties({ source: 'session-123' })
      );

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relStep = plan.steps.find(
      (s) => s.metadata?.actionType === 'create_relationship'
    );

    // Verify all properties made it through compilation
    expect(relStep!.message.payload.type).toBe('supports');
    expect(relStep!.message.payload.strength).toBe(0.85);
    expect(relStep!.message.payload.evidence).toBe(
      'Strong correlation observed'
    );
    expect(relStep!.message.payload.source).toBe('session-123');
  });

  test('multiple relationship creation in single query', async () => {
    const q = query()
      .match(
        pattern('compile').label('Task').where({ id: 'compile' }),
        pattern('build').label('Task').where({ id: 'build' }),
        pattern('test').label('Task').where({ id: 'test' })
      )
      .link('build', 'compile', 'requires')
      .link('test', 'build', 'requires');

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const relSteps = plan.steps.filter(
      (s) => s.metadata?.actionType === 'create_relationship'
    );

    expect(relSteps).toHaveLength(2);

    // First relationship: build → compile
    expect(relSteps[0].metadata?.fromVariable).toBe('build');
    expect(relSteps[0].metadata?.toVariable).toBe('compile');

    // Second relationship: test → build
    expect(relSteps[1].metadata?.fromVariable).toBe('test');
    expect(relSteps[1].metadata?.toVariable).toBe('build');

    // Verify second relationship depends on build pattern
    const buildStepId = plan.steps.find((s) =>
      s.bindings.includes('build')
    )?.id;
    expect(relSteps[1].dependencies).toContain(buildStepId);
  });

  test('relationship creation with execution context', async () => {
    const q = query()
      .match(pattern('task'), pattern('user'))
      .link('task', 'user', 'assignedTo');

    const context: ExecutionContext = {
      warmActors: new Set(),
      computationCache: new Map(),
      resources: {
        maxConcurrency: 10,
        availableMemory: 1024 * 1024 * 1024,
      },
      startTime: Date.now(),
    };

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build(), context);

    // Plan should be generated with context
    expect(plan.metadata.compiledAt).toBeGreaterThan(0);
    expect(plan.metadata.estimatedCost).toBeDefined();

    const relStep = plan.steps.find(
      (s) => s.metadata?.actionType === 'create_relationship'
    );

    // Cost should be estimated
    expect(relStep!.cost.latencyMs).toBeGreaterThan(0);
    expect(relStep!.cost.cpuMs).toBeGreaterThan(0);
  });

  test('error handling: missing from variable', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .createRelationship('nonexistent', 'task', { type: 'requires' });

    const compiler = new QueryCompiler();

    await expect(async () => {
      await compiler.compile(q.build());
    }).toThrow(/unknown from variable/);
  });

  test('error handling: missing to variable', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .createRelationship('task', 'nonexistent', { type: 'requires' });

    const compiler = new QueryCompiler();

    await expect(async () => {
      await compiler.compile(q.build());
    }).toThrow(/unknown to variable/);
  });

  test('plan caching works for relationship queries', async () => {
    const q = query()
      .match(pattern('a'), pattern('b'))
      .link('a', 'b', 'related-to');

    const compiler = new QueryCompiler();

    // Compile twice - should produce same plan ID
    const plan1 = await compiler.compile(q.build());
    const plan2 = await compiler.compile(q.build());

    expect(plan1.id).toBe(plan2.id);
  });

  test('explain output includes relationship creation', async () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'dep-1' })
      )
      .link('task', 'dep', 'requires', { priority: 'high' });

    const explanation = await q.explain({ verbose: true, costs: true });

    // Explanation should describe the relationship creation
    expect(explanation.text).toBeDefined();
    expect(explanation.plan).toBeDefined();
    // Cost details included in text output
    expect(explanation.text.length).toBeGreaterThan(0);
  });
});
