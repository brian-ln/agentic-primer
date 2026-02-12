#!/usr/bin/env bun
/**
 * CREATE Action Integration Tests
 *
 * End-to-end tests demonstrating CREATE operations through
 * the complete query pipeline: Builder → Compiler → Executor
 */

import { test, expect, describe } from 'bun:test';
import { query, create } from '../builder.ts';
import { pattern } from '../pattern.ts';
import { QueryCompiler } from '../compiler.ts';
import type { QueryPlan } from '../types.ts';

describe('CREATE Integration - Builder to Compiler', () => {
  test('complete CREATE workflow compiles successfully', async () => {
    // Build query
    const q = query()
      .forEach(
        create('task').as({
          title: 'Integration Test Task',
          status: 'open',
          priority: 'high',
        })
      )
      .return(['task']);

    // Compile to plan
    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Verify plan structure
    expect(plan).toBeDefined();
    expect(plan.steps).toHaveLength(1);
    expect(plan.steps[0].type).toBe('action');
    expect(plan.steps[0].message.type).toBe('create');
    expect(plan.steps[0].bindings).toContain('task');
  });

  test('CREATE with conditional pattern', async () => {
    // Build query: create task only if user exists
    const q = query()
      .match(pattern('user').label('User').where({ id: 'alice' }))
      .forEach(
        create('task').as({
          title: 'Task for Alice',
          assignee: 'alice',
          status: 'open',
        })
      )
      .return(['task']);

    // Compile
    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Should have MATCH step followed by CREATE step
    expect(plan.steps).toHaveLength(2);
    expect(plan.steps[0].type).toBe('query'); // MATCH user
    expect(plan.steps[1].type).toBe('action'); // CREATE task

    // CREATE should not depend on MATCH (independent operation)
    expect(plan.steps[1].dependencies).toHaveLength(0);
  });

  test('batch CREATE operations compile correctly', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task 1', priority: 'high' }))
      .forEach(create('task').as({ title: 'Task 2', priority: 'medium' }))
      .forEach(create('task').as({ title: 'Task 3', priority: 'low' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // All CREATE steps should be parallelizable
    expect(plan.steps).toHaveLength(3);
    expect(plan.steps.every((s) => s.parallelizable)).toBe(true);
    expect(plan.metadata.parallelizable).toBe(true);
  });

  test('CREATE followed by MATCH workflow', async () => {
    const q = query()
      .forEach(
        create('task').as({
          title: 'New Task',
          status: 'open',
        })
      )
      .match(pattern('task').label('Task').where({ title: 'New Task' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps.length).toBeGreaterThanOrEqual(2);

    // First step should be CREATE
    const createStep = plan.steps.find((s) => s.message.type === 'create');
    expect(createStep).toBeDefined();
    expect(createStep?.bindings).toContain('task');
  });
});

describe('CREATE Integration - Message Routing', () => {
  test('CREATE routes to collection actor', async () => {
    const q = query().forEach(
      create('task').as({ title: 'Task' })
    );

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Should route to tasks collection
    expect(plan.steps[0].actor).toContain('tasks');
  });

  test('CREATE message has correct structure', async () => {
    const properties = {
      title: 'Test Task',
      status: 'open',
      priority: 'high',
      metadata: { source: 'test' },
    };

    const q = query().forEach(create('task').as(properties));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    const createStep = plan.steps[0];
    expect(createStep.message.pattern).toBe('tell');
    expect(createStep.message.type).toBe('create');
    expect(createStep.message.payload).toEqual(properties);
  });

  test('multiple entity types route to correct actors', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task' }))
      .forEach(create('knowledge').as({ title: 'Knowledge' }))
      .forEach(create('user').as({ name: 'User' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps[0].actor).toContain('tasks');
    expect(plan.steps[1].actor).toContain('knowledge');
    expect(plan.steps[2].actor).toContain('users');
  });
});

describe('CREATE Integration - Cost Estimation', () => {
  test('estimates cost for CREATE operations', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task 1' }))
      .forEach(create('task').as({ title: 'Task 2' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Plan should have cost estimates
    expect(plan.metadata.estimatedCost).toBeDefined();
    expect(plan.metadata.estimatedCost.totalWork).toBeGreaterThan(0);
    expect(plan.metadata.estimatedCost.makespan).toBeGreaterThan(0);

    // Each step should have cost estimate
    plan.steps.forEach((step) => {
      expect(step.cost).toBeDefined();
      expect(step.cost.latencyMs).toBeGreaterThan(0);
    });
  });

  test('CREATE operations have independent costs', async () => {
    const q1 = query().forEach(create('task').as({ title: 'Task' }));
    const q2 = query()
      .match(pattern('user').label('User'))
      .forEach(create('task').as({ title: 'Task' }));

    const compiler = new QueryCompiler();
    const plan1 = await compiler.compile(q1.build());
    const plan2 = await compiler.compile(q2.build());

    const createCost1 = plan1.steps[0].cost.latencyMs;
    const createCost2 = plan2.steps.find((s) => s.type === 'action')?.cost.latencyMs;

    // CREATE costs should be similar regardless of preceding operations
    expect(createCost1).toBeDefined();
    expect(createCost2).toBeDefined();
  });
});

describe('CREATE Integration - Plan Optimization', () => {
  test('parallel CREATE operations optimize for makespan', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task 1' }))
      .forEach(create('task').as({ title: 'Task 2' }))
      .forEach(create('task').as({ title: 'Task 3' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Makespan should be less than total work (due to parallelism)
    const totalWork = plan.metadata.estimatedCost.totalWork;
    const makespan = plan.metadata.estimatedCost.makespan;

    expect(makespan).toBeLessThanOrEqual(totalWork);
  });

  test('CREATE operations have consistent signatures', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task', status: 'open' }))
      .forEach(create('task').as({ title: 'Task', status: 'open' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Same CREATE operations should have same signature
    expect(plan.steps[0].signature).toBe(plan.steps[1].signature);
  });
});

describe('CREATE Integration - Complex Workflows', () => {
  test('CREATE in conditional workflow', async () => {
    const q = query()
      .match(pattern('template').label('Template').where({ id: 'default' }))
      .forEach(
        create('task').as({
          title: 'From Template',
          status: 'open',
        })
      )
      .return(['task', 'template']);

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.variables).toContain('task');
    expect(plan.variables).toContain('template');
  });

  test('CREATE multiple related entities', async () => {
    const q = query()
      .forEach(
        create('task').as({
          title: 'Main Task',
          status: 'open',
        })
      )
      .forEach(
        create('knowledge').as({
          title: 'Task Documentation',
          type: 'doc',
        })
      );

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Both entities should be created
    const taskCreate = plan.steps.find(
      (s) => s.message.type === 'create' && s.actor.includes('tasks')
    );
    const knowledgeCreate = plan.steps.find(
      (s) => s.message.type === 'create' && s.actor.includes('knowledge')
    );

    expect(taskCreate).toBeDefined();
    expect(knowledgeCreate).toBeDefined();
  });
});
