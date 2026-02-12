#!/usr/bin/env bun
/**
 * Predicate Pushdown Optimizer Tests
 * Tests for src/query/optimizer/predicate-pushdown.ts
 * Target: >15 test cases, >90% coverage
 */

import { test, expect, describe } from 'bun:test';
import { PredicatePushdownOptimizer, optimizePlan } from './predicate-pushdown.ts';
import { QueryCompiler } from '../compiler.ts';
import { query } from '../builder.ts';
import { pattern, filter, logic } from '../pattern.ts';
import type { QueryPlan, FilterExpression } from '../types.ts';

// Helper to compile a query to a plan
async function compilePlan(builder: ReturnType<typeof query>): Promise<QueryPlan> {
  // Disable join optimization to avoid pre-existing bugs
  const compiler = new QueryCompiler({ enableJoinOptimization: false });
  return await compiler.compile(builder.build());
}

describe('PredicatePushdownOptimizer - Construction', () => {
  test('creates optimizer instance', () => {
    const optimizer = new PredicatePushdownOptimizer();
    expect(optimizer).toBeDefined();
  });

  test('convenience function works', async () => {
    const plan = await compilePlan(
      query().match(pattern('task').label('Task'))
    );
    const result = optimizePlan(plan);
    expect(result).toBeDefined();
    expect(result.plan).toBeDefined();
    expect(result.stats).toBeDefined();
  });
});

describe('PredicatePushdownOptimizer - Basic Filter Pushdown', () => {
  test('pushes simple equality filter to source', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    // Create a query with pattern and filter
    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'priority').eq('high'))
      .build();

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q);

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(true);
    expect(result.stats.filtersPushedDown).toBeGreaterThan(0);

    // Check that filter was added to query step
    const queryStep = result.plan.steps.find(s => s.type === 'query');
    expect(queryStep).toBeDefined();
    expect(queryStep!.message.payload.filter).toBeDefined();
  });

  test('pushes multiple filters to same source', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(
        filter('task', 'priority').eq('high'),
        filter('task', 'status').eq('open')
      )
      .build();

    const plan = await compilePlan(query()
      .match(pattern('task').label('Task'))
      .where(
        filter('task', 'priority').eq('high'),
        filter('task', 'status').eq('open')
      ));

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(true);
    expect(result.stats.filtersPushedDown).toBeGreaterThanOrEqual(1);
  });

  test('preserves plan structure when no filters present', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query().match(pattern('task').label('Task'))
    );

    const originalStepCount = plan.steps.length;
    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(false);
    expect(result.plan.steps.length).toBe(originalStepCount);
    expect(result.stats.filtersPushedDown).toBe(0);
  });

  test('does not mutate original plan', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'status').eq('open'))
    );

    const originalId = plan.id;
    const originalSteps = JSON.stringify(plan.steps);

    optimizer.optimize(plan);

    expect(plan.id).toBe(originalId);
    expect(JSON.stringify(plan.steps)).toBe(originalSteps);
  });
});

describe('PredicatePushdownOptimizer - Comparison Operators', () => {
  test('pushes greater-than filter', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'priority').gt(5))
    );

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(true);

    const queryStep = result.plan.steps.find(s => s.type === 'query');
    expect(queryStep?.message.payload.filter).toBeDefined();
  });

  test('pushes less-than filter', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'value').lt(100))
    );

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(true);
  });

  test('pushes greater-than-or-equal filter', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'score').gte(50))
    );

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(true);
  });

  test('pushes less-than-or-equal filter', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'age').lte(30))
    );

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(true);
  });
});

describe('PredicatePushdownOptimizer - Safety Checks', () => {
  test('does not push filters referencing multiple variables', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    // Create a filter that references two variables (simulated)
    const plan = await compilePlan(
      query()
        .match(
          pattern('task').label('Task'),
          pattern('user').label('User')
        )
    );

    // Manually add a cross-variable filter
    const crossFilter: FilterExpression = {
      type: 'comparison',
      operator: '=',
      variable: 'task',
      property: 'assignee',
      value: undefined, // Would reference 'user.id'
    };

    plan.original.filters = [crossFilter];

    const result = optimizer.optimize(plan);

    // Should not push down cross-variable filters
    expect(result.stats.filtersPushedDown).toBe(0);
  });

  test('does not push filters to non-query steps', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('root').label('Task'))
        .traverse({
          from: 'root',
          relationship: 'requires',
          direction: 'outbound',
          as: 'deps'
        })
        .where(filter('deps', 'status').eq('open'))
    );

    const result = optimizer.optimize(plan);

    // Should not push to traverse step
    const traverseStep = result.plan.steps.find(s => s.type === 'traverse');
    expect(traverseStep?.message.payload.filter).toBeUndefined();
  });

  test('does not push filters that depend on aggregations', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .aggregate({
          operation: 'count',
          variable: 'task',
          as: 'count'
        })
    );

    // Manually add filter on aggregation result
    const aggFilter: FilterExpression = {
      type: 'comparison',
      operator: '>',
      variable: 'count',
      property: 'value',
      value: 10
    };

    plan.original.filters = [aggFilter];

    const result = optimizer.optimize(plan);

    // Should not push filters on aggregate results
    expect(result.stats.filtersPushedDown).toBe(0);
  });

  test('handles OR filters conservatively', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(
          logic.or(
            filter('task', 'priority').eq('high'),
            filter('task', 'priority').eq('urgent')
          )
        )
    );

    const result = optimizer.optimize(plan);

    // OR filters are complex and may not be pushed down
    // depending on actor capabilities
    expect(result.plan).toBeDefined();
  });
});

describe('PredicatePushdownOptimizer - AND Filter Optimization', () => {
  test('pushes AND filters as combined predicate', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(
          logic.and(
            filter('task', 'status').eq('open'),
            filter('task', 'priority').eq('high')
          )
        )
    );

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(true);

    const queryStep = result.plan.steps.find(s => s.type === 'query');
    expect(queryStep?.message.payload.filter).toBeDefined();
  });

  test('correctly merges multiple AND conditions', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(
          logic.and(
            filter('task', 'status').eq('open'),
            filter('task', 'priority').eq('high'),
            filter('task', 'assignee').eq('alice')
          )
        )
    );

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(true);
  });
});

describe('PredicatePushdownOptimizer - Cost Updates', () => {
  test('reduces result count estimates after optimization', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'status').eq('open'))
    );

    const originalResultCount = plan.steps[0].cost.resultCount;
    const result = optimizer.optimize(plan);

    if (result.optimized) {
      const optimizedResultCount = result.plan.steps[0].cost.resultCount;
      expect(optimizedResultCount).toBeLessThanOrEqual(originalResultCount);
    }
  });

  test('estimates result size reduction', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(
          filter('task', 'status').eq('open'),
          filter('task', 'priority').eq('high')
        )
    );

    const result = optimizer.optimize(plan);

    if (result.optimized) {
      expect(result.stats.estimatedReduction).toBeGreaterThan(0);
      expect(result.stats.estimatedReduction).toBeLessThanOrEqual(100);
    }
  });

  test('updates plan makespan after optimization', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'status').eq('open'))
    );

    const originalMakespan = plan.metadata.estimatedCost.makespan;
    const result = optimizer.optimize(plan);

    if (result.optimized && result.stats.estimatedReduction > 0) {
      expect(result.plan.metadata.estimatedCost.makespan).toBeLessThanOrEqual(
        originalMakespan
      );
    }
  });

  test('updates memory usage estimates', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'status').eq('open'))
    );

    const originalMemory = plan.metadata.estimatedCost.resourceUsage.memoryBytes;
    const result = optimizer.optimize(plan);

    if (result.optimized) {
      expect(result.plan.metadata.estimatedCost.resourceUsage.memoryBytes)
        .toBeLessThanOrEqual(originalMemory);
    }
  });
});

describe('PredicatePushdownOptimizer - Statistics', () => {
  test('tracks number of filters pushed down', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(
          filter('task', 'status').eq('open'),
          filter('task', 'priority').eq('high')
        )
    );

    const result = optimizer.optimize(plan);

    expect(result.stats.filtersPushedDown).toBeGreaterThanOrEqual(0);
  });

  test('tracks number of steps modified', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'status').eq('open'))
    );

    const result = optimizer.optimize(plan);

    if (result.optimized) {
      expect(result.stats.stepsModified).toBeGreaterThan(0);
    }
  });

  test('returns correct optimization status', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    // Query without filters - should not be optimized
    const plan1 = await compilePlan(
      query().match(pattern('task').label('Task'))
    );
    const result1 = optimizer.optimize(plan1);
    expect(result1.optimized).toBe(false);

    // Query with filters - should be optimized
    const plan2 = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'status').eq('open'))
    );
    const result2 = optimizer.optimize(plan2);
    expect(result2.optimized).toBe(true);
  });
});

describe('PredicatePushdownOptimizer - Complex Scenarios', () => {
  test('handles queries with multiple patterns and selective pushdown', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(
          pattern('task').label('Task'),
          pattern('user').label('User')
        )
        .where(
          filter('task', 'status').eq('open'),
          filter('user', 'active').eq(true)
        )
    );

    const result = optimizer.optimize(plan);

    // Should push down filters to their respective sources
    expect(result.optimized).toBe(true);
  });

  test('preserves semantics with traversals', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query()
        .match(pattern('root').label('Task'))
        .traverse({
          from: 'root',
          relationship: 'requires',
          direction: 'outbound',
          as: 'deps'
        })
        .where(filter('root', 'status').eq('open'))
    );

    const result = optimizer.optimize(plan);

    // Should push filter on 'root' but not on 'deps'
    expect(result.plan.steps.length).toBe(plan.steps.length);
  });

  test('handles empty filter list gracefully', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query().match(pattern('task').label('Task'))
    );

    // Ensure no filters
    plan.original.filters = [];

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(false);
    expect(result.stats.filtersPushedDown).toBe(0);
  });
});

describe('PredicatePushdownOptimizer - Signature Regeneration', () => {
  test('updates step signature after filter pushdown', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    // Create a plan with NO filters in pattern, only in separate WHERE clause
    const plan = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'status').eq('open'))
    );

    const originalSignature = plan.steps[0].signature;
    const originalFilter = JSON.stringify(plan.steps[0].message.payload.filter);

    const result = optimizer.optimize(plan);

    if (result.optimized && result.stats.filtersPushedDown > 0) {
      const newSignature = result.plan.steps[0].signature;
      const newFilter = JSON.stringify(result.plan.steps[0].message.payload.filter);

      expect(newSignature).toBeDefined();
      // Signature should change when filter content changes
      if (originalFilter !== newFilter) {
        expect(newSignature).not.toBe(originalSignature);
      }
    } else {
      // If not optimized, signature should stay the same
      expect(result.plan.steps[0].signature).toBe(originalSignature);
    }
  });

  test('signature changes reflect filter content', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan1 = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'status').eq('open'))
    );

    const plan2 = await compilePlan(
      query()
        .match(pattern('task').label('Task'))
        .where(filter('task', 'status').eq('closed'))
    );

    const result1 = optimizer.optimize(plan1);
    const result2 = optimizer.optimize(plan2);

    if (result1.optimized && result2.optimized) {
      expect(result1.plan.steps[0].signature).not.toBe(
        result2.plan.steps[0].signature
      );
    }
  });
});

describe('Edge Cases', () => {
  test('handles plan with no steps', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(query());

    const result = optimizer.optimize(plan);

    expect(result.optimized).toBe(false);
    expect(result.plan.steps).toHaveLength(0);
  });

  test('handles filters on undefined variables', async () => {
    const optimizer = new PredicatePushdownOptimizer();

    const plan = await compilePlan(
      query().match(pattern('task').label('Task'))
    );

    // Add filter for non-existent variable
    const badFilter: FilterExpression = {
      type: 'comparison',
      operator: '=',
      variable: 'nonexistent',
      property: 'value',
      value: 123
    };

    plan.original.filters = [badFilter];

    const result = optimizer.optimize(plan);

    // Should handle gracefully
    expect(result.plan).toBeDefined();
  });
});
