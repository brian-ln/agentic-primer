#!/usr/bin/env bun
/**
 * Migration Tools Tests
 *
 * Comprehensive tests for migration analyzer, planner, refactor, and reporter.
 *
 * Phase 6: Migration Tooling & Automation
 */

import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { mkdtemp, writeFile, rm, readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { MigrationAnalyzer } from '../analyzer';
import { MigrationPlanner } from '../planner';
import { MigrationRefactor, transformAddressCalls } from '../refactor';
import { MigrationReporter } from '../reporter';

describe('Migration Analyzer', () => {
  let testDir: string;

  beforeEach(async () => {
    testDir = await mkdtemp(join(tmpdir(), 'migration-test-'));
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  test('detects flat ID usage in address() calls', async () => {
    const content = `
import { address } from './message';

const msg = address('flat-id-actor');
const msg2 = address('another-flat');
const pathMsg = address('domain/inference'); // Should NOT be flagged
`;

    await writeFile(join(testDir, 'test.ts'), content);

    const analyzer = new MigrationAnalyzer({ rootDir: testDir });
    const analysis = await analyzer.analyze();

    expect(analysis.flatIdUsages.length).toBe(2);
    expect(analysis.flatIdUsages[0].flatId).toBe('flat-id-actor');
    expect(analysis.flatIdUsages[1].flatId).toBe('another-flat');
  });

  test('detects actor registrations', async () => {
    const content = `
router.registerActor('flat-actor', actor);
router.registerActor('domain/path-actor', actor);
supervisor.addChild('child-flat', actor);
supervisor.addChild('child/path', actor);
`;

    await writeFile(join(testDir, 'test.ts'), content);

    const analyzer = new MigrationAnalyzer({ rootDir: testDir });
    const analysis = await analyzer.analyze();

    expect(analysis.actorRegistrations.length).toBe(4);
    expect(analysis.actorRegistrations.filter(r => r.isHierarchical).length).toBe(2);
    expect(analysis.actorRegistrations.filter(r => !r.isHierarchical).length).toBe(2);
  });

  test('detects alias registrations', async () => {
    const content = `
registerAlias('old-name', 'domain/new-name');
registerAlias('inference', 'domain/inference');
`;

    await writeFile(join(testDir, 'test.ts'), content);

    const analyzer = new MigrationAnalyzer({ rootDir: testDir });
    const analysis = await analyzer.analyze();

    const aliasUsages = analysis.flatIdUsages.filter(u => u.type === 'alias-registration');
    expect(aliasUsages.length).toBe(2);
    expect(aliasUsages[0].complexity).toBe('simple');
  });

  test('categorizes by complexity', async () => {
    const content = `
// Simple: address calls
const msg1 = address('simple-flat');

// Complex: actor registration
router.registerActor('complex-flat', actor);

// Moderate: string literal in routing context
router.ask('moderate-flat', 'test', {});
`;

    await writeFile(join(testDir, 'test.ts'), content);

    const analyzer = new MigrationAnalyzer({ rootDir: testDir });
    const analysis = await analyzer.analyze();

    expect(analysis.byComplexity.simple.length).toBeGreaterThan(0);
    expect(analysis.byComplexity.complex.length).toBeGreaterThan(0);
  });

  test('calculates statistics correctly', async () => {
    const content = `
router.registerActor('flat1', actor);
router.registerActor('flat2', actor);
router.registerActor('domain/path1', actor);
router.registerActor('services/path2', actor);
`;

    await writeFile(join(testDir, 'test.ts'), content);

    const analyzer = new MigrationAnalyzer({ rootDir: testDir });
    const analysis = await analyzer.analyze();

    expect(analysis.stats.totalActors).toBe(4);
    expect(analysis.stats.flatActors).toBe(2);
    expect(analysis.stats.hierarchicalActors).toBe(2);
    expect(analysis.stats.migrationProgress).toBe(50);
  });

  test('excludes test files by default', async () => {
    const testContent = `const msg = address('test-flat');`;
    const srcContent = `const msg = address('src-flat');`;

    await writeFile(join(testDir, 'test.test.ts'), testContent);
    await writeFile(join(testDir, 'src.ts'), srcContent);

    const analyzer = new MigrationAnalyzer({ rootDir: testDir });
    const analysis = await analyzer.analyze();

    // Should not include test.test.ts
    expect(analysis.filesScanned).toBe(1);
    expect(analysis.flatIdUsages[0].flatId).toBe('src-flat');
  });

  test('groups usages by file', async () => {
    const content1 = `
const msg1 = address('flat1');
const msg2 = address('flat2');
`;

    const content2 = `
const msg3 = address('flat3');
`;

    await writeFile(join(testDir, 'file1.ts'), content1);
    await writeFile(join(testDir, 'file2.ts'), content2);

    const analyzer = new MigrationAnalyzer({ rootDir: testDir });
    const analysis = await analyzer.analyze();

    expect(analysis.byFile.size).toBe(2);
    expect(analysis.byFile.get(join(testDir, 'file1.ts'))?.length).toBe(2);
    expect(analysis.byFile.get(join(testDir, 'file2.ts'))?.length).toBe(1);
  });
});

describe('Migration Planner', () => {
  test('generates path proposals from analysis', () => {
    const mockAnalysis = {
      timestamp: new Date(),
      filesScanned: 1,
      flatIdUsages: [
        {
          file: 'test.ts',
          line: 1,
          column: 0,
          flatId: 'inference',
          type: 'address-call' as const,
          context: '',
          complexity: 'simple' as const,
          recommendation: '',
        },
        {
          file: 'test.ts',
          line: 2,
          column: 0,
          flatId: 'executor',
          type: 'address-call' as const,
          context: '',
          complexity: 'simple' as const,
          recommendation: '',
        },
      ],
      actorRegistrations: [],
      stats: {
        totalFlatIds: 2,
        totalActors: 0,
        hierarchicalActors: 0,
        flatActors: 0,
        migrationProgress: 0,
      },
      byComplexity: {
        simple: [],
        moderate: [],
        complex: [],
      },
      byFile: new Map(),
    };

    const planner = new MigrationPlanner();
    const plan = planner.generatePlan(mockAnalysis);

    expect(plan.proposals.length).toBe(2);
    expect(plan.proposals.some(p => p.flatId === 'inference')).toBe(true);
    expect(plan.proposals.some(p => p.flatId === 'executor')).toBe(true);
  });

  test('categorizes actors by heuristics', () => {
    const mockAnalysis = {
      timestamp: new Date(),
      filesScanned: 1,
      flatIdUsages: [
        {
          file: 'test.ts',
          line: 1,
          column: 0,
          flatId: 'llm-service',
          type: 'address-call' as const,
          context: '',
          complexity: 'simple' as const,
          recommendation: '',
        },
      ],
      actorRegistrations: [],
      stats: {
        totalFlatIds: 1,
        totalActors: 0,
        hierarchicalActors: 0,
        flatActors: 0,
        migrationProgress: 0,
      },
      byComplexity: {
        simple: [],
        moderate: [],
        complex: [],
      },
      byFile: new Map(),
    };

    const planner = new MigrationPlanner();
    const plan = planner.generatePlan(mockAnalysis);

    const llmProposal = plan.proposals.find(p => p.flatId === 'llm-service');
    expect(llmProposal?.proposedPath).toContain('services/llm-service');
    expect(llmProposal?.confidence).toBe('high');
  });

  test('generates migration steps', () => {
    const mockAnalysis = {
      timestamp: new Date(),
      filesScanned: 1,
      flatIdUsages: [
        {
          file: 'test.ts',
          line: 1,
          column: 0,
          flatId: 'actor1',
          type: 'address-call' as const,
          context: '',
          complexity: 'simple' as const,
          recommendation: '',
        },
      ],
      actorRegistrations: [
        {
          file: 'test.ts',
          line: 2,
          actorId: 'actor1',
          isHierarchical: false,
          type: 'router.registerActor' as const,
          context: '',
        },
      ],
      stats: {
        totalFlatIds: 1,
        totalActors: 1,
        hierarchicalActors: 0,
        flatActors: 1,
        migrationProgress: 0,
      },
      byComplexity: {
        simple: [],
        moderate: [],
        complex: [],
      },
      byFile: new Map(),
    };

    const planner = new MigrationPlanner();
    const plan = planner.generatePlan(mockAnalysis);

    // Should have steps: create-alias, update-registration, update-usages, remove-alias
    expect(plan.steps.length).toBeGreaterThan(0);
    expect(plan.steps.some(s => s.type === 'create-alias')).toBe(true);
    expect(plan.steps.some(s => s.type === 'update-registration')).toBe(true);
  });

  test('groups steps into phases', () => {
    const mockAnalysis = {
      timestamp: new Date(),
      filesScanned: 1,
      flatIdUsages: [
        {
          file: 'test.ts',
          line: 1,
          column: 0,
          flatId: 'actor1',
          type: 'address-call' as const,
          context: '',
          complexity: 'simple' as const,
          recommendation: '',
        },
      ],
      actorRegistrations: [],
      stats: {
        totalFlatIds: 1,
        totalActors: 0,
        hierarchicalActors: 0,
        flatActors: 0,
        migrationProgress: 0,
      },
      byComplexity: {
        simple: [],
        moderate: [],
        complex: [],
      },
      byFile: new Map(),
    };

    const planner = new MigrationPlanner();
    const plan = planner.generatePlan(mockAnalysis);

    expect(plan.phases.length).toBeGreaterThan(0);
    expect(plan.phases[0].name).toContain('Alias');
  });

  test('calculates total effort', () => {
    const mockAnalysis = {
      timestamp: new Date(),
      filesScanned: 1,
      flatIdUsages: [
        {
          file: 'test.ts',
          line: 1,
          column: 0,
          flatId: 'actor1',
          type: 'address-call' as const,
          context: '',
          complexity: 'simple' as const,
          recommendation: '',
        },
      ],
      actorRegistrations: [],
      stats: {
        totalFlatIds: 1,
        totalActors: 0,
        hierarchicalActors: 0,
        flatActors: 0,
        migrationProgress: 0,
      },
      byComplexity: {
        simple: [],
        moderate: [],
        complex: [],
      },
      byFile: new Map(),
    };

    const planner = new MigrationPlanner();
    const plan = planner.generatePlan(mockAnalysis);

    expect(plan.totalEffort).toBeGreaterThan(0);
  });
});

// Migration completed - paths-only mode active (no flat ID support)
describe.skip('Migration Refactor', () => {
  let testDir: string;

  beforeEach(async () => {
    testDir = await mkdtemp(join(tmpdir(), 'refactor-test-'));
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  test('applies code changes to file', async () => {
    const originalContent = `
const msg = address('old-flat-id');
`;

    const testFile = join(testDir, 'test.ts');
    await writeFile(testFile, originalContent);

    const refactor = new MigrationRefactor({ dryRun: false });
    const session = refactor.startSession();

    const step = {
      step: 1,
      actorId: 'old-flat-id',
      targetPath: 'domain/new-path',
      type: 'update-usages' as const,
      description: 'Update usage',
      files: [testFile],
      changes: [
        {
          file: testFile,
          line: 2,
          before: "address('old-flat-id')",
          after: "address('domain/new-path')",
          type: 'replace' as const,
        },
      ],
      effort: 5,
      prerequisites: [],
    };

    const results = await refactor.applyStep(step);

    expect(results[0].success).toBe(true);
    expect(results[0].changesApplied).toBe(1);

    const newContent = await readFile(testFile, 'utf-8');
    expect(newContent).toContain("address('domain/new-path')");
    expect(newContent).not.toContain("address('old-flat-id')");

    refactor.endSession();
  });

  test('dry run does not modify files', async () => {
    const originalContent = `const msg = address('flat-id');`;
    const testFile = join(testDir, 'test.ts');
    await writeFile(testFile, originalContent);

    const refactor = new MigrationRefactor({ dryRun: true });
    const session = refactor.startSession();

    const step = {
      step: 1,
      actorId: 'flat-id',
      targetPath: 'domain/actor',
      type: 'update-usages' as const,
      description: 'Update',
      files: [testFile],
      changes: [
        {
          file: testFile,
          line: 1,
          before: "address('flat-id')",
          after: "address('domain/actor')",
          type: 'replace' as const,
        },
      ],
      effort: 5,
      prerequisites: [],
    };

    await refactor.applyStep(step);

    const content = await readFile(testFile, 'utf-8');
    expect(content).toBe(originalContent); // Unchanged

    refactor.endSession();
  });

  test('supports rollback', async () => {
    const originalContent = `const msg = address('flat');`;
    const testFile = join(testDir, 'test.ts');
    await writeFile(testFile, originalContent);

    const refactor = new MigrationRefactor({ dryRun: false, backup: true });
    const session = refactor.startSession();

    const step = {
      step: 1,
      actorId: 'flat',
      targetPath: 'domain/actor',
      type: 'update-usages' as const,
      description: 'Update',
      files: [testFile],
      changes: [
        {
          file: testFile,
          line: 1,
          before: "address('flat')",
          after: "address('domain/actor')",
          type: 'replace' as const,
        },
      ],
      effort: 5,
      prerequisites: [],
    };

    await refactor.applyStep(step);

    // Verify change applied
    let content = await readFile(testFile, 'utf-8');
    expect(content).toContain("address('domain/actor')");

    // Rollback
    await refactor.rollback();

    // Verify restored
    content = await readFile(testFile, 'utf-8');
    expect(content).toBe(originalContent);

    refactor.endSession();
  });

  test('transformAddressCalls replaces flat IDs', () => {
    const proposals = [
      {
        flatId: 'actor1',
        proposedPath: 'domain/actor1',
        reasoning: '',
        confidence: 'high' as const,
        dependencies: [],
        effort: 10,
      },
      {
        flatId: 'actor2',
        proposedPath: 'services/actor2',
        reasoning: '',
        confidence: 'high' as const,
        dependencies: [],
        effort: 10,
      },
    ];

    const content = `
const msg1 = address('actor1');
const msg2 = address('actor2');
const msg3 = address('domain/already-path');
`;

    const transform = transformAddressCalls(proposals);
    const result = transform(content);

    expect(result).toContain("address('domain/actor1')");
    expect(result).toContain("address('services/actor2')");
    expect(result).toContain("address('domain/already-path')");
    expect(result).not.toContain("address('actor1')");
    expect(result).not.toContain("address('actor2')");
  });
});

describe('Migration Reporter', () => {
  test('generates progress report', () => {
    const mockAnalysis = {
      timestamp: new Date(),
      filesScanned: 10,
      flatIdUsages: [],
      actorRegistrations: [
        { file: '', line: 1, actorId: 'flat1', isHierarchical: false, type: 'router.registerActor' as const, context: '' },
        { file: '', line: 2, actorId: 'flat2', isHierarchical: false, type: 'router.registerActor' as const, context: '' },
        { file: '', line: 3, actorId: 'path1', isHierarchical: true, type: 'router.registerActor' as const, context: '' },
        { file: '', line: 4, actorId: 'path2', isHierarchical: true, type: 'router.registerActor' as const, context: '' },
      ],
      stats: {
        totalFlatIds: 10,
        totalActors: 4,
        hierarchicalActors: 2,
        flatActors: 2,
        migrationProgress: 50,
      },
      byComplexity: {
        simple: [],
        moderate: [],
        complex: [],
      },
      byFile: new Map(),
    };

    const reporter = new MigrationReporter();
    const report = reporter.generateProgressReport(mockAnalysis);

    expect(report.overallProgress).toBe(50);
    expect(report.metrics.totalActors).toBe(4);
    expect(report.metrics.migratedActors).toBe(2);
    expect(report.metrics.remainingActors).toBe(2);
  });

  test('formats progress report', () => {
    const mockReport = {
      timestamp: new Date(),
      overallProgress: 75,
      metrics: {
        totalFlatIds: 20,
        migratedFlatIds: 15,
        remainingFlatIds: 5,
        totalActors: 10,
        migratedActors: 8,
        remainingActors: 2,
      },
      phaseProgress: [],
      complexityHeatMap: {
        simple: 3,
        moderate: 2,
        complex: 0,
      },
      blockers: [],
      recommendations: ['Start with simple migrations'],
    };

    const reporter = new MigrationReporter();
    const formatted = reporter.formatProgressReport(mockReport);

    expect(formatted).toContain('75.0%');
    expect(formatted).toContain('Total:      20');
    expect(formatted).toContain('Migrated:   15');
    expect(formatted).toContain('Start with simple migrations');
  });

  test('generates dashboard', () => {
    const mockAnalysis = {
      timestamp: new Date(),
      filesScanned: 5,
      flatIdUsages: [],
      actorRegistrations: [],
      stats: {
        totalFlatIds: 0,
        totalActors: 0,
        hierarchicalActors: 0,
        flatActors: 0,
        migrationProgress: 100,
      },
      byComplexity: {
        simple: [],
        moderate: [],
        complex: [],
      },
      byFile: new Map(),
    };

    const reporter = new MigrationReporter();
    const dashboard = reporter.generateDashboard(mockAnalysis);

    expect(dashboard.analysis).toBeDefined();
    expect(dashboard.progress).toBeDefined();
    expect(dashboard.progress.overallProgress).toBe(100);
  });
});

describe('Integration Tests', () => {
  let testDir: string;

  beforeEach(async () => {
    testDir = await mkdtemp(join(tmpdir(), 'integration-test-'));
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  test('complete migration workflow', async () => {
    // 1. Create test file with flat IDs
    const testContent = `
import { address } from './message';

router.registerActor('my-actor', actor);
const msg = address('my-actor');
`;

    const testFile = join(testDir, 'test.ts');
    await writeFile(testFile, testContent);

    // 2. Analyze
    const analyzer = new MigrationAnalyzer({ rootDir: testDir });
    const analysis = await analyzer.analyze();

    expect(analysis.flatIdUsages.length).toBeGreaterThan(0);
    expect(analysis.actorRegistrations.length).toBeGreaterThan(0);

    // 3. Plan
    const planner = new MigrationPlanner();
    const plan = planner.generatePlan(analysis);

    expect(plan.proposals.length).toBeGreaterThan(0);
    expect(plan.steps.length).toBeGreaterThan(0);

    // 4. Execute (dry run)
    const refactor = new MigrationRefactor({ dryRun: true });
    const session = refactor.startSession();

    for (const step of plan.steps) {
      await refactor.applyStep(step);
    }

    refactor.endSession();

    // 5. Report
    const reporter = new MigrationReporter();
    const report = reporter.generateProgressReport(analysis, plan);

    expect(report.overallProgress).toBeGreaterThanOrEqual(0);
  });
});
