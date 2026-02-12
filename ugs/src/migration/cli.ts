#!/usr/bin/env bun
/**
 * Migration CLI - Command-Line Interface
 *
 * CLI tool for migration workflow: analyze → plan → execute → verify.
 *
 * Phase 6: Migration Tooling & Automation
 *
 * @see docs/MIGRATION_TOOLING.md
 */

import { parseArgs } from 'node:util';
import { writeFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { MigrationAnalyzer, formatAnalysisReport } from './analyzer';
import { MigrationPlanner, formatMigrationPlan } from './planner';
import { MigrationRefactor } from './refactor';
import { MigrationReporter, exportDashboardJSON } from './reporter';

/**
 * CLI command type.
 */
type Command = 'analyze' | 'plan' | 'execute' | 'report' | 'verify' | 'help';

/**
 * CLI options.
 */
interface CLIOptions {
  command: Command;
  rootDir: string;
  outputDir: string;
  dryRun: boolean;
  verbose: boolean;
  phase?: number;
  json: boolean;
}

/**
 * Parse CLI arguments.
 */
function parseCLIArgs(): CLIOptions {
  const { values, positionals } = parseArgs({
    args: process.argv.slice(2),
    options: {
      'root-dir': { type: 'string', short: 'r', default: process.cwd() },
      'output-dir': { type: 'string', short: 'o', default: '.migration-output' },
      'dry-run': { type: 'boolean', short: 'd', default: false },
      'verbose': { type: 'boolean', short: 'v', default: false },
      'phase': { type: 'string', short: 'p' },
      'json': { type: 'boolean', short: 'j', default: false },
      'help': { type: 'boolean', short: 'h', default: false },
    },
    allowPositionals: true,
  });

  const command = (positionals[0] || 'help') as Command;

  if (values.help) {
    return { command: 'help' } as CLIOptions;
  }

  return {
    command,
    rootDir: values['root-dir'] as string,
    outputDir: values['output-dir'] as string,
    dryRun: values['dry-run'] as boolean,
    verbose: values['verbose'] as boolean,
    phase: values.phase ? parseInt(values.phase as string) : undefined,
    json: values.json as boolean,
  };
}

/**
 * Run CLI command.
 */
async function runCommand(options: CLIOptions): Promise<void> {
  switch (options.command) {
    case 'analyze':
      await runAnalyze(options);
      break;
    case 'plan':
      await runPlan(options);
      break;
    case 'execute':
      await runExecute(options);
      break;
    case 'report':
      await runReport(options);
      break;
    case 'verify':
      await runVerify(options);
      break;
    case 'help':
      printHelp();
      break;
    default:
      console.error(`Unknown command: ${options.command}`);
      printHelp();
      process.exit(1);
  }
}

/**
 * Run analyze command.
 */
async function runAnalyze(options: CLIOptions): Promise<void> {
  console.log('Analyzing codebase for flat ID usage...');
  console.log(`Root directory: ${options.rootDir}`);
  console.log('');

  const analyzer = new MigrationAnalyzer({
    rootDir: options.rootDir,
    verbose: options.verbose,
  });

  const analysis = await analyzer.analyze();

  // Ensure output directory exists
  await mkdir(options.outputDir, { recursive: true });

  // Save analysis results
  const analysisFile = join(options.outputDir, 'analysis.json');
  await writeFile(analysisFile, JSON.stringify(analysis, null, 2));

  if (options.json) {
    console.log(JSON.stringify(analysis, null, 2));
  } else {
    const report = formatAnalysisReport(analysis);
    console.log(report);

    // Save text report
    const reportFile = join(options.outputDir, 'analysis-report.txt');
    await writeFile(reportFile, report);
    console.log(`\nDetailed report saved to: ${reportFile}`);
  }

  console.log(`\nAnalysis data saved to: ${analysisFile}`);
}

/**
 * Run plan command.
 */
async function runPlan(options: CLIOptions): Promise<void> {
  console.log('Generating migration plan...');
  console.log('');

  // Load analysis results
  const analysisFile = join(options.outputDir, 'analysis.json');
  let analysis;

  try {
    const analysisData = await Bun.file(analysisFile).text();
    analysis = JSON.parse(analysisData);
  } catch {
    console.error(`Error: Analysis file not found at ${analysisFile}`);
    console.error('Run "npm run migrate analyze" first.');
    process.exit(1);
  }

  const planner = new MigrationPlanner({
    verbose: options.verbose,
  });

  const plan = planner.generatePlan(analysis);

  // Save plan
  const planFile = join(options.outputDir, 'plan.json');
  await writeFile(planFile, JSON.stringify(plan, null, 2));

  if (options.json) {
    console.log(JSON.stringify(plan, null, 2));
  } else {
    const report = formatMigrationPlan(plan);
    console.log(report);

    // Save text report
    const reportFile = join(options.outputDir, 'plan-report.txt');
    await writeFile(reportFile, report);
    console.log(`\nDetailed plan saved to: ${reportFile}`);
  }

  console.log(`\nPlan data saved to: ${planFile}`);
}

/**
 * Run execute command.
 */
async function runExecute(options: CLIOptions): Promise<void> {
  console.log('Executing migration...');
  console.log('');

  if (options.dryRun) {
    console.log('🔍 DRY RUN MODE - No files will be modified');
    console.log('');
  }

  // Load plan
  const planFile = join(options.outputDir, 'plan.json');
  let plan;

  try {
    const planData = await Bun.file(planFile).text();
    plan = JSON.parse(planData);
  } catch {
    console.error(`Error: Plan file not found at ${planFile}`);
    console.error('Run "npm run migrate plan" first.');
    process.exit(1);
  }

  // Filter steps by phase if specified
  let stepsToExecute = plan.steps;
  if (options.phase !== undefined) {
    const phase = plan.phases.find((p: any) => p.phase === options.phase);
    if (!phase) {
      console.error(`Error: Phase ${options.phase} not found`);
      process.exit(1);
    }

    stepsToExecute = plan.steps.filter((s: any) => phase.steps.includes(s.step));
    console.log(`Executing Phase ${options.phase}: ${phase.name}`);
    console.log(`Steps: ${stepsToExecute.length}`);
    console.log('');
  }

  // Execute migration
  const refactor = new MigrationRefactor({
    dryRun: options.dryRun,
    verbose: options.verbose,
  });

  const session = refactor.startSession();

  for (const step of stepsToExecute) {
    console.log(`Step ${step.step}: ${step.description}`);

    const results = await refactor.applyStep(step);

    for (const result of results) {
      if (result.success) {
        console.log(`  ✓ ${result.file} (${result.changesApplied} changes)`);
      } else {
        console.log(`  ✗ ${result.file}: ${result.error}`);
      }
    }

    console.log('');
  }

  const completedSession = refactor.endSession();

  // Save session results
  const sessionFile = join(options.outputDir, `session-${completedSession.id}.json`);
  await writeFile(sessionFile, JSON.stringify(completedSession, null, 2));

  const reporter = new MigrationReporter();
  const summary = reporter.formatSessionSummary(completedSession);
  console.log(summary);

  console.log(`\nSession data saved to: ${sessionFile}`);

  if (options.dryRun) {
    console.log('\n🔍 This was a DRY RUN - no files were modified');
    console.log('   Remove --dry-run flag to apply changes');
  }
}

/**
 * Run report command.
 */
async function runReport(options: CLIOptions): Promise<void> {
  console.log('Generating migration report...');
  console.log('');

  // Load analysis and plan if available
  const analysisFile = join(options.outputDir, 'analysis.json');
  const planFile = join(options.outputDir, 'plan.json');

  let analysis, plan;

  try {
    const analysisData = await Bun.file(analysisFile).text();
    analysis = JSON.parse(analysisData);
  } catch {
    // Analysis not found
  }

  try {
    const planData = await Bun.file(planFile).text();
    plan = JSON.parse(planData);
  } catch {
    // Plan not found
  }

  if (!analysis) {
    console.error('Error: No analysis data found.');
    console.error('Run "npm run migrate analyze" first.');
    process.exit(1);
  }

  const reporter = new MigrationReporter();

  if (options.json) {
    const dashboard = reporter.generateDashboard(analysis, plan);
    console.log(exportDashboardJSON(dashboard));
  } else {
    const progress = reporter.generateProgressReport(analysis, plan);
    const report = reporter.formatProgressReport(progress);
    console.log(report);

    // Save report
    const reportFile = join(options.outputDir, 'progress-report.txt');
    await writeFile(reportFile, report);
    console.log(`\nReport saved to: ${reportFile}`);
  }
}

/**
 * Run verify command.
 */
async function runVerify(options: CLIOptions): Promise<void> {
  console.log('Verifying migration...');
  console.log('');

  // Load original analysis
  const analysisFile = join(options.outputDir, 'analysis.json');
  let originalAnalysis;

  try {
    const analysisData = await Bun.file(analysisFile).text();
    originalAnalysis = JSON.parse(analysisData);
  } catch {
    console.error('Error: Original analysis not found.');
    console.error('Run "npm run migrate analyze" first.');
    process.exit(1);
  }

  // Re-analyze to check progress
  console.log('Re-analyzing codebase...');
  const analyzer = new MigrationAnalyzer({
    rootDir: options.rootDir,
    verbose: false,
  });

  const currentAnalysis = await analyzer.analyze();

  // Compare results
  console.log('VERIFICATION RESULTS');
  console.log('───────────────────────────────────────────────────────');
  console.log('');

  console.log('Before Migration:');
  console.log(`  Flat IDs:           ${originalAnalysis.stats.totalFlatIds}`);
  console.log(`  Flat Actors:        ${originalAnalysis.stats.flatActors}`);
  console.log(`  Migration Progress: ${originalAnalysis.stats.migrationProgress.toFixed(2)}%`);
  console.log('');

  console.log('After Migration:');
  console.log(`  Flat IDs:           ${currentAnalysis.stats.totalFlatIds}`);
  console.log(`  Flat Actors:        ${currentAnalysis.stats.flatActors}`);
  console.log(`  Migration Progress: ${currentAnalysis.stats.migrationProgress.toFixed(2)}%`);
  console.log('');

  const improvement = currentAnalysis.stats.migrationProgress - originalAnalysis.stats.migrationProgress;

  console.log('Progress:');
  console.log(`  Improvement:        ${improvement > 0 ? '+' : ''}${improvement.toFixed(2)}%`);
  console.log(`  Flat IDs Removed:   ${originalAnalysis.stats.totalFlatIds - currentAnalysis.stats.totalFlatIds}`);
  console.log(`  Actors Migrated:    ${currentAnalysis.stats.hierarchicalActors - originalAnalysis.stats.hierarchicalActors}`);
  console.log('');

  if (currentAnalysis.stats.migrationProgress === 100) {
    console.log('✅ Migration Complete! All actors using hierarchical paths.');
  } else if (improvement > 0) {
    console.log(`✓ Progress made! ${improvement.toFixed(1)}% improvement.`);
    console.log(`  ${currentAnalysis.stats.flatActors} actors remaining to migrate.`);
  } else {
    console.log('⚠️  No progress detected. Check execution logs.');
  }
}

/**
 * Print help message.
 */
function printHelp(): void {
  console.log(`
Migration CLI - Path-Based Addressing Migration Tool

USAGE:
  bun src/migration/cli.ts <command> [options]

COMMANDS:
  analyze    Scan codebase for flat ID usage
  plan       Generate migration plan from analysis
  execute    Execute migration steps
  report     Generate progress report
  verify     Verify migration progress
  help       Show this help message

OPTIONS:
  -r, --root-dir <path>     Root directory to scan (default: cwd)
  -o, --output-dir <path>   Output directory for reports (default: .migration-output)
  -d, --dry-run             Run without modifying files
  -v, --verbose             Enable verbose output
  -p, --phase <number>      Execute specific phase only
  -j, --json                Output in JSON format
  -h, --help                Show this help message

EXAMPLES:
  # Analyze codebase
  bun src/migration/cli.ts analyze

  # Generate migration plan
  bun src/migration/cli.ts plan

  # Execute migration (dry run)
  bun src/migration/cli.ts execute --dry-run

  # Execute Phase 1 only
  bun src/migration/cli.ts execute --phase 1

  # Generate progress report
  bun src/migration/cli.ts report

  # Verify migration
  bun src/migration/cli.ts verify

WORKFLOW:
  1. npm run migrate:analyze   - Scan for flat IDs
  2. npm run migrate:plan      - Generate plan
  3. npm run migrate:execute   - Apply changes (use --dry-run first!)
  4. npm run migrate:verify    - Verify progress

See docs/MIGRATION_TOOLING.md for detailed documentation.
`);
}

/**
 * Main entry point.
 */
async function main(): Promise<void> {
  try {
    const options = parseCLIArgs();
    await runCommand(options);
  } catch (error: any) {
    console.error('Error:', error.message);
    if (error.stack) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

// Run CLI if executed directly
if (import.meta.main) {
  main();
}

export { runCommand, parseCLIArgs };
