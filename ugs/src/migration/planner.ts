#!/usr/bin/env bun
/**
 * Migration Planner - Generate Step-by-Step Migration Plans
 *
 * Analyzes dependencies between actors and suggests hierarchical organization.
 * Creates actionable migration plans with effort estimates.
 *
 * Phase 6: Migration Tooling & Automation
 *
 * @see docs/MIGRATION_TOOLING.md
 */

import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import type { CodebaseAnalysis, FlatIdUsage, ActorRegistration } from './analyzer';

/**
 * Proposed hierarchical path for an actor.
 */
export interface PathProposal {
  /** Current flat ID */
  flatId: string;

  /** Proposed hierarchical path */
  proposedPath: string;

  /** Reasoning for this path */
  reasoning: string;

  /** Confidence level */
  confidence: 'high' | 'medium' | 'low';

  /** Related actors (dependencies) */
  dependencies: string[];

  /** Estimated effort (minutes) */
  effort: number;
}

/**
 * Migration step (atomic change).
 */
export interface MigrationStep {
  /** Step number */
  step: number;

  /** Actor being migrated */
  actorId: string;

  /** Target hierarchical path */
  targetPath: string;

  /** Step type */
  type: 'create-alias' | 'update-registration' | 'update-usages' | 'remove-alias';

  /** Description */
  description: string;

  /** Files to modify */
  files: string[];

  /** Code changes required */
  changes: CodeChange[];

  /** Estimated effort (minutes) */
  effort: number;

  /** Prerequisites (other steps) */
  prerequisites: number[];
}

/**
 * Code change specification.
 */
export interface CodeChange {
  /** File to modify */
  file: string;

  /** Line number */
  line: number;

  /** Current code */
  before: string;

  /** Replacement code */
  after: string;

  /** Change type */
  type: 'replace' | 'insert' | 'delete';
}

/**
 * Complete migration plan.
 */
export interface MigrationPlan {
  /** Plan creation timestamp */
  timestamp: Date;

  /** Path proposals for all actors */
  proposals: PathProposal[];

  /** Ordered migration steps */
  steps: MigrationStep[];

  /** Total estimated effort (hours) */
  totalEffort: number;

  /** Migration phases */
  phases: MigrationPhase[];

  /** Risk assessment */
  risks: string[];

  /** Validation checks */
  validationChecks: string[];
}

/**
 * Migration phase (grouped steps).
 */
export interface MigrationPhase {
  /** Phase number */
  phase: number;

  /** Phase name */
  name: string;

  /** Description */
  description: string;

  /** Steps in this phase */
  steps: number[];

  /** Estimated duration (hours) */
  duration: number;

  /** Can run in parallel with other phases? */
  parallelizable: boolean;
}

/**
 * Migration planner configuration.
 */
export interface PlannerConfig {
  /** Preferred root paths for organization */
  rootPaths?: string[];

  /** Actor categorization heuristics */
  categorization?: {
    services?: string[];
    domain?: string[];
    workflows?: string[];
    channels?: string[];
  };

  /** Enable verbose output */
  verbose?: boolean;
}

/**
 * Migration Planner - Generate migration plans from analysis.
 */
export class MigrationPlanner {
  private config: Required<PlannerConfig>;
  private pathMappings: Record<string, string> = {};

  constructor(config: PlannerConfig = {}) {
    this.config = {
      rootPaths: config.rootPaths || ['domain', 'services', 'workflows', 'channels'],
      categorization: config.categorization || {
        services: ['llm', 'storage', 'api', 'inference', 'executor'],
        domain: ['inference', 'executor', 'program'],
        workflows: ['task', 'pipeline', 'build', 'deploy'],
        channels: ['slack', 'telegram', 'webhook'],
      },
      verbose: config.verbose ?? false,
    };

    // Load path mappings from file if available
    this.pathMappings = this.loadPathMappings();
  }

  /**
   * Load path mappings from .migration-output/path-mappings.json
   */
  private loadPathMappings(): Record<string, string> {
    const mappingFile = join(process.cwd(), '.migration-output', 'path-mappings.json');
    try {
      const content = readFileSync(mappingFile, 'utf-8');
      const mappings = JSON.parse(content);
      if (this.config.verbose) {
        console.log(`✓ Loaded ${Object.keys(mappings).length} path mappings from ${mappingFile}`);
      }
      return mappings;
    } catch (error) {
      if (this.config.verbose) {
        console.warn('⚠ Path mappings file not found, using heuristic-based proposals');
      }
      return {};
    }
  }

  /**
   * Generate migration plan from analysis results.
   *
   * @param analysis - Codebase analysis
   * @returns Complete migration plan
   */
  generatePlan(analysis: CodebaseAnalysis): MigrationPlan {
    const proposals = this.generatePathProposals(analysis);
    const steps = this.generateMigrationSteps(analysis, proposals);
    const phases = this.groupIntoPhases(steps);
    const totalEffort = this.calculateTotalEffort(steps);
    const risks = this.assessRisks(analysis, proposals);
    const validationChecks = this.generateValidationChecks(proposals);

    return {
      timestamp: new Date(),
      proposals,
      steps,
      totalEffort,
      phases,
      risks,
      validationChecks,
    };
  }

  /**
   * Generate path proposals for all flat IDs.
   */
  private generatePathProposals(analysis: CodebaseAnalysis): PathProposal[] {
    const proposals: PathProposal[] = [];
    const uniqueFlatIds = new Set<string>();

    // Collect unique flat IDs
    for (const usage of analysis.flatIdUsages) {
      uniqueFlatIds.add(usage.flatId);
    }

    for (const registration of analysis.actorRegistrations) {
      if (!registration.isHierarchical) {
        uniqueFlatIds.add(registration.actorId);
      }
    }

    // Generate proposal for each flat ID
    for (const flatId of uniqueFlatIds) {
      const proposal = this.proposePathForActor(flatId, analysis);
      proposals.push(proposal);
    }

    return proposals.sort((a, b) => b.confidence === a.confidence ? 0 : b.confidence === 'high' ? 1 : -1);
  }

  /**
   * Propose hierarchical path for a single actor.
   */
  private proposePathForActor(
    flatId: string,
    analysis: CodebaseAnalysis
  ): PathProposal {
    // First, check if we have a manual mapping for this flat ID
    if (this.pathMappings[flatId]) {
      return {
        flatId,
        proposedPath: this.pathMappings[flatId],
        reasoning: `Manual mapping from path-mappings.json`,
        confidence: 'high',
        dependencies: this.findDependencies(flatId, analysis),
        effort: 10,
      };
    }

    // Try categorization heuristics
    for (const [category, keywords] of Object.entries(this.config.categorization)) {
      for (const keyword of keywords) {
        if (flatId.includes(keyword)) {
          return {
            flatId,
            proposedPath: `${category}/${flatId}`,
            reasoning: `Matches keyword "${keyword}" for category "${category}"`,
            confidence: 'high',
            dependencies: this.findDependencies(flatId, analysis),
            effort: 15,
          };
        }
      }
    }

    // Default: place in 'domain' category
    return {
      flatId,
      proposedPath: `domain/${flatId}`,
      reasoning: 'Default placement. Review and adjust based on actor purpose.',
      confidence: 'low',
      dependencies: this.findDependencies(flatId, analysis),
      effort: 30,
    };
  }

  /**
   * Find dependencies for an actor.
   */
  private findDependencies(flatId: string, analysis: CodebaseAnalysis): string[] {
    const dependencies: Set<string> = new Set();

    // Look for actors that this actor calls
    for (const usage of analysis.flatIdUsages) {
      if (usage.context.includes(flatId)) {
        // Found related usage
        dependencies.add(usage.flatId);
      }
    }

    return Array.from(dependencies).filter(dep => dep !== flatId);
  }

  /**
   * Generate migration steps from proposals.
   */
  private generateMigrationSteps(
    analysis: CodebaseAnalysis,
    proposals: PathProposal[]
  ): MigrationStep[] {
    const steps: MigrationStep[] = [];
    let stepNumber = 1;

    for (const proposal of proposals) {
      // Step 1: Create alias
      steps.push({
        step: stepNumber++,
        actorId: proposal.flatId,
        targetPath: proposal.proposedPath,
        type: 'create-alias',
        description: `Create alias mapping: ${proposal.flatId} → ${proposal.proposedPath}`,
        files: ['src/migration/aliases.ts'],
        changes: [
          {
            file: 'src/migration/aliases.ts',
            line: -1, // Insert at appropriate location
            before: '',
            after: `registerAlias('${proposal.flatId}', '${proposal.proposedPath}');`,
            type: 'insert',
          },
        ],
        effort: 5,
        prerequisites: [],
      });

      // Step 2: Update actor registration
      const registrations = analysis.actorRegistrations.filter(
        r => r.actorId === proposal.flatId
      );

      for (const reg of registrations) {
        steps.push({
          step: stepNumber++,
          actorId: proposal.flatId,
          targetPath: proposal.proposedPath,
          type: 'update-registration',
          description: `Update actor registration in ${reg.file}`,
          files: [reg.file],
          changes: [
            {
              file: reg.file,
              line: reg.line,
              before: `'${proposal.flatId}'`,
              after: `'${proposal.proposedPath}'`,
              type: 'replace',
            },
          ],
          effort: 10,
          prerequisites: [stepNumber - 2], // Depends on alias creation
        });
      }

      // Step 3: Update usages
      const usages = analysis.flatIdUsages.filter(
        u => u.flatId === proposal.flatId && u.type === 'address-call'
      );

      const usageFiles = new Map<string, FlatIdUsage[]>();
      for (const usage of usages) {
        const fileUsages = usageFiles.get(usage.file) || [];
        fileUsages.push(usage);
        usageFiles.set(usage.file, fileUsages);
      }

      for (const [file, fileUsages] of usageFiles) {
        steps.push({
          step: stepNumber++,
          actorId: proposal.flatId,
          targetPath: proposal.proposedPath,
          type: 'update-usages',
          description: `Update ${fileUsages.length} usage(s) in ${file}`,
          files: [file],
          changes: fileUsages.map(usage => ({
            file,
            line: usage.line,
            before: `address('${proposal.flatId}')`,
            after: `address('${proposal.proposedPath}')`,
            type: 'replace',
          })),
          effort: 5 * fileUsages.length,
          prerequisites: [stepNumber - 2 - registrations.length], // Depends on alias
        });
      }

      // Step 4: Remove alias (after migration complete)
      steps.push({
        step: stepNumber++,
        actorId: proposal.flatId,
        targetPath: proposal.proposedPath,
        type: 'remove-alias',
        description: `Remove alias after migration verified: ${proposal.flatId}`,
        files: ['src/migration/aliases.ts'],
        changes: [
          {
            file: 'src/migration/aliases.ts',
            line: -1,
            before: `registerAlias('${proposal.flatId}', '${proposal.proposedPath}');`,
            after: '',
            type: 'delete',
          },
        ],
        effort: 2,
        prerequisites: [stepNumber - 2], // Depends on usage updates
      });
    }

    return steps;
  }

  /**
   * Group steps into migration phases.
   */
  private groupIntoPhases(steps: MigrationStep[]): MigrationPhase[] {
    const phases: MigrationPhase[] = [];

    // Phase 1: Create all aliases (parallelizable)
    const aliasSteps = steps.filter(s => s.type === 'create-alias');
    if (aliasSteps.length > 0) {
      phases.push({
        phase: 1,
        name: 'Create Aliases',
        description: 'Register flat ID → path aliases for backward compatibility',
        steps: aliasSteps.map(s => s.step),
        duration: this.estimatePhaseHours(aliasSteps),
        parallelizable: true,
      });
    }

    // Phase 2: Update registrations (requires aliases)
    const regSteps = steps.filter(s => s.type === 'update-registration');
    if (regSteps.length > 0) {
      phases.push({
        phase: 2,
        name: 'Update Registrations',
        description: 'Update actor registrations to use hierarchical paths',
        steps: regSteps.map(s => s.step),
        duration: this.estimatePhaseHours(regSteps),
        parallelizable: false,
      });
    }

    // Phase 3: Update usages (can be parallelized per file)
    const usageSteps = steps.filter(s => s.type === 'update-usages');
    if (usageSteps.length > 0) {
      phases.push({
        phase: 3,
        name: 'Update Usages',
        description: 'Replace flat ID address() calls with hierarchical paths',
        steps: usageSteps.map(s => s.step),
        duration: this.estimatePhaseHours(usageSteps),
        parallelizable: true,
      });
    }

    // Phase 4: Verification & cleanup
    const cleanupSteps = steps.filter(s => s.type === 'remove-alias');
    if (cleanupSteps.length > 0) {
      phases.push({
        phase: 4,
        name: 'Verification & Cleanup',
        description: 'Verify migration and remove aliases',
        steps: cleanupSteps.map(s => s.step),
        duration: this.estimatePhaseHours(cleanupSteps),
        parallelizable: false,
      });
    }

    return phases;
  }

  /**
   * Calculate total effort in hours.
   */
  private calculateTotalEffort(steps: MigrationStep[]): number {
    const totalMinutes = steps.reduce((sum, step) => sum + step.effort, 0);
    return Math.round((totalMinutes / 60) * 100) / 100;
  }

  /**
   * Estimate phase duration in hours.
   */
  private estimatePhaseHours(steps: MigrationStep[]): number {
    const totalMinutes = steps.reduce((sum, step) => sum + step.effort, 0);
    return Math.round((totalMinutes / 60) * 100) / 100;
  }

  /**
   * Assess migration risks.
   */
  private assessRisks(
    analysis: CodebaseAnalysis,
    proposals: PathProposal[]
  ): string[] {
    const risks: string[] = [];

    if (analysis.byComplexity.complex.length > 10) {
      risks.push(
        `⚠️  ${analysis.byComplexity.complex.length} complex migrations requiring structural changes`
      );
    }

    const lowConfidence = proposals.filter(p => p.confidence === 'low');
    if (lowConfidence.length > 0) {
      risks.push(
        `⚠️  ${lowConfidence.length} path proposals have low confidence. Manual review recommended.`
      );
    }

    if (analysis.stats.totalFlatIds > 100) {
      risks.push(
        `⚠️  Large migration (${analysis.stats.totalFlatIds} usages). Consider incremental approach.`
      );
    }

    if (analysis.actorRegistrations.length > 20) {
      risks.push(
        `⚠️  ${analysis.actorRegistrations.length} actor registrations to update. Plan supervision tree carefully.`
      );
    }

    return risks;
  }

  /**
   * Generate validation checks.
   */
  private generateValidationChecks(proposals: PathProposal[]): string[] {
    return [
      '✓ All tests pass after alias creation',
      '✓ All tests pass after registration updates',
      '✓ All tests pass after usage updates',
      '✓ No flat ID deprecation warnings in logs',
      '✓ Router stats show 100% path usage',
      `✓ All ${proposals.length} aliases can be resolved`,
      '✓ No routing errors in production logs',
      '✓ Performance metrics within acceptable range',
    ];
  }
}

/**
 * Format migration plan as human-readable report.
 */
export function formatMigrationPlan(plan: MigrationPlan): string {
  const lines: string[] = [];

  lines.push('═══════════════════════════════════════════════════════');
  lines.push('  Migration Plan');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');
  lines.push(`Generated: ${plan.timestamp.toISOString()}`);
  lines.push(`Total Steps: ${plan.steps.length}`);
  lines.push(`Estimated Effort: ${plan.totalEffort.toFixed(1)} hours`);
  lines.push('');

  lines.push('PATH PROPOSALS');
  lines.push('───────────────────────────────────────────────────────');
  for (const proposal of plan.proposals) {
    const confidence = proposal.confidence === 'high' ? '✓' : proposal.confidence === 'medium' ? '~' : '?';
    lines.push(`  ${confidence} ${proposal.flatId} → ${proposal.proposedPath}`);
    lines.push(`     ${proposal.reasoning}`);
    if (proposal.dependencies.length > 0) {
      lines.push(`     Dependencies: ${proposal.dependencies.join(', ')}`);
    }
    lines.push('');
  }

  lines.push('MIGRATION PHASES');
  lines.push('───────────────────────────────────────────────────────');
  for (const phase of plan.phases) {
    const parallel = phase.parallelizable ? ' (parallelizable)' : '';
    lines.push(`  Phase ${phase.phase}: ${phase.name}${parallel}`);
    lines.push(`  Duration: ${phase.duration.toFixed(1)} hours`);
    lines.push(`  ${phase.description}`);
    lines.push(`  Steps: ${phase.steps.length}`);
    lines.push('');
  }

  if (plan.risks.length > 0) {
    lines.push('RISKS');
    lines.push('───────────────────────────────────────────────────────');
    for (const risk of plan.risks) {
      lines.push(`  ${risk}`);
    }
    lines.push('');
  }

  lines.push('VALIDATION CHECKLIST');
  lines.push('───────────────────────────────────────────────────────');
  for (const check of plan.validationChecks) {
    lines.push(`  ${check}`);
  }
  lines.push('');

  lines.push('NEXT STEPS');
  lines.push('───────────────────────────────────────────────────────');
  lines.push('  1. Review path proposals and adjust if needed');
  lines.push('  2. Execute Phase 1: npm run migrate:execute --phase 1');
  lines.push('  3. Run tests and verify');
  lines.push('  4. Continue with remaining phases');
  lines.push('  5. Monitor and validate');
  lines.push('');

  return lines.join('\n');
}
