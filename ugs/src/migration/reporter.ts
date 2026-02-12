#!/usr/bin/env bun
/**
 * Migration Reporter - Progress Reporting & Metrics
 *
 * Track and visualize migration progress from flat IDs to hierarchical paths.
 *
 * Phase 6: Migration Tooling & Automation
 *
 * @see docs/MIGRATION_TOOLING.md
 */

import type { CodebaseAnalysis } from './analyzer';
import type { MigrationPlan } from './planner';
import type { RefactorSession, RefactorResult } from './refactor';

/**
 * Migration progress report.
 */
export interface ProgressReport {
  /** Report timestamp */
  timestamp: Date;

  /** Overall progress percentage */
  overallProgress: number;

  /** Metrics */
  metrics: {
    totalFlatIds: number;
    migratedFlatIds: number;
    remainingFlatIds: number;
    totalActors: number;
    migratedActors: number;
    remainingActors: number;
  };

  /** Progress by phase */
  phaseProgress: {
    phase: number;
    name: string;
    completed: boolean;
    progress: number;
  }[];

  /** Complexity heat map */
  complexityHeatMap: {
    simple: number;
    moderate: number;
    complex: number;
  };

  /** Top blockers */
  blockers: string[];

  /** Recommendations */
  recommendations: string[];
}

/**
 * Migration dashboard (full status).
 */
export interface MigrationDashboard {
  /** Dashboard timestamp */
  timestamp: Date;

  /** Current analysis */
  analysis?: CodebaseAnalysis;

  /** Current plan */
  plan?: MigrationPlan;

  /** Execution sessions */
  sessions: RefactorSession[];

  /** Progress report */
  progress: ProgressReport;

  /** Performance metrics */
  performance: {
    routerStats?: {
      flatIdUsage: number;
      pathUsage: number;
      migrationProgress: number;
    };
    averageRoutingTime?: number;
    errorRate?: number;
  };
}

/**
 * Migration Reporter - Track and report progress.
 */
export class MigrationReporter {
  /**
   * Generate progress report from analysis.
   */
  generateProgressReport(
    analysis: CodebaseAnalysis,
    plan?: MigrationPlan
  ): ProgressReport {
    const metrics = {
      totalFlatIds: analysis.stats.totalFlatIds,
      migratedFlatIds: 0, // Will be calculated from actual migration
      remainingFlatIds: analysis.stats.totalFlatIds,
      totalActors: analysis.stats.totalActors,
      migratedActors: analysis.stats.hierarchicalActors,
      remainingActors: analysis.stats.flatActors,
    };

    const overallProgress = analysis.stats.migrationProgress;

    const phaseProgress = plan
      ? plan.phases.map((phase, index) => ({
          phase: phase.phase,
          name: phase.name,
          completed: false, // Would check actual execution status
          progress: 0,
        }))
      : [];

    const complexityHeatMap = {
      simple: analysis.byComplexity.simple.length,
      moderate: analysis.byComplexity.moderate.length,
      complex: analysis.byComplexity.complex.length,
    };

    const blockers = this.identifyBlockers(analysis);
    const recommendations = this.generateRecommendations(analysis, plan);

    return {
      timestamp: new Date(),
      overallProgress,
      metrics,
      phaseProgress,
      complexityHeatMap,
      blockers,
      recommendations,
    };
  }

  /**
   * Generate full migration dashboard.
   */
  generateDashboard(
    analysis?: CodebaseAnalysis,
    plan?: MigrationPlan,
    sessions: RefactorSession[] = []
  ): MigrationDashboard {
    const progress = analysis
      ? this.generateProgressReport(analysis, plan)
      : this.emptyProgressReport();

    return {
      timestamp: new Date(),
      analysis,
      plan,
      sessions,
      progress,
      performance: {}, // Would be populated from router metrics
    };
  }

  /**
   * Format progress report as text.
   */
  formatProgressReport(report: ProgressReport): string {
    const lines: string[] = [];

    lines.push('═══════════════════════════════════════════════════════');
    lines.push('  Migration Progress Report');
    lines.push('═══════════════════════════════════════════════════════');
    lines.push('');
    lines.push(`Generated: ${report.timestamp.toISOString()}`);
    lines.push(`Overall Progress: ${this.formatProgressBar(report.overallProgress)}`);
    lines.push('');

    lines.push('METRICS');
    lines.push('───────────────────────────────────────────────────────');
    lines.push(`  Flat IDs:`);
    lines.push(`    Total:      ${report.metrics.totalFlatIds}`);
    lines.push(`    Migrated:   ${report.metrics.migratedFlatIds}`);
    lines.push(`    Remaining:  ${report.metrics.remainingFlatIds}`);
    lines.push('');
    lines.push(`  Actors:`);
    lines.push(`    Total:      ${report.metrics.totalActors}`);
    lines.push(`    Migrated:   ${report.metrics.migratedActors}`);
    lines.push(`    Remaining:  ${report.metrics.remainingActors}`);
    lines.push('');

    if (report.phaseProgress.length > 0) {
      lines.push('PHASE PROGRESS');
      lines.push('───────────────────────────────────────────────────────');
      for (const phase of report.phaseProgress) {
        const status = phase.completed ? '✓' : '○';
        lines.push(`  ${status} Phase ${phase.phase}: ${phase.name}`);
        lines.push(`     ${this.formatProgressBar(phase.progress)}`);
      }
      lines.push('');
    }

    lines.push('COMPLEXITY HEAT MAP');
    lines.push('───────────────────────────────────────────────────────');
    const total =
      report.complexityHeatMap.simple +
      report.complexityHeatMap.moderate +
      report.complexityHeatMap.complex;
    lines.push(
      `  Simple:     ${this.formatBar(report.complexityHeatMap.simple, total)} ${report.complexityHeatMap.simple}`
    );
    lines.push(
      `  Moderate:   ${this.formatBar(report.complexityHeatMap.moderate, total)} ${report.complexityHeatMap.moderate}`
    );
    lines.push(
      `  Complex:    ${this.formatBar(report.complexityHeatMap.complex, total)} ${report.complexityHeatMap.complex}`
    );
    lines.push('');

    if (report.blockers.length > 0) {
      lines.push('BLOCKERS');
      lines.push('───────────────────────────────────────────────────────');
      for (const blocker of report.blockers) {
        lines.push(`  🚧 ${blocker}`);
      }
      lines.push('');
    }

    if (report.recommendations.length > 0) {
      lines.push('RECOMMENDATIONS');
      lines.push('───────────────────────────────────────────────────────');
      for (const rec of report.recommendations) {
        lines.push(`  💡 ${rec}`);
      }
      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Format execution results.
   */
  formatExecutionResults(results: RefactorResult[]): string {
    const lines: string[] = [];

    const successful = results.filter(r => r.success);
    const failed = results.filter(r => !r.success);
    const totalChanges = successful.reduce((sum, r) => sum + r.changesApplied, 0);

    lines.push('═══════════════════════════════════════════════════════');
    lines.push('  Execution Results');
    lines.push('═══════════════════════════════════════════════════════');
    lines.push('');
    lines.push(`Files Processed: ${results.length}`);
    lines.push(`Successful: ${successful.length}`);
    lines.push(`Failed: ${failed.length}`);
    lines.push(`Total Changes: ${totalChanges}`);
    lines.push('');

    if (successful.length > 0) {
      lines.push('SUCCESSFUL UPDATES');
      lines.push('───────────────────────────────────────────────────────');
      for (const result of successful) {
        lines.push(`  ✓ ${result.file} (${result.changesApplied} changes)`);
        if (result.warnings.length > 0) {
          for (const warning of result.warnings) {
            lines.push(`     ⚠️  ${warning}`);
          }
        }
      }
      lines.push('');
    }

    if (failed.length > 0) {
      lines.push('FAILED UPDATES');
      lines.push('───────────────────────────────────────────────────────');
      for (const result of failed) {
        lines.push(`  ✗ ${result.file}`);
        lines.push(`     Error: ${result.error}`);
      }
      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Format session summary.
   */
  formatSessionSummary(session: RefactorSession): string {
    const lines: string[] = [];

    const duration = session.endTime
      ? session.endTime.getTime() - session.startTime.getTime()
      : 0;

    lines.push('═══════════════════════════════════════════════════════');
    lines.push('  Refactoring Session Summary');
    lines.push('═══════════════════════════════════════════════════════');
    lines.push('');
    lines.push(`Session ID: ${session.id}`);
    lines.push(`Started: ${session.startTime.toISOString()}`);
    if (session.endTime) {
      lines.push(`Ended: ${session.endTime.toISOString()}`);
      lines.push(`Duration: ${(duration / 1000).toFixed(2)}s`);
    }
    lines.push(`Total Changes: ${session.totalChanges}`);
    lines.push(`Files Modified: ${session.results.length}`);
    lines.push('');

    const successful = session.results.filter(r => r.success);
    const failed = session.results.filter(r => !r.success);

    lines.push(`Success Rate: ${((successful.length / session.results.length) * 100).toFixed(1)}%`);
    lines.push(`Successful: ${successful.length}`);
    lines.push(`Failed: ${failed.length}`);
    lines.push('');

    if (session.rollbackData.size > 0) {
      lines.push(`Rollback Data: ${session.rollbackData.size} files`);
      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Identify blockers from analysis.
   */
  private identifyBlockers(analysis: CodebaseAnalysis): string[] {
    const blockers: string[] = [];

    if (analysis.byComplexity.complex.length > 5) {
      blockers.push(
        `${analysis.byComplexity.complex.length} complex migrations need planning`
      );
    }

    const filesWithManyUsages = Array.from(analysis.byFile.entries())
      .filter(([_, usages]) => usages.length > 10)
      .map(([file, usages]) => ({ file, count: usages.length }));

    if (filesWithManyUsages.length > 0) {
      const topFile = filesWithManyUsages[0];
      blockers.push(
        `${topFile.file} has ${topFile.count} flat ID usages (high concentration)`
      );
    }

    return blockers;
  }

  /**
   * Generate recommendations.
   */
  private generateRecommendations(
    analysis: CodebaseAnalysis,
    plan?: MigrationPlan
  ): string[] {
    const recs: string[] = [];

    if (analysis.byComplexity.simple.length > 0) {
      recs.push(`Start with ${analysis.byComplexity.simple.length} simple migrations for quick wins`);
    }

    if (plan && plan.phases.length > 0) {
      const firstPhase = plan.phases[0];
      recs.push(`Begin with Phase 1: ${firstPhase.name} (${firstPhase.duration}h)`);
    }

    if (analysis.stats.migrationProgress > 50) {
      recs.push(`You're over halfway there! Keep the momentum going.`);
    } else if (analysis.stats.migrationProgress < 10) {
      recs.push(`Create aliases first to enable gradual migration`);
    }

    if (analysis.stats.totalFlatIds > 50) {
      recs.push(`Consider migrating incrementally by feature/module`);
    }

    return recs;
  }

  /**
   * Format progress bar.
   */
  private formatProgressBar(percentage: number, width: number = 30): string {
    const filled = Math.round((percentage / 100) * width);
    const empty = width - filled;
    const bar = '█'.repeat(filled) + '░'.repeat(empty);
    return `[${bar}] ${percentage.toFixed(1)}%`;
  }

  /**
   * Format bar chart.
   */
  private formatBar(value: number, max: number, width: number = 20): string {
    if (max === 0) return '░'.repeat(width);
    const filled = Math.round((value / max) * width);
    const empty = width - filled;
    return '█'.repeat(filled) + '░'.repeat(empty);
  }

  /**
   * Empty progress report.
   */
  private emptyProgressReport(): ProgressReport {
    return {
      timestamp: new Date(),
      overallProgress: 0,
      metrics: {
        totalFlatIds: 0,
        migratedFlatIds: 0,
        remainingFlatIds: 0,
        totalActors: 0,
        migratedActors: 0,
        remainingActors: 0,
      },
      phaseProgress: [],
      complexityHeatMap: {
        simple: 0,
        moderate: 0,
        complex: 0,
      },
      blockers: [],
      recommendations: [],
    };
  }
}

/**
 * Export to JSON for programmatic access.
 */
export function exportDashboardJSON(dashboard: MigrationDashboard): string {
  return JSON.stringify(dashboard, null, 2);
}

/**
 * Export progress report to JSON.
 */
export function exportProgressJSON(report: ProgressReport): string {
  return JSON.stringify(report, null, 2);
}
