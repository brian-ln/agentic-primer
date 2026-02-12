#!/usr/bin/env bun
/**
 * Migration Analyzer - Codebase Analysis for Flat ID Usage
 *
 * Scans TypeScript/JavaScript codebases to identify flat ID usage patterns
 * and categorize migration complexity.
 *
 * Phase 6: Migration Tooling & Automation
 *
 * @see docs/MIGRATION_TOOLING.md
 */

import { readdir, readFile } from 'node:fs/promises';
import { join, relative } from 'node:path';

/**
 * Flat ID usage pattern detected in code.
 */
export interface FlatIdUsage {
  /** File path where usage found */
  file: string;

  /** Line number */
  line: number;

  /** Column number */
  column: number;

  /** Flat ID value */
  flatId: string;

  /** Usage type */
  type: 'address-call' | 'string-literal' | 'actor-registration' | 'alias-registration';

  /** Code context (surrounding lines) */
  context: string;

  /** Migration complexity */
  complexity: 'simple' | 'moderate' | 'complex';

  /** Migration recommendation */
  recommendation: string;
}

/**
 * Actor registration found in code.
 */
export interface ActorRegistration {
  /** File path */
  file: string;

  /** Line number */
  line: number;

  /** Actor ID (flat or path) */
  actorId: string;

  /** Is hierarchical path? */
  isHierarchical: boolean;

  /** Registration type */
  type: 'router.registerActor' | 'supervisor.addChild';

  /** Code context */
  context: string;
}

/**
 * Analysis results for entire codebase.
 */
export interface CodebaseAnalysis {
  /** Timestamp of analysis */
  timestamp: Date;

  /** Total files scanned */
  filesScanned: number;

  /** Flat ID usages found */
  flatIdUsages: FlatIdUsage[];

  /** Actor registrations found */
  actorRegistrations: ActorRegistration[];

  /** Summary statistics */
  stats: {
    totalFlatIds: number;
    totalActors: number;
    hierarchicalActors: number;
    flatActors: number;
    migrationProgress: number; // Percentage
  };

  /** Categorized by complexity */
  byComplexity: {
    simple: FlatIdUsage[];
    moderate: FlatIdUsage[];
    complex: FlatIdUsage[];
  };

  /** Categorized by file */
  byFile: Map<string, FlatIdUsage[]>;
}

/**
 * Analyzer configuration.
 */
export interface AnalyzerConfig {
  /** Root directory to scan */
  rootDir: string;

  /** File patterns to include (glob) */
  include?: string[];

  /** File patterns to exclude (glob) */
  exclude?: string[];

  /** Context lines before/after each usage */
  contextLines?: number;

  /** Enable verbose output */
  verbose?: boolean;
}

/**
 * Migration Analyzer - Scan codebase for flat ID usage.
 */
export class MigrationAnalyzer {
  private config: Required<AnalyzerConfig>;

  constructor(config: AnalyzerConfig) {
    this.config = {
      rootDir: config.rootDir,
      include: config.include || ['**/*.ts', '**/*.js'],
      exclude: config.exclude || [
        '**/node_modules/**',
        '**/dist/**',
        '**/*.test.ts',
        '**/*.bench.ts',
      ],
      contextLines: config.contextLines ?? 2,
      verbose: config.verbose ?? false,
    };
  }

  /**
   * Analyze codebase for flat ID usage.
   *
   * @returns Complete analysis results
   */
  async analyze(): Promise<CodebaseAnalysis> {
    const flatIdUsages: FlatIdUsage[] = [];
    const actorRegistrations: ActorRegistration[] = [];
    let filesScanned = 0;

    // Find all TypeScript/JavaScript files
    const files = await this.findSourceFiles(this.config.rootDir);

    for (const file of files) {
      if (this.config.verbose) {
        console.log(`Scanning: ${relative(this.config.rootDir, file)}`);
      }

      const content = await readFile(file, 'utf-8');
      const lines = content.split('\n');

      // Scan for flat ID patterns
      const fileUsages = this.scanFile(file, lines);
      flatIdUsages.push(...fileUsages);

      // Scan for actor registrations
      const fileRegistrations = this.scanActorRegistrations(file, lines);
      actorRegistrations.push(...fileRegistrations);

      filesScanned++;
    }

    // Calculate statistics
    const stats = this.calculateStats(flatIdUsages, actorRegistrations);

    // Categorize by complexity
    const byComplexity = {
      simple: flatIdUsages.filter(u => u.complexity === 'simple'),
      moderate: flatIdUsages.filter(u => u.complexity === 'moderate'),
      complex: flatIdUsages.filter(u => u.complexity === 'complex'),
    };

    // Categorize by file
    const byFile = new Map<string, FlatIdUsage[]>();
    for (const usage of flatIdUsages) {
      const fileUsages = byFile.get(usage.file) || [];
      fileUsages.push(usage);
      byFile.set(usage.file, fileUsages);
    }

    return {
      timestamp: new Date(),
      filesScanned,
      flatIdUsages,
      actorRegistrations,
      stats,
      byComplexity,
      byFile,
    };
  }

  /**
   * Find all source files to scan.
   */
  private async findSourceFiles(dir: string): Promise<string[]> {
    const files: string[] = [];

    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      // Check exclusions
      if (this.shouldExclude(fullPath)) {
        continue;
      }

      if (entry.isDirectory()) {
        // Recurse into subdirectories
        const subFiles = await this.findSourceFiles(fullPath);
        files.push(...subFiles);
      } else if (entry.isFile() && this.shouldInclude(entry.name)) {
        files.push(fullPath);
      }
    }

    return files;
  }

  /**
   * Scan file for flat ID usage patterns.
   */
  private scanFile(file: string, lines: string[]): FlatIdUsage[] {
    const usages: FlatIdUsage[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];

      // Pattern 1: address('test/flat-id') calls
      const addressMatches = line.matchAll(/address\s*\(\s*['"`]([^/'"`]+)['"`]\s*\)/g);
      for (const match of addressMatches) {
        const flatId = match[1];

        // Skip if it contains '/' (already a path)
        if (flatId.includes('/')) continue;

        usages.push({
          file,
          line: i + 1,
          column: match.index ?? 0,
          flatId,
          type: 'address-call',
          context: this.extractContext(lines, i),
          complexity: this.assessComplexity(line, 'address-call'),
          recommendation: this.generateRecommendation(flatId, 'address-call'),
        });
      }

      // Pattern 2: String literals that look like flat IDs in message routing
      // Skip this pattern to avoid false positives - focus on explicit address() calls
      // const stringLiteralMatches = line.matchAll(/['"`]([a-z0-9-_]+)['"`]/gi);
      // for (const match of stringLiteralMatches) {
      //   const value = match[1];
      //
      //   // Only flag if it looks like an actor ID and context suggests routing
      //   if (
      //     this.looksLikeActorId(value) &&
      //     this.isRoutingContext(line) &&
      //     !value.includes('/')
      //   ) {
      //     usages.push({
      //       file,
      //       line: i + 1,
      //       column: match.index ?? 0,
      //       flatId: value,
      //       type: 'string-literal',
      //       context: this.extractContext(lines, i),
      //       complexity: 'moderate',
      //       recommendation: this.generateRecommendation(value, 'string-literal'),
      //     });
      //   }
      // }

      // Pattern 3: router.registerActor('test/flat-id', ...)
      const registerMatches = line.matchAll(/\.registerActor\s*\(\s*['"`]([^/'"`]+)['"`]/g);
      for (const match of registerMatches) {
        const flatId = match[1];

        if (!flatId.includes('/')) {
          usages.push({
            file,
            line: i + 1,
            column: match.index ?? 0,
            flatId,
            type: 'actor-registration',
            context: this.extractContext(lines, i),
            complexity: 'complex',
            recommendation: this.generateRecommendation(flatId, 'actor-registration'),
          });
        }
      }

      // Pattern 4: registerAlias('flat-id', 'path')
      const aliasMatches = line.matchAll(/registerAlias\s*\(\s*['"`]([^/'"`]+)['"`]\s*,\s*['"`]([^'"`]+)['"`]/g);
      for (const match of aliasMatches) {
        const flatId = match[1];

        usages.push({
          file,
          line: i + 1,
          column: match.index ?? 0,
          flatId,
          type: 'alias-registration',
          context: this.extractContext(lines, i),
          complexity: 'simple',
          recommendation: `Alias already created for migration. Monitor usage and remove after transition.`,
        });
      }
    }

    return usages;
  }

  /**
   * Scan for actor registrations.
   */
  private scanActorRegistrations(file: string, lines: string[]): ActorRegistration[] {
    const registrations: ActorRegistration[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];

      // router.registerActor('test/id', actor)
      const registerMatches = line.matchAll(/\.registerActor\s*\(\s*['"`]([^'"`]+)['"`]/g);
      for (const match of registerMatches) {
        const actorId = match[1];
        registrations.push({
          file,
          line: i + 1,
          actorId,
          isHierarchical: actorId.includes('/'),
          type: 'router.registerActor',
          context: this.extractContext(lines, i),
        });
      }

      // supervisor.addChild('test/id', actor)
      const addChildMatches = line.matchAll(/\.addChild\s*\(\s*['"`]([^'"`]+)['"`]/g);
      for (const match of addChildMatches) {
        const actorId = match[1];
        registrations.push({
          file,
          line: i + 1,
          actorId,
          isHierarchical: actorId.includes('/'),
          type: 'supervisor.addChild',
          context: this.extractContext(lines, i),
        });
      }
    }

    return registrations;
  }

  /**
   * Extract context lines around target line.
   */
  private extractContext(lines: string[], lineIndex: number): string {
    const start = Math.max(0, lineIndex - this.config.contextLines);
    const end = Math.min(lines.length, lineIndex + this.config.contextLines + 1);

    return lines.slice(start, end).join('\n');
  }

  /**
   * Assess migration complexity.
   */
  private assessComplexity(line: string, type: FlatIdUsage['type']): FlatIdUsage['complexity'] {
    // Simple: Direct address() calls in test files
    if (type === 'address-call' && line.includes('address(')) {
      return 'simple';
    }

    // Complex: Actor registrations (structural changes)
    if (type === 'actor-registration') {
      return 'complex';
    }

    // Moderate: Everything else
    return 'moderate';
  }

  /**
   * Generate migration recommendation.
   */
  private generateRecommendation(flatId: string, type: FlatIdUsage['type']): string {
    switch (type) {
      case 'address-call':
        return `Replace address('domain/${flatId}') with address('appropriate/path/${flatId}')`;
      case 'string-literal':
        return `Review context and replace with hierarchical path if this is routing code`;
      case 'actor-registration':
        return `Update actor registration to use hierarchical path. Consider supervision tree structure.`;
      case 'alias-registration':
        return `Alias already registered. Monitor usage and remove after full migration.`;
    }
  }

  /**
   * Check if value looks like an actor ID.
   */
  private looksLikeActorId(value: string): boolean {
    // Actor IDs typically: lowercase, hyphens, maybe numbers
    return /^[a-z][a-z0-9-_]*$/.test(value) && value.length > 2;
  }

  /**
   * Check if line context suggests routing/messaging.
   */
  private isRoutingContext(line: string): boolean {
    const routingKeywords = [
      'router',
      'ask',
      'tell',
      'message',
      'send',
      'createMessage',
      'registerActor',
      'address',
    ];

    return routingKeywords.some(keyword => line.includes(keyword));
  }

  /**
   * Check if file should be excluded.
   */
  private shouldExclude(path: string): boolean {
    // Check if path contains any excluded pattern
    return this.config.exclude.some(pattern => {
      // Handle test files explicitly
      if (pattern.includes('.test.')) {
        return path.includes('.test.');
      }
      if (pattern.includes('.bench.')) {
        return path.includes('.bench.');
      }
      // Simple glob matching (check if path contains pattern)
      const cleanPattern = pattern.replace(/\*\*/g, '').replace(/\*/g, '');
      return path.includes(cleanPattern);
    });
  }

  /**
   * Check if file should be included.
   */
  private shouldInclude(filename: string): boolean {
    return this.config.include.some(pattern => {
      // Simple extension matching
      if (pattern.startsWith('**/*')) {
        const ext = pattern.slice(4); // Remove **/
        return filename.endsWith(ext);
      }
      return filename.endsWith(pattern);
    });
  }

  /**
   * Calculate statistics.
   */
  private calculateStats(
    usages: FlatIdUsage[],
    registrations: ActorRegistration[]
  ): CodebaseAnalysis['stats'] {
    const totalActors = registrations.length;
    const hierarchicalActors = registrations.filter(r => r.isHierarchical).length;
    const flatActors = totalActors - hierarchicalActors;
    const migrationProgress =
      totalActors > 0 ? (hierarchicalActors / totalActors) * 100 : 0;

    return {
      totalFlatIds: usages.length,
      totalActors,
      hierarchicalActors,
      flatActors,
      migrationProgress: Math.round(migrationProgress * 100) / 100,
    };
  }
}

/**
 * Format analysis results as human-readable report.
 */
export function formatAnalysisReport(analysis: CodebaseAnalysis): string {
  const lines: string[] = [];

  lines.push('═══════════════════════════════════════════════════════');
  lines.push('  Migration Analysis Report');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');
  lines.push(`Analysis Date: ${analysis.timestamp.toISOString()}`);
  lines.push(`Files Scanned: ${analysis.filesScanned}`);
  lines.push('');

  lines.push('STATISTICS');
  lines.push('───────────────────────────────────────────────────────');
  lines.push(`  Total Flat ID Usages:     ${analysis.stats.totalFlatIds}`);
  lines.push(`  Total Actors:             ${analysis.stats.totalActors}`);
  lines.push(`  Hierarchical Actors:      ${analysis.stats.hierarchicalActors}`);
  lines.push(`  Flat ID Actors:           ${analysis.stats.flatActors}`);
  lines.push(`  Migration Progress:       ${analysis.stats.migrationProgress.toFixed(2)}%`);
  lines.push('');

  lines.push('BY COMPLEXITY');
  lines.push('───────────────────────────────────────────────────────');
  lines.push(`  Simple:     ${analysis.byComplexity.simple.length} (quick fixes)`);
  lines.push(`  Moderate:   ${analysis.byComplexity.moderate.length} (requires review)`);
  lines.push(`  Complex:    ${analysis.byComplexity.complex.length} (structural changes)`);
  lines.push('');

  if (analysis.byComplexity.simple.length > 0) {
    lines.push('SIMPLE MIGRATIONS (Top 5)');
    lines.push('───────────────────────────────────────────────────────');
    for (const usage of analysis.byComplexity.simple.slice(0, 5)) {
      lines.push(`  📄 ${relative(process.cwd(), usage.file)}:${usage.line}`);
      lines.push(`     Flat ID: "${usage.flatId}"`);
      lines.push(`     Action:  ${usage.recommendation}`);
      lines.push('');
    }
  }

  if (analysis.byComplexity.complex.length > 0) {
    lines.push('COMPLEX MIGRATIONS (Requires Planning)');
    lines.push('───────────────────────────────────────────────────────');
    for (const usage of analysis.byComplexity.complex) {
      lines.push(`  📄 ${relative(process.cwd(), usage.file)}:${usage.line}`);
      lines.push(`     Flat ID: "${usage.flatId}"`);
      lines.push(`     Type:    ${usage.type}`);
      lines.push(`     Action:  ${usage.recommendation}`);
      lines.push('');
    }
  }

  lines.push('NEXT STEPS');
  lines.push('───────────────────────────────────────────────────────');
  lines.push('  1. Review complex migrations and plan hierarchy');
  lines.push('  2. Run migration planner: npm run migrate:plan');
  lines.push('  3. Execute migrations: npm run migrate:execute');
  lines.push('  4. Verify: npm run migrate:verify');
  lines.push('');

  return lines.join('\n');
}
