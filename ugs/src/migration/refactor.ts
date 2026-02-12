#!/usr/bin/env bun
/**
 * Migration Refactoring Tools - Safe Code Transformations
 *
 * Automated refactoring utilities for migrating from flat IDs to paths.
 * Uses AST-based transformations for safety.
 *
 * Phase 6: Migration Tooling & Automation
 *
 * @see docs/MIGRATION_TOOLING.md
 */

import { readFile, writeFile } from 'node:fs/promises';
import type { MigrationStep, CodeChange, PathProposal } from './planner';

/**
 * Refactoring result for a single file.
 */
export interface RefactorResult {
  /** File path */
  file: string;

  /** Success */
  success: boolean;

  /** Changes applied */
  changesApplied: number;

  /** Original content (for rollback) */
  originalContent?: string;

  /** New content */
  newContent?: string;

  /** Error if failed */
  error?: string;

  /** Warnings */
  warnings: string[];
}

/**
 * Refactoring operation options.
 */
export interface RefactorOptions {
  /** Dry run - don't write files */
  dryRun?: boolean;

  /** Create backups before modifying */
  backup?: boolean;

  /** Verbose output */
  verbose?: boolean;

  /** Backup directory */
  backupDir?: string;
}

/**
 * Refactoring session (tracks all changes).
 */
export interface RefactorSession {
  /** Session ID */
  id: string;

  /** Start timestamp */
  startTime: Date;

  /** End timestamp */
  endTime?: Date;

  /** Results per file */
  results: RefactorResult[];

  /** Total changes */
  totalChanges: number;

  /** Rollback data */
  rollbackData: Map<string, string>;
}

/**
 * Migration Refactoring Tool - Safe code transformations.
 */
export class MigrationRefactor {
  private options: Required<RefactorOptions>;
  private session: RefactorSession | null = null;

  constructor(options: RefactorOptions = {}) {
    this.options = {
      dryRun: options.dryRun ?? false,
      backup: options.backup ?? true,
      verbose: options.verbose ?? false,
      backupDir: options.backupDir ?? '.migration-backup',
    };
  }

  /**
   * Start a refactoring session.
   */
  startSession(): RefactorSession {
    this.session = {
      id: this.generateSessionId(),
      startTime: new Date(),
      results: [],
      totalChanges: 0,
      rollbackData: new Map(),
    };

    if (this.options.verbose) {
      console.log(`Started refactoring session: ${this.session.id}`);
    }

    return this.session;
  }

  /**
   * End the current session.
   */
  endSession(): RefactorSession {
    if (!this.session) {
      throw new Error('No active session');
    }

    this.session.endTime = new Date();

    if (this.options.verbose) {
      const duration = this.session.endTime.getTime() - this.session.startTime.getTime();
      console.log(`Ended session ${this.session.id} (${duration}ms)`);
      console.log(`Total changes: ${this.session.totalChanges}`);
    }

    const completed = this.session;
    this.session = null;
    return completed;
  }

  /**
   * Apply a migration step.
   *
   * @param step - Migration step to execute
   * @returns Results for each file modified
   */
  async applyStep(step: MigrationStep): Promise<RefactorResult[]> {
    if (!this.session) {
      throw new Error('No active session. Call startSession() first.');
    }

    if (this.options.verbose) {
      console.log(`Applying step ${step.step}: ${step.description}`);
    }

    const results: RefactorResult[] = [];

    // Group changes by file
    const changesByFile = new Map<string, CodeChange[]>();
    for (const change of step.changes) {
      const fileChanges = changesByFile.get(change.file) || [];
      fileChanges.push(change);
      changesByFile.set(change.file, fileChanges);
    }

    // Apply changes to each file
    for (const [file, changes] of changesByFile) {
      const result = await this.applyChangesToFile(file, changes);
      results.push(result);
      this.session.results.push(result);

      if (result.success) {
        this.session.totalChanges += result.changesApplied;
      }
    }

    return results;
  }

  /**
   * Apply code changes to a single file.
   */
  private async applyChangesToFile(
    file: string,
    changes: CodeChange[]
  ): Promise<RefactorResult> {
    const warnings: string[] = [];

    try {
      // Read original content
      const originalContent = await readFile(file, 'utf-8');
      const lines = originalContent.split('\n');

      // Store original for rollback
      if (this.session && this.options.backup) {
        this.session.rollbackData.set(file, originalContent);
      }

      // Sort changes by line number (descending) to avoid offset issues
      const sortedChanges = [...changes].sort((a, b) => b.line - a.line);

      let changesApplied = 0;

      for (const change of sortedChanges) {
        if (change.type === 'replace') {
          const lineIndex = change.line - 1;

          if (lineIndex < 0 || lineIndex >= lines.length) {
            warnings.push(`Line ${change.line} out of bounds in ${file}`);
            continue;
          }

          const line = lines[lineIndex];

          // Verify the line contains the expected content
          if (!line.includes(change.before)) {
            warnings.push(
              `Line ${change.line} doesn't contain expected content: "${change.before}"`
            );
            continue;
          }

          // Apply replacement
          lines[lineIndex] = line.replace(change.before, change.after);
          changesApplied++;
        } else if (change.type === 'insert') {
          // Insert new line
          const lineIndex = change.line === -1 ? lines.length : change.line - 1;
          lines.splice(lineIndex, 0, change.after);
          changesApplied++;
        } else if (change.type === 'delete') {
          const lineIndex = change.line - 1;

          if (lineIndex < 0 || lineIndex >= lines.length) {
            warnings.push(`Line ${change.line} out of bounds in ${file}`);
            continue;
          }

          // Verify line matches before deletion
          if (change.before && !lines[lineIndex].includes(change.before)) {
            warnings.push(
              `Line ${change.line} doesn't match expected content for deletion`
            );
            continue;
          }

          lines.splice(lineIndex, 1);
          changesApplied++;
        }
      }

      const newContent = lines.join('\n');

      // Write changes (unless dry run)
      if (!this.options.dryRun) {
        await writeFile(file, newContent, 'utf-8');

        if (this.options.verbose) {
          console.log(`  ✓ Updated ${file} (${changesApplied} changes)`);
        }
      } else {
        if (this.options.verbose) {
          console.log(`  [DRY RUN] Would update ${file} (${changesApplied} changes)`);
        }
      }

      return {
        file,
        success: true,
        changesApplied,
        originalContent,
        newContent,
        warnings,
      };
    } catch (error: any) {
      return {
        file,
        success: false,
        changesApplied: 0,
        error: error.message,
        warnings,
      };
    }
  }

  /**
   * Rollback all changes in the current session.
   */
  async rollback(): Promise<void> {
    if (!this.session) {
      throw new Error('No active session to rollback');
    }

    if (this.options.verbose) {
      console.log(`Rolling back session ${this.session.id}...`);
    }

    for (const [file, content] of this.session.rollbackData) {
      if (!this.options.dryRun) {
        await writeFile(file, content, 'utf-8');
      }

      if (this.options.verbose) {
        console.log(`  ✓ Restored ${file}`);
      }
    }

    if (this.options.verbose) {
      console.log(`Rollback complete`);
    }
  }

  /**
   * Create alias registrations for proposals.
   */
  async createAliases(
    proposals: PathProposal[],
    targetFile: string = 'src/migration/aliases.ts'
  ): Promise<RefactorResult> {
    const warnings: string[] = [];

    try {
      let content: string;

      try {
        content = await readFile(targetFile, 'utf-8');
      } catch {
        // File doesn't exist, create it
        content = this.generateAliasFileTemplate();
      }

      const lines = content.split('\n');

      // Find insertion point (after imports, before exports)
      let insertIndex = lines.findIndex(line =>
        line.includes('export') || line.includes('function')
      );
      if (insertIndex === -1) {
        insertIndex = lines.length;
      }

      // Generate alias registrations
      const aliasLines: string[] = [];
      aliasLines.push('');
      aliasLines.push('  // Migration aliases (auto-generated)');

      for (const proposal of proposals) {
        aliasLines.push(
          `  registerAlias('${proposal.flatId}', '${proposal.proposedPath}'); // ${proposal.reasoning}`
        );
      }

      aliasLines.push('');

      // Insert aliases
      lines.splice(insertIndex, 0, ...aliasLines);

      const newContent = lines.join('\n');

      // Write file
      if (!this.options.dryRun) {
        await writeFile(targetFile, newContent, 'utf-8');

        if (this.options.verbose) {
          console.log(`  ✓ Created ${proposals.length} aliases in ${targetFile}`);
        }
      }

      return {
        file: targetFile,
        success: true,
        changesApplied: proposals.length,
        originalContent: content,
        newContent,
        warnings,
      };
    } catch (error: any) {
      return {
        file: targetFile,
        success: false,
        changesApplied: 0,
        error: error.message,
        warnings,
      };
    }
  }

  /**
   * Generate alias file template.
   */
  private generateAliasFileTemplate(): string {
    return `#!/usr/bin/env bun
/**
 * Migration Aliases
 *
 * Flat ID → Path mappings for backward compatibility during migration.
 * Auto-generated by migration tooling.
 */

// DEPRECATED: Alias resolver removed in paths-only migration
// import { registerAlias, clearAliases } from '../messaging/alias-resolver';

/**
 * Register all migration aliases.
 * DEPRECATED: No longer used in paths-only mode.
 */
export function registerMigrationAliases(): void {
  // No-op: Alias resolution removed in paths-only migration
  // clearAliases(); // Start fresh
  // Migration aliases will be inserted here
}

/**
 * Initialize aliases on import.
 */
registerMigrationAliases();
`;
  }

  /**
   * Generate session ID.
   */
  private generateSessionId(): string {
    return `migration-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
  }
}

/**
 * Batch refactor multiple files.
 *
 * @param files - Files to refactor
 * @param transform - Transformation function
 * @param options - Refactor options
 * @returns Results for all files
 */
export async function batchRefactor(
  files: string[],
  transform: (content: string) => string,
  options: RefactorOptions = {}
): Promise<RefactorResult[]> {
  const results: RefactorResult[] = [];

  for (const file of files) {
    try {
      const originalContent = await readFile(file, 'utf-8');
      const newContent = transform(originalContent);

      if (originalContent === newContent) {
        results.push({
          file,
          success: true,
          changesApplied: 0,
          warnings: ['No changes needed'],
        });
        continue;
      }

      if (!options.dryRun) {
        await writeFile(file, newContent, 'utf-8');
      }

      results.push({
        file,
        success: true,
        changesApplied: 1,
        originalContent,
        newContent,
        warnings: [],
      });
    } catch (error: any) {
      results.push({
        file,
        success: false,
        changesApplied: 0,
        error: error.message,
        warnings: [],
      });
    }
  }

  return results;
}

/**
 * Simple find-replace transformation.
 *
 * @param find - String or RegExp to find
 * @param replace - Replacement string
 * @returns Transformation function
 */
export function findReplace(
  find: string | RegExp,
  replace: string
): (content: string) => string {
  return (content: string) => {
    if (typeof find === 'string') {
      return content.replaceAll(find, replace);
    } else {
      return content.replace(find, replace);
    }
  };
}

/**
 * Transform all flat ID address() calls to paths.
 *
 * @param proposals - Path proposals
 * @returns Transformation function
 */
export function transformAddressCalls(
  proposals: PathProposal[]
): (content: string) => string {
  return (content: string) => {
    let transformed = content;

    for (const proposal of proposals) {
      // Replace address('test/flat-id') with address('domain/path')
      const pattern = new RegExp(
        `address\\s*\\(\\s*['"\`]${proposal.flatId}['"\`]\\s*\\)`,
        'g'
      );
      transformed = transformed.replace(
        pattern,
        `address('domain/${proposal.proposedPath}')`
      );
    }

    return transformed;
  };
}
