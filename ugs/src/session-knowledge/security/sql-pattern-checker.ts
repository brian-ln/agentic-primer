#!/usr/bin/env bun
/**
 * SQL Injection Pattern Checker
 * Static analysis tool to detect unsafe SQL patterns in TypeScript code
 * Epic: agentic-primer-0lg.2
 *
 * This tool scans TypeScript files for common SQL injection vulnerabilities:
 * - Raw string interpolation in SQL queries
 * - LIKE queries without sanitization
 * - LIKE queries without ESCAPE clauses
 * - Unsafe parameter concatenation
 */

import { readFileSync, readdirSync, statSync } from 'fs';
import { join, relative } from 'path';

interface Finding {
  file: string;
  line: number;
  column: number;
  severity: 'error' | 'warning' | 'info';
  message: string;
  snippet: string;
  fix?: string;
}

interface CheckResult {
  findings: Finding[];
  filesScanned: number;
  errors: number;
  warnings: number;
}

/**
 * Patterns that indicate SQL injection vulnerabilities
 */
const UNSAFE_PATTERNS = [
  {
    name: 'string-interpolation-in-sql',
    pattern: /(?:query|execute|prepare|sql|db\.query|db\.execute)\s*\(\s*`[^`]*\$\{[^}]+\}[^`]*LIKE/gi,
    severity: 'error' as const,
    message: 'SQL LIKE query uses template literal interpolation - use sanitizeLikePattern() instead',
    fix: 'Import sanitizeLikePattern from security/input-validation and sanitize the input before using it in the query',
  },
  {
    name: 'like-without-escape',
    pattern: /LIKE\s+['"`?\w${}[\]]+(?!\s+ESCAPE)/gi,
    severity: 'error' as const,
    message: 'LIKE query missing ESCAPE clause - add ESCAPE \'\\\\\' to prevent wildcard injection',
    fix: "Add ESCAPE '\\\\' after the LIKE pattern parameter",
  },
  {
    name: 'raw-like-concatenation',
    pattern: /LIKE\s+['"]%?\s*\+\s*[a-zA-Z_$][\w$]*\s*\+\s*%?['"]/gi,
    severity: 'error' as const,
    message: 'LIKE pattern uses string concatenation - use sanitizeLikePattern() instead',
    fix: 'Use sanitizeLikePattern() to escape wildcards before building the pattern',
  },
  {
    name: 'template-literal-in-query',
    pattern: /(?:query|execute|prepare|sql)\s*\(\s*`[^`]*\$\{(?!sanitize)[^}]*\}[^`]*`/gi,
    severity: 'warning' as const,
    message: 'Template literal with interpolation in SQL query - ensure input is sanitized or use parameterized queries',
    fix: 'Use parameterized queries with ? placeholders or sanitize input with appropriate validation functions',
  },
  {
    name: 'direct-user-input-in-where',
    pattern: /WHERE\s+\w+\s*=\s*['"]?\$\{[^}]*(?:input|param|arg|value)[^}]*\}['"]?/gi,
    severity: 'error' as const,
    message: 'Direct user input in WHERE clause - use parameterized queries with ? placeholder',
    fix: 'Replace ${userInput} with ? placeholder and pass the value as a parameter',
  },
];

/**
 * Good patterns that indicate proper security practices
 */
const SAFE_PATTERNS = [
  /sanitizeLikePattern\s*\(/,
  /ESCAPE\s+['"]/,
  /validateSessionId\s*\(/,
  /validateLength\s*\(/,
  /withRateLimit\s*\(/,
];

/**
 * Check if a line contains safe patterns that might exempt it from warnings
 */
function hasSafePattern(line: string): boolean {
  return SAFE_PATTERNS.some(pattern => pattern.test(line));
}

/**
 * Get context around a line (3 lines before and after)
 */
function getContextSnippet(lines: string[], lineIndex: number): string {
  const start = Math.max(0, lineIndex - 1);
  const end = Math.min(lines.length, lineIndex + 2);
  return lines.slice(start, end).join('\n');
}

/**
 * Check a single file for SQL injection patterns
 */
function checkFile(filePath: string, baseDir: string): Finding[] {
  const content = readFileSync(filePath, 'utf-8');
  const lines = content.split('\n');
  const findings: Finding[] = [];
  const relativePath = relative(baseDir, filePath);

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineNumber = i + 1;

    // Skip comments and test files
    if (line.trim().startsWith('//') || line.trim().startsWith('*')) {
      continue;
    }

    // Check for each unsafe pattern
    for (const { name, pattern, severity, message, fix } of UNSAFE_PATTERNS) {
      // Reset regex state
      pattern.lastIndex = 0;
      const match = pattern.exec(line);

      if (match) {
        // Check if this line has safe patterns that might exempt it
        if (hasSafePattern(line)) {
          // Still warn but with lower severity
          findings.push({
            file: relativePath,
            line: lineNumber,
            column: match.index + 1,
            severity: 'info',
            message: `${message} (appears to have sanitization - verify manually)`,
            snippet: getContextSnippet(lines, i),
            fix,
          });
        } else {
          findings.push({
            file: relativePath,
            line: lineNumber,
            column: match.index + 1,
            severity,
            message,
            snippet: getContextSnippet(lines, i),
            fix,
          });
        }
      }
    }
  }

  return findings;
}

/**
 * Recursively find all TypeScript files in a directory
 */
function findTypeScriptFiles(dir: string, files: string[] = []): string[] {
  const entries = readdirSync(dir);

  for (const entry of entries) {
    const fullPath = join(dir, entry);
    const stat = statSync(fullPath);

    if (stat.isDirectory()) {
      // Skip node_modules, .git, dist
      if (!['node_modules', '.git', 'dist', 'coverage', '.claude'].includes(entry)) {
        findTypeScriptFiles(fullPath, files);
      }
    } else if (stat.isFile() && /\.tsx?$/.test(entry)) {
      files.push(fullPath);
    }
  }

  return files;
}

/**
 * Check all TypeScript files in a directory
 */
export function checkDirectory(dir: string): CheckResult {
  const files = findTypeScriptFiles(dir);
  const allFindings: Finding[] = [];

  for (const file of files) {
    const findings = checkFile(file, dir);
    allFindings.push(...findings);
  }

  const errors = allFindings.filter(f => f.severity === 'error').length;
  const warnings = allFindings.filter(f => f.severity === 'warning').length;

  return {
    findings: allFindings,
    filesScanned: files.length,
    errors,
    warnings,
  };
}

/**
 * Format findings for console output
 */
function formatFindings(result: CheckResult): string {
  const { findings, filesScanned, errors, warnings } = result;

  if (findings.length === 0) {
    return `✅ No SQL injection vulnerabilities found (scanned ${filesScanned} files)`;
  }

  let output = '';

  // Group findings by file
  const byFile = new Map<string, Finding[]>();
  for (const finding of findings) {
    if (!byFile.has(finding.file)) {
      byFile.set(finding.file, []);
    }
    byFile.get(finding.file)!.push(finding);
  }

  // Output findings by file
  for (const [file, fileFindings] of byFile) {
    output += `\n📄 ${file}\n`;

    for (const finding of fileFindings) {
      const icon = finding.severity === 'error' ? '❌' : finding.severity === 'warning' ? '⚠️' : 'ℹ️';
      output += `  ${icon} Line ${finding.line}:${finding.column} - ${finding.message}\n`;

      if (finding.snippet) {
        output += `     ${finding.snippet.split('\n').join('\n     ')}\n`;
      }

      if (finding.fix) {
        output += `     💡 Fix: ${finding.fix}\n`;
      }
      output += '\n';
    }
  }

  // Summary
  output += `\n📊 Summary:\n`;
  output += `   Files scanned: ${filesScanned}\n`;
  output += `   Errors: ${errors}\n`;
  output += `   Warnings: ${warnings}\n`;
  output += `   Total issues: ${findings.length}\n`;

  return output;
}

/**
 * CLI entry point
 */
if (import.meta.main) {
  const args = process.argv.slice(2);
  const dir = args[0] || process.cwd();

  console.log(`🔍 Scanning for SQL injection vulnerabilities in: ${dir}\n`);

  const result = checkDirectory(dir);
  console.log(formatFindings(result));

  // Exit with error code if errors found
  if (result.errors > 0) {
    process.exit(1);
  }
}

export { Finding, CheckResult };
