/**
 * Input Validation and Sanitization
 * Security utilities for the Session Knowledge System
 * Epic: agentic-primer-0lg.2
 */

/**
 * Input length limits
 */
export const INPUT_LIMITS = {
  MAX_TEXT_LENGTH: 10_000,      // 10KB for general text
  MAX_QUERY_LENGTH: 1_000,      // 1KB for search queries
  MAX_SESSION_ID_LENGTH: 100,   // Reasonable UUID length + buffer
  MAX_FILE_PATH_LENGTH: 1_000,  // Max path length
  MAX_CATEGORY_LENGTH: 100,     // Category names
  MAX_ERROR_TYPE_LENGTH: 200,   // Error type strings
} as const;

/**
 * UUID validation regex (RFC 4122 compliant)
 */
const UUID_REGEX = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

/**
 * Validate string length
 */
export function validateLength(
  input: string,
  maxLength: number,
  fieldName: string
): string {
  if (input.length > maxLength) {
    throw new Error(
      `${fieldName} exceeds maximum length of ${maxLength} characters (got ${input.length})`
    );
  }
  return input;
}

/**
 * Validate UUID format
 * Allows special keywords: 'all', 'today', 'yesterday', 'manual'
 */
export function validateSessionId(id: string): string {
  // Allow special keywords
  const allowedKeywords = ['all', 'today', 'yesterday', 'manual'];
  if (allowedKeywords.includes(id.toLowerCase())) {
    return id;
  }

  // Validate UUID format
  if (!UUID_REGEX.test(id)) {
    throw new Error(`Invalid session ID format: ${id.slice(0, 50)}...`);
  }

  return id;
}

/**
 * Sanitize LIKE pattern for SQL queries
 * Escapes special characters: %, _, \
 */
export function sanitizeLikePattern(input: string): string {
  return input.replace(/[%_\\]/g, '\\$&');
}

/**
 * Validate and sanitize file path
 * Prevents directory traversal attacks and symlink escape
 */
export function validateFilePath(path: string, allowedBasePath: string): string {
  const { resolve, normalize, relative, dirname, isAbsolute } = require('path');
  const { lstatSync, readlinkSync } = require('fs');

  // Normalize to remove . and .. segments
  const normalized = normalize(path);

  // Resolve to absolute path
  const resolved = resolve(normalized);
  const base = resolve(allowedBasePath);

  // Check for symlinks
  try {
    const stats = lstatSync(resolved);
    if (stats.isSymbolicLink()) {
      const target = readlinkSync(resolved);
      const targetResolved = resolve(dirname(resolved), target);
      const targetRel = relative(base, targetResolved);
      // Symlink target must be within base (not start with .. and not escape via absolute path)
      if (targetRel.startsWith('..') || targetRel === '..' || resolve(base, targetRel) !== targetResolved) {
        throw new Error(`Invalid file path: symlink points outside allowed directory in ${path.slice(0, 50)}...`);
      }
    }
  } catch (err: any) {
    if (err.code !== 'ENOENT') throw err; // Allow non-existent paths
  }

  // Check if resolved path is within allowed base
  const rel = relative(base, resolved);
  if (rel.startsWith('..') || rel === '..' || resolve(base, rel) !== resolved) {
    throw new Error(`Invalid file path: directory traversal detected in ${path.slice(0, 50)}...`);
  }

  return resolved;
}

/**
 * Validate project directory name
 * Ensures safe directory name without special characters
 */
export function validateProjectName(cwd: string): string {
  const { normalize } = require('path');

  // Check for empty input
  if (!cwd || cwd.length === 0) {
    throw new Error('Invalid project path: empty project name');
  }

  // Normalize path
  const normalized = normalize(cwd);

  // Convert to safe directory name
  // Replace / with - and . with - to avoid relative paths
  // Prevent leading dashes or multiple dashes
  const projectName = normalized
    .replace(/\//g, '-')
    .replace(/\./g, '-')
    .replace(/^-+/, '')
    .replace(/-+/g, '-')
    .replace(/-+$/, ''); // Remove trailing dashes too

  // Validate resulting name
  if (projectName.length === 0) {
    throw new Error('Invalid project path: empty project name');
  }

  // Prevent names that could cause issues (after conversion)
  const forbidden = ['con', 'prn', 'aux', 'nul', 'com1', 'lpt1'];
  if (forbidden.includes(projectName.toLowerCase())) {
    throw new Error(`Invalid project name: ${projectName} is a reserved name`);
  }

  // Check for dangerous characters (should be cleaned by now, but double check)
  if (/[<>:"|?*\x00-\x1f.]/g.test(projectName)) {
    throw new Error(`Invalid project name: contains forbidden characters`);
  }

  return projectName;
}

/**
 * Validate integer input
 */
export function validateInteger(
  input: string | number,
  min: number,
  max: number,
  fieldName: string
): number {
  const num = typeof input === 'string' ? parseInt(input, 10) : input;

  if (isNaN(num)) {
    throw new Error(`${fieldName} must be a valid integer`);
  }

  // Check if the parsed value is actually an integer (no decimal parts)
  if (typeof input === 'string' && input.includes('.')) {
    throw new Error(`${fieldName} must be a valid integer`);
  }

  if (num < min || num > max) {
    throw new Error(`${fieldName} must be between ${min} and ${max} (got ${num})`);
  }

  return num;
}

/**
 * Validate date string format (YYYY-MM-DD)
 */
export function validateDateString(dateStr: string): Date {
  // Remove quotes if present
  const cleaned = dateStr.replace(/['"]/g, '');

  // Validate format
  if (!/^\d{4}-\d{2}-\d{2}$/.test(cleaned)) {
    throw new Error(`Invalid date format: ${cleaned}. Use YYYY-MM-DD`);
  }

  // Parse and validate that the date components are valid
  const [year, month, day] = cleaned.split('-').map(Number);

  // Validate ranges
  if (month < 1 || month > 12) {
    throw new Error(`Invalid date: ${cleaned} (invalid month)`);
  }

  // Days per month (non-leap year, February will be checked separately)
  const daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

  // Check for leap year
  const isLeapYear = (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
  const maxDay = month === 2 && isLeapYear ? 29 : daysInMonth[month - 1];

  if (day < 1 || day > maxDay) {
    throw new Error(`Invalid date: ${cleaned} (invalid day for month)`);
  }

  // Create date and verify it matches input (catches issues like Feb 30)
  const date = new Date(cleaned);
  if (isNaN(date.getTime())) {
    throw new Error(`Invalid date: ${cleaned}`);
  }

  // Verify the parsed date matches the input (prevents things like month overflow)
  const reconstructed = `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, '0')}-${String(date.getDate()).padStart(2, '0')}`;
  if (reconstructed !== cleaned) {
    throw new Error(`Invalid date: ${cleaned}`);
  }

  return date;
}

/**
 * Sanitize output for terminal display
 * Escapes ANSI control codes and special characters
 */
export function sanitizeOutput(text: string): string {
  return text
    .replace(/\x1b/g, '\\x1b')    // ANSI escape sequences
    .replace(/\r/g, '\\r')         // Carriage return
    .replace(/\x00/g, '\\x00');    // Null bytes
}

/**
 * Sanitize error message to remove sensitive information
 */
export function sanitizeErrorMessage(error: unknown): string {
  if (error instanceof Error) {
    return error.message
      .replace(/\/[^\s]+\/(\.claude|home|Users|tmp)[^\s]*/g, '[PATH]')
      .replace(/\b[A-Z0-9]{20,}\b/g, '[KEY]')
      .replace(/\/\/[^@]+@/g, '//[CREDENTIALS]@')
      .replace(/password[=:]\S+/gi, 'password=[REDACTED]')
      .replace(/token[=:]\S+/gi, 'token=[REDACTED]');
  }
  return 'An unexpected error occurred';
}

/**
 * Sanitize stack trace to remove sensitive file paths
 * Redacts paths containing sensitive directories like .claude, home, Users, tmp, Applications
 */
export function sanitizeStackTrace(stack: string | undefined): string {
  if (!stack) return '';
  return stack
    .split('\n')
    .map(line => {
      // Match absolute paths containing sensitive directory names
      // Pattern: /path/to/(sensitive-dir)/rest but preserve line numbers (:10:15)
      return line.replace(/\/(?:[^\s\/]*\/)*(\.claude|home|Users|tmp|Applications)[^\s):)]*/g, '[PATH]');
    })
    .join('\n');
}

/**
 * Validate category type
 */
export function validateCategory(category: string): string {
  const validCategories = ['decision', 'learning', 'error', 'workflow'];
  if (!validCategories.includes(category)) {
    throw new Error(
      `Invalid category: ${category}. Must be one of: ${validCategories.join(', ')}`
    );
  }
  return category;
}

/**
 * Validate workflow type
 */
export function validateWorkflowType(type: string): string {
  const validTypes = ['delegation', 'organization', 'planning', 'collaboration', 'tooling'];
  if (!validTypes.includes(type)) {
    throw new Error(
      `Invalid workflow type: ${type}. Must be one of: ${validTypes.join(', ')}`
    );
  }
  return type;
}

/**
 * Validate error type
 */
export function validateErrorType(type: string): string {
  const validTypes = [
    'TypeError',
    'NetworkError',
    'ImportError',
    'DatabaseError',
    'ValidationError',
    'AuthError',
    'RateLimitError',
    'TimeoutError'
  ];

  // Allow custom error types but validate length
  validateLength(type, INPUT_LIMITS.MAX_ERROR_TYPE_LENGTH, 'Error type');

  return type;
}
