/**
 * Input Validation and Sanitization
 * Portable security utilities -- no runtime-specific dependencies.
 */

export const INPUT_LIMITS = {
  MAX_TEXT_LENGTH: 10_000,
  MAX_QUERY_LENGTH: 1_000,
  MAX_SESSION_ID_LENGTH: 100,
  MAX_FILE_PATH_LENGTH: 1_000,
  MAX_CATEGORY_LENGTH: 100,
  MAX_ERROR_TYPE_LENGTH: 200,
} as const;

const UUID_REGEX = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

export function validateLength(input: string, maxLength: number, fieldName: string): string {
  if (input.length > maxLength) {
    throw new Error(`${fieldName} exceeds maximum length of ${maxLength} characters (got ${input.length})`);
  }
  return input;
}

export function validateSessionId(id: string): string {
  const allowedKeywords = ['all', 'today', 'yesterday', 'manual'];
  if (allowedKeywords.includes(id.toLowerCase())) return id;
  if (!UUID_REGEX.test(id)) {
    throw new Error(`Invalid session ID format: ${id.slice(0, 50)}...`);
  }
  return id;
}

export function sanitizeLikePattern(input: string): string {
  return input.replace(/[%_\\]/g, '\\$&');
}

export function validateInteger(input: string | number, min: number, max: number, fieldName: string): number {
  const num = typeof input === 'string' ? parseInt(input, 10) : input;
  if (isNaN(num)) throw new Error(`${fieldName} must be a valid integer`);
  if (typeof input === 'string' && input.includes('.')) throw new Error(`${fieldName} must be a valid integer`);
  if (num < min || num > max) throw new Error(`${fieldName} must be between ${min} and ${max} (got ${num})`);
  return num;
}

export function validateDateString(dateStr: string): Date {
  const cleaned = dateStr.replace(/['"]/g, '');
  if (!/^\d{4}-\d{2}-\d{2}$/.test(cleaned)) {
    throw new Error(`Invalid date format: ${cleaned}. Use YYYY-MM-DD`);
  }

  const [year, month, day] = cleaned.split('-').map(Number);
  if (month < 1 || month > 12) throw new Error(`Invalid date: ${cleaned} (invalid month)`);

  const daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  const isLeapYear = (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
  const maxDay = month === 2 && isLeapYear ? 29 : daysInMonth[month - 1];

  if (day < 1 || day > maxDay) throw new Error(`Invalid date: ${cleaned} (invalid day for month)`);

  const date = new Date(cleaned);
  if (isNaN(date.getTime())) throw new Error(`Invalid date: ${cleaned}`);

  const reconstructed = `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, '0')}-${String(date.getDate()).padStart(2, '0')}`;
  if (reconstructed !== cleaned) throw new Error(`Invalid date: ${cleaned}`);

  return date;
}

export function sanitizeOutput(text: string): string {
  return text.replace(/\x1b/g, '\\x1b').replace(/\r/g, '\\r').replace(/\x00/g, '\\x00');
}

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

export function sanitizeStackTrace(stack: string | undefined): string {
  if (!stack) return '';
  return stack.split('\n').map(line =>
    line.replace(/\/(?:[^\s/]*\/)*(\.claude|home|Users|tmp|Applications)[^\s):)]*/g, '[PATH]')
  ).join('\n');
}

export function validateCategory(category: string): string {
  const validCategories = ['decision', 'learning', 'error', 'workflow'];
  if (!validCategories.includes(category)) {
    throw new Error(`Invalid category: ${category}. Must be one of: ${validCategories.join(', ')}`);
  }
  return category;
}

export function validateWorkflowType(type: string): string {
  const validTypes = ['delegation', 'organization', 'planning', 'collaboration', 'tooling'];
  if (!validTypes.includes(type)) {
    throw new Error(`Invalid workflow type: ${type}. Must be one of: ${validTypes.join(', ')}`);
  }
  return type;
}

export function validateErrorType(type: string): string {
  validateLength(type, INPUT_LIMITS.MAX_ERROR_TYPE_LENGTH, 'Error type');
  return type;
}
