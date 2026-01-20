/**
 * Timestamp parsing utilities for CLI filters
 *
 * Supports ISO-8601 timestamps and relative time shortcuts for filtering
 * entities by creation/modification timestamps.
 */

export interface TimestampFilterOptions {
  createdAfter?: Date;
  createdBefore?: Date;
  startedAfter?: Date;
  startedBefore?: Date;
  completedAfter?: Date;
  completedBefore?: Date;
}

/**
 * Parse ISO-8601 timestamp string with error handling
 *
 * Supported formats:
 * - Full: 2026-01-17T23:10:50Z
 * - With timezone: 2026-01-17T23:10:50-05:00
 * - Without seconds: 2026-01-17T23:10Z
 * - Date only: 2026-01-17 (assumes 00:00:00 UTC)
 *
 * @param value - Timestamp string
 * @param flagName - Name of flag for error messages
 * @returns Parsed Date object
 * @throws Error if invalid format
 */
export function parseTimestamp(value: string, flagName: string): Date {
  // Try parsing as ISO-8601
  const date = new Date(value);

  // Check if valid
  if (isNaN(date.getTime())) {
    throw new Error(
      `Invalid timestamp for ${flagName}: "${value}"\n` +
      `Expected ISO-8601 format:\n` +
      `  Full format:    2026-01-17T23:10:50Z\n` +
      `  With timezone:  2026-01-17T23:10:50-05:00\n` +
      `  Date only:      2026-01-17 (assumes 00:00:00 UTC)`
    );
  }

  return date;
}

/**
 * Parse relative time shortcut flag
 *
 * Supported shortcuts:
 * - --last-hour: 1 hour ago
 * - --last-day: 24 hours ago
 * - --last-week: 7 days ago
 * - --last-month: 30 days ago
 *
 * @param flag - Flag name (e.g., "--last-day")
 * @returns Date object representing the cutoff time
 * @throws Error if unknown flag
 */
export function parseRelativeTime(flag: string): Date {
  const now = new Date();

  const offsets: Record<string, number> = {
    '--last-hour': 60 * 60 * 1000,           // 1 hour in ms
    '--last-day': 24 * 60 * 60 * 1000,       // 24 hours in ms
    '--last-week': 7 * 24 * 60 * 60 * 1000,  // 7 days in ms
    '--last-month': 30 * 24 * 60 * 60 * 1000 // 30 days in ms
  };

  const offset = offsets[flag];
  if (!offset) {
    throw new Error(`Unknown relative time flag: ${flag}`);
  }

  return new Date(now.getTime() - offset);
}

/**
 * Format timestamp for display
 *
 * @param date - Date object or ISO string
 * @param format - Output format ("iso" | "local" | "relative")
 * @returns Formatted timestamp string
 */
export function formatTimestamp(
  date: Date | string,
  format: 'iso' | 'local' | 'relative' = 'iso'
): string {
  const d = typeof date === 'string' ? new Date(date) : date;

  switch (format) {
    case 'iso':
      return d.toISOString();

    case 'local':
      // Format: "2026-01-17 18:10:50 EST"
      return d.toLocaleString('en-US', {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit',
        timeZoneName: 'short'
      });

    case 'relative':
      const now = new Date();
      const diffMs = now.getTime() - d.getTime();
      const diffMins = Math.floor(diffMs / (60 * 1000));
      const diffHours = Math.floor(diffMs / (60 * 60 * 1000));
      const diffDays = Math.floor(diffMs / (24 * 60 * 60 * 1000));

      if (diffMins < 1) return 'just now';
      if (diffMins < 60) return `${diffMins}m ago`;
      if (diffHours < 24) return `${diffHours}h ago`;
      if (diffDays < 7) return `${diffDays}d ago`;
      return d.toISOString().split('T')[0]; // Just date

    default:
      return d.toISOString();
  }
}

/**
 * Validate timestamp filter combinations
 *
 * Checks for conflicting filter ranges (e.g., after > before)
 *
 * @param filters - Timestamp filter options
 * @throws Error if filters conflict
 */
export function validateTimestampFilters(filters: TimestampFilterOptions): void {
  // Check createdAt filters
  if (filters.createdAfter && filters.createdBefore) {
    if (filters.createdAfter.getTime() >= filters.createdBefore.getTime()) {
      throw new Error(
        `Conflicting timestamp filters:\n` +
        `  --created-after ${filters.createdAfter.toISOString()} is not before\n` +
        `  --created-before ${filters.createdBefore.toISOString()}\n` +
        `  This would match no entities.`
      );
    }
  }

  // Check startedAt filters
  if (filters.startedAfter && filters.startedBefore) {
    if (filters.startedAfter.getTime() >= filters.startedBefore.getTime()) {
      throw new Error(
        `Conflicting timestamp filters:\n` +
        `  --started-after ${filters.startedAfter.toISOString()} is not before\n` +
        `  --started-before ${filters.startedBefore.toISOString()}\n` +
        `  This would match no entities.`
      );
    }
  }

  // Check completedAt filters
  if (filters.completedAfter && filters.completedBefore) {
    if (filters.completedAfter.getTime() >= filters.completedBefore.getTime()) {
      throw new Error(
        `Conflicting timestamp filters:\n` +
        `  --completed-after ${filters.completedAfter.toISOString()} is not before\n` +
        `  --completed-before ${filters.completedBefore.toISOString()}\n` +
        `  This would match no entities.`
      );
    }
  }
}

/**
 * Apply timestamp filters to a list of entities
 *
 * Filters entities based on createdAt, startedAt, and completedAt timestamps.
 * All filters use AND logic.
 *
 * @param entities - Array of entities with timestamp fields
 * @param filters - Timestamp filter options
 * @returns Filtered array
 */
export function applyTimestampFilters<T extends { createdAt: Date | string }>(
  entities: T[],
  filters: TimestampFilterOptions
): T[] {
  // Validate filters first
  validateTimestampFilters(filters);

  return entities.filter(entity => {
    const createdAt = typeof entity.createdAt === 'string'
      ? new Date(entity.createdAt)
      : entity.createdAt;

    // Apply createdAt filters
    if (filters.createdAfter && createdAt.getTime() <= filters.createdAfter.getTime()) {
      return false;
    }
    if (filters.createdBefore && createdAt.getTime() >= filters.createdBefore.getTime()) {
      return false;
    }

    // Apply startedAt filters (for tasks)
    // If filter is specified but entity doesn't have the field, filter it out
    if (filters.startedAfter || filters.startedBefore) {
      if (!('startedAt' in entity) || !entity.startedAt) {
        return false; // Entity doesn't have startedAt, so it can't match the filter
      }

      const startedAt = typeof entity.startedAt === 'string'
        ? new Date(entity.startedAt)
        : entity.startedAt as Date;

      if (filters.startedAfter && startedAt.getTime() <= filters.startedAfter.getTime()) {
        return false;
      }
      if (filters.startedBefore && startedAt.getTime() >= filters.startedBefore.getTime()) {
        return false;
      }
    }

    // Apply completedAt filters (for tasks)
    // If filter is specified but entity doesn't have the field, filter it out
    if (filters.completedAfter || filters.completedBefore) {
      if (!('completedAt' in entity) || !entity.completedAt) {
        return false; // Entity doesn't have completedAt, so it can't match the filter
      }

      const completedAt = typeof entity.completedAt === 'string'
        ? new Date(entity.completedAt)
        : entity.completedAt as Date;

      if (filters.completedAfter && completedAt.getTime() <= filters.completedAfter.getTime()) {
        return false;
      }
      if (filters.completedBefore && completedAt.getTime() >= filters.completedBefore.getTime()) {
        return false;
      }
    }

    return true;
  });
}
