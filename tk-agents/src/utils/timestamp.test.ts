import { test, expect, describe } from "bun:test";
import {
  parseTimestamp,
  parseRelativeTime,
  formatTimestamp,
  validateTimestampFilters,
  applyTimestampFilters,
  type TimestampFilterOptions
} from "./timestamp.ts";

describe("parseTimestamp", () => {
  test("parses ISO-8601 full format with Z", () => {
    const result = parseTimestamp("2026-01-17T23:10:50Z", "--created-after");
    expect(result.toISOString()).toBe("2026-01-17T23:10:50.000Z");
  });

  test("parses ISO-8601 with timezone offset", () => {
    const result = parseTimestamp("2026-01-17T18:10:50-05:00", "--created-after");
    expect(result.toISOString()).toBe("2026-01-17T23:10:50.000Z");
  });

  test("parses ISO-8601 without seconds", () => {
    const result = parseTimestamp("2026-01-17T23:10Z", "--created-after");
    expect(result.toISOString()).toBe("2026-01-17T23:10:00.000Z");
  });

  test("parses date only (assumes 00:00:00 UTC)", () => {
    const result = parseTimestamp("2026-01-17", "--created-after");
    expect(result.toISOString()).toBe("2026-01-17T00:00:00.000Z");
  });

  test("throws error for invalid timestamp", () => {
    expect(() => {
      parseTimestamp("invalid-date", "--created-after");
    }).toThrow(/Invalid timestamp for --created-after/);
  });

  test("throws error with helpful message", () => {
    expect(() => {
      parseTimestamp("not-a-date", "--created-after");
    }).toThrow(/Expected ISO-8601 format/);
  });
});

describe("parseRelativeTime", () => {
  test("parses --last-hour", () => {
    const now = new Date();
    const result = parseRelativeTime("--last-hour");
    const diffMs = now.getTime() - result.getTime();
    const diffMins = Math.floor(diffMs / (60 * 1000));

    expect(diffMins).toBeGreaterThanOrEqual(59);
    expect(diffMins).toBeLessThanOrEqual(61); // Allow 1 min variance
  });

  test("parses --last-day", () => {
    const now = new Date();
    const result = parseRelativeTime("--last-day");
    const diffMs = now.getTime() - result.getTime();
    const diffHours = Math.floor(diffMs / (60 * 60 * 1000));

    expect(diffHours).toBeGreaterThanOrEqual(23);
    expect(diffHours).toBeLessThanOrEqual(25); // Allow 1 hour variance
  });

  test("parses --last-week", () => {
    const now = new Date();
    const result = parseRelativeTime("--last-week");
    const diffMs = now.getTime() - result.getTime();
    const diffDays = Math.floor(diffMs / (24 * 60 * 60 * 1000));

    expect(diffDays).toBeGreaterThanOrEqual(6);
    expect(diffDays).toBeLessThanOrEqual(8); // Allow 1 day variance
  });

  test("parses --last-month", () => {
    const now = new Date();
    const result = parseRelativeTime("--last-month");
    const diffMs = now.getTime() - result.getTime();
    const diffDays = Math.floor(diffMs / (24 * 60 * 60 * 1000));

    expect(diffDays).toBeGreaterThanOrEqual(29);
    expect(diffDays).toBeLessThanOrEqual(31); // Allow 1 day variance
  });

  test("throws error for unknown flag", () => {
    expect(() => {
      parseRelativeTime("--last-year");
    }).toThrow(/Unknown relative time flag/);
  });
});

describe("formatTimestamp", () => {
  const testDate = new Date("2026-01-17T23:10:50.000Z");

  test("formats as ISO-8601 by default", () => {
    const result = formatTimestamp(testDate);
    expect(result).toBe("2026-01-17T23:10:50.000Z");
  });

  test("formats as ISO-8601 when specified", () => {
    const result = formatTimestamp(testDate, "iso");
    expect(result).toBe("2026-01-17T23:10:50.000Z");
  });

  test("formats as local time", () => {
    const result = formatTimestamp(testDate, "local");
    expect(result).toContain("2026");
    expect(result).toContain("01");
    expect(result).toContain("17");
  });

  test("formats relative time - just now", () => {
    const now = new Date();
    const result = formatTimestamp(now, "relative");
    expect(result).toBe("just now");
  });

  test("formats relative time - minutes ago", () => {
    const fiveMinsAgo = new Date(Date.now() - 5 * 60 * 1000);
    const result = formatTimestamp(fiveMinsAgo, "relative");
    expect(result).toMatch(/^[45]m ago$/);
  });

  test("formats relative time - hours ago", () => {
    const twoHoursAgo = new Date(Date.now() - 2 * 60 * 60 * 1000);
    const result = formatTimestamp(twoHoursAgo, "relative");
    expect(result).toMatch(/^[12]h ago$/);
  });

  test("formats relative time - days ago", () => {
    const threeDaysAgo = new Date(Date.now() - 3 * 24 * 60 * 60 * 1000);
    const result = formatTimestamp(threeDaysAgo, "relative");
    expect(result).toMatch(/^[23]d ago$/);
  });

  test("formats relative time - date for old timestamps", () => {
    const oldDate = new Date(Date.now() - 10 * 24 * 60 * 60 * 1000);
    const result = formatTimestamp(oldDate, "relative");
    expect(result).toMatch(/^\d{4}-\d{2}-\d{2}$/);
  });

  test("accepts ISO string input", () => {
    const result = formatTimestamp("2026-01-17T23:10:50.000Z", "iso");
    expect(result).toBe("2026-01-17T23:10:50.000Z");
  });
});

describe("validateTimestampFilters", () => {
  test("allows valid createdAt range", () => {
    const filters: TimestampFilterOptions = {
      createdAfter: new Date("2026-01-15T00:00:00Z"),
      createdBefore: new Date("2026-01-17T00:00:00Z")
    };

    expect(() => validateTimestampFilters(filters)).not.toThrow();
  });

  test("throws for conflicting createdAt range", () => {
    const filters: TimestampFilterOptions = {
      createdAfter: new Date("2026-01-17T00:00:00Z"),
      createdBefore: new Date("2026-01-15T00:00:00Z")
    };

    expect(() => validateTimestampFilters(filters)).toThrow(/Conflicting timestamp filters/);
  });

  test("throws for equal createdAt range", () => {
    const sameDate = new Date("2026-01-17T00:00:00Z");
    const filters: TimestampFilterOptions = {
      createdAfter: sameDate,
      createdBefore: sameDate
    };

    expect(() => validateTimestampFilters(filters)).toThrow(/Conflicting timestamp filters/);
  });

  test("allows valid startedAt range", () => {
    const filters: TimestampFilterOptions = {
      startedAfter: new Date("2026-01-15T00:00:00Z"),
      startedBefore: new Date("2026-01-17T00:00:00Z")
    };

    expect(() => validateTimestampFilters(filters)).not.toThrow();
  });

  test("throws for conflicting startedAt range", () => {
    const filters: TimestampFilterOptions = {
      startedAfter: new Date("2026-01-17T00:00:00Z"),
      startedBefore: new Date("2026-01-15T00:00:00Z")
    };

    expect(() => validateTimestampFilters(filters)).toThrow(/Conflicting timestamp filters/);
  });

  test("allows valid completedAt range", () => {
    const filters: TimestampFilterOptions = {
      completedAfter: new Date("2026-01-15T00:00:00Z"),
      completedBefore: new Date("2026-01-17T00:00:00Z")
    };

    expect(() => validateTimestampFilters(filters)).not.toThrow();
  });

  test("throws for conflicting completedAt range", () => {
    const filters: TimestampFilterOptions = {
      completedAfter: new Date("2026-01-17T00:00:00Z"),
      completedBefore: new Date("2026-01-15T00:00:00Z")
    };

    expect(() => validateTimestampFilters(filters)).toThrow(/Conflicting timestamp filters/);
  });
});

describe("applyTimestampFilters", () => {
  const entities = [
    {
      id: "task_1",
      createdAt: new Date("2026-01-15T10:00:00Z"),
      startedAt: new Date("2026-01-15T11:00:00Z"),
      completedAt: new Date("2026-01-15T15:00:00Z")
    },
    {
      id: "task_2",
      createdAt: new Date("2026-01-16T10:00:00Z"),
      startedAt: new Date("2026-01-16T11:00:00Z"),
      completedAt: undefined as any
    },
    {
      id: "task_3",
      createdAt: new Date("2026-01-17T10:00:00Z"),
      startedAt: undefined as any,
      completedAt: undefined as any
    },
    {
      id: "task_4",
      createdAt: "2026-01-18T10:00:00Z", // Test string format
      startedAt: "2026-01-18T11:00:00Z",
      completedAt: "2026-01-18T15:00:00Z"
    }
  ];

  test("filters by createdAfter", () => {
    const filters: TimestampFilterOptions = {
      createdAfter: new Date("2026-01-16T00:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(3);
    expect(result.map(e => e.id)).toEqual(["task_2", "task_3", "task_4"]);
  });

  test("filters by createdBefore", () => {
    const filters: TimestampFilterOptions = {
      createdBefore: new Date("2026-01-17T00:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(2);
    expect(result.map(e => e.id)).toEqual(["task_1", "task_2"]);
  });

  test("filters by createdAt range", () => {
    const filters: TimestampFilterOptions = {
      createdAfter: new Date("2026-01-15T12:00:00Z"),
      createdBefore: new Date("2026-01-17T12:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(2);
    expect(result.map(e => e.id)).toEqual(["task_2", "task_3"]);
  });

  test("filters by startedAfter", () => {
    const filters: TimestampFilterOptions = {
      startedAfter: new Date("2026-01-16T00:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(2);
    expect(result.map(e => e.id)).toEqual(["task_2", "task_4"]);
  });

  test("filters by startedBefore", () => {
    const filters: TimestampFilterOptions = {
      startedBefore: new Date("2026-01-17T00:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(2);
    expect(result.map(e => e.id)).toEqual(["task_1", "task_2"]);
  });

  test("filters by completedAfter", () => {
    const filters: TimestampFilterOptions = {
      completedAfter: new Date("2026-01-16T00:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(1);
    expect(result.map(e => e.id)).toEqual(["task_4"]);
  });

  test("filters by completedBefore", () => {
    const filters: TimestampFilterOptions = {
      completedBefore: new Date("2026-01-16T00:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(1);
    expect(result.map(e => e.id)).toEqual(["task_1"]);
  });

  test("combines multiple filters with AND logic", () => {
    const filters: TimestampFilterOptions = {
      createdAfter: new Date("2026-01-15T12:00:00Z"),
      createdBefore: new Date("2026-01-18T00:00:00Z"),
      startedAfter: new Date("2026-01-15T12:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(1);
    expect(result.map(e => e.id)).toEqual(["task_2"]);
  });

  test("handles entities with undefined timestamps", () => {
    const filters: TimestampFilterOptions = {
      startedAfter: new Date("2026-01-15T00:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    // Only entities WITH startedAt matching the filter should be returned
    // task_1: startedAt 2026-01-15T11:00:00Z (after 2026-01-15T00:00:00Z) ✓
    // task_2: startedAt 2026-01-16T11:00:00Z (after 2026-01-15T00:00:00Z) ✓
    // task_3: startedAt undefined - filtered out
    // task_4: startedAt 2026-01-18T11:00:00Z (after 2026-01-15T00:00:00Z) ✓
    expect(result.length).toBe(3);
    expect(result.map(e => e.id)).toEqual(["task_1", "task_2", "task_4"]);
  });

  test("returns empty array when no matches", () => {
    const filters: TimestampFilterOptions = {
      createdAfter: new Date("2026-01-20T00:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(0);
  });

  test("returns all entities when no filters applied", () => {
    const filters: TimestampFilterOptions = {};

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(4);
  });

  test("handles string createdAt timestamps", () => {
    const filters: TimestampFilterOptions = {
      createdAfter: new Date("2026-01-17T12:00:00Z")
    };

    const result = applyTimestampFilters(entities, filters);
    expect(result.length).toBe(1);
    expect(result.map(e => e.id)).toEqual(["task_4"]);
  });

  test("validates filters before applying", () => {
    const filters: TimestampFilterOptions = {
      createdAfter: new Date("2026-01-17T00:00:00Z"),
      createdBefore: new Date("2026-01-15T00:00:00Z")
    };

    expect(() => applyTimestampFilters(entities, filters)).toThrow(/Conflicting timestamp filters/);
  });
});
