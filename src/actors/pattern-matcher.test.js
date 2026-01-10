import { test, expect, describe, beforeEach } from "bun:test";
import { createPatternMatcher } from "./pattern-matcher.js";

describe("PatternMatcherActor", () => {
  let matcher;

  beforeEach(() => {
    matcher = createPatternMatcher();
  });

  describe("registerPattern", () => {
    test("registers a valid pattern", () => {
      const result = matcher.registerPattern({
        id: "test-pattern",
        predicate: "event.type === 'test'"
      });

      expect(result.success).toBe(true);
      expect(result.pattern.id).toBe("test-pattern");
      expect(result.pattern.priority).toBe(0);
    });

    test("registers pattern with priority", () => {
      const result = matcher.registerPattern({
        id: "high-priority",
        predicate: "true",
        priority: 10
      });

      expect(result.success).toBe(true);
      expect(result.pattern.priority).toBe(10);
    });

    test("registers pattern with metadata", () => {
      const result = matcher.registerPattern({
        id: "with-metadata",
        predicate: "true",
        metadata: { description: "Test pattern" }
      });

      expect(result.success).toBe(true);
      expect(result.pattern.metadata.description).toBe("Test pattern");
    });

    test("rejects pattern without id", () => {
      const result = matcher.registerPattern({
        predicate: "true"
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("id");
    });

    test("rejects pattern without predicate", () => {
      const result = matcher.registerPattern({
        id: "no-predicate"
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("predicate");
    });

    test("rejects invalid predicate syntax", () => {
      const result = matcher.registerPattern({
        id: "invalid",
        predicate: "this is not valid JavaScript {{"
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("Invalid predicate syntax");
    });

    test("rejects duplicate pattern id", () => {
      matcher.registerPattern({
        id: "duplicate",
        predicate: "true"
      });

      const result = matcher.registerPattern({
        id: "duplicate",
        predicate: "false"
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("already exists");
    });

    test("rejects non-object pattern", () => {
      const result = matcher.registerPattern("not an object");

      expect(result.success).toBe(false);
      expect(result.error).toContain("must be an object");
    });
  });

  describe("unregisterPattern", () => {
    test("unregisters existing pattern", () => {
      matcher.registerPattern({
        id: "to-remove",
        predicate: "true"
      });

      const result = matcher.unregisterPattern("to-remove");

      expect(result.success).toBe(true);
      expect(result.id).toBe("to-remove");
      expect(matcher.getPattern("to-remove")).toBeNull();
    });

    test("rejects non-existent pattern", () => {
      const result = matcher.unregisterPattern("does-not-exist");

      expect(result.success).toBe(false);
      expect(result.error).toContain("not found");
    });

    test("rejects invalid id", () => {
      const result = matcher.unregisterPattern(null);

      expect(result.success).toBe(false);
      expect(result.error).toContain("must be a string");
    });
  });

  describe("listPatterns", () => {
    test("returns empty array when no patterns", () => {
      const patterns = matcher.listPatterns();

      expect(patterns).toEqual([]);
    });

    test("lists all registered patterns", () => {
      matcher.registerPattern({ id: "p1", predicate: "true" });
      matcher.registerPattern({ id: "p2", predicate: "true" });
      matcher.registerPattern({ id: "p3", predicate: "true" });

      const patterns = matcher.listPatterns();

      expect(patterns.length).toBe(3);
      expect(patterns.map(p => p.id).sort()).toEqual(["p1", "p2", "p3"]);
    });

    test("sorts by priority when requested", () => {
      matcher.registerPattern({ id: "low", predicate: "true", priority: 1 });
      matcher.registerPattern({ id: "high", predicate: "true", priority: 10 });
      matcher.registerPattern({ id: "medium", predicate: "true", priority: 5 });

      const patterns = matcher.listPatterns({ sortByPriority: true });

      expect(patterns[0].id).toBe("high");
      expect(patterns[1].id).toBe("medium");
      expect(patterns[2].id).toBe("low");
    });

    test("uses id as tiebreaker when priorities equal", () => {
      matcher.registerPattern({ id: "zebra", predicate: "true", priority: 5 });
      matcher.registerPattern({ id: "alpha", predicate: "true", priority: 5 });
      matcher.registerPattern({ id: "beta", predicate: "true", priority: 5 });

      const patterns = matcher.listPatterns({ sortByPriority: true });

      expect(patterns[0].id).toBe("alpha");
      expect(patterns[1].id).toBe("beta");
      expect(patterns[2].id).toBe("zebra");
    });
  });

  describe("matchEvent", () => {
    test("matches event against simple predicate", () => {
      matcher.registerPattern({
        id: "type-check",
        predicate: "event.type === 'user.created'"
      });

      const result = matcher.matchEvent({
        type: "user.created",
        userId: "123"
      });

      expect(result.success).toBe(true);
      expect(result.matches.length).toBe(1);
      expect(result.matches[0].patternId).toBe("type-check");
      expect(result.errors.length).toBe(0);
    });

    test("matches event against complex predicate", () => {
      matcher.registerPattern({
        id: "complex",
        predicate: "event.type === 'order' && event.amount > 100 && event.status === 'pending'"
      });

      const result = matcher.matchEvent({
        type: "order",
        amount: 150,
        status: "pending"
      });

      expect(result.success).toBe(true);
      expect(result.matches.length).toBe(1);
      expect(result.matches[0].patternId).toBe("complex");
    });

    test("returns empty matches when no patterns match", () => {
      matcher.registerPattern({
        id: "no-match",
        predicate: "event.type === 'other'"
      });

      const result = matcher.matchEvent({
        type: "user.created"
      });

      expect(result.success).toBe(true);
      expect(result.matches.length).toBe(0);
    });

    test("matches multiple patterns in priority order", () => {
      matcher.registerPattern({
        id: "low",
        predicate: "event.type === 'test'",
        priority: 1
      });
      matcher.registerPattern({
        id: "high",
        predicate: "event.type === 'test'",
        priority: 10
      });
      matcher.registerPattern({
        id: "medium",
        predicate: "event.type === 'test'",
        priority: 5
      });

      const result = matcher.matchEvent({ type: "test" });

      expect(result.success).toBe(true);
      expect(result.matches.length).toBe(3);
      expect(result.matches[0].patternId).toBe("high");
      expect(result.matches[1].patternId).toBe("medium");
      expect(result.matches[2].patternId).toBe("low");
    });

    test("includes pattern metadata in matches", () => {
      matcher.registerPattern({
        id: "with-meta",
        predicate: "true",
        metadata: { action: "notify", channel: "email" }
      });

      const result = matcher.matchEvent({ type: "test" });

      expect(result.success).toBe(true);
      expect(result.matches[0].metadata.action).toBe("notify");
      expect(result.matches[0].metadata.channel).toBe("email");
    });

    test("handles predicate runtime errors gracefully", () => {
      matcher.registerPattern({
        id: "error-pattern",
        predicate: "event.user.profile.name === 'test'" // Will fail if user is undefined
      });
      matcher.registerPattern({
        id: "safe-pattern",
        predicate: "event.type === 'test'"
      });

      const result = matcher.matchEvent({ type: "test" });

      expect(result.success).toBe(true);
      expect(result.matches.length).toBe(1); // Only safe-pattern matches
      expect(result.matches[0].patternId).toBe("safe-pattern");
      expect(result.errors.length).toBe(1);
      expect(result.errors[0].patternId).toBe("error-pattern");
      expect(result.errors[0].error).toBeDefined();
    });

    test("accesses full event object in predicate", () => {
      matcher.registerPattern({
        id: "nested-access",
        predicate: "event.payload && event.payload.items && event.payload.items.length > 0"
      });

      const result = matcher.matchEvent({
        type: "cart.update",
        payload: {
          items: [{ id: 1 }, { id: 2 }]
        }
      });

      expect(result.success).toBe(true);
      expect(result.matches.length).toBe(1);
    });

    test("supports JavaScript operators and functions", () => {
      matcher.registerPattern({
        id: "js-operators",
        predicate: "event.tags && event.tags.includes('urgent') && event.priority >= 5"
      });

      const result = matcher.matchEvent({
        type: "issue",
        tags: ["urgent", "bug"],
        priority: 8
      });

      expect(result.success).toBe(true);
      expect(result.matches.length).toBe(1);
    });

    test("rejects non-object event", () => {
      const result = matcher.matchEvent("not an object");

      expect(result.success).toBe(false);
      expect(result.error).toContain("must be an object");
      expect(result.matches).toEqual([]);
    });

    test("includes matched event in result", () => {
      matcher.registerPattern({
        id: "any",
        predicate: "true"
      });

      const testEvent = { type: "test", data: "value" };
      const result = matcher.matchEvent(testEvent);

      expect(result.eventMatched).toEqual(testEvent);
    });
  });

  describe("getPattern", () => {
    test("retrieves pattern by id", () => {
      matcher.registerPattern({
        id: "test",
        predicate: "true",
        priority: 5
      });

      const pattern = matcher.getPattern("test");

      expect(pattern).toBeDefined();
      expect(pattern.id).toBe("test");
      expect(pattern.priority).toBe(5);
    });

    test("returns null for non-existent pattern", () => {
      const pattern = matcher.getPattern("does-not-exist");

      expect(pattern).toBeNull();
    });
  });

  describe("getPatternCount", () => {
    test("returns 0 when no patterns", () => {
      expect(matcher.getPatternCount()).toBe(0);
    });

    test("returns correct count after registrations", () => {
      matcher.registerPattern({ id: "p1", predicate: "true" });
      matcher.registerPattern({ id: "p2", predicate: "true" });
      matcher.registerPattern({ id: "p3", predicate: "true" });

      expect(matcher.getPatternCount()).toBe(3);
    });

    test("returns correct count after unregistration", () => {
      matcher.registerPattern({ id: "p1", predicate: "true" });
      matcher.registerPattern({ id: "p2", predicate: "true" });
      matcher.unregisterPattern("p1");

      expect(matcher.getPatternCount()).toBe(1);
    });
  });

  describe("clearPatterns", () => {
    test("clears all patterns", () => {
      matcher.registerPattern({ id: "p1", predicate: "true" });
      matcher.registerPattern({ id: "p2", predicate: "true" });
      matcher.registerPattern({ id: "p3", predicate: "true" });

      const result = matcher.clearPatterns();

      expect(result.success).toBe(true);
      expect(result.clearedCount).toBe(3);
      expect(matcher.getPatternCount()).toBe(0);
    });

    test("returns 0 when clearing empty matcher", () => {
      const result = matcher.clearPatterns();

      expect(result.success).toBe(true);
      expect(result.clearedCount).toBe(0);
    });
  });
});
