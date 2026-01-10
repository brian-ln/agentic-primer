# Blocking Questions for Event System MVP

This document tracks ambiguous areas where clarity is needed before implementation can proceed confidently.

## Status: RESOLVED

All questions below have been answered with pragmatic Phase 1 decisions. No blocking issues remain.

---

## Q1: HTTP Response Format

**Question**: What format should HTTP responses use (JSON, plaintext, structured errors)?

**Status**: ✅ RESOLVED

**Decision**: JSON for all responses, standard error format

**Details**:
```javascript
// Success response
{
  "eventId": "evt_01JBCD...",
  "status": "accepted"
}

// Error response
{
  "error": "Invalid event type",
  "code": "VALIDATION_ERROR",
  "status": 400
}
```

**Rationale**: JSON is standard for HTTP APIs, easy to parse, well-supported.

**Phase**: 1d (HTTP Interface)

---

## Q2: CLI Input Piping

**Question**: Should CLI support piping input (e.g., `cat events.json | event-system emit`)?

**Status**: ✅ RESOLVED (DEFERRED)

**Decision**: Phase 2 feature - not in Phase 1 MVP

**Details**: Keep Phase 1 simple with explicit arguments only:
```bash
./event-system emit --type "test" --data '{"x": 1}'
```

**Rationale**:
- Simpler implementation for Phase 1
- Most use cases covered by explicit args
- Can add piping in Phase 2 without breaking changes

**Phase**: Phase 2 (out of scope)

---

## Q3: Event Log Growth

**Question**: What happens when event log grows to 1GB? 10GB?

**Status**: ✅ RESOLVED (DEFERRED)

**Decision**: Phase 2 - log rotation and archiving

**Details**: Phase 1 constraints:
- No automatic log rotation
- Manual checkpoint/replay if needed
- Document max recommended log size (100MB)
- Warn users if log exceeds threshold

**Rationale**:
- Phase 1 is proof-of-concept, not production
- Log rotation adds complexity
- Can be retrofitted without breaking event format
- JSONL format makes splitting logs trivial

**Mitigation for Phase 1**:
- Document in README: "Phase 1 is not for production use"
- Add warning at 100MB: "Event log is large, consider archiving"

**Phase**: Phase 2 (log management)

---

## Q4: Function Isolation

**Question**: Should functions run in separate processes (isolation) or same process (performance)?

**Status**: ✅ RESOLVED

**Decision**: Same process for Phase 1, isolate in Phase 2

**Details**:
- Phase 1: Functions imported as ES modules, run in main process
- No sandboxing or resource limits
- Trust all functions (user's own code)

**Rationale**:
- Simpler implementation
- Faster execution
- Easier debugging
- Phase 1 is single-user, localhost only

**Phase 2 Consideration**:
- Use Bun's worker threads or child processes
- Add resource limits (CPU, memory, time)
- Sandbox untrusted code

**Phase**: 1c (Function Execution) - resolved as "no isolation"

---

## Q5: Function Error Handling

**Question**: What happens when a function throws an error? Retry? Circuit break? Ignore?

**Status**: ✅ RESOLVED

**Decision**: Log error, emit error event, continue system operation

**Details**:
```javascript
try {
  const result = await executeFunction(functionId, event);
  await emit({ type: "function.executed", data: { result } });
} catch (error) {
  logger.error(`Function ${functionId} failed: ${error.message}`);
  await emit({
    type: "function.error",
    data: {
      functionId,
      error: error.message,
      stack: error.stack,
      triggeringEvent: event.id
    }
  });
}
// System continues, daemon stays up
```

**Rationale**:
- Function crash becomes observable event
- Other functions unaffected
- System stays resilient
- Error events can trigger recovery functions

**No Retry in Phase 1**:
- Retry logic is complex (exponential backoff, max attempts, etc.)
- Can be added in Phase 2
- For Phase 1, user can register error-handling function:
  ```javascript
  // Pattern: event.type === "function.error"
  // Function: retry-failed-function.js
  ```

**Phase**: 1c (Function Execution)

---

## Q6: TypeScript Support

**Question**: Should functions be written in TypeScript or JavaScript?

**Status**: ✅ RESOLVED (DEFERRED)

**Decision**: JavaScript only for Phase 1, TypeScript in Phase 2

**Details**:
- Phase 1: `.js` files with ES modules
- Bun natively supports TypeScript, so adding `.ts` support is trivial
- No type checking in Phase 1

**Rationale**:
- Keep Phase 1 simple
- JavaScript is lowest common denominator
- TypeScript can be added without breaking changes
- Bun handles TypeScript natively (no transpilation needed)

**Phase 2 Addition**:
```javascript
// functions/my-function.ts
export default async function(event: Event, context: Context): Promise<Result> {
  // TypeScript with full type safety
}
```

**Phase**: Phase 2 (enhanced developer experience)

---

## Q7: Predicate Safety

**Question**: How to safely execute user-provided JavaScript predicates without security risks?

**Status**: ✅ RESOLVED (WITH CAVEAT)

**Decision**: Use `new Function()` for Phase 1, trust user code

**Details**:
```javascript
// Pattern registration
const predicate = new Function('event', predicateBody);
// predicateBody = "return event.type === 'test.event'"

// Evaluation
const matches = predicate(event);
```

**Security Caveat**:
- Phase 1 is localhost-only, single-user
- NO untrusted code execution
- Document in README: "Do not expose to network"

**Phase 2 Consideration**:
- Use `vm.Script` with context isolation
- Or: Use declarative predicate DSL (JSON-based)
  ```json
  {
    "type": "equals",
    "field": "event.type",
    "value": "test.event"
  }
  ```

**Phase**: 1b (Pattern Matching)

---

## Q8: Event Replay Behavior

**Question**: When replaying events, should functions execute again? What if they have side effects?

**Status**: ✅ RESOLVED

**Decision**: Replay executes functions again - user responsibility to handle idempotency

**Details**:
- Replay = re-process events through pattern matcher
- Functions execute as if events are new
- Functions should be idempotent (user's responsibility)
- Document best practices:
  ```javascript
  // Good: Idempotent function
  export default async function(event, context) {
    // Check if work already done
    const existing = await checkIfProcessed(event.id);
    if (existing) return { status: "already_processed" };

    // Do work
    await doWork(event.data);

    return { status: "processed" };
  }
  ```

**Rationale**:
- Replay is for debugging and state reconstruction
- Making replay "side-effect free" is complex
- Idempotency is a good practice anyway
- Document clearly in architecture and examples

**Phase**: 1a (EventLogActor), 1f (Testing - document best practices)

---

## Q9: Configuration Hot Reload

**Question**: Should config changes (maxStackDepth, etc.) require daemon restart or hot reload?

**Status**: ✅ RESOLVED (DEFERRED)

**Decision**: Restart required for Phase 1, hot reload in Phase 2

**Details**:
- Phase 1: Read `config.json` on daemon start only
- To change config: stop daemon, edit config, start daemon
- Simple and predictable

**Phase 2 Enhancement**:
```bash
./event-system daemon reload
# Or: Watch config.json for changes
```

**Rationale**:
- Hot reload adds complexity (validation, rollback, consistency)
- Phase 1 is dev/test environment, restart is acceptable
- Can be added without breaking changes

**Phase**: Phase 2 (daemon management)

---

## Q10: Multiple Daemon Instances

**Question**: Can multiple daemons run on same machine (different ports/logs)?

**Status**: ✅ RESOLVED (DEFERRED)

**Decision**: Single daemon per directory for Phase 1

**Details**:
- Daemon binds to port in `config.json`
- One `events.jsonl` per directory
- No multi-tenancy in Phase 1

**Workaround for Multiple Instances**:
```bash
# Terminal 1
cd /path/to/project-a
./event-system daemon start  # Uses config.json (port 3000)

# Terminal 2
cd /path/to/project-b
./event-system daemon start  # Uses config.json (port 3001)
```

**Phase 2 Enhancement**:
- Support `--config` flag to specify config file
- Support `--data-dir` flag for log location
- Multi-instance coordination (if needed)

**Phase**: Phase 2 (advanced deployment)

---

## Summary

**Total Questions**: 10
**Resolved**: 10
**Deferred to Phase 2**: 5 (Q2, Q3, Q6, Q9, Q10)
**Blocking**: 0

**All Phase 1 ambiguities are resolved.** Implementation can proceed with confidence.

---

## Adding New Questions

If you encounter ambiguity during implementation:

1. Add question to this document
2. Create blocking sub-task bead: `bd create task "Resolve: [question]" --priority 0`
3. Link blocking task as dependency: `bd dep add [implementation-task] [blocking-task]`
4. Discuss and resolve
5. Update this document with decision
6. Close blocking task

**Format for new questions**:
```markdown
## Q##: [Question Title]

**Question**: [Clear statement of ambiguity]

**Status**: 🚫 BLOCKING / ⚠️ NEEDS DECISION / ✅ RESOLVED

**Decision**: [What was decided]

**Details**: [Specifics, code examples, constraints]

**Rationale**: [Why this decision]

**Phase**: [Which phase this affects]
```

---

**Document Status**: v1.0 (Complete)
**Last Updated**: 2026-01-10
**Author**: Claude (Sonnet 4.5)
**Review**: All questions resolved, no blockers
