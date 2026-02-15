# Actor Design Patterns

This directory contains production-tested actor patterns extracted from real implementations. All patterns are backed by comprehensive test suites (128+ tests) and include code examples, test strategies, and decision guidelines.

## Available Patterns

### [Session Gateway Pattern](./session-gateway-pattern.md)

**WebSocket routing layer for multi-actor backends**

- **Use when:** Per-client sessions, routing to multiple DO namespaces, dashboard UIs
- **Complexity:** Medium-High (custom routing logic)
- **Test coverage:** 60+ tests
- **Lines of code:** ~220 (SessionActor)

```typescript
// Routes client messages to backend actors
SessionActor → CoordinatorActor
SessionActor → WorkerActor
SessionActor → Other Actors
```

**Key features:**
- Wire protocol with structured addressing (`coordinator:main`, `worker:w1`)
- Client-side routing rules (responses go to `client:coordinator-proxy`)
- Hibernation API for WebSocket lifecycle
- Error handling with correlation IDs

**When NOT to use:**
- Single-DO architecture → use WebSocketBridge instead
- Simple broadcast scenarios → use WebSocketBridge.broadcast()
- No per-client isolation needed → use shared ActorSystem

---

### [Coordinator-Worker Pattern](./coordinator-worker-pattern.md)

**Task queue with worker pool orchestration**

- **Use when:** Distributed task processing, work queue semantics, auto-scaling workers
- **Complexity:** Medium (FIFO queue + availability tracking)
- **Test coverage:** 35+ tests
- **Lines of code:** ~120 (CoordinatorActor), ~75 (WorkerActor)

```typescript
// Auto-assignment workflow
Client → assign_task → Coordinator
         ↓ (if worker available)
         WorkerActor.process_task
         ↓ (when complete)
         Coordinator.task_complete
         ↓ (if queue not empty)
         WorkerActor.process_task (next task)
```

**Key features:**
- Auto-assignment on worker registration
- Auto-assignment on task completion (work stealing)
- Fire-and-forget task dispatch (`ctx.waitUntil`)
- State persistence across DO restarts

**Performance characteristics:**
- Workers: ~100 (O(N) availability scan)
- Queue depth: ~10,000 (array operations)
- Active tasks: ~100 (Map operations)

**When NOT to use:**
- Tasks are independent → use direct actor messaging
- Need priority queues → pattern is FIFO only
- Huge scale (> 1000 workers) → use sharded coordinators

---

### [Testing Actor Systems](./testing-actor-systems.md)

**Comprehensive testing methodology for actor systems**

- **Strategy:** Dual coverage (LSP static analysis + Vitest runtime tests)
- **Total tests:** 128+ across all actors
- **Coverage:** 100% branch coverage (65/65 branches)
- **Test/Code ratio:** 3.1:1

**Test categories:**
1. **WebSocket connection** (2 tests) - Upgrade, rejection
2. **Message routing** (4 tests) - Coordinator, worker, validation
3. **Wire protocol** (4 tests) - Format validation, error cases
4. **Complete workflows** (1 test) - Register → assign → status
5. **State persistence** (2 tests) - Coordinator, worker state
6. **Direct actor calls** (3 tests) - Bypass SessionActor
7. **Edge cases** (7 tests) - Unknown types, binary data, routing logic

**Key techniques:**
- Test isolation with unique session IDs
- Message collection with filtering (avoid order assumptions)
- Timing strategies for async operations
- WebSocket cleanup after tests

**Tools:**
- Vitest with `cloudflare:test` environment
- LSP-based branch counting (`/coverage-analysis` skill)
- Static analysis to identify untested code paths

---

## Comparison Matrix

| Pattern | Purpose | Isolation | Routing | Use Case |
|---------|---------|-----------|---------|----------|
| **SessionActor** | Gateway routing layer | Per-client session | Multiple DO namespaces | Dashboards, multi-actor backends |
| **WebSocketBridge** | Direct ActorSystem bridge | Shared ActorSystem | Local ActorSystem | Single-DO, broadcasts |
| **CoordinatorActor** | Task queue orchestration | Shared coordinator state | Worker pool | Distributed task processing |
| **DOActorSystem** | DO lifecycle wrapper | Per-DO ActorSystem | Internal ActorSystem | Actor system foundations |

## Pattern Combinations

### SessionActor + Coordinator-Worker

**Perfect for:** Dashboard monitoring of distributed task processing

```typescript
// Client connects via SessionActor
Browser ──WebSocket──► SessionActor
                        │
                        ├──► CoordinatorActor (get_status)
                        ├──► WorkerActor (get_state)
                        └──► WorkerActor (get_state)
```

**Implementation:** See `session-gateway-pattern.md` → "Integration with SessionActor"

### WebSocketBridge + DOActorSystem

**Perfect for:** Single-DO actor systems with WebSocket client

```typescript
// Client connects directly to DO
Browser ──WebSocket──► DOActorSystem
                        └──► ActorSystem
                             ├──► Actor1
                             ├──► Actor2
                             └──► Actor3
```

**Implementation:** See `/Users/bln/play/agentic-primer/packages/cloudflare/src/do-actor-system.ts`

## Source Code References

All patterns are extracted from production implementations:

- **SessionActor**: `/Users/bln/play/projects/proj-20260211-140744/src/actor-system-session.ts` (lines 266-484)
- **CoordinatorActor**: Same file (lines 47-170)
- **WorkerActor**: Same file (lines 176-250)
- **Tests**: `/Users/bln/play/projects/proj-20260211-140744/src/session-actor.test.ts` (1665 lines, 128 tests)

**Existing infrastructure:**
- **WebSocketBridge**: `/Users/bln/play/agentic-primer/packages/cloudflare/src/transports/websocket-bridge.ts`
- **DOActorSystem**: `/Users/bln/play/agentic-primer/packages/cloudflare/src/do-actor-system.ts`
- **DOActorCheckpoint**: `/Users/bln/play/agentic-primer/packages/cloudflare/src/do-actor-checkpoint.ts`

## When to Create New Patterns

**Extract a pattern when:**
- Implementation is battle-tested (> 50 tests)
- Pattern solves a recurring problem
- Clear decision criteria exist (when to use vs alternatives)
- Performance characteristics are documented
- Trade-offs are well-understood

**Don't extract when:**
- Pattern is experimental or untested
- Use case is too specific
- No clear alternatives to compare against
- Performance characteristics unknown

## Contributing Patterns

To add a new pattern:

1. **Implement** - Build and test the pattern (target 100% branch coverage)
2. **Document** - Create `{pattern-name}-pattern.md` following existing format:
   - Overview + When to Use
   - Architecture diagram
   - Implementation with code examples
   - Key design decisions
   - Testing strategy
   - Performance characteristics
   - Pattern variations
   - When NOT to use
   - References
3. **Test** - Document test coverage and methodology
4. **Compare** - Add comparison to existing patterns
5. **Update** - Add entry to this README

## Pattern Template

```markdown
# Pattern Name

## Overview
[1-2 sentence description]

## When to Use
**Use when:** [bulleted list]
**Don't use when:** [bulleted list]

## Architecture
[ASCII diagram + message flow]

## Implementation
[Code examples with inline comments]

## Key Design Decisions
[Rationale for major choices]

## Testing Strategy
[Test categories + coverage metrics]

## Performance Characteristics
[Complexity table + scalability limits]

## Pattern Variations
[Alternative implementations]

## When Not to Use
[Anti-patterns + alternatives]

## References
[Source code paths]
```

## License

All patterns are extracted from real implementations and are provided as educational reference material. See individual source code files for licensing information.
