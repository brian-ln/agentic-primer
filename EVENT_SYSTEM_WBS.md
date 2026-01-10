# Event Capture System - Work Breakdown Structure

## Overview

This document breaks down the Event Capture System MVP into phased, hierarchical tasks with clear dependencies, deliverables, and acceptance criteria.

## Execution Strategy

- **Phases**: 6 main phases (1a through 1f)
- **Dependencies**: Explicit task ordering where needed
- **Parallelization**: Many tasks can run concurrently
- **Deliverables**: Concrete files/features for each phase
- **Validation**: Acceptance criteria for each phase

## Phase 1a: Foundation

**Goal**: Basic infrastructure - daemon skeleton, event log, minimal CLI

**Duration Estimate**: 4-6 hours

### Tasks

#### Task 1a.1: Project Setup
**Description**: Initialize project structure, dependencies, configuration

**Work Items**:
- Create `src/` directory structure
- Create `functions/` directory
- Create `tests/` directory
- Initialize `package.json` (Bun)
- Create `config.json` with defaults
- Create `.gitignore`

**Dependencies**: None

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/package.json`
- `/Users/bln/play/agentic-primer/.wt/event-system/config.json`
- `/Users/bln/play/agentic-primer/.wt/event-system/src/` (directory)
- `/Users/bln/play/agentic-primer/.wt/event-system/functions/` (directory)
- `/Users/bln/play/agentic-primer/.wt/event-system/.gitignore`

**Acceptance Criteria**:
- `bun install` runs successfully
- Directory structure matches architecture doc
- `config.json` contains all required sections

**Research Needed**: None

---

#### Task 1a.2: Universal Actor Protocol (UAP)
**Description**: Implement message structure and utilities

**Work Items**:
- Create `src/protocol/uap.js`
- Implement `createMessage(action, data, metadata)` function
- Implement `validateMessage(msg)` function
- Implement `createReply(originalMsg, data)` function
- Add ULID generation for message IDs
- Add basic tests/examples

**Dependencies**: Task 1a.1 (project setup)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/protocol/uap.js`

**Acceptance Criteria**:
- Can create valid UAP messages
- Message validation catches malformed messages
- ULID IDs are sortable and unique
- Reply messages correctly reference original

**Research Needed**: None (spec defined in architecture)

---

#### Task 1a.3: Event Log Actor (JSONL)
**Description**: Implement append-only event persistence

**Work Items**:
- Create `src/actors/event-log.js`
- Implement `appendEvent(event)` - write to JSONL
- Implement `queryEvents(filters)` - read with filters
- Implement `checkpoint()` - mark current position
- Implement `replayFrom(checkpoint, callback)` - replay events
- Handle file I/O errors gracefully
- Add ULID generation for event IDs

**Dependencies**: Task 1a.2 (UAP)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/event-log.js`

**Acceptance Criteria**:
- Events written to `events.jsonl` (one per line)
- Can read events back with filters
- Checkpoints work correctly
- Replay processes events in order
- File corruption doesn't crash system

**Research Needed**: None

---

#### Task 1a.4: Daemon Skeleton
**Description**: Main orchestrator with basic lifecycle

**Work Items**:
- Create `src/daemon.js`
- Implement `start()` - initialize system
- Implement `stop()` - graceful shutdown
- Implement `getStatus()` - health check
- Spawn EventLogActor
- Handle SIGINT/SIGTERM for graceful shutdown
- Load configuration from `config.json`

**Dependencies**: Task 1a.2 (UAP), Task 1a.3 (EventLogActor)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/daemon.js`

**Acceptance Criteria**:
- Daemon starts without errors
- Can stop daemon gracefully
- Status shows "running" or "stopped"
- EventLogActor is spawned and functional
- CTRL+C shuts down cleanly

**Research Needed**: None

---

#### Task 1a.5: Basic CLI
**Description**: Command-line interface for daemon control

**Work Items**:
- Create `src/cli.js`
- Implement `daemon start` command
- Implement `daemon stop` command
- Implement `daemon status` command
- Implement `emit` command (basic version)
- Parse CLI arguments (use built-in or simple library)
- Add help text and usage examples

**Dependencies**: Task 1a.4 (Daemon)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/cli.js`
- `/Users/bln/play/agentic-primer/.wt/event-system/event-system` (executable wrapper)

**Acceptance Criteria**:
- `./event-system daemon start` starts daemon
- `./event-system daemon stop` stops daemon
- `./event-system daemon status` shows status
- `./event-system emit --type "test" --data '{"x":1}'` emits event
- Help text explains all commands

**Research Needed**: None

---

### Phase 1a Deliverables Summary

**Files Created**:
- `package.json`
- `config.json`
- `.gitignore`
- `src/protocol/uap.js`
- `src/actors/event-log.js`
- `src/daemon.js`
- `src/cli.js`
- `event-system` (CLI wrapper)

**Capabilities**:
- Daemon can start/stop
- Events can be emitted via CLI
- Events persist to JSONL
- Basic health check works

**Validation**:
```bash
# Start daemon
./event-system daemon start

# Emit event
./event-system emit --type "test.event" --data '{"hello": "world"}'

# Check log
cat events.jsonl  # Should contain event

# Check status
./event-system daemon status  # Should show "running"

# Stop daemon
./event-system daemon stop
```

---

## Phase 1b: Pattern Matching

**Goal**: Predicate engine and function registry

**Duration Estimate**: 4-6 hours

### Tasks

#### Task 1b.1: Pattern Matcher Actor
**Description**: Evaluate events against predicates

**Work Items**:
- Create `src/actors/pattern-matcher.js`
- Implement `matchEvent(event)` - evaluate all patterns
- Implement `registerPattern(patternId, predicate, functionId)` - add pattern
- Implement `unregisterPattern(patternId)` - remove pattern
- Implement `listPatterns()` - query patterns
- Support JavaScript predicates (function bodies)
- Sort matches by priority
- Handle predicate errors gracefully

**Dependencies**: Task 1a.2 (UAP)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/pattern-matcher.js`

**Acceptance Criteria**:
- Can register predicates as JavaScript functions
- Predicates can access full event object
- Match returns list of {functionId, patternId}
- Higher priority patterns evaluated first
- Invalid predicates don't crash system

**Research Needed**: How to safely eval JavaScript predicates (use `new Function()` or vm module)

---

#### Task 1b.2: Function Registry Actor
**Description**: Catalog of available functions

**Work Items**:
- Create `src/actors/function-registry.js`
- Implement `registerFunction(functionId, metadata)` - add function
- Implement `unregisterFunction(functionId)` - remove function
- Implement `getFunction(functionId)` - retrieve metadata
- Implement `listFunctions()` - query all functions
- Implement `scanDirectory(path)` - auto-discover .js files
- Support both "code" and "agent" types
- Store function metadata (name, description, author)

**Dependencies**: Task 1a.2 (UAP)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/function-registry.js`

**Acceptance Criteria**:
- Can register functions manually
- Can scan `functions/` directory
- Metadata includes type, path, config
- Can retrieve function by ID
- List returns all registered functions

**Research Needed**: None

---

#### Task 1b.3: Integrate Pattern Matcher with Daemon
**Description**: Wire pattern matching into event flow

**Work Items**:
- Update `src/daemon.js` to spawn PatternMatcherActor
- On event emission, call `matchEvent()`
- Store pattern registrations in memory
- Add CLI command: `patterns register`
- Add CLI command: `patterns list`
- Emit "match.found" event when pattern matches

**Dependencies**: Task 1a.4 (Daemon), Task 1b.1 (PatternMatcher)

**Deliverables**:
- Updated `src/daemon.js`
- Updated `src/cli.js`

**Acceptance Criteria**:
- Emitting event triggers pattern matching
- Matches are logged
- CLI can register patterns
- CLI can list patterns

**Research Needed**: None

---

#### Task 1b.4: Integrate Function Registry with Daemon
**Description**: Wire function registry into daemon

**Work Items**:
- Update `src/daemon.js` to spawn FunctionRegistryActor
- On startup, scan `functions/` directory
- Add CLI command: `functions register`
- Add CLI command: `functions list`
- Store function metadata in memory

**Dependencies**: Task 1a.4 (Daemon), Task 1b.2 (FunctionRegistry)

**Deliverables**:
- Updated `src/daemon.js`
- Updated `src/cli.js`

**Acceptance Criteria**:
- Daemon discovers functions on startup
- CLI can register functions manually
- CLI can list functions

**Research Needed**: None

---

### Phase 1b Deliverables Summary

**Files Created**:
- `src/actors/pattern-matcher.js`
- `src/actors/function-registry.js`

**Files Updated**:
- `src/daemon.js` (spawn new actors)
- `src/cli.js` (add pattern/function commands)

**Capabilities**:
- Can register patterns with predicates
- Events matched against patterns
- Functions discovered from filesystem
- Pattern matches logged

**Validation**:
```bash
# Register function
./event-system functions register ./functions/echo.js

# Register pattern
./event-system patterns register --function "echo" --predicate "event.type === 'test.echo'"

# Emit event
./event-system emit --type "test.echo" --data '{"msg": "hello"}'

# Check logs - should see match.found event
cat events.jsonl | grep "match.found"
```

---

## Phase 1c: Function Execution

**Goal**: Execute code functions with loop prevention

**Duration Estimate**: 6-8 hours

### Tasks

#### Task 1c.1: Function Executor Actor
**Description**: Execute individual function invocations

**Work Items**:
- Create `src/actors/function-executor.js`
- Implement `execute(functionId, event)` - run function
- Load function from filesystem (ES module import)
- Create execution context: `{emit, logger, config}`
- Capture function return value
- Handle function errors (try/catch)
- Emit "function.executed" event with result
- Emit "function.error" event on failure

**Dependencies**: Task 1a.2 (UAP), Task 1b.2 (FunctionRegistry)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/function-executor.js`

**Acceptance Criteria**:
- Can import and execute .js functions
- Function receives (event, context) arguments
- Context has emit(), logger, config
- Return value captured
- Errors don't crash daemon

**Research Needed**: Bun dynamic import syntax

---

#### Task 1c.2: Depth Counter (Loop Prevention)
**Description**: Track and limit event depth

**Work Items**:
- Create `src/loop-prevention/depth-counter.js`
- Add `metadata.depth` to events
- Initial events (CLI/HTTP) start at depth 0
- Child events increment parent depth: `depth = parent.depth + 1`
- Reject events exceeding `maxStackDepth`
- Emit "loop.prevented.depth" event when rejected

**Dependencies**: Task 1a.3 (EventLogActor)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/loop-prevention/depth-counter.js`

**Acceptance Criteria**:
- All events have depth field
- Child events have correct depth
- Events over max depth rejected
- Rejection logged

**Research Needed**: None

---

#### Task 1c.3: Fingerprinting (Loop Prevention)
**Description**: Prevent duplicate events via content hash

**Work Items**:
- Create `src/loop-prevention/fingerprinting.js`
- Implement `fingerprint(event)` - hash(type + data)
- Maintain sliding window of recent fingerprints
- Check new events against window
- Reject if duplicate found
- Emit "loop.prevented.duplicate" event when rejected

**Dependencies**: Task 1a.3 (EventLogActor)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/loop-prevention/fingerprinting.js`

**Acceptance Criteria**:
- Fingerprints are unique per event content
- Sliding window tracks last N fingerprints
- Duplicates rejected within window
- Old fingerprints expire correctly

**Research Needed**: None (use crypto.createHash)

---

#### Task 1c.4: Ancestry Chain (Loop Prevention)
**Description**: Prevent cyclical event chains

**Work Items**:
- Create `src/loop-prevention/ancestry-chain.js`
- Add `metadata.triggeredBy` to events
- Build full ancestry: [evt_1, evt_2, evt_3]
- Check if current event type in ancestry
- Reject if cycle detected
- Emit "loop.prevented.cycle" event when rejected

**Dependencies**: Task 1a.3 (EventLogActor)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/loop-prevention/ancestry-chain.js`

**Acceptance Criteria**:
- Ancestry chain correctly built
- Cycles detected (same type in ancestry)
- Rejection prevents infinite loops
- Works across multiple hops

**Research Needed**: None

---

#### Task 1c.5: Circuit Breaker (Loop Prevention)
**Description**: Prevent high-frequency event storms

**Work Items**:
- Create `src/loop-prevention/circuit-breaker.js`
- Track event emission rate per function
- Implement states: closed, open, half-open
- Open circuit if threshold exceeded
- Auto-reset after timeout
- Emit "loop.prevented.circuit" event when open

**Dependencies**: Task 1c.1 (FunctionExecutor)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/loop-prevention/circuit-breaker.js`

**Acceptance Criteria**:
- Circuit opens on high rate
- Functions fail fast when open
- Auto-recovery after timeout
- Half-open state works correctly

**Research Needed**: None (standard circuit breaker pattern)

---

#### Task 1c.6: Integrate Function Execution
**Description**: Wire function execution into daemon

**Work Items**:
- Update `src/daemon.js` to spawn FunctionExecutorActor per match
- Pass event + context to executor
- Integrate all 4 loop prevention mechanisms
- Capture emitted events from functions
- Apply loop checks before emitting child events

**Dependencies**: Tasks 1c.1, 1c.2, 1c.3, 1c.4, 1c.5

**Deliverables**:
- Updated `src/daemon.js`

**Acceptance Criteria**:
- Pattern matches trigger function execution
- Functions can emit new events
- Loop prevention blocks infinite chains
- All 4 mechanisms active

**Research Needed**: None

---

#### Task 1c.7: Example Code Function
**Description**: Create working example function

**Work Items**:
- Create `functions/echo.js`
- Simple function that echoes input
- Emits "echo.response" event
- Add metadata (name, description)

**Dependencies**: Task 1c.1 (FunctionExecutor)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/functions/echo.js`

**Acceptance Criteria**:
- Function loads correctly
- Executes without errors
- Emits response event
- Can be called via CLI

**Research Needed**: None

---

### Phase 1c Deliverables Summary

**Files Created**:
- `src/actors/function-executor.js`
- `src/loop-prevention/depth-counter.js`
- `src/loop-prevention/fingerprinting.js`
- `src/loop-prevention/ancestry-chain.js`
- `src/loop-prevention/circuit-breaker.js`
- `functions/echo.js`

**Files Updated**:
- `src/daemon.js` (integrate execution + loop prevention)

**Capabilities**:
- Functions execute on pattern match
- Functions can emit new events
- Loop prevention blocks infinite chains
- Example function works end-to-end

**Validation**:
```bash
# Register pattern to echo
./event-system patterns register --function "echo" --predicate "event.type === 'test.echo'"

# Emit event
./event-system emit --type "test.echo" --data '{"msg": "hello"}'

# Check logs - should see function.executed and echo.response
cat events.jsonl | grep -E "(function.executed|echo.response)"

# Test loop prevention - emit event that triggers itself
./event-system patterns register --function "loop" --predicate "event.type === 'loop.test'"
# Create functions/loop.js that emits "loop.test"
./event-system emit --type "loop.test" --data '{}'
# Should see loop.prevented.* events
```

---

## Phase 1d: HTTP Interface

**Goal**: HTTP server for event ingestion and queries

**Duration Estimate**: 3-4 hours

### Tasks

#### Task 1d.1: HTTP Server Actor
**Description**: HTTP API endpoints

**Work Items**:
- Create `src/actors/http-server.js`
- Implement `POST /events` - emit event
- Implement `GET /events` - query events
- Implement `GET /functions` - list functions
- Implement `GET /patterns` - list patterns
- Implement `GET /health` - health check
- Use Bun's built-in HTTP server
- Parse JSON request bodies
- Return JSON responses

**Dependencies**: Task 1a.2 (UAP)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/http-server.js`

**Acceptance Criteria**:
- Server binds to configured port (default 3000)
- All endpoints respond correctly
- JSON parsing works
- Errors return proper HTTP status codes

**Research Needed**: Bun HTTP server API

---

#### Task 1d.2: Integrate HTTP Server with Daemon
**Description**: Wire HTTP server into daemon

**Work Items**:
- Update `src/daemon.js` to spawn HTTPServerActor
- Route HTTP requests to appropriate actors
- `POST /events` → EventLogActor
- `GET /events` → EventLogActor query
- `GET /functions` → FunctionRegistryActor
- `GET /patterns` → PatternMatcherActor
- `GET /health` → DaemonActor status

**Dependencies**: Task 1d.1 (HTTPServerActor), Task 1a.4 (Daemon)

**Deliverables**:
- Updated `src/daemon.js`

**Acceptance Criteria**:
- HTTP server starts with daemon
- All endpoints functional
- Events emitted via HTTP trigger pattern matching
- Health check returns accurate status

**Research Needed**: None

---

### Phase 1d Deliverables Summary

**Files Created**:
- `src/actors/http-server.js`

**Files Updated**:
- `src/daemon.js` (spawn HTTP server)

**Capabilities**:
- HTTP API for event ingestion
- Query events via HTTP
- List functions/patterns via HTTP
- Health check endpoint

**Validation**:
```bash
# Start daemon
./event-system daemon start

# Emit event via HTTP
curl -X POST http://localhost:3000/events \
  -H "Content-Type: application/json" \
  -d '{"type": "test.http", "data": {"msg": "hello"}}'

# Query events
curl http://localhost:3000/events?type=test.http

# List functions
curl http://localhost:3000/functions

# Health check
curl http://localhost:3000/health
```

---

## Phase 1e: Agent Functions

**Goal**: Special function type that calls Claude CLI

**Duration Estimate**: 3-4 hours

### Tasks

#### Task 1e.1: Agent Function Support
**Description**: Detect and handle agent functions

**Work Items**:
- Update `src/actors/function-executor.js` to detect agent type
- Implement `executeAgent(functionId, event)` method
- Spawn `claude` CLI as subprocess
- Build prompt from event data
- Stream prompt to stdin
- Capture stdout as response
- Parse Claude response
- Wrap in standard function result

**Dependencies**: Task 1c.1 (FunctionExecutor)

**Deliverables**:
- Updated `src/actors/function-executor.js`

**Acceptance Criteria**:
- Agent functions detected via `config.type === "agent"`
- Claude CLI spawned correctly
- Prompt passed via stdin
- Response captured
- Errors handled gracefully

**Research Needed**: Bun subprocess API

---

#### Task 1e.2: Agent Context Helper
**Description**: Add `context.claudeCLI()` helper

**Work Items**:
- Update execution context to include `claudeCLI(prompt, options)`
- Options: model, maxTokens, temperature
- Use configured Claude command from config.json
- Return parsed response

**Dependencies**: Task 1e.1 (Agent support)

**Deliverables**:
- Updated `src/actors/function-executor.js`

**Acceptance Criteria**:
- Functions can call `context.claudeCLI(prompt)`
- Options override defaults
- Response is structured data

**Research Needed**: None

---

#### Task 1e.3: Example Agent Function
**Description**: Create working agent function

**Work Items**:
- Create `functions/analyze-error.agent.js`
- Takes error event
- Calls Claude CLI
- Emits "analysis.complete" event
- Add metadata

**Dependencies**: Task 1e.2 (claudeCLI helper)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/functions/analyze-error.agent.js`

**Acceptance Criteria**:
- Function loads correctly
- Calls Claude CLI
- Emits result event
- Works end-to-end

**Research Needed**: None

---

### Phase 1e Deliverables Summary

**Files Created**:
- `functions/analyze-error.agent.js`

**Files Updated**:
- `src/actors/function-executor.js` (agent support)

**Capabilities**:
- Agent functions can call Claude CLI
- Claude responses emitted as events
- Example agent function works

**Validation**:
```bash
# Register pattern
./event-system patterns register --function "analyze-error" --predicate "event.type === 'error.critical'"

# Emit error event
./event-system emit --type "error.critical" --data '{"error": "NullPointerException", "stack": "..."}'

# Check logs - should see analysis.complete event
cat events.jsonl | grep "analysis.complete"
```

---

## Phase 1f: Testing & Validation

**Goal**: Manual test harness and example functions

**Duration Estimate**: 2-3 hours

### Tasks

#### Task 1f.1: Manual Test Plan
**Description**: Document manual acceptance tests

**Work Items**:
- Create `tests/manual-test-plan.md`
- Document all acceptance tests from phases 1a-1e
- Add step-by-step instructions
- Include expected outputs
- Add troubleshooting tips

**Dependencies**: All previous phases

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/tests/manual-test-plan.md`

**Acceptance Criteria**:
- All critical paths covered
- Instructions clear and reproducible
- Expected outputs documented

**Research Needed**: None

---

#### Task 1f.2: Example Events
**Description**: Sample events for testing

**Work Items**:
- Create `tests/example-events.json`
- Include various event types
- Cover edge cases (deep nesting, large payloads, etc.)
- Add comments explaining each

**Dependencies**: None

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/tests/example-events.json`

**Acceptance Criteria**:
- Examples cover common use cases
- Edge cases included
- Valid JSON format

**Research Needed**: None

---

#### Task 1f.3: Example Functions
**Description**: Additional example functions for testing

**Work Items**:
- Create `functions/notify-slack.js` (mock)
- Create `functions/transform-data.js`
- Create `functions/conditional-emit.js`
- Add variety of patterns

**Dependencies**: Task 1c.1 (FunctionExecutor)

**Deliverables**:
- `/Users/bln/play/agentic-primer/.wt/event-system/functions/notify-slack.js`
- `/Users/bln/play/agentic-primer/.wt/event-system/functions/transform-data.js`
- `/Users/bln/play/agentic-primer/.wt/event-system/functions/conditional-emit.js`

**Acceptance Criteria**:
- All functions execute correctly
- Cover different patterns
- Well-commented for learning

**Research Needed**: None

---

#### Task 1f.4: End-to-End Validation
**Description**: Execute full test suite

**Work Items**:
- Run all tests from manual test plan
- Verify each acceptance criterion
- Document any failures
- Create issues for bugs found

**Dependencies**: Task 1f.1 (Manual test plan), all previous phases

**Deliverables**:
- Test execution results (in beads or log)

**Acceptance Criteria**:
- All acceptance tests pass
- No critical bugs
- System stable under normal load

**Research Needed**: None

---

### Phase 1f Deliverables Summary

**Files Created**:
- `tests/manual-test-plan.md`
- `tests/example-events.json`
- `functions/notify-slack.js`
- `functions/transform-data.js`
- `functions/conditional-emit.js`

**Capabilities**:
- Complete test coverage
- Example functions for learning
- Validated system

**Validation**:
```bash
# Follow tests/manual-test-plan.md
# All tests should pass
```

---

## Dependency Graph

```
Phase 1a (Foundation)
├── 1a.1 Project Setup
├── 1a.2 UAP ← 1a.1
├── 1a.3 EventLogActor ← 1a.2
├── 1a.4 Daemon ← 1a.2, 1a.3
└── 1a.5 CLI ← 1a.4

Phase 1b (Pattern Matching)
├── 1b.1 PatternMatcher ← 1a.2
├── 1b.2 FunctionRegistry ← 1a.2
├── 1b.3 Integrate PatternMatcher ← 1a.4, 1b.1
└── 1b.4 Integrate FunctionRegistry ← 1a.4, 1b.2

Phase 1c (Function Execution)
├── 1c.1 FunctionExecutor ← 1a.2, 1b.2
├── 1c.2 DepthCounter ← 1a.3
├── 1c.3 Fingerprinting ← 1a.3
├── 1c.4 AncestryChain ← 1a.3
├── 1c.5 CircuitBreaker ← 1c.1
├── 1c.6 Integrate Execution ← 1c.1-1c.5
└── 1c.7 Example Function ← 1c.1

Phase 1d (HTTP Interface)
├── 1d.1 HTTPServerActor ← 1a.2
└── 1d.2 Integrate HTTP ← 1d.1, 1a.4

Phase 1e (Agent Functions)
├── 1e.1 Agent Support ← 1c.1
├── 1e.2 claudeCLI Helper ← 1e.1
└── 1e.3 Example Agent ← 1e.2

Phase 1f (Testing)
├── 1f.1 Manual Test Plan ← all
├── 1f.2 Example Events
├── 1f.3 Example Functions ← 1c.1
└── 1f.4 E2E Validation ← 1f.1, all
```

## Parallelization Opportunities

**Can work in parallel:**

- Phase 1b tasks (1b.1, 1b.2) - independent actors
- Phase 1c tasks (1c.2, 1c.3, 1c.4) - independent loop prevention
- Phase 1d and 1e - no dependencies between them
- Phase 1f tasks (1f.1, 1f.2, 1f.3) - documentation/examples

**Must be sequential:**

- Phase 1a → Phase 1b, 1c, 1d, 1e (foundation required)
- Integration tasks must wait for their actors (1b.3, 1b.4, 1c.6, 1d.2)

## Effort Summary

| Phase | Tasks | Estimated Hours | Can Parallelize |
|-------|-------|-----------------|-----------------|
| 1a Foundation | 5 | 4-6 | Minimal |
| 1b Pattern Matching | 4 | 4-6 | Yes (1b.1, 1b.2) |
| 1c Function Execution | 7 | 6-8 | Yes (1c.2-1c.4) |
| 1d HTTP Interface | 2 | 3-4 | Yes (with 1e) |
| 1e Agent Functions | 3 | 3-4 | Yes (with 1d) |
| 1f Testing | 4 | 2-3 | Yes (docs/examples) |
| **Total** | **25** | **22-31** | - |

**With parallelization**: 18-25 hours (optimistic)
**Sequential execution**: 22-31 hours

## Risks & Mitigations

### Risk 1: Bun API Changes
**Impact**: High
**Likelihood**: Low
**Mitigation**: Pin Bun version in package.json, document version used

### Risk 2: Loop Prevention Complexity
**Impact**: Medium
**Likelihood**: Medium
**Mitigation**: Test each mechanism independently, add comprehensive logging

### Risk 3: Claude CLI Integration
**Impact**: Medium
**Likelihood**: Medium
**Mitigation**: Mock Claude CLI for testing, graceful fallback if unavailable

### Risk 4: File I/O Bottlenecks
**Impact**: Low
**Likelihood**: Medium
**Mitigation**: Phase 1 doesn't need high performance, optimize in Phase 2

### Risk 5: Scope Creep
**Impact**: High
**Likelihood**: High
**Mitigation**: Strict adherence to "Phase 1 Constraints" - NO features beyond MVP

## Success Metrics

Phase 1 is complete when:

1. All 25 tasks have deliverables
2. All acceptance criteria pass
3. Manual test plan validates successfully
4. Example functions work end-to-end
5. System runs stable for 1 hour under normal load
6. All loop prevention mechanisms active and tested

## Next Phase Preview (Phase 2)

**Potential Phase 2 features** (out of scope for Phase 1):
- Automated testing framework
- Log rotation and archiving
- TypeScript support for functions
- Web UI dashboard
- Enhanced querying (indexes, full-text search)
- Distributed event log (multi-node)
- Authentication and authorization
- Rate limiting per client
- Metrics and monitoring
- Function versioning
- Hot reload of functions
- CLI input piping
- Event replay UI
- Pattern testing tools

---

**Document Status**: v1.0 (Initial)
**Last Updated**: 2026-01-10
**Author**: Claude (Sonnet 4.5)
**Review**: Pending user feedback
