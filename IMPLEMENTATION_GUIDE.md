# Event System MVP - Implementation Guide

## Quick Start

You now have a complete architecture, work breakdown, and tracking system for the Event Capture System MVP.

## Documents Created

### 1. EVENT_SYSTEM_MVP_ARCHITECTURE.md
**Location**: `/Users/bln/play/agentic-primer/.wt/event-system/EVENT_SYSTEM_MVP_ARCHITECTURE.md`

**Contents**:
- System overview and goals
- 6 actor types with detailed specs
- Universal Actor Protocol (UAP)
- Loop prevention mechanisms (4 strategies)
- Event sourcing patterns
- API surface (CLI, HTTP, JavaScript)
- Function system (code-based and agent functions)
- Phase 1 constraints and success criteria

**Use this for**: Understanding what to build and how it works

---

### 2. EVENT_SYSTEM_WBS.md
**Location**: `/Users/bln/play/agentic-primer/.wt/event-system/EVENT_SYSTEM_WBS.md`

**Contents**:
- 6 phases (1a-1f) with 25 tasks
- Dependencies and parallelization opportunities
- Deliverables and acceptance criteria
- Time estimates (22-31 hours total)
- Risk assessment and mitigation

**Use this for**: Breaking down work, tracking progress, estimating effort

---

### 3. BLOCKING_QUESTIONS.md
**Location**: `/Users/bln/play/agentic-primer/.wt/event-system/BLOCKING_QUESTIONS.md`

**Contents**:
- 10 common ambiguities resolved
- 5 features deferred to Phase 2
- 0 blocking issues
- Template for adding new questions

**Use this for**: Quick reference for design decisions, avoiding re-discussion

---

## Beads Issue Tracker

### Epic
**ID**: agentic-primer-7tq
**Title**: Event Capture System MVP (Phase 1)
**Priority**: P0 (Critical)
**Labels**: event-system, mvp, phase-1

**View**:
```bash
bd show agentic-primer-7tq
```

### Implementation Tasks (11 total)

#### Phase 1a: Foundation (4 tasks)
1. **agentic-primer-c3s** [P1] - Set up project structure and UAP
   - **READY TO START** (no dependencies)
   - Labels: event-system, mvp, phase-1a, foundation

2. **agentic-primer-soe** [P1] - Implement event log actor with JSONL
   - Depends on: agentic-primer-c3s
   - Labels: event-system, mvp, phase-1a, foundation

3. **agentic-primer-tkb** [P1] - Implement daemon skeleton with Bun.js
   - Depends on: agentic-primer-c3s, agentic-primer-soe
   - Labels: event-system, mvp, phase-1a, foundation

4. **agentic-primer-8oh** [P1] - Implement basic CLI for event capture
   - Depends on: agentic-primer-tkb
   - Labels: event-system, mvp, phase-1a, foundation

#### Phase 1b: Pattern Matching (2 tasks)
5. **agentic-primer-1wq** [P1] - Implement predicate matching engine
   - Depends on: agentic-primer-c3s
   - Labels: event-system, mvp, phase-1b

6. **agentic-primer-6u5** [P1] - Implement function registry actor
   - Depends on: agentic-primer-c3s
   - Labels: event-system, mvp, phase-1b

#### Phase 1c: Function Execution (2 tasks)
7. **agentic-primer-4mx** [P2] - Implement code-based function execution
   - Depends on: agentic-primer-c3s, agentic-primer-6u5
   - Labels: event-system, mvp, phase-1c

8. **agentic-primer-cne** [P1] - Implement loop prevention mechanisms
   - Depends on: agentic-primer-soe, agentic-primer-4mx
   - Labels: event-system, mvp, phase-1c

#### Phase 1d: HTTP Interface (1 task)
9. **agentic-primer-hre** [P2] - Implement HTTP server actor
   - Depends on: agentic-primer-c3s
   - Labels: event-system, mvp, phase-1d

#### Phase 1e: Agent Functions (1 task)
10. **agentic-primer-dmm** [P2] - Implement agent function type
    - Depends on: agentic-primer-4mx
    - Labels: event-system, mvp, phase-1e

#### Phase 1f: Testing (1 task)
11. **agentic-primer-0ic** [P2] - Create test harness and examples
    - Depends on: ALL previous tasks
    - Labels: event-system, mvp, phase-1f

### View Tasks

**List all MVP tasks**:
```bash
bd list --label mvp --pretty
```

**List tasks by phase**:
```bash
bd list --label phase-1a --pretty  # Foundation
bd list --label phase-1b --pretty  # Pattern Matching
bd list --label phase-1c --pretty  # Function Execution
bd list --label phase-1d --pretty  # HTTP Interface
bd list --label phase-1e --pretty  # Agent Functions
bd list --label phase-1f --pretty  # Testing
```

**Show ready-to-start tasks** (no unmet dependencies):
```bash
bd ready
```

**Show specific task details**:
```bash
bd show agentic-primer-c3s  # First task to implement
```

---

## Dependency Structure

```
agentic-primer-c3s (Project Setup + UAP)
├── agentic-primer-soe (EventLogActor)
│   ├── agentic-primer-tkb (Daemon)
│   │   └── agentic-primer-8oh (CLI)
│   └── agentic-primer-cne (Loop Prevention)*
├── agentic-primer-1wq (PatternMatcher)
├── agentic-primer-6u5 (FunctionRegistry)
│   └── agentic-primer-4mx (FunctionExecutor)
│       ├── agentic-primer-cne (Loop Prevention)*
│       └── agentic-primer-dmm (Agent Functions)
└── agentic-primer-hre (HTTPServer)

All → agentic-primer-0ic (Testing)

* agentic-primer-cne depends on both soe and 4mx
```

---

## Implementation Workflow

### Step 1: Start with Foundation
```bash
# Check ready tasks
bd ready

# Start first task
bd update agentic-primer-c3s --status in_progress

# Work on task...
# (Create package.json, config.json, src/protocol/uap.js, etc.)

# Mark complete when done
bd update agentic-primer-c3s --status closed

# Check what's ready next
bd ready  # Should show agentic-primer-soe, 1wq, 6u5, hre
```

### Step 2: Parallel Work (Phase 1a + 1b)
Once `agentic-primer-c3s` is complete, you can work on multiple tasks in parallel:

**Terminal 1**: Phase 1a continuation
```bash
bd update agentic-primer-soe --status in_progress
# Implement EventLogActor
```

**Terminal 2**: Phase 1b (independent)
```bash
bd update agentic-primer-1wq --status in_progress
# Implement PatternMatcherActor
```

**Terminal 3**: Phase 1b (independent)
```bash
bd update agentic-primer-6u5 --status in_progress
# Implement FunctionRegistryActor
```

**Terminal 4**: Phase 1d (independent)
```bash
bd update agentic-primer-hre --status in_progress
# Implement HTTPServerActor
```

### Step 3: Integration
Once core actors complete, integrate them:
```bash
bd update agentic-primer-tkb --status in_progress
# Wire EventLogActor, PatternMatcherActor, FunctionRegistryActor into Daemon
```

### Step 4: Function Execution
```bash
bd update agentic-primer-4mx --status in_progress
# Implement FunctionActor

bd update agentic-primer-cne --status in_progress
# Add loop prevention mechanisms
```

### Step 5: Advanced Features
```bash
bd update agentic-primer-dmm --status in_progress
# Add agent function support (Claude CLI integration)
```

### Step 6: Testing
```bash
bd update agentic-primer-0ic --status in_progress
# Create manual test plan, example functions, validate system
```

---

## Progress Tracking

### View Open Tasks
```bash
bd list --label mvp --status open
```

### View In-Progress Tasks
```bash
bd list --label mvp --status in_progress
```

### View Completed Tasks
```bash
bd list --label mvp --status closed
```

### View Blocked Tasks
```bash
bd blocked
```

### View Statistics
```bash
bd stats
```

---

## Adding Notes to Tasks

As you work, add notes to tasks:

```bash
bd update agentic-primer-c3s --notes "Created package.json with Bun dependencies.
Implemented UAP protocol with ULID support.
Config.json has all required sections."
```

Or add comments:

```bash
bd comment add agentic-primer-c3s "Note: Using ulid package from npm for ID generation"
```

---

## Updating Dependencies

If you discover new dependencies:

```bash
bd dep add [dependent-task] [dependency-task]

# Example: If you realize 4mx also needs tkb
bd dep add agentic-primer-4mx agentic-primer-tkb
```

---

## File Structure (Target)

Once implementation is complete, the event-system directory will look like:

```
/Users/bln/play/agentic-primer/.wt/event-system/
├── README.md
├── EVENT_SYSTEM_MVP_ARCHITECTURE.md
├── EVENT_SYSTEM_WBS.md
├── BLOCKING_QUESTIONS.md
├── IMPLEMENTATION_GUIDE.md (this file)
│
├── package.json
├── config.json
├── .gitignore
│
├── src/
│   ├── daemon.js
│   ├── cli.js
│   ├── protocol/
│   │   └── uap.js
│   ├── actors/
│   │   ├── event-log.js
│   │   ├── pattern-matcher.js
│   │   ├── function-registry.js
│   │   ├── http-server.js
│   │   └── function-executor.js
│   └── loop-prevention/
│       ├── depth-counter.js
│       ├── fingerprinting.js
│       ├── ancestry-chain.js
│       └── circuit-breaker.js
│
├── functions/
│   ├── echo.js
│   ├── notify-slack.js
│   ├── transform-data.js
│   ├── conditional-emit.js
│   └── analyze-error.agent.js
│
├── tests/
│   ├── manual-test-plan.md
│   └── example-events.json
│
├── events.jsonl (generated at runtime)
└── event-system (CLI wrapper executable)
```

---

## Success Criteria

Phase 1 MVP is complete when:

1. ✅ All 11 task beads marked as closed
2. ✅ All files in target structure exist
3. ✅ Daemon starts and stops via CLI
4. ✅ Events can be emitted via CLI and HTTP
5. ✅ Events persist to JSONL
6. ✅ Pattern matching triggers functions
7. ✅ Functions execute successfully
8. ✅ Loop prevention blocks infinite chains
9. ✅ Agent functions can call Claude CLI
10. ✅ Replay works from checkpoint
11. ✅ All acceptance tests pass (manual test plan)

---

## Next Steps

1. **Read the architecture**: Understand the actor model and UAP protocol
2. **Review the WBS**: Familiarize yourself with task breakdown
3. **Start implementing**: Begin with `agentic-primer-c3s` (project setup)
4. **Track progress**: Update bead status as you work
5. **Test frequently**: Validate each phase before moving to next
6. **Document learnings**: Add notes to beads, update BLOCKING_QUESTIONS if needed

---

## Getting Help

**Questions about architecture?**
- Read EVENT_SYSTEM_MVP_ARCHITECTURE.md
- Check BLOCKING_QUESTIONS.md

**Questions about what to build next?**
- Run `bd ready`
- Check EVENT_SYSTEM_WBS.md for task details

**Questions about dependencies?**
- Run `bd show [task-id]` to see blockers
- Review dependency graph in this document

**Questions about design decisions?**
- Check BLOCKING_QUESTIONS.md
- All Phase 1 decisions are documented there

---

**Document Status**: v1.0 (Initial)
**Last Updated**: 2026-01-10
**Author**: Claude (Sonnet 4.5)
**Activity Worktree**: /Users/bln/play/agentic-primer/.wt/event-system/
