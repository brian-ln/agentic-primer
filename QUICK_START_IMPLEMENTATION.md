# Event System MVP - Quick Start Implementation Guide

**Last Updated**: 2026-01-10
**Time Estimate**: 2-2.5 hours (with parallelization)
**Prerequisites**: PostToolUse hook installed, 11 beads tasks created

---

## TL;DR - The 5-Step Process

1. **Foundation** (DIRECT): Do `c3s` - project structure + UAP
2. **Spawn 4 Agents** (PARALLEL): Launch `soe`, `tkb`, `1wq`, `6u5`
3. **Execute Remaining** (MIXED): `8oh` (direct), `4mx` (agent), `cne` (direct), `hre` (agent), `dmm` (direct)
4. **Test Everything** (DIRECT): Do `0ic` - integration testing
5. **Ship It**: Close epic, commit, celebrate

---

## Decision Tree: Agent or Direct?

```
Quick task (<5 min)?  → DIRECT
Foundation task?      → DIRECT
Testing/debugging?    → DIRECT
Iteration needed?     → DIRECT
Everything else?      → AGENT (if >15 min)
```

---

## The 11 Tasks - Classified

| Phase | Task | ID | Execute | Why |
|-------|------|----|---------|-----|
| **1a** | Project setup + UAP | `c3s` | DIRECT | Foundation (5 min) |
| **1a** | Event log actor | `soe` | AGENT | Isolated, 20 min |
| **1a** | Daemon skeleton | `tkb` | AGENT | Isolated, 20 min |
| **1a** | CLI wrapper | `8oh` | DIRECT | Quick, 10 min |
| **1b** | Pattern matcher | `1wq` | AGENT | Complex, 30 min |
| **1b** | Function registry | `6u5` | AGENT | Isolated, 20 min |
| **1c** | Function executor | `4mx` | AGENT | Complex, 25 min |
| **1c** | Loop prevention | `cne` | DIRECT | Needs iteration, 30 min |
| **1d** | HTTP server | `hre` | AGENT | Isolated, 20 min |
| **1e** | Agent functions | `dmm` | DIRECT | Extends 4mx, 15 min |
| **1f** | Testing harness | `0ic` | DIRECT | Integration, 20 min |

---

## Execution Timeline (Optimized)

```
Time    Main Session              Agent 1      Agent 2      Agent 3      Agent 4
------  ------------------------  -----------  -----------  -----------  -----------
0:00    c3s (setup) →
0:05    spawn agents →            soe          tkb          1wq          6u5
0:10    [wait for agents]         [running]    [running]    [running]    [running]
0:25    [tkb done] 8oh (CLI) →    [done]       [done]       [running]    [running]
0:35    spawn 4mx agent →                                   [done]       [done]
                                                4mx
0:40    [wait for 4mx]                          [running]
0:55    [4mx done] cne (direct) →               [done]
1:25    spawn hre agent →                                                hre
                                                                         [running]
1:30    dmm (direct) →
1:45    [wait for all agents]                                            [done]
1:50    0ic (testing) →
2:20    [COMPLETE]
```

**Total Time**: ~2 hours 20 minutes (vs 3.5 hours sequential)

---

## Command Sequence (Copy-Paste Ready)

### Step 1: Foundation
```bash
# Do c3s directly
bd update c3s --status in_progress
# ... create dirs, package.json, config.json, .gitignore, src/protocol/uap.js ...
bd update c3s --status closed

# Hook fires: shows 4 tasks ready
```

### Step 2: Parallel Phase 1a + 1b
```bash
# Spawn 4 agents
/bg "Implement EventLogActor (agentic-primer-soe): JSONL append-only storage with replay. Deliverables: src/actors/event-log.js. See: bd show soe. Close task when done."

/bg "Implement DaemonActor (agentic-primer-tkb): Main orchestrator with start/stop/status. Deliverables: src/daemon.js. See: bd show tkb. Close task when done."

/bg "Implement PatternMatcherActor (agentic-primer-1wq): JavaScript predicate matching engine. Deliverables: src/actors/pattern-matcher.js. See: bd show 1wq. Close task when done."

/bg "Implement FunctionRegistryActor (agentic-primer-6u5): Function catalog with auto-discovery. Deliverables: src/actors/function-registry.js. See: bd show 6u5. Close task when done."
```

### Step 3: CLI (after tkb completes)
```bash
# Wait for tkb agent to finish
# Check: ls src/daemon.js

# Do CLI directly
bd update 8oh --status in_progress
# ... create src/cli.js, event-system wrapper ...
bd update 8oh --status closed
```

### Step 4: Function Execution
```bash
# Spawn executor agent
/bg "Implement FunctionActor executor (agentic-primer-4mx): Dynamic ES module loading with context. Deliverables: src/actors/function-executor.js. See: bd show 4mx. Close task when done."

# Wait for agent to complete
# Check: ls src/actors/function-executor.js

# Do loop prevention directly (needs iteration)
bd update cne --status in_progress
# ... implement 4 mechanisms: depth, fingerprint, ancestry, circuit-breaker ...
bd update cne --status closed
```

### Step 5: HTTP + Agent Functions
```bash
# Spawn HTTP agent
/bg "Implement HTTPServerActor (agentic-primer-hre): Bun HTTP server with API endpoints. Deliverables: src/actors/http-server.js. See: bd show hre. Close task when done."

# Do agent function type directly
bd update dmm --status in_progress
# ... extend executor for Claude subprocess, create example function ...
bd update dmm --status closed
```

### Step 6: Testing
```bash
# Wait for all agents to complete
# Check: bd list --status in_progress (should be empty)

# Do integration testing directly
bd update 0ic --status in_progress
# ... create test plan, examples, run full suite ...
bd update 0ic --status closed

# Hook fires: Celebrates completion!
```

### Step 7: Ship It
```bash
# Close epic
bd update 7tq --status closed

# Update README
# (add usage instructions)

# Commit
git add .
git commit -m "feat: Implement Event Capture System MVP

- Complete event-driven architecture with actor model
- JSONL event sourcing with replay capability
- Pattern matching with JavaScript predicates
- Code and agent function execution
- 4-layer loop prevention (depth, fingerprint, ancestry, circuit-breaker)
- HTTP API and CLI interfaces
- Full test harness with examples

Closes: agentic-primer-7tq
"
```

---

## Hook Usage

The PostToolUse hook automatically:
- Shows newly-ready tasks after closing any task
- Suggests parallelization when multiple tasks ready
- Celebrates phase completion

**You don't need to manually run** `bd ready` - the hook does it for you!

**Example hook output**:
```
🔄 Checking for newly-ready tasks...

📋 Ready tasks (4):
1. [P1] agentic-primer-soe: Implement event log actor
2. [P1] agentic-primer-tkb: Implement daemon skeleton
3. [P1] agentic-primer-1wq: Implement predicate matching
4. [P1] agentic-primer-6u5: Implement function registry

💡 Parallelization check:
   → 4 event-system tasks ready - consider parallel agents
   → Use: /bg for background task agents
```

---

## Troubleshooting

### Agent not closing task
**Problem**: Agent completes but forgets to close task

**Solution**:
```bash
# Check task status
bd show <task-id>

# Close manually
bd update <task-id> --status closed
```

### Too many agents
**Problem**: System sluggish, 6+ agents running

**Solution**:
```bash
# Check active agents
bd list --status in_progress

# Wait for some to complete before spawning more
# Limit: 4 concurrent agents max
```

### Task stuck in_progress
**Problem**: Task shows in_progress for >30 minutes

**Solution**:
```bash
# Reset and retry
bd update <task-id> --status open
/bg "Retry task <task-id>..."

# OR take over manually
bd update <task-id> --status in_progress
# ... do the work ...
bd update <task-id> --status closed
```

---

## Success Checklist

After completing all 11 tasks:

- [ ] Daemon starts: `bun src/daemon.js`
- [ ] CLI works: `./event-system daemon status`
- [ ] Events emit: `./event-system emit '{"type":"test","data":{}}'`
- [ ] Patterns match: Check events.jsonl for matches
- [ ] Functions execute: Create test function, verify execution
- [ ] Loop prevention: Trigger loop, verify prevention
- [ ] Agent functions: Test Claude subprocess call
- [ ] HTTP API: `curl http://localhost:3000/health`
- [ ] Replay works: Stop daemon, restart, verify replay
- [ ] Test suite passes: All manual tests green

---

## Time Savings

| Approach | Time | Speedup |
|----------|------|---------|
| **Sequential** (no agents) | 3.5 hours | Baseline |
| **This harness** (agents + hook) | 2-2.5 hours | **30-40% faster** |

**Plus**:
- Lower cognitive load (hook handles status)
- Better visibility (immediate feedback)
- Higher quality (clear acceptance criteria)

---

## Next Steps After MVP

1. Extract this harness pattern for other projects
2. Create `/create-harness` skill for automation
3. Add production hardening (error handling, logging, monitoring)
4. Start Phase 2 features (web UI, real-time streaming, marketplace)

---

**Ready to build? Start with Step 1!**
