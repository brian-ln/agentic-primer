# Event System MVP Implementation Harness

**Date**: 2026-01-10
**Context**: Implementation execution plan for 11-task Event Capture System MVP
**Purpose**: Maximize parallelization, coordinate agents, automate progress tracking

---

## Overview

This harness provides:
1. **Lifecycle hooks** - Automated progress tracking and next-step suggestions
2. **Parallelization strategy** - Decision criteria for agent vs direct execution
3. **Agent orchestration** - Pattern for spawning and coordinating background agents
4. **Execution loop** - Integrated workflow combining all components

---

## 1. Lifecycle Hooks

### 1.1 Hook Design Decision

**Question**: Do we need Claude Code hooks for the event-system harness?

**Answer**: **YES** - A PostToolUse hook will automate progress tracking and reduce cognitive load.

**Rationale**:
- 11 tasks across 6 phases require constant state checking
- Manual "what's ready now?" queries are repetitive
- Automation removes friction from task completion
- Hook can suggest next steps immediately after closing a task

### 1.2 Hook Implementation

**Hook Type**: `PostToolUse`
**Trigger**: After any tool execution (watches for `bd update` or `bd close`)
**Location**: `.wt/event-system/.claude/hooks/PostToolUse`

**Hook Script**:

```bash
#!/usr/bin/env bash
# PostToolUse hook for event-system implementation harness
# Triggers after every tool use to check for newly-ready tasks

set -euo pipefail

# Only activate in event-system directory
if [[ "$PWD" != *".wt/event-system"* ]]; then
  exit 0
fi

# Parse the tool use from stdin (Claude passes tool info via stdin)
TOOL_NAME="${1:-}"
TOOL_OUTPUT="${2:-}"

# Only react to beads update/close commands
if [[ "$TOOL_NAME" != "Bash" ]]; then
  exit 0
fi

# Check if this was a bd update or bd close command
if ! echo "$TOOL_OUTPUT" | grep -q "bd \(update\|close\)"; then
  exit 0
fi

# Give user brief feedback
echo ""
echo "🔄 Checking for newly-ready tasks..."

# Check what's ready now
READY_OUTPUT=$(bd ready --format json 2>/dev/null || echo "[]")
READY_COUNT=$(echo "$READY_OUTPUT" | jq 'length')

if [[ "$READY_COUNT" -eq 0 ]]; then
  echo "✅ No new tasks ready (waiting on dependencies)"
  exit 0
fi

# Show ready tasks
echo ""
echo "📋 Ready tasks ($READY_COUNT):"
bd ready | head -20

# Suggest parallelization opportunities
echo ""
echo "💡 Parallelization check:"

# Count event-system tasks that are ready
EVENT_SYSTEM_READY=$(echo "$READY_OUTPUT" | jq '[.[] | select(.labels | contains(["event-system"]))] | length')

if [[ "$EVENT_SYSTEM_READY" -gt 1 ]]; then
  echo "   → $EVENT_SYSTEM_READY event-system tasks ready - consider parallel agents"
  echo "   → Use: /bg for background task agents"
fi

# Check for phase-1a completion
PHASE_1A_OPEN=$(bd list --label phase-1a --status open --format json 2>/dev/null | jq 'length')
if [[ "$PHASE_1A_OPEN" -eq 0 ]]; then
  PHASE_1A_CLOSED=$(bd list --label phase-1a --status closed --format json 2>/dev/null | jq 'length')
  if [[ "$PHASE_1A_CLOSED" -gt 0 ]]; then
    echo "   🎉 Phase 1a complete! Phase 1b tasks should be ready."
  fi
fi

echo ""
exit 0
```

**Installation**:
```bash
# Create hooks directory
mkdir -p .wt/event-system/.claude/hooks

# Create hook script
cat > .wt/event-system/.claude/hooks/PostToolUse << 'EOF'
[paste script above]
EOF

# Make executable
chmod +x .wt/event-system/.claude/hooks/PostToolUse
```

**What It Does**:
- **Fires after every tool execution** (including `bd update`, `bd close`)
- **Checks for ready tasks** using `bd ready`
- **Suggests parallelization** when multiple tasks are ready
- **Celebrates phase completion** when all tasks in a phase close
- **Low noise**: Only shows output if something actionable happened

**Benefits**:
- No manual "what's next?" queries needed
- Immediate visibility into unlocked tasks
- Prompts for parallelization opportunities
- Reduces mental overhead during implementation

---

## 2. Parallelization Strategy

### 2.1 Agent vs Direct Decision Criteria

**Use AGENT when**:
- Task is **implementation-heavy** (creating actors, algorithms)
- Task is **independent** (no tight coupling to current work)
- Task takes **>15 minutes** (estimated)
- Task has **clear acceptance criteria** (can validate independently)
- Task is **self-contained** (doesn't require context from main session)

**Use DIRECT when**:
- Task is **quick** (<5 minutes: creating config files, directories)
- Task is **tightly coupled** to current work (UAP protocol affects everything)
- Task requires **iteration/debugging** (testing loop prevention logic)
- Task is **contextually dependent** (needs knowledge from previous step)
- Agent overhead **exceeds task complexity** (creating .gitignore)

### 2.2 Task Classification

Here's the classification of our 11 event-system tasks:

| Task | ID | Type | Reasoning |
|------|-----|------|-----------|
| **Phase 1a: Foundation** |
| Set up project structure and UAP | `c3s` | **DIRECT** | Quick setup (5 min), foundation for everything |
| Implement event log actor | `soe` | **AGENT** | Isolated actor (20 min), clear interface |
| Implement daemon skeleton | `tkb` | **AGENT** | Isolated actor (20 min), orchestrator logic |
| Implement basic CLI | `8oh` | **DIRECT** | Quick CLI wrapper (10 min), tightly coupled to daemon |
| **Phase 1b: Pattern Matching** |
| Implement predicate matching | `1wq` | **AGENT** | Complex algorithm (30 min), independent |
| Implement function registry | `6u5` | **AGENT** | Isolated actor (20 min), clear interface |
| **Phase 1c: Function Execution** |
| Implement code-based functions | `4mx` | **AGENT** | Core executor (25 min), complex logic |
| Implement loop prevention | `cne` | **DIRECT** | Requires testing/iteration (30 min), 4 mechanisms |
| **Phase 1d: HTTP Interface** |
| Implement HTTP server | `hre` | **AGENT** | Isolated actor (20 min), API endpoints |
| **Phase 1e: Agent Functions** |
| Implement agent function type | `dmm` | **DIRECT** | Extends executor (15 min), subprocess logic |
| **Phase 1f: Testing** |
| Create test harness | `0ic` | **DIRECT** | Integration testing (20 min), requires full system |

**Summary**:
- **7 AGENT tasks**: `soe`, `tkb`, `1wq`, `6u5`, `4mx`, `hre` (dmm if time)
- **4 DIRECT tasks**: `c3s`, `8oh`, `cne`, `0ic` (plus `dmm`)

### 2.3 Parallelization Opportunities

**Phase 1a** (4 tasks):
```
c3s (DIRECT) → [soe (AGENT), tkb (AGENT)] → 8oh (DIRECT)
```
- Do `c3s` first (foundation)
- Spawn **2 parallel agents** for `soe` and `tkb` (once `c3s` done)
- Do `8oh` after `tkb` completes (depends on daemon)

**Phase 1b** (2 tasks):
```
[1wq (AGENT), 6u5 (AGENT)] in parallel
```
- Both depend only on `c3s` (UAP)
- Spawn **2 parallel agents** immediately after `c3s`
- Can overlap with Phase 1a agents

**Phase 1c** (2 tasks):
```
4mx (AGENT) → cne (DIRECT)
```
- Do `4mx` as agent first
- Do `cne` directly (needs iteration)

**Phase 1d-1f** (3 tasks):
```
hre (AGENT) in parallel with dmm → 0ic (DIRECT)
```
- `hre` can run as agent (parallel with other work)
- `dmm` extends `4mx` (do directly)
- `0ic` requires everything (do last, directly)

**Maximum Parallelism**:
- **4 agents simultaneously**: `soe`, `tkb`, `1wq`, `6u5` (after `c3s` completes)
- **Agent budget**: 4-6 concurrent agents max (avoid overwhelming system)

---

## 3. Agent Orchestration Pattern

### 3.1 Agent Lifecycle

**Workflow**:
```
1. Identify agent-worthy tasks → bd ready + filter
2. Prepare task context → Extract description + acceptance criteria
3. Spawn agent → /bg <task-description>
4. Track agent → Store agent ID + task mapping
5. Monitor progress → Periodic check or wait
6. Validate completion → Review output, close task
7. Unblock dependents → Hook shows newly-ready tasks
```

### 3.2 Spawning Agents

**Pattern**:
```bash
# 1. Check ready tasks
bd ready --format json > /tmp/ready.json

# 2. Filter for agent-worthy event-system tasks
jq '[.[] | select(.labels | contains(["event-system"])) | select(.priority == "P1")]' /tmp/ready.json

# 3. For each agent-worthy task:
TASK_ID="soe"
TASK_DESCRIPTION=$(bd show $TASK_ID --format json | jq -r '.description')

# 4. Spawn agent with full context
/bg "Implement event log actor (task agentic-primer-soe):

$TASK_DESCRIPTION

Deliverables:
- /Users/bln/play/agentic-primer/.wt/event-system/src/actors/event-log.js

When done:
- Run: bd update soe --status closed
- Report: Completion status and any issues"

# 5. Track agent
echo "Agent spawned for task soe at $(date)" >> .agents.log
```

### 3.3 Agent Coordination Algorithm

**Pseudo-code**:
```python
def execute_phase(phase_label):
    """Execute a phase with maximum parallelization"""

    # 1. Get ready tasks for this phase
    ready_tasks = bd_ready(label=phase_label, status="open")

    # 2. Classify tasks
    agent_tasks = [t for t in ready_tasks if is_agent_worthy(t)]
    direct_tasks = [t for t in ready_tasks if not is_agent_worthy(t)]

    # 3. Spawn agents (max 4 at once)
    agents = []
    for task in agent_tasks[:4]:  # Limit concurrent agents
        agent = spawn_agent(task)
        agents.append({"task": task.id, "agent": agent})

    # 4. Work on direct tasks while agents run
    for task in direct_tasks:
        execute_direct(task)
        bd_close(task.id)

    # 5. Wait for agents to complete
    for agent in agents:
        wait_for_agent(agent)
        validate_agent_output(agent.task)
        bd_close(agent.task)

    # 6. Check for newly-ready tasks (hook does this automatically)
    # Recurse or move to next phase

def is_agent_worthy(task):
    """Decision criteria for agent vs direct"""
    if task.estimated_time < 15:  # minutes
        return False
    if task.coupling == "tight":
        return False
    if task.type in ["setup", "config", "testing"]:
        return False
    return True
```

### 3.4 Practical Implementation Commands

**Phase 1a Execution**:
```bash
# Step 1: Foundation (DIRECT)
# Do c3s directly - it's quick and foundational
# Creates: package.json, config.json, .gitignore, src/protocol/uap.js

bd update c3s --status in_progress
# ... implement UAP and project structure ...
bd update c3s --status closed

# Hook fires: Shows soe, tkb, 1wq, 6u5 are ready

# Step 2: Spawn 4 parallel agents (AGENT)
/bg "Implement EventLogActor (agentic-primer-soe) - JSONL append-only storage with replay. See bd show soe for details. Close task when done."

/bg "Implement DaemonActor (agentic-primer-tkb) - Main orchestrator with start/stop/status. See bd show tkb for details. Close task when done."

/bg "Implement PatternMatcherActor (agentic-primer-1wq) - JavaScript predicate matching. See bd show 1wq for details. Close task when done."

/bg "Implement FunctionRegistryActor (agentic-primer-6u5) - Function catalog with auto-discovery. See bd show 6u5 for details. Close task when done."

# Step 3: Wait for soe + tkb to complete (8oh depends on tkb)
# Monitor agent progress via /task or by checking file creation

# Step 4: When tkb completes, do CLI directly (DIRECT)
bd update 8oh --status in_progress
# ... implement CLI wrapper ...
bd update 8oh --status closed

# Phase 1a complete! Hook shows Phase 1b ready.
```

**Phase 1b Execution**:
```bash
# 1wq and 6u5 agents already running from Phase 1a
# Just wait for completion and validate
```

**Phase 1c Execution**:
```bash
# Step 1: Spawn agent for executor (AGENT)
/bg "Implement FunctionActor executor (agentic-primer-4mx) - Dynamic ES module loading with context. See bd show 4mx for details. Close task when done."

# Step 2: While agent runs, prepare for loop prevention
# Wait for 4mx to complete

# Step 3: Do loop prevention directly (DIRECT) - needs iteration
bd update cne --status in_progress
# ... implement all 4 mechanisms, test, debug ...
bd update cne --status closed
```

**Phase 1d-1f Execution**:
```bash
# Step 1: Spawn HTTP agent (AGENT)
/bg "Implement HTTPServerActor (agentic-primer-hre) - Bun HTTP server with API endpoints. See bd show hre for details. Close task when done."

# Step 2: Do agent function type directly (DIRECT)
bd update dmm --status in_progress
# ... extend executor for Claude subprocess ...
bd update dmm --status closed

# Step 3: Do testing directly (DIRECT) - integration testing
bd update 0ic --status in_progress
# ... create test plan, examples, run full suite ...
bd update 0ic --status closed

# Hook fires: All tasks closed, MVP complete!
```

### 3.5 Agent Failure Handling

**Problem**: Agent fails or gets stuck (task stays `in_progress`)

**Detection**:
```bash
# Manual check
bd list --status in_progress

# Or periodic check (every 30 min)
watch -n 1800 'bd list --status in_progress'
```

**Recovery**:
```bash
# Option 1: Retry with new agent
bd update <task-id> --status open  # Reset
/bg "Retry task <task-id>: ..."

# Option 2: Take over manually
bd update <task-id> --status in_progress
# ... fix the issue directly ...
bd update <task-id> --status closed

# Option 3: Create sub-tasks (if agent revealed complexity)
bd create --title "Fix issue in <task-id>" --depends-on <task-id>
```

---

## 4. Integrated Implementation Loop

### 4.1 Enhanced Execution Loop

```
┌─────────────────────────────────────────────────────────────┐
│ STARTUP: Event System MVP Implementation                    │
│ - Check pre-flight: git status, bd ready, working directory │
│ - Load harness: Read IMPLEMENTATION_HARNESS.md              │
│ - Verify hook: Ensure PostToolUse hook installed            │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│ PHASE LOOP: For each phase (1a → 1b → 1c → 1d → 1e → 1f)  │
└─────────────────────────────────────────────────────────────┘
                           ↓
          ┌────────────────────────────────┐
          │ 1. CHECK READY                 │
          │ - Run: bd ready                │
          │ - Filter: Current phase tasks  │
          │ - Classify: Agent vs Direct    │
          └────────────────────────────────┘
                           ↓
          ┌────────────────────────────────┐
          │ 2. EXECUTE FOUNDATION          │
          │ - Do foundation tasks DIRECT   │
          │ - Example: c3s (UAP + setup)   │
          │ - Close when done              │
          │ → HOOK FIRES: Shows new ready  │
          └────────────────────────────────┘
                           ↓
          ┌────────────────────────────────┐
          │ 3. SPAWN AGENTS                │
          │ - Filter: Agent-worthy tasks   │
          │ - Limit: Max 4 concurrent      │
          │ - Command: /bg per task        │
          │ - Track: Log agent IDs         │
          └────────────────────────────────┘
                           ↓
          ┌────────────────────────────────┐
          │ 4. PARALLEL WORK               │
          │ - Agents: Run independently    │
          │ - Main: Do direct tasks        │
          │ - Main: Monitor agent progress │
          └────────────────────────────────┘
                           ↓
          ┌────────────────────────────────┐
          │ 5. VALIDATE & CLOSE            │
          │ - Review: Agent outputs        │
          │ - Test: Acceptance criteria    │
          │ - Close: bd update → closed    │
          │ → HOOK FIRES: Shows next ready │
          └────────────────────────────────┘
                           ↓
          ┌────────────────────────────────┐
          │ 6. PHASE GATE                  │
          │ - Check: All phase tasks done? │
          │ - If NO: Return to step 1      │
          │ - If YES: Celebrate, next phase│
          └────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│ COMPLETION: All phases done                                 │
│ - Run: bd show 7tq (verify epic complete)                   │
│ - Test: Execute full test plan (task 0ic)                   │
│ - Document: Update README with usage                        │
│ - Commit: Create commit with all changes                    │
└─────────────────────────────────────────────────────────────┘
```

### 4.2 Loop with Hook Integration

**How Hook Enhances Loop**:

| Loop Step | Without Hook | With Hook |
|-----------|--------------|-----------|
| **After closing task** | Manually run `bd ready` | Hook auto-shows ready tasks |
| **Parallelization** | Manually count ready tasks | Hook suggests "N tasks ready - use /bg" |
| **Phase completion** | Manually check phase status | Hook celebrates "Phase 1a complete!" |
| **Next step** | Manually decide what to do | Hook shows actionable next steps |

**Example Session Flow**:
```
User: Start implementing event system
Assistant: Let me begin with Phase 1a - foundation setup...

[Implements c3s - UAP and project structure]
bd update c3s --status closed

🔄 Checking for newly-ready tasks...  # ← HOOK OUTPUT

📋 Ready tasks (4):
1. [P1] agentic-primer-soe: Implement event log actor
2. [P1] agentic-primer-tkb: Implement daemon skeleton
3. [P1] agentic-primer-1wq: Implement predicate matching
4. [P1] agentic-primer-6u5: Implement function registry

💡 Parallelization check:
   → 4 event-system tasks ready - consider parallel agents
   → Use: /bg for background task agents
Assistant: Perfect! Let me spawn 4 agents for parallel execution...

/bg "soe: EventLogActor" # Spawns agent 1
/bg "tkb: DaemonActor"   # Spawns agent 2  
/bg "1wq: PatternMatcher" # Spawns agent 3
/bg "6u5: FunctionRegistry" # Spawns agent 4

Now working on CLI while agents run...
[Later: agents complete, hook fires again]

🔄 Checking for newly-ready tasks...

📋 Ready tasks (1):
1. [P1] agentic-primer-8oh: Implement basic CLI

[Implements 8oh directly]
bd update 8oh --status closed

🔄 Checking for newly-ready tasks...
   🎉 Phase 1a complete! Phase 1b tasks should be ready.

📋 Ready tasks (0): (1wq and 6u5 already handled by earlier agents)
```

**Benefits Demonstrated**:
1. Hook eliminates manual checking
2. Hook suggests parallelization opportunities
3. Hook celebrates phase completion
4. Reduced cognitive overhead
5. Immediate feedback loop

---

## 5. Quick Reference

### 5.1 Command Cheatsheet

```bash
# Check ready tasks
bd ready
bd ready --format json

# Filter by phase
bd list --label phase-1a --status open
bd list --label event-system --status open

# Show task details
bd show <task-id>

# Update status
bd update <task-id> --status in_progress
bd update <task-id> --status closed

# Spawn background agent
/bg "Task description with acceptance criteria"

# Check agent progress (if /bg provides task IDs)
# Monitor file creation or check beads status

# Check for stuck agents
bd list --status in_progress
```

### 5.2 Parallelization Decision Tree

```
Is task estimated > 15 minutes?
├─ NO → DO DIRECT
└─ YES
    │
    Is task tightly coupled to current work?
    ├─ YES → DO DIRECT
    └─ NO
        │
        Does task require iteration/debugging?
        ├─ YES → DO DIRECT
        └─ NO
            │
            Does task have clear acceptance criteria?
            ├─ YES → USE AGENT
            └─ NO → DO DIRECT (clarify first)
```

### 5.3 Phase Execution Strategy

| Phase | Lead Task | Parallel Agents | Direct Tasks | Estimate |
|-------|-----------|-----------------|--------------|----------|
| **1a** | c3s (DIRECT) | soe, tkb | 8oh | 60 min |
| **1b** | - | 1wq, 6u5 | - | 40 min |
| **1c** | 4mx (AGENT) | - | cne | 45 min |
| **1d** | hre (AGENT) | - | - | 20 min |
| **1e** | - | - | dmm | 15 min |
| **1f** | - | - | 0ic | 30 min |
| **TOTAL** | | | | **~3.5 hours** |

**With Parallelization**: Estimated 2-2.5 hours (30-40% faster)

---

## 6. Implementation Checklist

### Pre-Implementation
- [ ] Install PostToolUse hook
- [ ] Verify hook executable (`chmod +x`)
- [ ] Test hook with dummy task
- [ ] Review task dependencies (`bd show 7tq`)
- [ ] Verify all 11 tasks created
- [ ] Check working directory (`.wt/event-system`)

### Phase 1a: Foundation (60 min)
- [ ] Do c3s DIRECT: project structure + UAP
- [ ] Spawn agent: soe (EventLogActor)
- [ ] Spawn agent: tkb (DaemonActor)
- [ ] Wait for tkb completion
- [ ] Do 8oh DIRECT: CLI wrapper
- [ ] Verify: daemon starts/stops

### Phase 1b: Pattern Matching (40 min, overlaps with 1a)
- [ ] Spawn agent: 1wq (PatternMatcher) - already done in 1a
- [ ] Spawn agent: 6u5 (FunctionRegistry) - already done in 1a
- [ ] Validate: agents completed successfully
- [ ] Test: pattern registration and matching

### Phase 1c: Function Execution (45 min)
- [ ] Spawn agent: 4mx (FunctionExecutor)
- [ ] Wait for 4mx completion
- [ ] Do cne DIRECT: loop prevention (4 mechanisms)
- [ ] Test: each loop prevention mechanism

### Phase 1d: HTTP Interface (20 min)
- [ ] Spawn agent: hre (HTTPServerActor)
- [ ] Validate: endpoints respond
- [ ] Test: POST /events, GET /events

### Phase 1e: Agent Functions (15 min)
- [ ] Do dmm DIRECT: agent function type
- [ ] Create example: functions/analyze-error.agent.js
- [ ] Test: Claude CLI subprocess invocation

### Phase 1f: Testing (30 min)
- [ ] Do 0ic DIRECT: test harness
- [ ] Create manual test plan
- [ ] Create example functions (4)
- [ ] Execute full test suite
- [ ] Document results

### Post-Implementation
- [ ] Close epic: bd update 7tq --status closed
- [ ] Update README with usage instructions
- [ ] Commit all changes
- [ ] Tag release (optional): v0.1.0-mvp

---

## 7. Troubleshooting

### Hook Not Firing
**Symptom**: PostToolUse hook doesnt execute after bd commands

**Solutions**:
```bash
# Check hook exists
ls -la .claude/hooks/PostToolUse

# Check executable permission
chmod +x .claude/hooks/PostToolUse

# Test hook manually
bash .claude/hooks/PostToolUse "Bash" "bd ready"

# Check hook output
cat .claude/hooks/PostToolUse | head -20
```

### Too Many Agents
**Symptom**: System slow, agents conflicting

**Solutions**:
- Limit to 4 concurrent agents max
- Wait for agents to complete before spawning more
- Use `bd list --status in_progress` to check active work

### Agent Stuck
**Symptom**: Task stays in_progress for >30 minutes

**Solutions**:
```bash
# Check whats stuck
bd list --status in_progress

# Reset task
bd update <task-id> --status open

# Take over manually
bd update <task-id> --status in_progress
# ... do work ...
bd update <task-id> --status closed
```

### Dependency Confusion
**Symptom**: "Task X depends on Y" but Y is closed

**Solutions**:
```bash
# Check dependencies
bd show <task-id>

# Verify blocker status
bd show <blocker-id>

# If blocker closed, task should be ready
bd ready # Should show task
```

---

## 8. Success Metrics

### Implementation Velocity
- **Target**: Complete MVP in 2-2.5 hours (with parallelization)
- **Measure**: Track time from c3s start to 0ic completion
- **Baseline**: Sequential execution ~3.5 hours

### Agent Efficiency
- **Target**: 4-6 agents used throughout implementation
- **Measure**: Count `/bg` invocations
- **Baseline**: 7 agent-worthy tasks identified

### Hook Effectiveness
- **Target**: Zero manual `bd ready` queries (hook handles all)
- **Measure**: Count manual status checks
- **Baseline**: 10-15 manual checks expected without hook

### Completion Quality
- **Target**: All acceptance criteria met for all 11 tasks
- **Measure**: Pass/fail for each tasks acceptance criteria
- **Baseline**: 100% pass rate required

---

## 9. Next Steps After MVP

Once Phase 1 MVP is complete:

1. **Production Hardening**
   - Add error handling
   - Add logging (structured)
   - Add monitoring
   - Add graceful degradation

2. **Phase 2 Features**
   - Web UI for event browsing
   - Real-time event streaming (WebSockets)
   - Function marketplace/discovery
   - Multi-tenancy support

3. **Extract Harness Framework**
   - Generalize hook for any beads project
   - Create `/create-harness` skill
   - Document reusability pattern
   - Share with community

4. **Metrics Dashboard**
   - Track implementation velocity
   - Track agent efficiency
   - Track time savings from parallelization
   - A/B test with/without hook

---

## Appendix A: Task Dependency Graph

```
c3s (foundation)
 ├─→ soe (event log)
 │    └─→ cne (loop prevention)
 ├─→ tkb (daemon)
 │    └─→ 8oh (CLI)
 ├─→ 1wq (pattern matcher)
 │    └─→ 0ic (testing)
 ├─→ 6u5 (function registry)
 │    └─→ 4mx (executor)
 │         ├─→ cne (loop prevention)
 │         │    └─→ 0ic (testing)
 │         └─→ dmm (agent functions)
 │              └─→ 0ic (testing)
 └─→ hre (HTTP server)
      └─→ 0ic (testing)
```

**Critical Path**: c3s → 6u5 → 4mx → cne → 0ic (longest dependency chain)

**Parallel Paths**:
- c3s → soe (independent of critical path)
- c3s → tkb → 8oh (independent of critical path)
- c3s → 1wq (independent of critical path)
- c3s → hre (independent of critical path)

**Maximum Parallelism**: 4 tasks can run simultaneously after c3s completes

---

## Appendix B: Beads Query Patterns

Useful beads queries for this implementation:

```bash
# Show all event-system tasks
bd list --label event-system

# Show ready event-system tasks
bd ready | grep event-system

# Show tasks by phase
bd list --label phase-1a
bd list --label phase-1b
bd list --label phase-1c

# Show tasks by priority
bd list --priority P1 --status open

# Show whats blocking a task
bd show <task-id> | grep "Depends on"

# Show what a task is blocking
bd show <task-id> | grep "Blocks"

# Show all in-progress work
bd list --status in_progress

# Show all closed work (completed)
bd list --status closed --label event-system

# Export for analysis
bd list --format json > /tmp/tasks.json
jq [.[] | {id, title, status, priority, labels}] /tmp/tasks.json
```

---

## Conclusion

This harness provides:

1. **Automation**: PostToolUse hook reduces manual overhead
2. **Parallelization**: 7 agent-worthy tasks, 4 concurrent max
3. **Orchestration**: Clear pattern for spawning/tracking agents
4. **Integration**: Hook + agents + beads = seamless workflow

**Expected Results**:
- 30-40% faster implementation (2-2.5 hrs vs 3.5 hrs)
- Lower cognitive load (hook handles status tracking)
- Higher quality (clear acceptance criteria per task)
- Better visibility (immediate feedback on progress)

**Key Success Factors**:
- Install and test hook before starting
- Stick to agent vs direct criteria
- Limit concurrent agents (4 max)
- Validate each tasks acceptance criteria
- Use hook output to guide next steps

Ready to implement the Event Capture System MVP with maximum efficiency!

