# Subagent Protocols Quick Reference

**Fast lookup for parent agents monitoring background tasks**

---

## The Four Signals

### 1. CLARIFICATION_NEEDED - "I need information"

**When:** Subagent encounters missing information that blocks progress

**What to do:**
1. Extract questions from signal
2. Ask user (one by one per CLAUDE.md)
3. Resume with answers

**Template:**
```bash
Task resume="<agent-id>" prompt="
## CLARIFICATION RESPONSE
Q1: [question]
A1: [answer]

## RESUME INSTRUCTIONS
[Next steps]
"
```

---

### 2. STOP_WORK - "I cannot continue"

**When:** Subagent hits blocker (dependency, error, resource limit)

**What to do:**
1. Read blocker_type and details
2. Resolve blocker (install deps, fix error, reduce scope)
3. Resume with resolution

**Template:**
```bash
Task resume="<agent-id>" prompt="
## BLOCKER RESOLVED
Issue: [blocker]
Resolution: [what was done]

## STATE RESTORATION
[state_snapshot from signal]

## RESUME INSTRUCTIONS
[Next steps]
"
```

---

### 3. DELEGATE_WORK - "Launch another agent"

**When:** Subagent identifies parallelizable or specialized work

**What to do:**
1. Evaluate: Is delegation justified?
2. If yes: Launch new agent with /bg
3. Acknowledge to requesting agent

**Approve template:**
```bash
Task resume="<agent-id>" prompt="
## DELEGATION APPROVED
New agent: <new-agent-id>
Task: [description]

## RESUME INSTRUCTIONS
[How to proceed]
"
```

**Deny template:**
```bash
Task resume="<agent-id>" prompt="
## DELEGATION DENIED
Reason: [why]
Alternative: [what instead]

## RESUME INSTRUCTIONS
[How to proceed]
"
```

---

### 4. COMPLETION_REPORT - "I'm done"

**When:** Subagent finishes successfully

**What to do:**
1. Validate deliverables exist
2. Check metrics vs success criteria
3. Review recommendations
4. Notify user (no resume needed)

**User notification:**
```
✓ Background task completed

Agent ID: <id>
Duration: <time>
Deliverables: [list]
Summary: [what was done]
Recommendations: [follow-ups]
```

---

## Monitoring Workflow

### 1. Launch
```bash
/bg "Task with clear context, goals, and metrics"
# Command adds EXECUTION CONTEXT automatically
```

### 2. Poll
```bash
TaskOutput agent-id=<id> block=false
# Every 10-30 seconds
```

### 3. Detect
```bash
if output contains "[CLARIFICATION_NEEDED]"; then handle_clarification
if output contains "[STOP_WORK]"; then handle_stop
if output contains "[DELEGATE_WORK]"; then handle_delegation
if output contains "[COMPLETION_REPORT]"; then handle_completion
```

### 4. Respond
Use templates above based on signal type

---

## Signal Format (YAML)

### CLARIFICATION_NEEDED
```yaml
[CLARIFICATION_NEEDED]
agent_id: <id>
timestamp: <ISO-8601>
blocked_at: <where stopped>
reason: <why>
questions:
  - question_id: Q1
    text: <question>
    context: <why this matters>
can_resume_with: <what info needed>
current_state: <what completed>
[/CLARIFICATION_NEEDED]
```

### STOP_WORK
```yaml
[STOP_WORK]
agent_id: <id>
timestamp: <ISO-8601>
stop_reason: blocker|error|completion
blocker_type: missing_info|external_dependency|error|resource_limit
details: <description>
completed_work: <what finished>
blocked_work: <what couldn't start>
state_snapshot: <checkpoint>
resume_requirements: <what needed>
[/STOP_WORK]
```

### DELEGATE_WORK
```yaml
[DELEGATE_WORK]
agent_id: <id>
timestamp: <ISO-8601>
delegation_reason: <why delegate>
new_task_description: <what new agent should do>
independence: can_proceed_parallel|blocks_current_work|optional
priority: P0|P1|P2
context_required: <what new agent needs>
coordination: <how agents coordinate>
estimated_duration: <time>
[/DELEGATE_WORK]
```

### COMPLETION_REPORT
```yaml
[COMPLETION_REPORT]
agent_id: <id>
timestamp: <ISO-8601>
status: success|partial_success|failed
deliverables: <files created>
summary: <what accomplished>
metrics_achieved: <vs success criteria>
issues_encountered: <problems solved>
recommendations: <follow-ups>
total_duration: <time>
[/COMPLETION_REPORT]
```

---

## Common Scenarios

### Scenario 1: Missing Info
```
Agent → [CLARIFICATION_NEEDED]
Parent → Ask user questions
Parent → Resume with answers
Agent → Continues and completes
```

### Scenario 2: External Blocker
```
Agent → [STOP_WORK] (external_dependency)
Parent → Installs dependency
Parent → Resume with confirmation
Agent → Continues from checkpoint
```

### Scenario 3: Parallel Work
```
Agent → [DELEGATE_WORK] (can_proceed_parallel)
Parent → Launches new agent
Parent → Resume original agent
Both → Work in parallel
Both → Complete independently
Parent → Merges results
```

### Scenario 4: Sequential Dependency
```
Agent A → [DELEGATE_WORK] (blocks_current_work)
Parent → Launches Agent B
Agent A → Waits (stopped)
Agent B → Completes
Parent → Resume Agent A with B's results
Agent A → Continues and completes
```

---

## Troubleshooting Quick Fixes

### Agent doesn't signal
→ Check EXECUTION CONTEXT was added to prompt
→ Re-launch with /bg command (adds protocols automatically)

### Can't detect signals
→ Verify signal format (YAML syntax, delimiters)
→ Use grep to find signal markers
→ Increase polling frequency

### Resume doesn't restore state
→ Include state_snapshot in resume prompt
→ Reference completed work explicitly
→ Provide next steps clearly

### Delegated agents conflict
→ Verify independence claim before approving
→ Ensure separate resource boundaries
→ Use append-only files for shared data

---

## One-Line Reminders

| Do | Don't |
|----|-------|
| Monitor actively (poll every 10-30s) | Launch and forget |
| Respond to signals within 1-2 cycles | Ignore signals |
| Include state_snapshot when resuming | Resume without context |
| Validate independence before delegating | Auto-approve all delegations |
| Create checkpoints before stopping | Stop without saving state |
| Use protocols for all communication | Use AskUserQuestion in subagents |

---

## Full Documentation

- **SUBAGENT_PROTOCOLS.md** - Complete protocol specification
- **SUBAGENT_IMPLEMENTATION_GUIDE.md** - Detailed examples and patterns
- **~/.claude/commands/bg.md** - /bg command with integrated protocols

---

**Quick Start:** Just use `/bg` with clear task description - protocols are automatic!
