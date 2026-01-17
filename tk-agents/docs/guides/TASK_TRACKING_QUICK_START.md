# Task Tracking Quick Start

**Last Updated:** 2026-01-16 19:14 EST
**For:** Background Agents and Human Users

---

## The Pattern (3 Steps)

### Before Launching Agent

```bash
bun src/cli/task.ts add "Agent: <what agent will do>" \
  --labels agent,<type> \
  --priority <P0-P4>
```

**Example:**
```bash
bun src/cli/task.ts add "Agent: Research error handling patterns" \
  --labels agent,research \
  --priority P1

# Output: Added task: task_11
```

### Launch Agent

```bash
/bg <your agent prompt>
```

### After Agent Completes

```bash
bun src/cli/task.ts update task_11 complete
```

**If blocked:**
```bash
bun src/cli/task.ts update task_11 block "Reason"
```

---

## Label Guide

**Always include:** `agent`

**Add type:**
- `research` - Finding patterns, analyzing systems
- `implementation` - Writing code, building features
- `analysis` - Reviewing, critiquing, evaluating
- `testing` - Writing tests, validation
- `design` - Architecture, specifications
- `documentation` - Writing docs, guides

**Priority:**
- P0 - Critical/blocking
- P1 - High priority
- P2 - Normal (default)
- P3-P4 - Lower/exploration

---

## Query Commands

```bash
# All agent tasks
bun src/cli/task.ts list --label agent

# By type
bun src/cli/task.ts list --label research

# Active work
bun src/cli/task.ts list --label agent --status active

# Show details
bun src/cli/task.ts show task_11

# Dependency graph
bun src/cli/task.ts graph task_11
```

---

## Sub-Agent Pattern

When spawning a child agent:

```bash
bun src/cli/task.ts add "Sub-agent: <description>" \
  --labels agent,child,<type> \
  --parent <parent_task_id>
```

This creates a `spawned_by` edge.

---

## Why This Matters

- **Survives compaction:** Tasks persist in tasks.json
- **Queryable history:** "What agents ran last week?"
- **Dependency tracking:** See blockers and dependencies
- **Audit trail:** Complete record of agent work

---

## Full Documentation

- **Design:** `TASK_TRACKING_AUTOMATION.md`
- **CLI Reference:** `src/cli/TASK_CLI.spec.md`
- **Instructions:** `CLAUDE.md` (search "Task Tracking")
- **Hooks:** `hooks/README.md` (future automation)
- **Completion Report:** `COMPLETION_REPORT.md`

---

## Checklist

Before launching ANY background agent:

- [ ] Created task with `--labels agent,<type>`?
- [ ] Set priority (P0-P4)?
- [ ] For sub-agents: used `--parent`?

**If NO to any: STOP and create task first.**

---

**That's it! Simple 3-step pattern for persistent agent tracking.**
