# Agent-to-Task Mapping

**Created:** 2026-01-16 19:14 EST
**Purpose:** Track migration of active agents to task system

---

## Current Active Agents

| Agent ID | Task ID | Description | Status | Labels | Priority | Created |
|----------|---------|-------------|--------|--------|----------|---------|
| background-subagent | task_10 | Design task tracking automation | active | agent,design,background-subagent | P1 | 2026-01-16 |

---

## Historical Agents (Pre-Migration)

These agents were mentioned in the task description but may no longer be active:

| Agent Reference | Description | Status | Notes |
|----------------|-------------|--------|-------|
| a9e9c7b | Project reflection analysis | unknown | Mentioned in task context, not confirmed active |
| a8f57b5 | Graph query research | unknown | Mentioned in task context, not confirmed active |
| a9d4ef7 | CLI specs + wrapper | unknown | Mentioned in task context, not confirmed active |

**Note:** Task IDs will be assigned when/if these agents are confirmed active.

---

## Update Instructions

### When Agent Completes

```bash
# Update task status
bun src/cli/task.ts update <task_id> complete

# Example
bun src/cli/task.ts update task_10 complete
```

### When Agent Gets Blocked

```bash
# Mark as blocked with reason
bun src/cli/task.ts update <task_id> block "Reason for blocking"

# Example
bun src/cli/task.ts update task_10 block "Waiting for user input"
```

### Query Agent Status

```bash
# List all agent tasks
bun src/cli/task.ts list --label agent

# Show specific task
bun src/cli/task.ts show task_10

# View dependency graph
bun src/cli/task.ts graph task_10
```

---

## Migration Verification

```bash
# Count agent tasks
bun src/cli/task.ts list --label agent --json | jq '.data | length'

# Expected: 1 (this background agent)

# Show all agent tasks
bun src/cli/task.ts list --label agent
```

---

## Next Steps

1. **Immediate:** Complete this task (task_5) when deliverables are done
2. **Week 1:** Track all new agent launches
3. **Week 2:** Review adoption and iterate

---

## Deliverables Checklist

Task: task_10 (Design task tracking automation)

- [x] TASK_TRACKING_AUTOMATION.md created
- [x] CLAUDE.md updated with instructions
- [x] Hook scripts created (reference implementation)
- [x] hooks/README.md created
- [ ] This agent task completed
- [ ] User notified via COMPLETION_REPORT

---

## Document History

| Date | Changes |
|------|---------|
| 2026-01-16 19:14 EST | Initial mapping created, task_10 added for this agent |

---

**End of Mapping Document**
