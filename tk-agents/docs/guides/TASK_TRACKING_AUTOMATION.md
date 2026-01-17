# Task Tracking Automation Design

**Created:** 2026-01-16 (as of 19:14 EST)
**Status:** Design Document
**Version:** 1.0

---

## Problem Statement

### Current State

We have a fully functional Task CLI (`src/cli/task.ts`) with comprehensive features:
- JSON-based persistence (`tasks.json`)
- Hierarchical task graphs with dependencies
- Priority system (P0-P4)
- Labels and filtering
- State machine (created → ready → active → completed/failed)
- Batch operations and `--json` output

However, we are NOT using it consistently:

1. **Background agents aren't tracked as tasks**
   - Agents launched with `/bg` or `run_in_background: true` in Bash tool
   - No persistent record of what agents ran
   - Work history lost across session compactions

2. **Work tracked in TodoWrite (ephemeral)**
   - TodoWrite is session-only
   - Lost during conversation compaction
   - Cannot query "what agents ran last week?"

3. **No dependency tracking for agent work**
   - Can't see "Agent A blocked on Agent B's output"
   - Can't identify ready-to-work tasks

4. **Sub-agents not tracked**
   - Background agents spawn other agents
   - No visibility into agent hierarchy
   - No tracing of spawned work

### Impact

- Cannot query agent work history
- Cannot identify blockers or blocked work
- No persistent context across compactions
- Agent work invisible to task queries
- Missing audit trail for agent execution

---

## Requirements

### Must Have

1. **Automatic task creation for background agents**
   - Every background agent gets a task
   - Task captures: goal, agent type, deliverables
   - Timestamp tracking (started/completed)

2. **CLAUDE.md instructions for manual tracking**
   - Clear, explicit guidance
   - Pattern/template for task creation
   - Examples for common scenarios

3. **Migration of current in-flight work**
   - Create tasks for active agents (if any)
   - Document current state in tasks.json

4. **Sub-agent propagation pattern**
   - Child tasks linked to parent tasks
   - `spawned_by` edges for hierarchy
   - Labels for agent lineage

### Should Have

5. **Hook-based automation** (future)
   - PreToolUse hook detects `run_in_background: true`
   - Auto-create task from agent description
   - PostToolUse hook updates task status

6. **Task ID passing to sub-agents**
   - Parent task ID available to child agents
   - Child agents create linked tasks

### Nice to Have

7. **Agent metadata in tasks**
   - `agent_id` field
   - `agent_type` (research, implementation, etc.)
   - `deliverables` (files created)
   - Duration metrics

---

## Design Options

### Option 1: CLAUDE.md Instructions Only

**Description:**
Add explicit instructions to project CLAUDE.md that require manual task creation before launching agents.

**Pros:**
- Simple to implement (just documentation)
- No complex infrastructure needed
- Works immediately
- Easy to understand and follow
- User has full control

**Cons:**
- Manual, can be forgotten
- Relies on agent discipline
- No enforcement mechanism
- Sub-agents may not inherit pattern

**Implementation:**
```markdown
## Task Tracking for Agent Work

CRITICAL: All background agent work MUST be tracked in tasks.json

BEFORE launching a background agent:
1. Create task: `bun src/cli/task.ts add "Agent: <description>" --labels agent,<type> --priority <P0-P4>`
2. Note task ID
3. Launch agent (refer to task ID in commit/notes)
4. When agent completes, update task: `bun src/cli/task.ts update <id> complete`

Pattern:
```bash
# 1. Create task FIRST
bun src/cli/task.ts add "Research graph query patterns" --labels agent,research --priority P1

# 2. Launch agent
/bg <agent work>

# 3. On completion
bun src/cli/task.ts update task_5 complete
```

For sub-agents:
- Create child task with parent ID
- Use spawned_by relationship
```

---

### Option 2: Hook-Based Automation

**Description:**
Use Claude Code hooks (PreToolUse, PostToolUse) to automatically create and update tasks when agents launch.

**Pros:**
- Fully automatic
- Cannot be forgotten
- Consistent tracking
- Captures all agent metadata
- Works for sub-agents

**Cons:**
- Complex setup (hook scripts)
- Requires hook infrastructure
- Project-specific configuration
- Debugging is harder
- Hook maintenance overhead

**Implementation:**

**Hook Location:** `~/.claude/hooks/` (global) or `.claude/hooks/` (project)

**PreToolUse Hook (`pre-tool-use.sh`):**
```bash
#!/bin/bash
# Auto-create task when background agent launches

# Parse tool call data (available as JSON in environment)
TOOL_NAME="${CLAUDE_HOOK_TOOL_NAME}"
RUN_IN_BACKGROUND="${CLAUDE_HOOK_RUN_IN_BACKGROUND}"

# Check if this is a background agent
if [[ "$RUN_IN_BACKGROUND" == "true" ]]; then
  # Extract description from command
  DESCRIPTION="${CLAUDE_HOOK_DESCRIPTION}"

  # Generate task ID and create task
  cd "$CLAUDE_PROJECT_DIR"
  TASK_JSON=$(bun src/cli/task.ts add "Agent: $DESCRIPTION" \
    --labels agent,auto \
    --priority P2 \
    --json)

  TASK_ID=$(echo "$TASK_JSON" | jq -r '.data.id')

  # Store mapping for PostToolUse hook
  AGENT_ID="${CLAUDE_HOOK_AGENT_ID:-unknown}"
  echo "$AGENT_ID=$TASK_ID" >> ~/.claude/projects/*/hook-state/agent-task-map.txt

  echo "Created task $TASK_ID for agent"
fi

# Return continue signal
echo '{"continue": true}'
```

**PostToolUse Hook (`post-tool-use.sh`):**
```bash
#!/bin/bash
# Update task when agent completes

RUN_IN_BACKGROUND="${CLAUDE_HOOK_RUN_IN_BACKGROUND}"

if [[ "$RUN_IN_BACKGROUND" == "true" ]]; then
  AGENT_ID="${CLAUDE_HOOK_AGENT_ID}"

  # Lookup task ID from mapping
  TASK_ID=$(grep "^$AGENT_ID=" ~/.claude/projects/*/hook-state/agent-task-map.txt | cut -d= -f2)

  if [[ -n "$TASK_ID" ]]; then
    cd "$CLAUDE_PROJECT_DIR"
    bun src/cli/task.ts update "$TASK_ID" complete
    echo "Completed task $TASK_ID"
  fi
fi

echo '{"continue": true}'
```

**Challenges:**
- Hook environment variables may not be standardized
- Error handling for task creation failures
- Race conditions with concurrent agents
- Hook execution timing (before vs after tool)

---

### Option 3: Hybrid (RECOMMENDED)

**Description:**
Start with CLAUDE.md instructions for immediate use, add hooks later if automation proves valuable.

**Pros:**
- Best of both worlds
- Immediate solution (CLAUDE.md)
- Future automation path (hooks)
- Low risk, incremental rollout
- Learn usage patterns before automating

**Cons:**
- Two systems to maintain
- Manual work initially
- Potential inconsistency during transition

**Implementation:**

**Phase 1 (NOW):**
- Add CLAUDE.md instructions
- Migrate current agents to tasks
- Use manual workflow for 2-4 weeks

**Phase 2 (LATER):**
- Evaluate hook feasibility
- Implement PreToolUse hook if valuable
- Keep CLAUDE.md as fallback documentation

**Decision Criteria for Phase 2:**
- Are we consistently forgetting to create tasks?
- Do we have 10+ agent launches per week?
- Is manual tracking causing friction?

If YES to 2+ questions → implement hooks

---

## Recommended Approach: Hybrid (Immediate CLAUDE.md)

### Rationale

1. **Low Barrier to Entry:** Just add documentation
2. **Immediate Value:** Can start tracking today
3. **Learn Before Automating:** Understand patterns before building automation
4. **Flexibility:** Hook automation remains an option
5. **Simplicity:** Easier to debug and understand

### Success Metrics

- All new agent work tracked in tasks.json (100%)
- Zero work lost across compactions
- Can query agent history via `task list --label agent`
- Sub-agents properly linked with `spawned_by` edges

---

## Implementation Details

### CLAUDE.md Additions

**File:** `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/CLAUDE.md`

**Add after existing Bun instructions:**

```markdown
---

## Task Tracking for Agent Work

### CRITICAL REQUIREMENT

**All background agent work MUST be tracked in tasks.json before launching.**

This ensures:
- Work persists across conversation compactions
- Agent history is queryable
- Dependencies and blockers are visible
- Audit trail exists for all agent work

### Workflow Pattern

#### 1. BEFORE Launching Agent

Create a task that describes the agent's goal:

```bash
bun src/cli/task.ts add "Agent: <description>" \
  --labels agent,<type> \
  --priority <P0-P4>
```

**Labels:**
- Always include: `agent`
- Add type: `research`, `implementation`, `analysis`, `testing`, etc.

**Priority:**
- P0: Critical/blocking other work
- P1: High priority
- P2: Normal (default for most agent work)
- P3-P4: Lower priority/exploration

#### 2. Launch Agent

```bash
# Use /bg command or run_in_background
/bg <your agent prompt>
```

**Optional:** Reference task ID in commit message or notes

#### 3. AFTER Agent Completes

Update task status:

```bash
bun src/cli/task.ts update <task_id> complete
```

**If agent fails or gets blocked:**

```bash
bun src/cli/task.ts update <task_id> block "Reason for blocking"
```

### Sub-Agent Pattern

When a background agent spawns another agent:

```bash
# Parent agent creates child task
bun src/cli/task.ts add "Sub-agent: <description>" \
  --labels agent,child,<type> \
  --parent <parent_task_id> \
  --priority <inherit-from-parent>
```

This creates a `spawned_by` edge linking child → parent.

### Complete Example

```bash
# Research task
bun src/cli/task.ts add "Agent: Research Datalog query optimization patterns" \
  --labels agent,research \
  --priority P1

# Output: Added task: task_5

# Launch agent
/bg Research Datalog query optimization patterns for graph systems

# (Agent runs in background...)

# On completion
bun src/cli/task.ts update task_5 complete

# Output: Completed task task_5
```

### Query Agent Work

```bash
# List all agent tasks
bun src/cli/task.ts list --label agent

# Filter by type
bun src/cli/task.ts list --label research

# Show active agent work
bun src/cli/task.ts list --label agent --status active

# See agent dependency graph
bun src/cli/task.ts graph task_5
```

### Verification Checklist

Before launching ANY background agent, ask:
- [ ] Did I create a task first?
- [ ] Did I add the `agent` label?
- [ ] Did I set appropriate priority?
- [ ] For sub-agents: Did I link to parent task?

**If NO to any question: STOP and create the task first.**

---
```

---

### Migration Plan: Current In-Flight Work

**Context:** As of 2026-01-16 19:14 EST, check for active background agents.

#### Step 1: Identify Active Agents

```bash
# Check Claude project directories for active background tasks
# (This would normally list running agents)
```

**Placeholder for active agents:**
Based on task description, 3 agents were mentioned:
- `a9e9c7b`: Project reflection analysis
- `a8f57b5`: Graph query research
- `a9d4ef7`: CLI specs + wrapper

#### Step 2: Create Tasks for Active Agents

```bash
cd /Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents

# Agent 1: Project reflection
bun src/cli/task.ts add "Agent: Project reflection and architecture analysis" \
  --labels agent,analysis,reflection \
  --priority P1

# Agent 2: Graph query research
bun src/cli/task.ts add "Agent: Research graph query patterns and optimization" \
  --labels agent,research,graph \
  --priority P1

# Agent 3: CLI specs + wrapper
bun src/cli/task.ts add "Agent: Design CLI specifications and wrapper implementation" \
  --labels agent,implementation,cli \
  --priority P2
```

#### Step 3: Document Mapping

Create a mapping file for reference:

**File:** `docs/agent-task-mapping.md`

```markdown
# Agent-to-Task Mapping

Created: 2026-01-16

| Agent ID | Task ID | Description | Status | Notes |
|----------|---------|-------------|--------|-------|
| a9e9c7b | task_X | Project reflection analysis | active | Creating architecture analysis docs |
| a8f57b5 | task_Y | Graph query research | active | Researching Datalog optimization |
| a9d4ef7 | task_Z | CLI specs + wrapper | active | Designing task CLI extensions |

## Update Instructions

When agents complete:
```bash
bun src/cli/task.ts update task_X complete
```
```

#### Step 4: Verify Migration

```bash
# List all agent tasks
bun src/cli/task.ts list --label agent

# Should show all 3 migrated tasks
```

---

## Sub-Agent Propagation

### Pattern: Parent Creates Child Task

When a background agent needs to spawn a sub-agent:

```bash
# Inside parent agent context
PARENT_TASK_ID="task_5"

# Create child task
bun src/cli/task.ts add "Sub-agent: Implement specific feature X" \
  --labels agent,child,implementation \
  --parent "$PARENT_TASK_ID" \
  --priority P2

# This creates spawned_by edge: child → parent
```

### Pattern: Shared tasks.json

All agents (parent + children) read/write the same `tasks.json` file:

```
/project-root/
  tasks.json          # Shared task graph
  src/cli/task.ts     # CLI tool
```

**Benefits:**
- Single source of truth
- Automatic visibility across agents
- Parent can query child status
- No cross-agent synchronization needed

### Pattern: Agent Metadata

Tasks should capture agent context:

```typescript
// Extended task properties (future)
interface AgentTaskMetadata {
  agent_id?: string;           // Background agent ID
  agent_type?: string;         // research|implementation|analysis
  parent_agent_id?: string;    // Parent agent (if sub-agent)
  deliverables?: string[];     // Files created
  duration_seconds?: number;   // Execution time
  command?: string;            // Full agent command
}
```

Store as task properties or in `knownInformation`:

```bash
bun src/cli/task.ts add "Agent: Research X" \
  --known-info "agent_id=a9e9c7b" \
  --known-info "agent_type=research"
```

---

## Testing Plan

### Manual Testing (Phase 1)

1. **Test 1: Create agent task**
   ```bash
   bun src/cli/task.ts add "Agent: Test research task" --labels agent,research --priority P1
   # Verify: Task created with correct labels
   ```

2. **Test 2: Launch agent + complete**
   ```bash
   # Create task
   TASK_ID=$(bun src/cli/task.ts add "Agent: Quick test" --labels agent --priority P2 --json | jq -r '.data.id')

   # Launch mock agent
   echo "Agent work simulation" > test-output.txt

   # Complete task
   bun src/cli/task.ts update "$TASK_ID" complete

   # Verify: Task state = completed
   bun src/cli/task.ts show "$TASK_ID"
   ```

3. **Test 3: Sub-agent linkage**
   ```bash
   PARENT_ID=$(bun src/cli/task.ts add "Parent agent" --labels agent,parent --json | jq -r '.data.id')
   CHILD_ID=$(bun src/cli/task.ts add "Child agent" --labels agent,child --parent "$PARENT_ID" --json | jq -r '.data.id')

   # Verify: spawned_by edge exists
   bun src/cli/task.ts graph "$CHILD_ID"
   ```

4. **Test 4: Query agent work**
   ```bash
   # List all agents
   bun src/cli/task.ts list --label agent

   # Filter by type
   bun src/cli/task.ts list --label research
   ```

### Integration Testing (Phase 2 - Hooks)

```bash
# Test hook triggers
# 1. Launch background agent
# 2. Verify task auto-created
# 3. Agent completes
# 4. Verify task auto-updated
```

---

## Rollout Plan

### Phase 1: Documentation + Manual Workflow (Week 1-2)

**Day 1:**
- [ ] Add CLAUDE.md instructions ✅
- [ ] Migrate current in-flight agents
- [ ] Test task creation workflow

**Week 1:**
- [ ] Use manual workflow for ALL new agents
- [ ] Collect feedback on friction points
- [ ] Document edge cases

**Week 2:**
- [ ] Review adoption (are tasks being created?)
- [ ] Identify automation opportunities
- [ ] Decide: proceed to Phase 2 or iterate on docs

### Phase 2: Hook Automation (Week 3-4, if needed)

**Prerequisites:**
- [ ] Manual workflow working consistently
- [ ] Hook infrastructure understood
- [ ] 10+ agent launches tracked manually

**Implementation:**
- [ ] Write PreToolUse hook
- [ ] Test hook on single agent
- [ ] Add PostToolUse hook
- [ ] Test end-to-end automation
- [ ] Roll out to all agents

### Phase 3: Refinement (Ongoing)

- [ ] Add agent metadata fields
- [ ] Improve query capabilities
- [ ] Add dashboards/visualizations
- [ ] Integrate with external tools

---

## Success Criteria

### Must Achieve (Phase 1)

- [x] CLAUDE.md instructions added
- [ ] All new agent work tracked in tasks.json (100% rate)
- [ ] Zero work lost across compactions
- [ ] Can query: "What agents ran in last 7 days?"
- [ ] Sub-agents linked with `spawned_by` edges

### Should Achieve (Phase 2)

- [ ] Hook automation implemented
- [ ] Task creation fully automatic
- [ ] Agent metadata captured
- [ ] Zero manual tracking overhead

### Nice to Have (Future)

- [ ] Task analytics (agent duration, success rate)
- [ ] Dependency graph visualizations
- [ ] Integration with beads/other systems
- [ ] CI/CD hooks for agent testing

---

## Appendices

### A. Hooks System (Background Research)

Based on analysis of `~/.claude/.claude-backup/hook-logs/all-events.jsonl`:

**Hook Event Lifecycle:**
1. `UserPromptSubmit` → User sends message
2. `PreToolUse` → Before tool execution (can modify/block)
3. `PostToolUse` → After tool execution (can capture output)
4. `Stop` → Conversation stops
5. `Notification` → Background events

**Background Agent Detection:**
```json
{
  "event": "PreToolUse",
  "data": {
    "tool_name": "Bash",
    "tool_input": {
      "run_in_background": true,
      "description": "Launch agent for X"
    }
  }
}
```

**Hook Response Schema:**
```json
{
  "continue": true,
  "hookSpecificOutput": {
    "permissionDecision": "allow"  // PreToolUse only
  }
}
```

### B. Task CLI Quick Reference

```bash
# Create task
bun src/cli/task.ts add "<goal>" [--labels l1,l2] [--priority P0-P4] [--parent id]

# Update task
bun src/cli/task.ts update <id> start|complete|block ["reason"]

# List tasks
bun src/cli/task.ts list [--label L] [--status S] [--priority P]

# Show task details
bun src/cli/task.ts show <id>

# Dependency graph
bun src/cli/task.ts graph <id>

# Delete task
bun src/cli/task.ts delete <id> [--force]

# JSON output
bun src/cli/task.ts <command> --json
```

### C. Example Agent Task Workflow

```bash
# Research Agent
bun src/cli/task.ts add "Agent: Research error handling patterns in actor systems" \
  --labels agent,research,error-handling \
  --priority P1 \
  --deliverables "Research doc,Pattern catalog" \
  --criteria "patterns:count:10"

# Launch
/bg Research error handling patterns in Erlang, Akka, Orleans actor systems

# On completion
bun src/cli/task.ts update task_8 complete
bun src/cli/task.ts eval task_8  # Check criteria
```

---

## Document History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-16 | 1.0 | Initial design document |

---

## Next Steps

1. **Immediate:**
   - Add CLAUDE.md instructions (this document, Section: "CLAUDE.md Additions")
   - Test task creation workflow
   - Migrate any active agents

2. **Week 1:**
   - Use manual workflow consistently
   - Document lessons learned
   - Create agent-task-mapping.md

3. **Week 2:**
   - Review adoption metrics
   - Decide on Phase 2 (hooks)
   - Iterate on documentation

4. **Future:**
   - Implement hook automation (if valuable)
   - Add agent metadata
   - Build dashboards

---

## Questions for User

1. Are there currently active background agents that need migration?
2. Should we implement hooks immediately or start with manual workflow?
3. What agent metadata is most valuable to capture?
4. Any specific query patterns needed (e.g., "agents by deliverable type")?

---

**End of Design Document**
