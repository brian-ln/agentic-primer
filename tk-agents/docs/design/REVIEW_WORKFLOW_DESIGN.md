# Review Workflow Design

**Created:** 2026-01-16 19:16 EST
**Version:** 1.0
**Status:** Design Specification

---

## Problem Statement

### User Pain Points (as of 2026-01-16)

The user is experiencing **information overload** from parallel agent execution:

> "I'm drowning in a firestorm of tokens" - User (bln)

**Specific Issues:**

1. **No Review Tracking**
   - 4+ agents running in parallel
   - Multiple deliverables per agent (22+ MD files in root alone)
   - Agent completes → announces → user hears about it
   - NO task assigned for reviewing deliverables
   - NO way to mark "reviewed" vs "pending review"

2. **Cognitive Overload**
   - Cannot see "what needs my attention NOW"
   - No prioritization of review work
   - No distinction between blocking vs FYI reviews
   - Lost track of what's been completed vs what needs action

3. **Lack of Accountability**
   - Deliverables announced but not formally handed off
   - No closure loop ("I reviewed this, accept/reject")
   - No tracking of review decisions

4. **Activity Invisibility**
   - Cannot see active agents at a glance
   - Cannot see pending reviews
   - Cannot see completed work today/this week
   - No dashboard or overview

### Impact

- User paralyzed by volume of work
- Important reviews may be missed
- No clear "next action"
- Agents keep producing without feedback loop
- Context loss ("what was this agent working on again?")

---

## Solution Overview

**Core Principle:** **Every agent completion creates a review task that blocks dependent work until user approves.**

### Three-Part Solution

1. **Review Task Pattern** - Systematic creation of review tasks
2. **Priority Triage System** - What needs attention NOW vs later
3. **Activity Dashboard** - Visibility into all work streams

### Workflow Flow

```
Agent Completes
     ↓
Create Review Task (automatic via CLAUDE.md guidance)
     ↓
User Sees: "Review: <deliverable>" (P0 if blocking)
     ↓
User Reviews Deliverables
     ↓
User Marks Complete or Requests Changes
     ↓
Downstream Work Unblocks (if applicable)
```

---

## Review Task Pattern

### Task Structure

Every agent completion generates a review task with this structure:

```bash
task add "Review: <deliverable-name>" \
  --parent <agent-task-id> \
  --labels review,<priority-level> \
  --priority <P0-P3> \
  --deliverables "<file1>,<file2>,<fileN>" \
  --assignee bln
```

### Task Fields Explained

| Field | Purpose | Example |
|-------|---------|---------|
| Goal | "Review: X" format | "Review: Task CLI optimizations" |
| Parent | Links to agent task | task_agent_a8f57b5 |
| Labels | Always includes `review` | review, blocking, cli |
| Priority | P0-P3 (see triage below) | P0 (blocking other work) |
| Deliverables | Files to review | "src/cli/task.ts, P0_FEATURES_IMPLEMENTED.md" |
| Assignee | User (bln) | bln |

### Task Creation Timing

**WHEN:** Immediately when agent completes or announces deliverables

**WHO CREATES:**
- **Option A:** Agent creates own review task before exit (requires CLAUDE.md guidance)
- **Option B:** Parent session creates review task on agent announcement (manual)
- **Option C:** Hook automation (future Phase 2)

**RECOMMENDED:** Option A (agent self-creates review task)

### Example: Agent Self-Creates Review Task

```markdown
## Agent Completion Protocol

BEFORE announcing completion:

1. Create review task for your deliverables
2. Link to your own agent task (parent)
3. Set priority based on blocking status
4. List all deliverable files

```bash
# Agent creates review task
task add "Review: Graph query optimization research" \
  --parent task_agent_a8f57b5 \
  --labels review,research \
  --priority P1 \
  --deliverables "GRAPH_QUERY_RESEARCH.md,examples/optimizations/" \
  --assignee bln
```

5. Announce completion WITH review task ID
```

**Agent Announcement Format:**

```
COMPLETION: Graph query optimization research

Deliverables:
- GRAPH_QUERY_RESEARCH.md (57KB, optimization patterns)
- examples/optimizations/ (5 examples)

Review Task: task_review_15 (P1, research label)

Status: COMPLETE - Ready for review
```

---

## Review Types & Priority

### Priority Triage Rules

| Priority | Review Type | When to Use | Blocks Work? | Response Time |
|----------|-------------|-------------|--------------|---------------|
| **P0** | Blocking | Deliverables block other tasks, critical path work, security/breaking changes | YES | Immediate (< 1 hour) |
| **P1** | High | Feature work used by other agents, significant code changes | MAYBE | Same day (< 8 hours) |
| **P2** | Normal | Research/documentation, non-blocking improvements | NO | Within 2 days |
| **P3** | Low | Experiments, draft work, background research | NO | When convenient |

### P0 (Blocking) Reviews

**Criteria for P0:**
- Other agents waiting on this deliverable
- Critical path work (launch blocker)
- Security vulnerabilities or breaking changes
- User explicitly requested immediate feedback
- Integration work that blocks testing

**Examples:**
- Task CLI implementation (blocks other agents from creating tasks)
- Actor system core (blocks all dependent agents)
- Bug fixes in production code

**SLA:** Review within 1 hour of completion

### P1 (High) Reviews

**Criteria for P1:**
- Feature work that will be used soon
- Significant code changes (500+ LOC)
- Architecture decisions documented
- Research informing next sprint

**Examples:**
- Graph query research (informs future implementation)
- CLI design specs (guides implementation)
- Test suite improvements

**SLA:** Review same day (within 8 hours)

### P2 (Normal) Reviews

**Criteria for P2:**
- Research documentation
- Non-blocking improvements
- Refactoring with passing tests
- Documentation updates

**Examples:**
- Project reflection analysis
- Markdown-as-graph exploration
- Performance optimization research

**SLA:** Review within 2 days

### P3 (Low) Reviews

**Criteria for P3:**
- Experimental/exploratory work
- Draft documents
- Background research
- Ideas for future consideration

**Examples:**
- Actor model variations (not chosen)
- Speculative optimizations
- Literature reviews

**SLA:** Review when convenient (no deadline)

---

## Review Task Lifecycle

### State Machine

```
created → assigned → in_review → approved/rejected → completed
```

### States Explained

| State | Meaning | User Action | Next State |
|-------|---------|-------------|------------|
| created | Review task exists | None yet | assigned |
| assigned | User has been notified | User acknowledges | in_review |
| in_review | User actively reviewing | Reviewing deliverables | approved/rejected |
| approved | Deliverables accepted | Mark complete | completed |
| rejected | Changes needed | Request revisions | (new task created) |
| completed | Review closed | None | (final) |

### Transition Commands

```bash
# User starts review
task update task_review_15 start

# User approves
task update task_review_15 complete "Approved - looks good"

# User requests changes
task update task_review_15 block "Need clarification on error handling"
# Then create new task for agent to address feedback
task add "Address review feedback: error handling clarification" \
  --parent task_review_15 \
  --depends task_agent_a8f57b5 \
  --priority P1
```

---

## Review Workflow: Step-by-Step

### Step 1: Agent Completes Work

Agent finishes implementation/research and has deliverables ready.

### Step 2: Agent Creates Review Task

```bash
# Agent runs this before announcing
task add "Review: <concise-description>" \
  --parent <agent-task-id> \
  --labels review,<type> \
  --priority <P0-P3> \
  --deliverables "<comma-separated-files>" \
  --assignee bln
```

**Output:** `task_review_XX`

### Step 3: Agent Announces Completion

Agent includes review task ID in completion announcement:

```
COMPLETION: <work-description>

Deliverables:
- <file1> (<size>, <description>)
- <file2> (<size>, <description>)

Review Task: task_review_XX (<priority>, <labels>)

Status: COMPLETE - Ready for review
```

### Step 4: User Triages Review

User checks priority and decides when to review:

```bash
# See all pending reviews
task list --label review --status created

# Filter by priority
task list --label review --priority P0
```

### Step 5: User Reviews Deliverables

User opens files, reads code/docs, tests functionality.

```bash
# Start review (optional, marks in-progress)
task update task_review_XX start

# While reviewing:
- Read deliverable files
- Test code changes
- Verify against requirements
- Check for issues/questions
```

### Step 6: User Provides Feedback

**If Approved:**
```bash
task update task_review_XX complete "Approved - excellent work on X"
```

**If Changes Needed:**
```bash
# Block the review task
task update task_review_XX block "Need changes: <specific-feedback>"

# Create follow-up task
task add "Address review feedback: <issue>" \
  --parent task_review_XX \
  --depends <original-agent-task> \
  --priority P1 \
  --assignee <agent-or-bln>
```

### Step 7: Dependent Work Unblocks

If other tasks depended on this review:

```bash
# Check what was blocked
task list --status blocked

# Dependencies automatically resolve when review completes
```

---

## Integration with Task CLI

### New Commands for Review Workflow

**Proposed additions to task CLI:**

```bash
# List all pending reviews (sugar for list --label review --status created)
task reviews

# List blocking reviews (P0 only)
task reviews --blocking

# Show review details with deliverables
task review-show task_review_XX

# Approve review (sugar for update complete)
task review-approve task_review_XX "Looks good"

# Request changes (creates follow-up task automatically)
task review-reject task_review_XX "Need fix for X"
```

### Task Queries for Reviews

```bash
# All reviews
task list --label review

# Pending reviews
task list --label review --status created

# Blocking reviews (P0)
task list --label review --priority P0

# Reviews by agent type
task list --label review,research

# My assigned reviews
task list --assignee bln --label review

# Reviews completed today
task list --label review --status completed  # filter by completedAt
```

### Dependency Tracking

```bash
# Create downstream task that depends on review
task add "Integrate graph query optimizations" \
  --depends task_review_15 \
  --priority P1

# This task will show as blocked until review completes
task status task_new

# Output:
# Status: blocked
# Blockers:
#   - task_review_15 (review, P1) - not completed
```

---

## Examples

### Example 1: Blocking Review (P0)

**Scenario:** Agent implements critical Task CLI features that other agents need.

```bash
# Agent task (already created)
task_agent_a574515: "Agent: Implement Task CLI P0 features"

# Agent completes and creates review task
task add "Review: Task CLI P0 features implementation" \
  --parent task_agent_a574515 \
  --labels review,blocking,cli \
  --priority P0 \
  --deliverables "src/cli/task.ts,P0_FEATURES_IMPLEMENTED.md,examples/task-usage/" \
  --assignee bln

# Output: task_review_20

# Agent announces
COMPLETION: Task CLI P0 features implementation

Deliverables:
- src/cli/task.ts (1353 LOC, full lifecycle + batch ops)
- P0_FEATURES_IMPLEMENTED.md (11KB, implementation details)
- examples/task-usage/ (5 examples)

Review Task: task_review_20 (P0, blocking)

Status: COMPLETE - Ready for review

# User sees this in primer dashboard
🔴 P0 Review: Task CLI P0 features
   → Deliverables: src/cli/task.ts, P0_FEATURES_IMPLEMENTED.md, examples/
   → Blocking: 2 tasks waiting on this review

# User reviews immediately (P0 SLA: < 1 hour)
task update task_review_20 start
# ... reads code, tests CLI ...
task update task_review_20 complete "Approved - CLI works perfectly"

# Downstream tasks unblock automatically
```

### Example 2: Research Review (P1)

**Scenario:** Agent completes graph query research.

```bash
# Agent task
task_agent_a8f57b5: "Agent: Research graph query patterns"

# Agent completes and creates review task
task add "Review: Graph query optimization research" \
  --parent task_agent_a8f57b5 \
  --labels review,research,graph \
  --priority P1 \
  --deliverables "GRAPH_QUERY_RESEARCH.md,examples/query-patterns/" \
  --assignee bln

# Output: task_review_21

# User reviews within 8 hours (P1 SLA)
task update task_review_21 complete "Excellent research - will inform Phase 2"
```

### Example 3: Review with Changes Requested

**Scenario:** User finds issues in deliverables.

```bash
# User reviews
task update task_review_22 start
# ... finds issues ...

# User requests changes
task update task_review_22 block "Error handling incomplete - see lines 45-67"

# Create follow-up task
task add "Fix error handling in CLI implementation" \
  --parent task_review_22 \
  --depends task_agent_a574515 \
  --priority P1 \
  --assignee bln  # or agent

# Output: task_followup_23

# After fix
task update task_followup_23 complete "Fixed error handling"
task update task_review_22 complete "Approved after fixes"
```

---

## CLAUDE.md Integration

### Instructions for Agents

Add to project CLAUDE.md:

```markdown
## Agent Completion Protocol

### CRITICAL: Create Review Task Before Announcing Completion

When you complete agent work:

1. **Create review task FIRST:**

```bash
task add "Review: <concise-description>" \
  --parent <your-agent-task-id> \
  --labels review,<work-type> \
  --priority <P0-P3> \
  --deliverables "<file1>,<file2>" \
  --assignee bln
```

2. **Priority guide:**
   - P0: Blocks other work, critical path
   - P1: Feature work, significant changes
   - P2: Research, documentation (default)
   - P3: Experiments, drafts

3. **Announce completion WITH review task ID:**

```
COMPLETION: <work-description>

Deliverables:
- <file1> (<size>, <description>)
- <file2> (<size>, <description>)

Review Task: task_review_XX (<priority>, <labels>)

Status: COMPLETE - Ready for review
```

### Example

```bash
# Agent completed graph research
task add "Review: Graph query optimization research" \
  --parent task_agent_a8f57b5 \
  --labels review,research,graph \
  --priority P1 \
  --deliverables "GRAPH_QUERY_RESEARCH.md,examples/" \
  --assignee bln

# Announce
COMPLETION: Graph query optimization research

Deliverables:
- GRAPH_QUERY_RESEARCH.md (57KB, 10 patterns)
- examples/ (5 query examples)

Review Task: task_review_21 (P1, research)

Status: COMPLETE - Ready for review
```
```

---

## Future Enhancements

### Phase 2: Hook Automation

Use Claude Code hooks to auto-create review tasks:

**PostToolUse Hook:**
```bash
#!/bin/bash
# Detect agent completion
if [[ "$TASK_OUTPUT" == *"COMPLETION:"* ]]; then
  # Extract details from completion message
  # Auto-create review task
  task add "Review: $DESCRIPTION" --parent $AGENT_TASK_ID --labels review --priority P2
fi
```

### Phase 3: Review Analytics

```bash
# Review metrics
task review-stats
# Output:
# Total reviews: 42
# Avg time to review: 3.2 hours
# Approval rate: 85%
# Top reviewers: bln (42), agent_x (5)

# Review bottlenecks
task review-bottlenecks
# Output:
# Blocked on reviews:
#   - task_123 (waiting 2 days on review_45)
#   - task_124 (waiting 1 day on review_46)
```

---

## Success Criteria

### Must Achieve

- [ ] Every agent completion creates a review task
- [ ] User can see all pending reviews in one command
- [ ] Blocking reviews (P0) surface at top
- [ ] Downstream work blocks until review completes
- [ ] Review approval/rejection tracked

### Should Achieve

- [ ] Review tasks created automatically via CLAUDE.md protocol
- [ ] 90%+ of reviews have clear priority
- [ ] Average time-to-review meets SLAs (P0 < 1hr, P1 < 8hr)

### Nice to Have

- [ ] Hook automation for review task creation
- [ ] Review analytics dashboard
- [ ] Email/notification on P0 reviews

---

## Related Documents

- [ACTIVITY_DASHBOARD_DESIGN.md](./ACTIVITY_DASHBOARD_DESIGN.md) - Dashboard for visibility
- [IMPLEMENTATION_PLAN.md](./IMPLEMENTATION_PLAN.md) - Rollout plan
- [TASK_TRACKING_AUTOMATION.md](./TASK_TRACKING_AUTOMATION.md) - Agent task tracking

---

## Document History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-16 19:16 EST | 1.0 | Initial design specification |

---

**END OF DOCUMENT**
