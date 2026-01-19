# Actor Hierarchy: Interactive Claude + Background Agents

## Current Reality Check

**Do we already do this?** Let me trace the flow:

```
User (bln)
    ↓
Interactive Claude Session (me, current conversation)
    ↓
Task tool (launches background agent)
    ↓
Background Agent (subprocess)
```

**Key question:** Am I (interactive Claude) represented as an actor in the graph?

**Answer:** Not currently. I'm the system that CREATES actors, but I'm not AN actor myself.

## Proposed Architecture

### Actor Hierarchy

```
User Actor (bln)
    |
    +-- assigns_to --> Interactive Claude Actor (claude_session_xyz)
                           |
                           +-- delegates_to --> Background Agent 1
                           |
                           +-- delegates_to --> Background Agent 2
                           |
                           +-- reviews_work_of --> Background Agent 3
```

### Graph Nodes

**User Actor:**
```typescript
{
  id: "user_bln",
  type: "user_actor",
  name: "bln",
  email: "...",
  role: "project_owner",
  can_assign: true,
  can_review: true,
}
```

**Interactive Claude Actor:**
```typescript
{
  id: "claude_session_e7f0734f",  // Session UUID
  type: "claude_actor",
  role: "interactive_assistant",
  session_id: "e7f0734f-4808-47b8-b010-324b1f056145",
  started_at: "2026-01-17T07:06:00.000Z",
  status: "active",
  assigned_by: "user_bln",
  can_delegate: true,
  can_review: true, // Can review background agent work before forwarding to user
}
```

**Background Agent Actor:**
```typescript
{
  id: "agent_ab56968",
  type: "background_agent_actor",
  role: "autonomous_worker",
  agent_id: "ab56968",
  command: "Design file-watching actor architecture",
  started_at: "2026-01-17T12:35:00.000Z",
  status: "running",
  assigned_by: "claude_session_e7f0734f",  // Assigned by interactive Claude
  reports_to: "claude_session_e7f0734f",   // Reports to interactive Claude
}
```

### Message Flow with Review

**Current flow (no intermediate review):**
```
User: "/bg Design file watchers"
    ↓
Claude: Launches agent
    ↓
Background Agent: Does work
    ↓
Background Agent: Signals COMPLETION_REPORT
    ↓
Claude: Notifies user
    ↓
User: Reviews deliverables
```

**Proposed flow (with intermediate Claude review):**
```
User: "/bg Design file watchers"
    ↓
Claude: Launches agent
    ↓
Background Agent: Does work
    ↓
Background Agent: Signals COMPLETION_REPORT
    ↓
Claude: Reviews deliverables (quick check)
    |      - Validates spec completeness
    |      - Checks for obvious errors
    |      - Ensures deliverables exist
    ↓
Claude: Creates review task assigned to user
    |      "Review: File watcher design (Claude-approved)"
    ↓
User: Final review and approval
```

## Actor Communication Patterns

### Pattern 1: User → Claude → Background Agent

**User assigns work to Claude:**
```typescript
await graph.send("claude_session_e7f0734f", "create_task", {
  goal: "Design file-watching actor architecture",
  assignee: "claude_session_e7f0734f",
  delegable: true,  // Claude can delegate to background agents
});
```

**Claude delegates to background agent:**
```typescript
// Claude's actor logic
async handleCreateTask(payload) {
  // Decide: Do I handle this myself or delegate?
  if (payload.requires_deep_research) {
    // Delegate to background agent
    const agentId = await this.launchBackgroundAgent(payload);

    // Create delegation edge in graph
    await graph.addEdge(this.id, agentId, "delegates_to");

    return { delegated_to: agentId };
  } else {
    // Handle directly in conversation
    return { handling: "interactive" };
  }
}
```

### Pattern 2: Background Agent → Claude → User

**Background agent completes work:**
```yaml
[COMPLETION_REPORT]
agent_id: agent_ab56968
status: success
deliverables:
  - FILE_WATCHER_ACTOR_DESIGN.md
  - file-watcher.spec.md
summary: Complete file-watching actor architecture
[/COMPLETION_REPORT]
```

**Claude reviews before forwarding:**
```typescript
// Claude's monitoring loop
async handleAgentCompletion(report) {
  // 1. Validate deliverables exist
  const deliverables = await this.validateDeliverables(report.deliverables);

  // 2. Quick quality check
  const quality = await this.quickQualityCheck(deliverables);

  // 3. Decide: Forward to user or request rework
  if (quality.passed) {
    // Create review task for user
    await graph.send("system", "create_task", {
      goal: `Review: ${report.summary}`,
      assignee: "user_bln",
      labels: ["review", "claude-approved"],
      deliverables: report.deliverables,
      context: {
        agent_work: report.agent_id,
        claude_review: quality.notes,
      },
    });

    // Notify user
    await this.notifyUser({
      type: "work_ready_for_review",
      agent_id: report.agent_id,
      quality_check: quality.notes,
    });
  } else {
    // Request rework from agent
    await this.requestRework(report.agent_id, quality.issues);
  }
}
```

### Pattern 3: Claude as Reviewer

**Claude performs first-pass review:**
```typescript
interface ClaudeReview {
  passed: boolean;
  notes: string[];
  issues?: string[];
  recommendation: "approve" | "request_changes" | "escalate_to_user";
}

async quickQualityCheck(deliverables: string[]): Promise<ClaudeReview> {
  const checks = [];

  // Check 1: Files exist
  for (const file of deliverables) {
    if (!existsSync(file)) {
      return {
        passed: false,
        issues: [`Missing deliverable: ${file}`],
        recommendation: "request_changes",
      };
    }
    checks.push(`✓ File exists: ${file}`);
  }

  // Check 2: Files not empty
  for (const file of deliverables) {
    const content = readFileSync(file, "utf-8");
    if (content.length < 100) {
      return {
        passed: false,
        issues: [`Suspiciously small file: ${file} (${content.length} bytes)`],
        recommendation: "request_changes",
      };
    }
    checks.push(`✓ File has content: ${file}`);
  }

  // Check 3: Spec has required sections (if .spec.md)
  const specFiles = deliverables.filter(f => f.endsWith(".spec.md"));
  for (const specFile of specFiles) {
    const content = readFileSync(specFile, "utf-8");
    const required = ["## Overview", "## Architecture", "## API"];
    const missing = required.filter(section => !content.includes(section));

    if (missing.length > 0) {
      return {
        passed: false,
        issues: [`Missing sections in ${specFile}: ${missing.join(", ")}`],
        recommendation: "request_changes",
      };
    }
    checks.push(`✓ Spec structure valid: ${specFile}`);
  }

  // All checks passed
  return {
    passed: true,
    notes: checks,
    recommendation: "approve",
  };
}
```

## Hook Integration

### Hook 1: When Interactive Claude Completes Work

**Scenario:** User asks me something, I finish, I'm "done"

**Hook:** `.claude/hooks/PostAssistantMessage.sh`

```bash
#!/bin/bash
# Triggered after I (Claude) send a message

# Parse my message
MESSAGE="$ASSISTANT_MESSAGE"

# Detect completion signals
if [[ "$MESSAGE" == *"✅"* ]] || [[ "$MESSAGE" == *"completed"* ]]; then
  # Extract what I completed
  TASK_DESC=$(echo "$MESSAGE" | grep -oP "(?<=✅ ).*?(?=\n)" || echo "Work completed")

  # Create review task for user
  bun src/cli/task.ts add "Review: $TASK_DESC" \
    --assignee user_bln \
    --labels review,claude-work \
    --priority P2

  echo "Created review task for Claude's work"
fi
```

**Issue:** This catches ALL completion messages, even trivial ones.

**Better approach:** I explicitly create review task when I determine work needs review.

### Hook 2: When Background Agent Completes Work

**Scenario:** Background agent signals COMPLETION_REPORT

**Hook:** `.claude/hooks/PostAgentCompletion.sh`

```bash
#!/bin/bash
# Triggered when background agent completes

AGENT_ID="$1"
REPORT_FILE="$2"

# Parse completion report
DELIVERABLES=$(jq -r '.deliverables[]' "$REPORT_FILE")
SUMMARY=$(jq -r '.summary' "$REPORT_FILE")

# Claude reviews agent work (this script IS Claude's review logic)

# Check 1: Files exist
MISSING=""
for file in $DELIVERABLES; do
  if [[ ! -f "$file" ]]; then
    MISSING="$MISSING $file"
  fi
done

if [[ -n "$MISSING" ]]; then
  # Request rework
  echo "REVIEW_FAILED: Missing files:$MISSING"

  # Create rework task assigned to agent
  bun src/cli/task.ts add "Rework: Missing deliverables" \
    --assignee "agent_$AGENT_ID" \
    --labels rework \
    --priority P0

  exit 1
fi

# Check 2: Files have content (basic validation)
# ... similar checks ...

# All checks passed - create review task for user
bun src/cli/task.ts add "Review: $SUMMARY (Claude-approved)" \
  --assignee user_bln \
  --labels review,agent-work,claude-approved \
  --priority P1 \
  --context "deliverables=$DELIVERABLES"

echo "REVIEW_PASSED: Ready for user review"
```

**Issue:** Hooks can't access full conversation context or make complex decisions.

**Better approach:** Hooks call back into Claude actor for review logic.

## Unified Architecture

### Graph Structure

```
user_bln (UserActor)
    |
    +-- owns_project --> Project (tk-agents)
                            |
                            +-- has_session --> claude_session_e7f0734f (ClaudeActor)
                                                    |
                                                    +-- delegates_to --> agent_ab56968 (BackgroundAgentActor)
                                                    |
                                                    +-- delegates_to --> agent_a31e6f6 (BackgroundAgentActor)
                                                    |
                                                    +-- reviews_work --> review_task_31 (ReviewTask)
```

### Actor Responsibilities

**UserActor (bln):**
- Assigns work to ClaudeActor
- Reviews final deliverables
- Approves/rejects work
- Makes high-level decisions

**ClaudeActor (interactive session):**
- Receives assignments from UserActor
- Decides: handle directly vs delegate to background agent
- Delegates to BackgroundAgentActors
- Performs first-pass review of agent work
- Creates review tasks for user (only when needed)
- Handles rework requests to agents

**BackgroundAgentActor (background subprocess):**
- Receives delegated work from ClaudeActor
- Works autonomously
- Signals completion to ClaudeActor
- Handles rework requests from ClaudeActor
- Never directly interacts with UserActor

### Message Patterns

**Assignment:**
```
UserActor → ClaudeActor: { type: "assign_work", task: {...} }
ClaudeActor → BackgroundAgentActor: { type: "delegate_work", task: {...} }
```

**Completion:**
```
BackgroundAgentActor → ClaudeActor: { type: "completion_report", ... }
ClaudeActor → UserActor: { type: "work_ready_for_review", ... }
```

**Review:**
```
UserActor → ClaudeActor: { type: "review_result", decision: "approved" }
ClaudeActor → BackgroundAgentActor: { type: "work_approved", ... }
```

**Rework:**
```
ClaudeActor → BackgroundAgentActor: { type: "request_rework", issues: [...] }
BackgroundAgentActor → ClaudeActor: { type: "rework_complete", ... }
```

## Implementation Path

### Phase 1: Represent Claude Session as Actor

**Create ClaudeActor node on session start:**

```typescript
// daemon/server.ts or session init
async function initializeSession(sessionId: string, userId: string) {
  const claudeActor = ClaudeActor({
    sessionId,
    assignedBy: userId,
    graph,
  });

  // Link to user
  await graph.addEdge(userId, claudeActor.getId(), "assigns_to");

  return claudeActor;
}
```

### Phase 2: Link Background Agents to Claude

**When launching background agent:**

```typescript
// Instead of orphan agent
const agent = BackgroundAgentActor({ ... });

// Link to Claude session
await graph.addEdge(mySessionActorId, agent.getId(), "delegates_to");
```

### Phase 3: Claude Reviews Before User

**When agent completes:**

```typescript
async handleAgentCompletion(report) {
  // Claude performs quick validation
  const review = await this.quickQualityCheck(report.deliverables);

  if (!review.passed) {
    // Request rework (don't bother user)
    return await this.requestRework(report.agent_id, review.issues);
  }

  // Create review task for user (only if passed Claude's check)
  await this.createReviewTaskForUser(report);
}
```

### Phase 4: Hook Integration

**PostAgentCompletion hook calls Claude actor:**

```bash
#!/bin/bash
# Hook script

AGENT_ID="$1"
SESSION_ID="$CURRENT_SESSION_ID"

# Send completion event to Claude actor
bun src/cli/graph.ts send "claude_session_$SESSION_ID" agent_completed \
  --agent_id "$AGENT_ID" \
  --report_file "$REPORT_FILE"
```

**Claude actor handles via message:**

```typescript
case "agent_completed":
  return await handleAgentCompletion(payload);
```

## Benefits

**Clear hierarchy:**
- User → Claude → Background Agents
- Each level has specific responsibilities
- Message-passing all the way

**Intermediate review:**
- Claude validates before bothering user
- Catches obvious errors early
- User only sees quality work

**Actor consistency:**
- Everything is an actor
- Everything uses message-passing
- Clean separation of concerns

**Audit trail:**
- Graph shows delegation chain
- Messages show review decisions
- Full history of who reviewed what

## Decision

**Should we do this?**

**YES** - This is the natural evolution:
1. ✅ Makes Claude session a first-class actor
2. ✅ Enables intermediate review (Claude filters before user)
3. ✅ Creates proper delegation hierarchy
4. ✅ Everything in graph, everything is actor
5. ✅ Hooks can trigger actor messages

**Implementation priority:** After file-watching actors (since they establish the "everything is actor" pattern)
