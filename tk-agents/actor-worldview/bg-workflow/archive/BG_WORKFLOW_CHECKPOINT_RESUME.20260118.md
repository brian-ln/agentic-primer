# Background Agent Checkpoint & Resume Pattern

## Problem Statement

**From user feedback:**
> "The background agent can't assume that its message is going to be immediately processed by you, or by me and responded to in some tight SLA (5 seconds...). It could poll for a little while to keep its prompt cache hot, but it should hang up and be 'resumed' when the message comes in. I don't want to burn tokens waiting and handling polling. How do we accomplish that?"

**Current design problem (BG_WORKFLOW_CLARIFICATIONS.md):**
```typescript
// Agent polls indefinitely - BURNS TOKENS
while (true) {
  const response = await checkForClarificationResponse(agentId);
  if (response && response.resume_signal) {
    break;
  }
  await sleep(5000); // Poll every 5 seconds FOREVER
}
```

**Issues:**
- ❌ Burns tokens while waiting (could be hours/days)
- ❌ Keeps agent alive unnecessarily
- ❌ Wastes compute if user is away
- ❌ Prompt cache expires after 5 minutes anyway

---

## Efficient Checkpoint-Resume Pattern

### Phase 1: Agent Polls Briefly (Cache Warming)

**Agent polls for short period to keep cache warm:**

```typescript
const BRIEF_POLL_DURATION = 60_000;  // 1 minute
const POLL_INTERVAL = 5_000;         // 5 seconds
const MAX_POLLS = BRIEF_POLL_DURATION / POLL_INTERVAL; // 12 polls

console.log("[CLARIFICATION] Polling briefly to keep cache warm...");

for (let i = 0; i < MAX_POLLS; i++) {
  const response = await checkForClarificationResponse(agentId);

  if (response && response.resume_signal) {
    console.log("[RESUMED] Response received during brief poll!");
    applyResponses(response);
    return; // Continue work immediately
  }

  await sleep(POLL_INTERVAL);
}

// No response after 1 minute → checkpoint and exit
console.log("[CHECKPOINT] No response yet - saving state and exiting");
```

**Benefits:**
- ✅ Quick responses (< 1 min) don't require resume overhead
- ✅ Cache stays hot for immediate responses
- ✅ Agent doesn't burn tokens indefinitely

---

### Phase 2: Agent Checkpoints and Exits

**Agent saves complete state and terminates:**

```typescript
// Write checkpoint state
const checkpoint = {
  agentId: agentId,
  timestamp: new Date().toISOString(),
  phase: "waiting_clarification",
  clarificationRequestId: clarificationId,

  // Complete state snapshot
  workState: {
    completed: [
      "Read session logs",
      "Analyzed context",
      "Created initial design"
    ],
    inProgress: [
      "Supervision pattern implementation"
    ],
    blocked: [
      "Supervision pattern selection (needs Q1 answer)",
      "Actor lifecycle integration (needs Q2 answer)"
    ],
    parallelWorkAvailable: [
      "File watching logic (independent)",
      "Event emission structure",
      "Test harness setup"
    ]
  },

  // Context preservation
  extractedContext: sessionContext,
  decisions: recentDecisions,
  filesCreated: ["file-watcher.spec.md", "src/actors/file-watcher-actor.ts"],

  // Resume instructions
  resumeInstructions: {
    readClarificationFrom: `/tmp/claude/clarifications/${agentId}.response.yaml`,
    applyResponsesTo: "workState.blocked",
    nextPhase: "implementation",
    estimatedRemainingWork: "15-20 minutes"
  }
};

// Write checkpoint
await Bun.write(
  `/tmp/claude/checkpoints/${agentId}.checkpoint.json`,
  JSON.stringify(checkpoint, null, 2)
);

console.log(`[CHECKPOINT_SAVED] ${checkpoint.phase}`);
console.log(`[EXIT] Agent terminating - will resume when response arrives`);

// EXIT - don't burn tokens waiting
process.exit(0);
```

**Key points:**
- Agent saves COMPLETE state (everything needed to resume)
- Agent explicitly EXITS (no more token burn)
- Checkpoint includes:
  - What was completed
  - What's blocked
  - What can continue in parallel
  - How to resume

---

### Phase 3: Parent Monitors (Not Agent)

**Parent monitors for clarification response - agent is NOT running:**

```typescript
// Parent monitoring loop (lightweight)
async function monitorForClarificationResponse(agentId: string) {
  const clarificationPath = `/tmp/claude/clarifications/${agentId}.needed.yaml`;
  const responsePath = `/tmp/claude/clarifications/${agentId}.response.yaml`;

  // Parent presents questions to user
  const clarification = await loadClarificationNeeded(clarificationPath);
  console.log("\n[AGENT_QUESTION] Background agent needs clarification:");
  console.log(formatClarificationForUser(clarification));

  // Wait for user response (parent is lightweight, this is OK)
  const userResponse = await waitForUserResponse(clarification.questions);

  // Write response file
  await writeClarificationResponse(responsePath, {
    agentId,
    timestamp: new Date().toISOString(),
    responses: userResponse,
    resume_signal: true
  });

  console.log("[RESPONSE_WRITTEN] Response ready for agent");

  // Now RESUME the agent
  await resumeAgent(agentId);
}
```

**Benefits:**
- ✅ Parent is lightweight (not burning tokens like agent would)
- ✅ User takes as long as they need
- ✅ No polling overhead
- ✅ Agent only runs when there's work to do

---

### Phase 4: Resume Agent with Response

**Parent resumes agent using Task tool's resume capability:**

```typescript
async function resumeAgent(agentId: string) {
  // Load checkpoint
  const checkpointPath = `/tmp/claude/checkpoints/${agentId}.checkpoint.json`;
  const checkpoint = JSON.parse(await Bun.file(checkpointPath).text());

  // Load clarification response
  const responsePath = checkpoint.resumeInstructions.readClarificationFrom;
  const response = await loadClarificationResponse(responsePath);

  // Build resume prompt
  const resumePrompt = `
## RESUME FROM CLARIFICATION

You were waiting for clarification and checkpointed your state.

### Previous State
${JSON.stringify(checkpoint.workState, null, 2)}

### Clarification Responses
${formatResponses(response.responses)}

### Resume Instructions
${checkpoint.resumeInstructions.nextPhase}

### Next Steps
1. Apply clarification responses to blocked work
2. Continue implementation with answers
3. Complete remaining work (estimated: ${checkpoint.resumeInstructions.estimatedRemainingWork})

### Context Preserved
- Session context: Available in checkpoint
- Decisions made: ${checkpoint.decisions.length} recorded
- Files created: ${checkpoint.filesCreated.join(", ")}

## Continue Work
`;

  // Resume using Task tool
  console.log(`[RESUMING] Agent ${agentId} with clarification responses`);

  await Task({
    subagent_type: "general-purpose",
    description: "Resume from clarification",
    resume: agentId,  // KEY: Resume existing agent
    prompt: resumePrompt,
    run_in_background: true
  });

  console.log(`[RESUMED] Agent ${agentId} is now running again`);
}
```

**Key insight: Task tool supports `resume` parameter!**
- Agent exits after checkpointing
- Parent resumes agent when ready
- Resumed agent has full context from checkpoint
- No token burn during wait

---

## Comparison: Polling vs Checkpoint-Resume

### Polling (OLD - Inefficient)

```
Agent launched → Work → CLARIFICATION_NEEDED
                           ↓
                    Poll every 5s ← [BURNS TOKENS]
                           ↓
                    Poll every 5s ← [BURNS TOKENS]
                           ↓
                    [30 minutes later...]
                           ↓
                    Poll every 5s ← [BURNS TOKENS]
                           ↓
                    Response arrives → Continue
```

**Cost:** 360 polling iterations × token cost = EXPENSIVE

**Cache:** Expires after 5 minutes anyway (wasted polling)

---

### Checkpoint-Resume (NEW - Efficient)

```
Agent launched → Work → CLARIFICATION_NEEDED
                           ↓
                    Poll 12 times (1 min) ← Keep cache hot
                           ↓
                    No response? → CHECKPOINT → EXIT
                           ↓
                    [Agent not running - NO TOKEN BURN]
                           ↓
                    [30 minutes later...]
                           ↓
                    Parent: Response arrives → RESUME agent
                           ↓
                    Agent restored → Continue
```

**Cost:** 12 polling iterations + resume overhead = CHEAP

**Cache:** Fresh cache on resume (efficient)

---

## Implementation Details

### Checkpoint File Format

```json
{
  "agentId": "task_bg_a7e4d2",
  "timestamp": "2026-01-18T10:15:23Z",
  "phase": "waiting_clarification",
  "clarificationRequestId": "clarif_xyz789",

  "workState": {
    "completed": [
      "Session log analysis",
      "Initial design document"
    ],
    "inProgress": [
      "Supervision implementation"
    ],
    "blocked": [
      "Supervision pattern selection (Q1)",
      "Lifecycle integration (Q2)"
    ],
    "parallelWorkAvailable": [
      "File watching logic",
      "Test harness"
    ]
  },

  "context": {
    "sessionContext": { /* full extracted context */ },
    "decisions": [ /* recent decisions */ ],
    "filesCreated": [ /* files created so far */ ],
    "filesModified": [ /* files modified */ ]
  },

  "resumeInstructions": {
    "readClarificationFrom": "/tmp/claude/clarifications/task_bg_a7e4d2.response.yaml",
    "applyResponsesTo": "workState.blocked",
    "nextPhase": "implementation",
    "estimatedRemainingWork": "15-20 minutes"
  },

  "metadata": {
    "totalDuration": "8m 23s",
    "waitStartTime": "2026-01-18T10:15:23Z",
    "tokensBurned": 12543,
    "checkpointSize": "47KB"
  }
}
```

### Parent Monitoring (Lightweight)

```typescript
// Parent tracks checkpointed agents
const checkpointedAgents = new Map<string, CheckpointInfo>();

// When agent checkpoints
function onAgentCheckpoint(agentId: string, checkpoint: Checkpoint) {
  checkpointedAgents.set(agentId, {
    agentId,
    checkpointPath: `/tmp/claude/checkpoints/${agentId}.checkpoint.json`,
    clarificationPath: `/tmp/claude/clarifications/${agentId}.needed.yaml`,
    responsePath: `/tmp/claude/clarifications/${agentId}.response.yaml`,
    timestamp: checkpoint.timestamp
  });

  console.log(`[TRACKED] Agent ${agentId} checkpointed, monitoring for response`);
}

// Check periodically if response arrived (parent is lightweight)
async function checkCheckpointedAgents() {
  for (const [agentId, info] of checkpointedAgents) {
    if (await Bun.file(info.responsePath).exists()) {
      console.log(`[RESPONSE_READY] Agent ${agentId} can be resumed`);
      await resumeAgent(agentId);
      checkpointedAgents.delete(agentId);
    }
  }
}

// Parent checks every 30 seconds (not agent!)
setInterval(checkCheckpointedAgents, 30_000);
```

**Parent overhead:** Lightweight file existence checks
**Agent overhead:** ZERO (not running)

---

## Optimization: Brief Poll Duration Tuning

**How long should agent poll before checkpointing?**

### Option A: 30 seconds (Aggressive Checkpoint)
- **Pro:** Minimal token burn
- **Pro:** Fast checkpoint for long waits
- **Con:** More resume overhead for quick responses
- **Best for:** User often takes >5 min to respond

### Option B: 60 seconds (Balanced - RECOMMENDED)
- **Pro:** Catches most quick responses
- **Pro:** Cache stays warm
- **Con:** 1 min token burn
- **Best for:** Most cases

### Option C: 5 minutes (Cache Preservation)
- **Pro:** Maximizes cache hit rate
- **Pro:** No resume overhead for fast responses
- **Con:** 5 min token burn
- **Best for:** User usually responds quickly

**Recommendation:** Start with 60 seconds, tune based on usage patterns

---

## Protocol Updates

### Updated CLARIFICATION_NEEDED Format

```yaml
[CLARIFICATION_NEEDED]
agent_id: task_bg_a7e4d2
timestamp: 2026-01-18T10:15:23Z
blocked_at: "Supervision pattern selection"
reason: "..."

questions:
  - question_id: Q1
    text: "..."
    # ... same as before

# NEW: Checkpoint information
checkpoint_info:
  brief_poll_duration: 60s
  checkpoint_path: /tmp/claude/checkpoints/task_bg_a7e4d2.checkpoint.json
  agent_will_exit_after_poll: true
  resume_required: true

work_continues: true  # Can do parallel work
parallel_work_available: |
  Can implement:
  - File watching logic
  - Event emission structure
  - Test harness setup
[/CLARIFICATION_NEEDED]
```

### Updated CLARIFICATION_RESPONSE Format

```yaml
[CLARIFICATION_RESPONSE]
agent_id: task_bg_a7e4d2
timestamp: 2026-01-18T10:18:15Z

responses:
  - question_id: Q1
    answer: "..."

resume_signal: true
resume_method: checkpoint  # NEW: Indicates agent checkpointed

# NEW: Instructions for resuming
resume_instructions:
  checkpoint_path: /tmp/claude/checkpoints/task_bg_a7e4d2.checkpoint.json
  use_task_resume: true
  resume_prompt_includes: clarification_responses
[/CLARIFICATION_RESPONSE]
```

---

## Integration with Task Tool

**The Task tool already supports resume!**

```typescript
// From claude-code-agent Task tool documentation
interface TaskParams {
  subagent_type: string;
  description: string;
  prompt: string;
  run_in_background?: boolean;
  resume?: string;  // <-- EXISTING FEATURE!
}

// Resume usage
Task({
  subagent_type: "general-purpose",
  description: "Resume from clarification",
  resume: agentId,  // Agent continues with full context
  prompt: resumePrompt,
  run_in_background: true
});
```

**Benefits of using Task.resume:**
- ✅ Agent context preserved automatically
- ✅ No need to re-parse session logs
- ✅ Efficient cache usage
- ✅ Standard pattern across all agents

---

## Benefits Summary

### Token Efficiency

| Scenario | Polling (OLD) | Checkpoint-Resume (NEW) | Savings |
|----------|--------------|------------------------|---------|
| 5 min wait | 60 polls × cost | 12 polls + resume | ~80% |
| 30 min wait | 360 polls × cost | 12 polls + resume | ~96% |
| 2 hour wait | 1440 polls × cost | 12 polls + resume | ~99% |
| Overnight | 17,280 polls × cost | 12 polls + resume | ~99.9% |

### Operational Benefits

- ✅ **Agent**: Exits quickly, no wasted compute
- ✅ **Parent**: Lightweight monitoring only
- ✅ **User**: Take as long as needed to respond
- ✅ **System**: Resources freed for other work
- ✅ **Cache**: Fresh on resume (vs stale from hours of polling)

---

## Example Scenario

### User asks: "Implement file watcher with supervision"

**Timeline:**

```
10:00:00 - Agent launches
10:00:05 - Reads session logs
10:00:08 - Finds supervision pattern unclear
10:00:10 - Signals CLARIFICATION_NEEDED
10:00:10 - Begins brief poll (12 iterations)
10:00:15 - Poll 1/12... no response
10:00:20 - Poll 2/12... no response
...
10:01:10 - Poll 12/12... no response
10:01:10 - Writes checkpoint
10:01:10 - Agent EXITS (no more token burn)

[Agent is NOT running - user thinks about it for 30 minutes]

10:31:45 - User provides answer in conversation
10:31:46 - Parent writes CLARIFICATION_RESPONSE
10:31:47 - Parent detects response file
10:31:48 - Parent calls Task({resume: agentId, ...})
10:31:50 - Agent RESUMES with fresh context
10:31:55 - Agent applies responses
10:32:00 - Agent continues implementation
10:45:00 - Agent completes and signals COMPLETION_REPORT
```

**Total agent runtime:** 1 min (initial) + 13 min (after resume) = 14 minutes
**Polling that would have occurred:** 30 minutes × 12 polls/min = 360 polls
**Savings:** 360 - 12 = 348 unnecessary polls avoided!

---

## Recommendations

### 1. Update BG_WORKFLOW_CLARIFICATIONS.md

**Replace polling loop section with:**

```markdown
### Agent Wait Pattern: Checkpoint-Resume

Agent does NOT poll indefinitely. Instead:

1. **Brief poll** (60 seconds) to catch quick responses
2. **Checkpoint state** with complete work snapshot
3. **Exit cleanly** (no token burn)
4. **Parent monitors** for response (lightweight)
5. **Resume on response** using Task.resume

See BG_WORKFLOW_CHECKPOINT_RESUME.md for details.
```

### 2. Update BG_WORKFLOW_REDESIGN.md

**Update Protocol sections (lines 283-312) with:**

```typescript
// Agent clarification pattern
if (needsClarification) {
  await writeClarificationNeeded(clarificationData);
  console.log("[WAITING] Brief poll for quick response...");

  // Poll briefly (1 minute)
  for (let i = 0; i < 12; i++) {
    const response = await checkForClarificationResponse(agentId);
    if (response) {
      applyResponses(response);
      return; // Continue immediately
    }
    await sleep(5000);
  }

  // No response - checkpoint and exit
  console.log("[CHECKPOINT] Saving state and exiting");
  await writeCheckpoint(checkpointData);
  process.exit(0); // Exit cleanly
}
```

### 3. Document Checkpoint Format

Create standard checkpoint schema that all agents use.

### 4. Implement Parent Resume Logic

Build parent monitoring and resume capability.

---

## Summary

**Problem:** Agent polling indefinitely burns tokens unnecessarily

**Solution:** Checkpoint-Resume pattern
1. Poll briefly (1 min) for quick responses
2. Checkpoint complete state
3. Exit cleanly
4. Resume when response arrives

**Benefits:**
- 80-99% token savings depending on wait time
- Agent only runs when there's work
- User can take as long as needed
- Fresh cache on resume

**Key insight:** The Task tool already supports resume - we just need to use it properly with checkpointing!
