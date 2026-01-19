# /bg Workflow Clarifications

## Questions from User

User asked about BG_WORKFLOW_REDESIGN.md:
1. Does "forward answers" mean resume the agent?
2. Does the agent "complete" when clarification needed or wait on parent?
3. Should JQ commands use time range not size?
4. Do we know our current timestamp accurately?
5. Could we write a bun.js script for this?
6. Think about integrating with logs as actors in graph

---

## Answers

### 1. Does "Forward Answers" Mean Resume the Agent?

**YES, indirectly.**

The agent is in a **polling loop** waiting for the parent to write a response file:

```typescript
// From BG_WORKFLOW_REDESIGN.md:283-311
while (true) {
  const response = await checkForClarificationResponse(agentId);
  if (response && response.resume_signal) {
    console.log("[RESUMED] Received clarification responses");
    applyResponses(response);
    break;  // Exit wait loop and continue work
  }
  await sleep(5000); // Poll every 5 seconds
}
```

**Workflow:**
1. Agent signals `[CLARIFICATION_NEEDED]` → writes `/tmp/claude/clarifications/${AGENT_ID}.needed.yaml`
2. Agent enters polling loop (checks every 5s for response file)
3. Parent detects clarification → forwards questions to user
4. User provides answers
5. Parent "forwards answers" → writes `/tmp/claude/clarifications/${AGENT_ID}.response.yaml`
6. Agent detects response → resumes automatically

**"Forward answers" = Write the response file that agent is polling for**

---

### 2. Does Agent Complete or Wait?

**Agent WAITS in a polling loop.**

**It does NOT:**
- Exit/complete
- Return control
- Terminate

**It DOES:**
- Poll every 5 seconds for response file
- Optionally continue **parallel work** (`work_continues: true`)
- Block on the specific question until answered
- Resume automatically when response detected

**Example from BG_WORKFLOW_REDESIGN.md:829-845:**

```yaml
current_state: |
  Completed:
  - Read file-watcher.spec.md
  - Analyzed session logs for context

  Can proceed with parallel work:
  - File watching logic (independent of supervision)
  - Event emission structure
  - Test harness setup

  Blocked:
  - Supervision implementation
  - Actor lifecycle integration

work_continues: true
```

Agent continues parallel work **while waiting** for supervision pattern answer.

---

### 3. Should JQ Commands Use Time Range Not Size?

**YES! Excellent catch.**

**Current approach (SIZE-BASED):**
```bash
tail -50 session.jsonl  # Last 50 LINES
tail -100 session.jsonl # Last 100 LINES
```

**Problem:**
- If messages are long (like design docs), 50 lines might be only 1-2 messages
- If messages are short, 50 lines might be 50 tiny messages from irrelevant context
- Line count doesn't correlate with temporal relevance

**Better approach (TIME-BASED):**

```bash
# Messages from last 30 minutes
jq -r --arg since "$(date -u -v-30M +%Y-%m-%dT%H:%M:%SZ)" \
  'select(.timestamp >= $since) | .message.content[].text' \
  session.jsonl

# Messages from last 5 minutes (very recent context)
jq -r --arg since "$(date -u -v-5M +%Y-%m-%dT%H:%M:%SZ)" \
  'select(.timestamp >= $since) | .message.content[].text' \
  session.jsonl

# Messages from last hour (broader context)
jq -r --arg since "$(date -u -v-1H +%Y-%m-%dT%H:%M:%SZ)" \
  'select(.timestamp >= $since) | .message.content[].text' \
  session.jsonl
```

**Hybrid approach (best of both):**
```typescript
// Ensure minimum messages even if time window is small
const recentMessages =
  messagesInTimeWindow.length >= 20
    ? messagesInTimeWindow
    : allMessages.slice(-20);
```

**Session log format includes timestamps:**
```json
{
  "type": "user",
  "timestamp": "2026-01-18T08:35:22.123Z",
  "message": {...}
}
```

---

### 4. Do We Know Current Timestamp Accurately?

**YES!** Verified with `date`:

```bash
$ date
Sun Jan 18 09:56:54 EST 2026
```

**ISO-8601 format:**
```bash
$ date -u +%Y-%m-%dT%H:%M:%SZ
2026-01-18T14:56:54Z
```

**We can accurately:**
- Calculate time ranges (now - 30 minutes)
- Filter session logs by timestamp
- Compare timestamps for freshness
- Build time-based context windows

**Implementation:**
```typescript
const now = new Date();
const startTime = new Date(now.getTime() - minutesBack * 60 * 1000);
const startISO = startTime.toISOString();

// Filter messages
messages.filter(m => m.timestamp >= startISO && m.timestamp <= endISO);
```

---

### 5. Could We Write a Bun.js Script for This?

**YES! Already implemented:** `src/utils/session-context.ts`

**Features:**
- ✅ Time-based context extraction (not line count)
- ✅ Configurable time window (default: 30 minutes)
- ✅ Minimum message guarantee (20 messages even if time window small)
- ✅ Extracts 6 context types:
  - Recent decisions
  - Active files
  - User goals
  - Technical constraints
  - Project patterns
  - Recent messages

**Usage:**

```bash
# Extract last 30 minutes of context
bun src/utils/session-context.ts ~/.claude/projects/.../session.jsonl 30

# Extract last 5 minutes (very recent)
bun src/utils/session-context.ts ~/.claude/projects/.../session.jsonl 5

# Extract last hour
bun src/utils/session-context.ts ~/.claude/projects/.../session.jsonl 60
```

**Output:**
```
📊 SESSION CONTEXT
────────────────────────────────────────────────────────────────────────────────
Time Range: 2026-01-18T14:27:58.967Z → 2026-01-18T14:57:58.967Z
Messages: 143

🎯 Recent Decisions:
  1. Better approach - time-based: jq with timestamp filtering
  2. Agent WAITS in polling loop, doesn't complete
  ...

📁 Active Files:
  - /path/to/bg.ts
  - /path/to/BG_WORKFLOW_LOCAL.md
  ...

🎯 User Goals:
  - Launch background tasks quickly
  - Agent autonomy via session log reading
  ...
```

**Programmatic usage:**

```typescript
import { extractSessionContext } from "./src/utils/session-context.ts";

const context = await extractSessionContext(sessionLogPath, {
  minutesBack: 30,
  minMessages: 20
});

console.log(context.recentDecisions);
console.log(context.activeFiles);
console.log(context.userGoals);
```

---

### 6. Integration with Logs as Actors in Graph

**Excellent connection!** This relates to `SESSION_LOGS_ACTORS_RESURRECTION.md`.

**Current state:**
- ✅ Session log actor infrastructure EXISTS (~1,600 LOC)
- ✅ Physical offset tracking (StreamWatcher)
- ✅ Directory watchers (FileWatcherSupervisor)
- ✅ File tailers (real-time streaming)
- ⚠️ **Gap**: Link semantic clusters to log provenance (150 LOC)

**Integration vision:**

```
primer.sessions (supervisor)
├─ primer.sessions.file_watcher (watches ~/.claude/projects/)
├─ primer.sessions.session_63c20fac (actor per session)
│   ├─ read_range(start_time, end_time) → messages
│   ├─ extract_context(minutes_back) → SessionContext
│   ├─ search(keywords) → matches
│   └─ link_to_cluster(cluster_id, line_range)
└─ primer.sessions.indexer (searchable index)
```

**How this connects:**

1. **Current script** (`session-context.ts`):
   - Standalone utility
   - Time-based extraction
   - Can be used by background agents

2. **Future actor model**:
   - `primer.sessions.session_63c20fac` actor
   - Same functionality via message passing
   - Example:
     ```typescript
     send('primer.sessions.session_63c20fac', {
       type: 'extract_context',
       payload: { minutesBack: 30 }
     });
     ```

3. **Semantic integration** (Phase 2):
   - Link idea clusters to log segments
   - `primer.ideas.cluster_7` → session lines 450-523
   - Retrieve conversation context for any idea
   - Full audit trail: idea → prompts → session logs

**Implementation path:**

```typescript
// Phase 1: Utility function (NOW)
const context = await extractSessionContext(logPath, { minutesBack: 30 });

// Phase 2: Actor message (FUTURE)
const context = await send('primer.sessions.current', {
  type: 'extract_context',
  payload: { minutesBack: 30 }
});

// Phase 3: Graph integration (FUTURE)
// Semantic cluster ─edges→ session lines ─reads→ actual content
query(`
  ?[cluster_id, session_id, line_range, content] :=
    *cluster{ cluster_id },
    *provenance{ cluster_id, session_id, line_range },
    session_actor = concat("primer.sessions.session_", session_id),
    content = actor_call(session_actor, "read_range", line_range)
`);
```

**Design → Fitness → Optimize:**

1. **Design**: Model session logs as actors with query capabilities
2. **Fitness**: Fast context extraction (<1s), accurate results (>80%)
3. **Optimize**: May use direct file reads if actor message passing too slow
4. **Validate**: Session log actor preserves intent of time-based context extraction

**This is a perfect actor model use case:**
- Sessions are autonomous entities (actors)
- Context extraction is a message (request/response)
- Log reading is an effect (I/O boundary)
- System manages placement (local file vs remote vs cached)

---

## Recommendations

### 1. Update BG_WORKFLOW_REDESIGN.md

**Replace size-based JQ commands with time-based:**

```bash
# OLD (lines 114-121)
tail -50 "$CURRENT_SESSION" | jq ...
tail -100 "$CURRENT_SESSION" | jq ...

# NEW
jq -r --arg since "$(date -u -v-30M +%Y-%m-%dT%H:%M:%SZ)" \
  'select(.timestamp >= $since) | ...' \
  "$CURRENT_SESSION"
```

**Update agent prompt template (Appendix C, lines 1442-1454):**

```bash
# OLD
tail -100 session.jsonl | jq ...

# NEW
# Extract context from last 30 minutes
bun src/utils/session-context.ts session.jsonl 30

# Or using jq directly
jq -r --arg since "$(date -u -v-30M +%Y-%m-%dT%H:%M:%SZ)" \
  'select(.timestamp >= $since) | ...' \
  session.jsonl
```

### 2. Integrate with /bg Local Implementation

**Update `src/cli/bg.ts` to use time-based extraction:**

```typescript
import { extractSessionContext } from "../utils/session-context.ts";

// In buildEnhancedPrompt():
const context = await extractSessionContext(sessionLogPath, {
  minutesBack: 30,
  minMessages: 20
});

const enhancedPrompt = `
${taskDescription}

## RECENT CONTEXT (Last 30 minutes)

**Recent Decisions:**
${context.recentDecisions.map(d => `- ${d}`).join('\n')}

**Active Files:**
${context.activeFiles.map(f => `- ${f}`).join('\n')}

**User Goals:**
${context.userGoals.map(g => `- ${g.substring(0, 100)}...`).join('\n')}

**Project Patterns:**
${context.projectPatterns.map(p => `- ${p}`).join('\n')}

... [rest of prompt]
`;
```

### 3. Document Agent Clarification Behavior

**Clarify in BG_WORKFLOW_LOCAL.md that:**
- Agent WAITS (doesn't complete)
- Parent must monitor for clarification signals
- Agent polls every 5 seconds
- Optional parallel work during wait

### 4. Plan Actor Integration (Future)

**Create integration design document:**
- How `session-context.ts` utility becomes `primer.sessions.*` actors
- Message formats for context extraction
- Graph integration for semantic → session log linking
- Migration path from utility to actor

### 5. Test Time-Based Extraction

**Validate on real sessions:**
```bash
# Test different time windows
bun src/utils/session-context.ts <session-log> 5   # Last 5 min
bun src/utils/session-context.ts <session-log> 30  # Last 30 min
bun src/utils/session-context.ts <session-log> 60  # Last hour

# Compare to size-based
tail -50 <session-log> | jq '...'   # OLD
vs
bun src/utils/session-context.ts <session-log> 30  # NEW
```

**Measure:**
- Message count variance
- Context relevance
- Extraction time
- Accuracy vs manual inspection

---

## Summary

| Question | Answer | Status |
|----------|--------|--------|
| 1. Forward answers = resume? | Yes, writes response file agent is polling for | ✅ Clarified |
| 2. Agent complete or wait? | **WAITS** in polling loop, optionally continues parallel work | ✅ Clarified |
| 3. Use time range not size? | **YES** - time-based is better, hybrid recommended | ✅ Agreed |
| 4. Know timestamp accurately? | **YES** - verified with `date`, ISO-8601 available | ✅ Confirmed |
| 5. Write bun.js script? | **YES** - implemented `session-context.ts` | ✅ Built |
| 6. Integrate with logs as actors? | **Future** - perfect actor model use case, plan integration | 📋 Planned |

---

## Next Steps

1. **Immediate**: Update BG_WORKFLOW_REDESIGN.md with time-based commands
2. **Short-term**: Integrate `session-context.ts` into `/bg` local implementation
3. **Medium-term**: Test time-based extraction vs size-based on real sessions
4. **Long-term**: Design actor integration (sessions as actors in graph)

---

**Key Insight**: Time-based context extraction is more semantically accurate than line counts. Agent waiting behavior needs clarification in docs. Integration with logs-as-actors is a natural evolution following actor worldview principles.
