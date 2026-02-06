# Subagent Communication Protocols - Architecture Diagram

**Visual representation of parent-subagent communication flows**

---

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         USER                                    │
│                           │                                      │
│                           ▼                                      │
│                    /bg "Task description"                        │
└───────────────────────────┬─────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                    PARENT AGENT                                 │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │ PHASE 1: Pre-launch Validation                            │ │
│  │  - Validate context, goals, metrics                       │ │
│  │  - Clarify with user if needed                            │ │
│  └─────────────────────────┬─────────────────────────────────┘ │
│                            ▼                                    │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │ PHASE 2: Prepare Launch                                   │ │
│  │  - Add EXECUTION CONTEXT                                  │ │
│  │  - Include protocol instructions                          │ │
│  │  - Launch Task tool (run_in_background=true)              │ │
│  └─────────────────────────┬─────────────────────────────────┘ │
│                            ▼                                    │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │ PHASE 3: Return Agent ID                                  │ │
│  │  - Inform user of launch                                  │ │
│  │  - Provide monitoring instructions                        │ │
│  └─────────────────────────┬─────────────────────────────────┘ │
│                            ▼                                    │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │ PHASE 4: Monitor and Respond (LOOP)                       │ │
│  │  ┌────────────────────────────────────────────────────┐   │ │
│  │  │ Poll: TaskOutput agent-id=<id> block=false         │   │ │
│  │  │ Frequency: Every 10-30 seconds                     │   │ │
│  │  └────────────────┬───────────────────────────────────┘   │ │
│  │                   ▼                                        │ │
│  │  ┌────────────────────────────────────────────────────┐   │ │
│  │  │ Detect Signal:                                     │   │ │
│  │  │  - [CLARIFICATION_NEEDED]                          │   │ │
│  │  │  - [STOP_WORK]                                     │   │ │
│  │  │  - [DELEGATE_WORK]                                 │   │ │
│  │  │  - [COMPLETION_REPORT]                             │   │ │
│  │  └────────────────┬───────────────────────────────────┘   │ │
│  │                   ▼                                        │ │
│  │  ┌────────────────────────────────────────────────────┐   │ │
│  │  │ Respond:                                           │   │ │
│  │  │  - Ask user (CLARIFICATION_NEEDED)                 │   │ │
│  │  │  - Resolve blocker (STOP_WORK)                     │   │ │
│  │  │  - Evaluate/launch (DELEGATE_WORK)                 │   │ │
│  │  │  - Notify user (COMPLETION_REPORT)                 │   │ │
│  │  └────────────────┬───────────────────────────────────┘   │ │
│  │                   ▼                                        │ │
│  │  ┌────────────────────────────────────────────────────┐   │ │
│  │  │ Resume: Task resume="<id>" prompt="..."            │   │ │
│  │  │  - Include state restoration                       │   │ │
│  │  │  - Provide next steps                              │   │ │
│  │  └────────────────┬───────────────────────────────────┘   │ │
│  │                   └───────────┐                            │ │
│  │                               │ (loop until completion)    │ │
│  │                   ┌───────────┘                            │ │
│  └───────────────────┼────────────────────────────────────────┘ │
└────────────────────────┼───────────────────────────────────────┘
                         │
      ┌──────────────────┼──────────────────┐
      ▼                  ▼                  ▼
┌───────────┐      ┌───────────┐      ┌───────────┐
│ SUBAGENT  │      │ SUBAGENT  │      │ SUBAGENT  │
│ bg-task-1 │      │ bg-task-2 │      │ bg-task-3 │
│ (running) │      │ (running) │      │(completed)│
└─────┬─────┘      └─────┬─────┘      └─────┬─────┘
      │                  │                  │
      │ [PROTOCOL        │ [PROTOCOL        │ [COMPLETION
      │  SIGNALS]        │  SIGNALS]        │  REPORT]
      │                  │                  │
      └──────────────────┴──────────────────┘
```

---

## Protocol Flow Diagrams

### Protocol 1: CLARIFICATION_NEEDED

```
SUBAGENT                      PARENT                        USER
   │                             │                           │
   │ Encounters ambiguity        │                           │
   │ (e.g., which auth strategy?)│                           │
   │                             │                           │
   │ Create checkpoint           │                           │
   │ (save progress)             │                           │
   │                             │                           │
   │──[CLARIFICATION_NEEDED]────>│                           │
   │  - blocked_at: "..."        │                           │
   │  - questions: [Q1, Q2]      │                           │
   │  - current_state: "..."     │                           │
   │                             │                           │
   │                             │ Parse signal              │
   │                             │ Extract questions         │
   │                             │                           │
   │                             │──AskUserQuestion─────────>│
   │                             │  Q1: "Analyze OAuth2,     │
   │                             │       JWT, or both?"      │
   │                             │                           │
   │                             │<─────Answer──────────────│
   │                             │  A1: "Both strategies"    │
   │                             │                           │
   │                             │──AskUserQuestion─────────>│
   │                             │  Q2: "What depth?"        │
   │                             │                           │
   │                             │<─────Answer──────────────│
   │                             │  A2: "Deep dive"          │
   │                             │                           │
   │<────Task resume="id"────────│                           │
   │  prompt: "CLARIFICATION     │                           │
   │           RESPONSE          │                           │
   │           Q1: ... A1: ...   │                           │
   │           Q2: ... A2: ..."  │                           │
   │                             │                           │
   │ Restore state from          │                           │
   │ checkpoint                  │                           │
   │                             │                           │
   │ Continue work with          │                           │
   │ clarified information       │                           │
   │                             │                           │
   ▼                             ▼                           ▼
```

### Protocol 2: STOP_WORK

```
SUBAGENT                      PARENT                    ENVIRONMENT
   │                             │                           │
   │ Attempts operation          │                           │
   │ (e.g., npm audit)           │                           │
   │                             │                           │
   │ Encounters blocker          │                           │
   │ (node_modules missing)      │                           │
   │                             │                           │
   │ Create checkpoint           │                           │
   │ (save completed work)       │                           │
   │                             │                           │
   │────[STOP_WORK]─────────────>│                           │
   │  - blocker_type:            │                           │
   │    "external_dependency"    │                           │
   │  - details: "need npm       │                           │
   │    install"                 │                           │
   │  - state_snapshot: "..."    │                           │
   │                             │                           │
   │                             │ Parse signal              │
   │                             │ Read blocker details      │
   │                             │                           │
   │                             │────npm install───────────>│
   │                             │                           │
   │                             │<───Installation success──│
   │                             │    (node_modules created) │
   │                             │                           │
   │<────Task resume="id"────────│                           │
   │  prompt: "BLOCKER RESOLVED  │                           │
   │           Issue: ...        │                           │
   │           Resolution: ...   │                           │
   │           [state_snapshot]" │                           │
   │                             │                           │
   │ Restore from checkpoint     │                           │
   │                             │                           │
   │ Continue with resolved      │                           │
   │ dependency                  │                           │
   │                             │                           │
   ▼                             ▼                           ▼
```

### Protocol 3: DELEGATE_WORK

```
SUBAGENT A                    PARENT                    SUBAGENT B
   │                             │                           │
   │ Analyzes task               │                           │
   │ Identifies separate         │                           │
   │ parallelizable work         │                           │
   │                             │                           │
   │───[DELEGATE_WORK]──────────>│                           │
   │  - new_task_description     │                           │
   │  - independence:            │                           │
   │    "can_proceed_parallel"   │                           │
   │  - coordination: "..."      │                           │
   │                             │                           │
   │                             │ Evaluate request:         │
   │                             │  - Justified? ✓           │
   │                             │  - Independent? ✓         │
   │                             │  - Resources? ✓           │
   │                             │                           │
   │                             │ Decision: APPROVE         │
   │                             │                           │
   │                             │────/bg launch────────────>│
   │                             │  (new task description)   │
   │                             │                           │
   │                             │                           │ Start work
   │<────Task resume="A"─────────│                           │
   │  prompt: "DELEGATION        │                           │
   │           APPROVED          │                           │
   │           New agent: B      │                           │
   │           Coordination:..." │                           │
   │                             │                           │
   │ Continue work               │                           │ Continue work
   │ (parallel to B)             │                           │ (parallel to A)
   │                             │                           │
   │───[COMPLETION_REPORT]──────>│                           │
   │                             │                           │
   │                             │<──[COMPLETION_REPORT]────│
   │                             │                           │
   │                             │ Both completed            │
   │                             │ Merge results             │
   │                             │ Notify user               │
   │                             │                           │
   ▼                             ▼                           ▼
```

### Protocol 4: COMPLETION_REPORT

```
SUBAGENT                      PARENT                        USER
   │                             │                           │
   │ Completes all phases        │                           │
   │                             │                           │
   │ Validate deliverables       │                           │
   │ Check against metrics       │                           │
   │                             │                           │
   │──[COMPLETION_REPORT]───────>│                           │
   │  - status: "success"        │                           │
   │  - deliverables: [...]      │                           │
   │  - metrics_achieved: "..."  │                           │
   │  - recommendations: [...]   │                           │
   │                             │                           │
   │                             │ Parse signal              │
   │                             │ Validate deliverables     │
   │                             │ exist (file checks)       │
   │                             │                           │
   │                             │ Compare metrics_achieved  │
   │                             │ vs original criteria      │
   │                             │                           │
   │                             │ Extract recommendations   │
   │                             │                           │
   │                             │────Notification──────────>│
   │                             │  "Task completed          │
   │                             │   Deliverables: ...       │
   │                             │   Metrics: ...            │
   │                             │   Next steps: ..."        │
   │                             │                           │
   │ (terminates)                │                           │
   │                             │                           │
   ▼                             ▼                           ▼
```

---

## Multi-Agent Coordination Patterns

### Pattern 1: Parallel Independence

```
                    PARENT
                      │
          ┌───────────┼───────────┐
          ▼           ▼           ▼
     AGENT A      AGENT B      AGENT C
    (module 1)   (module 2)   (module 3)
          │           │           │
          │ No shared │           │
          │ resources │           │
          │           │           │
          ▼           ▼           ▼
     OUTPUT A     OUTPUT B     OUTPUT C
          │           │           │
          └───────────┼───────────┘
                      ▼
                   PARENT
               (merges results)
                      │
                      ▼
              USER NOTIFICATION
```

### Pattern 2: Sequential Dependency

```
                    PARENT
                      │
                      ▼
                  AGENT A
              (setup infrastructure)
                      │
                      ▼
              [DELEGATE_WORK]
           (blocks_current_work)
                      │
                      ▼
                    PARENT
              (launches AGENT B)
                      │
                      ▼
                  AGENT B
              (uses infrastructure)
                      │
                      ▼
              [COMPLETION_REPORT]
                      │
                      ▼
                    PARENT
            (resumes AGENT A with B's results)
                      │
                      ▼
                  AGENT A
              (continues with results)
                      │
                      ▼
              [COMPLETION_REPORT]
```

### Pattern 3: Shared Resource (Append-Only)

```
                    PARENT
                      │
          ┌───────────┼───────────┐
          ▼           ▼           ▼
     AGENT A      AGENT B      AGENT C
    (batch 1)    (batch 2)    (batch 3)
          │           │           │
          │           │           │
          └───────────┼───────────┘
                      ▼
              SHARED_FINDINGS.jsonl
                (append-only)
                      │
                      ▼
       [All agents complete]
                      │
                      ▼
                   PARENT
          (reads SHARED_FINDINGS.jsonl)
          (validates and merges)
                      │
                      ▼
              USER NOTIFICATION
```

---

## State Preservation Flow

### Checkpoint-Resume Mechanism

```
┌──────────────────────────────────────────────────────────┐
│ SUBAGENT EXECUTION                                       │
│                                                          │
│  Phase 1: Initial work                                  │
│  ├─ Task A ✓                                            │
│  ├─ Task B ✓                                            │
│  └─ Task C ✓                                            │
│                                                          │
│  ┌────────────────────────────────────────┐             │
│  │ CREATE CHECKPOINT                      │             │
│  │ - Completed tasks: [A, B, C]           │             │
│  │ - Generated files: [file1, file2]      │             │
│  │ - Working directory: /path/to/work     │             │
│  │ - Next task: Task D                    │             │
│  └────────────────────────────────────────┘             │
│                                                          │
│  Phase 2: Encounter blocker                             │
│  └─ Task D ✗ (blocker: missing dependency)             │
│                                                          │
│  Signal: [STOP_WORK]                                    │
│  - state_snapshot: [checkpoint above]                   │
│  - blocked_work: "Task D"                               │
│                                                          │
│  (agent pauses)                                         │
└──────────────────────────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────┐
│ PARENT RESOLUTION                                        │
│                                                          │
│  - Detects [STOP_WORK] signal                           │
│  - Reads state_snapshot                                 │
│  - Resolves blocker (installs dependency)               │
│  - Prepares resume prompt:                              │
│                                                          │
│    "BLOCKER RESOLVED                                    │
│     Issue: missing dependency                           │
│     Resolution: installed successfully                  │
│                                                          │
│     STATE RESTORATION                                   │
│     Completed: [A, B, C]                                │
│     Files: [file1, file2]                               │
│     Directory: /path/to/work                            │
│                                                          │
│     RESUME INSTRUCTIONS                                 │
│     Continue with Task D                                │
│     Then proceed to Task E"                             │
│                                                          │
│  - Issues: Task resume="<id>" prompt="..."              │
└──────────────────────────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────┐
│ SUBAGENT RESUME                                          │
│                                                          │
│  ┌────────────────────────────────────────┐             │
│  │ RESTORE FROM CHECKPOINT                │             │
│  │ - Verify files: [file1, file2] ✓       │             │
│  │ - Verify directory: /path/to/work ✓    │             │
│  │ - Context: Tasks A, B, C completed     │             │
│  │ - Next: Task D (blocker resolved)      │             │
│  └────────────────────────────────────────┘             │
│                                                          │
│  Phase 2 (resumed): Continue work                       │
│  ├─ Task D ✓ (blocker resolved)                         │
│  ├─ Task E ✓                                            │
│  └─ Task F ✓                                            │
│                                                          │
│  Signal: [COMPLETION_REPORT]                            │
│  - All phases complete                                  │
│  - No work lost during stop/resume                      │
└──────────────────────────────────────────────────────────┘
```

---

## Signal Detection and Parsing Pipeline

```
┌────────────────────────────────────────────────────────┐
│ PARENT MONITORING LOOP                                 │
│                                                        │
│  Every 10-30 seconds:                                  │
│                                                        │
│  1. Poll subagent output                               │
│     ┌──────────────────────────────────────────┐      │
│     │ TaskOutput agent-id=<id> block=false     │      │
│     └────────────────┬─────────────────────────┘      │
│                      ▼                                 │
│  2. Raw output (may contain signal)                    │
│     ┌──────────────────────────────────────────┐      │
│     │ Working on task...                       │      │
│     │ Completed step 1                         │      │
│     │ [CLARIFICATION_NEEDED]                   │      │
│     │ agent_id: bg-task-abc                    │      │
│     │ timestamp: 2026-01-11T09:00:00-05:00     │      │
│     │ ...                                      │      │
│     │ [/CLARIFICATION_NEEDED]                  │      │
│     │ Waiting for response...                  │      │
│     └────────────────┬─────────────────────────┘      │
│                      ▼                                 │
│  3. Detect signal markers                              │
│     ┌──────────────────────────────────────────┐      │
│     │ if grep -q "\[CLARIFICATION_NEEDED\]"    │      │
│     │   → CLARIFICATION protocol               │      │
│     │ if grep -q "\[STOP_WORK\]"               │      │
│     │   → STOP_WORK protocol                   │      │
│     │ if grep -q "\[DELEGATE_WORK\]"           │      │
│     │   → DELEGATE_WORK protocol               │      │
│     │ if grep -q "\[COMPLETION_REPORT\]"       │      │
│     │   → COMPLETION_REPORT protocol           │      │
│     └────────────────┬─────────────────────────┘      │
│                      ▼                                 │
│  4. Extract signal block                               │
│     ┌──────────────────────────────────────────┐      │
│     │ sed -n '/\[SIGNAL\]/,/\[\/SIGNAL\]/p'    │      │
│     └────────────────┬─────────────────────────┘      │
│                      ▼                                 │
│  5. Parse YAML fields                                  │
│     ┌──────────────────────────────────────────┐      │
│     │ agent_id: <extract>                      │      │
│     │ timestamp: <extract>                     │      │
│     │ <protocol-specific fields>               │      │
│     └────────────────┬─────────────────────────┘      │
│                      ▼                                 │
│  6. Execute protocol response                          │
│     ┌──────────────────────────────────────────┐      │
│     │ handle_clarification()                   │      │
│     │ handle_stop_work()                       │      │
│     │ handle_delegation()                      │      │
│     │ handle_completion()                      │      │
│     └──────────────────────────────────────────┘      │
│                                                        │
│  Loop continues until COMPLETION_REPORT received       │
└────────────────────────────────────────────────────────┘
```

---

## Integration Points

### /bg Command Flow

```
USER INPUT
   │
   ▼
/bg "task description"
   │
   ├─ PHASE 1: Validation
   │   ├─ Check context ✓
   │   ├─ Check goals ✓
   │   ├─ Check metrics ✓
   │   └─ Check actionability ✓
   │
   ├─ PHASE 2: Enhancement
   │   ├─ Add EXECUTION CONTEXT
   │   │   ├─ Subagent identity statement
   │   │   ├─ Constraint awareness
   │   │   └─ Protocol instructions
   │   │
   │   ├─ Include 4 signal templates
   │   │   ├─ CLARIFICATION_NEEDED format
   │   │   ├─ STOP_WORK format
   │   │   ├─ DELEGATE_WORK format
   │   │   └─ COMPLETION_REPORT format
   │   │
   │   └─ Launch Task tool
   │       └─ run_in_background=true
   │
   ├─ PHASE 3: Return control
   │   ├─ Inform user of agent ID
   │   └─ Explain monitoring
   │
   ├─ PHASE 4: Monitor (automatic)
   │   ├─ Poll subagent
   │   ├─ Detect signals
   │   ├─ Respond appropriately
   │   └─ Loop until complete
   │
   └─ PHASE 5: Complete
       ├─ Validate deliverables
       └─ Notify user
```

---

## Error Recovery Patterns

### Pattern 1: Retry from Checkpoint

```
SUBAGENT                        PARENT
   │                               │
   │ Phase 1 ✓                     │
   │ Phase 2 ✓                     │
   │ Checkpoint created            │
   │                               │
   │ Phase 3 ✗ (error)             │
   │──[STOP_WORK]─────────────────>│
   │  stop_reason: error           │
   │  state_snapshot: [checkpoint] │
   │                               │
   │                               │ Analyze error
   │                               │ Apply fix
   │                               │
   │<──Task resume="id"────────────│
   │  "ERROR FIXED                 │
   │   Applied fix: ...            │
   │   Restore from: [checkpoint]  │
   │   Retry: Phase 3"             │
   │                               │
   │ Restore checkpoint            │
   │ Retry Phase 3 ✓               │
   │                               │
   ▼                               ▼
```

### Pattern 2: Skip and Continue

```
SUBAGENT                        PARENT
   │                               │
   │ Processing items 1-100        │
   │ Items 1-50 ✓                  │
   │ Item 51 ✗ (error)             │
   │──[STOP_WORK]─────────────────>│
   │  details: "Item 51 error"     │
   │                               │
   │                               │ Decision: Skip problematic
   │                               │
   │<──Task resume="id"────────────│
   │  "SKIP ITEM 51                │
   │   Log to SKIPPED_ITEMS.md     │
   │   Continue with item 52"      │
   │                               │
   │ Log item 51 to skipped        │
   │ Continue items 52-100 ✓       │
   │                               │
   ▼                               ▼
```

### Pattern 3: Delegate Problematic Work

```
SUBAGENT A                      PARENT                  SUBAGENT B
   │                               │                        │
   │ Phase 1 ✓                     │                        │
   │ Phase 2 ✗ (repeated errors)   │                        │
   │──[STOP_WORK]─────────────────>│                        │
   │  blocker_type: error          │                        │
   │  details: "5 retries failed"  │                        │
   │  resume_requirements:         │                        │
   │    "Delegate to specialist"   │                        │
   │                               │                        │
   │                               │ Decision: Delegate     │
   │                               │                        │
   │                               │────/bg launch─────────>│
   │                               │  "Specialized Phase 2" │
   │                               │                        │
   │<──Task resume="A"─────────────│                        │
   │  "PARTIAL RESUME              │                        │
   │   Skip Phase 2 (delegated)    │                        │
   │   Continue Phase 3"           │                        │
   │                               │                        │
   │ Phase 3 ✓                     │    Phase 2 ✓          │
   │                               │       (specialized)    │
   │                               │                        │
   ▼                               ▼                        ▼
```

---

**End of Architecture Diagram**
