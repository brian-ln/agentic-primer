# Agent Interaction Patterns - Visual Process Maps

Analysis date: Mon Jan 5 22:00 EST 2026

---

## Process Flow Diagrams

### Opus: Single-Pass Analysis

```
┌─────────────┐
│ User Prompt │
└──────┬──────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Mental Model Construction          │
│  • Parse prompt semantics           │
│  • Identify ambiguities             │
│  • List required assumptions        │
└──────┬──────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Hypothetical File Planning         │
│  • What WOULD be created            │
│  • Why each file matters            │
│  • What content would go where      │
└──────┬──────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Self-Assessment                    │
│  • Grade each dimension (1-10)      │
│  • Identify critical gaps           │
│  • List required clarifications     │
└──────┬──────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Output Complete Analysis           │
│  (Single assistant message)         │
└─────────────────────────────────────┘

Total time: ~10 seconds
Total messages: 2
Tool calls: 0
```

---

### Sonnet: Research-Validate-Document

```
┌─────────────┐
│ User Prompt │
└──────┬──────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Web Research Phase                 │
│  WebSearch("GitHub Copilot...2026") │──┐
│  WebSearch("issue automation...")   │  │
│  WebSearch("knowledge base...")     │  │
└──────┬──────────────────────────────┘  │
       │                                  │ 3-4 searches
       ▼                                  │ in parallel
┌─────────────────────────────────────┐  │
│  Codebase Exploration Phase         │◄─┘
│  Bash("ls -la repo")                │
│  Read(BOOTLOADER.md)                │──┐
│  Glob("*.md")                       │  │
│  Read(existing files)               │  │
└──────┬──────────────────────────────┘  │
       │                                  │ 2-5 file
       ▼                                  │ reads
┌─────────────────────────────────────┐  │
│  Synthesis Phase                    │◄─┘
│  • Combine web research findings    │
│  • Map to existing codebase         │
│  • Identify patterns to extend      │
└──────┬──────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Documentation Phase                │
│  • What files SHOULD be created     │
│  • Why (research-backed)            │
│  • What patterns to follow          │
│  • What gaps remain                 │
└──────┬──────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Output Analysis (No files created) │
└─────────────────────────────────────┘

Total time: ~2-3 minutes
Total messages: 16-29
Tool calls: 6-9
```

---

### Haiku (10/14-word): Build-Iterate-Refine

```
┌─────────────┐
│ User Prompt │
└──────┬──────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Quick Reconnaissance               │
│  Bash("date")                       │──┐ Gather
│  Bash("pwd && git log")             │  │ context
│  Bash("ls -la")                     │  │ quickly
│  Bash("find . -name *.md")          │  │
└──────┬──────────────────────────────┘◄─┘
       │
       ▼
┌─────────────────────────────────────┐
│  Aggressive File Creation           │
│  Write(SIMULATION.md)               │──┐
│  Write(QUICK_REFERENCE.md)          │  │
│  Write(SUMMARY.md)                  │  │ Create 6
│  Write(INDEX.md)                    │  │ files
│  Write(FILES_CREATED.md)            │  │
│  Write(RESULTS.md)                  │  │
└──────┬──────────────────────────────┘◄─┘
       │
       ▼
┌─────────────────────────────────────┐
│  Verification Phase                 │
│  Read(SIMULATION.md)                │──┐
│  Bash("ls -lah COPILOT*.md")        │  │ Verify
│  Read(created files)                │  │ outputs
└──────┬──────────────────────────────┘◄─┘
       │
       ▼
┌─────────────────────────────────────┐
│  Iteration Phase                    │
│  Edit(file1) - add detail           │──┐
│  Edit(file2) - fix formatting       │  │ Refine
│  Write(new index)                   │  │ based on
│  Read(verify changes)               │  │ reading
└──────┬──────────────────────────────┘◄─┘
       │
       ▼
┌─────────────────────────────────────┐
│  Final Documentation                │
│  Write(master index)                │
└─────────────────────────────────────┘

Total time: ~3-5 minutes
Total messages: 62-68
Tool calls: 23
```

---

### Haiku (35-word): Analysis-First (Behavior Flip!)

```
┌─────────────┐
│ User Prompt │
│ (35 words)  │
└──────┬──────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Recognize Clarity Threshold        │
│  • Prompt is detailed enough        │
│  • Switch to analysis mode          │
└──────┬──────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Web Research Phase                 │
│  WebSearch("issue templates...")    │──┐
│  WebSearch("CODEOWNERS patterns")   │  │ 3 tactical
│  WebSearch("knowledge base docs")   │  │ searches
└──────┬──────────────────────────────┘◄─┘
       │
       ▼
┌─────────────────────────────────────┐
│  Codebase Exploration               │
│  Bash("find . -type f")             │──┐
│  Read(existing files)               │  │ Light
│  Bash("ls .github")                 │  │ exploration
└──────┬──────────────────────────────┘◄─┘
       │
       ▼
┌─────────────────────────────────────┐
│  Documentation Phase                │
│  • What would be created            │
│  • Research-backed approach         │
│  • NO FILE CREATION                 │
└─────────────────────────────────────┘

Total time: ~2 minutes
Total messages: 26
Tool calls: 10
```

---

## Tool Call Sequences

### Opus (All Prompts)
```
START → [no tools] → OUTPUT
```

### Sonnet 10-word (a7c3dfb)
```
START
  ├─ WebSearch (1)
  ├─ WebSearch (2)
  ├─ WebSearch (3)
  ├─ Bash (ls)
  ├─ Glob (*.md)
  ├─ Read (file1)
  ├─ Read (file2)
  ├─ Read (file3)
  ├─ Bash (find)
  └─ OUTPUT (analysis)
```

### Haiku 10-word (a525bb6)
```
START
  ├─ Bash (date)
  ├─ Bash (pwd)
  ├─ Bash (ls)
  ├─ Bash (find)
  ├─ Read (BOOTLOADER.md)
  ├─ Write (file1) ─┐
  ├─ Write (file2)  │
  ├─ Write (file3)  │ Bulk
  ├─ Write (file4)  │ creation
  ├─ Write (file5)  │
  ├─ Write (file6) ─┘
  ├─ Read (file1) ──┐
  ├─ Bash (ls verify)│ Verification
  ├─ Read (file2)    │ loop
  ├─ Read (file3)   ─┘
  ├─ Bash (more checks)
  └─ OUTPUT (summary)
```

---

## Temporal Analysis

### Message Velocity (messages per minute)

Based on log timestamps and message counts:

```
Opus:    ~12 msgs/min (but only 2 total, so N/A)
Sonnet:  ~8-10 msgs/min
Haiku:   ~15-20 msgs/min
```

**Interpretation:**
- Haiku iterates fastest (most messages/min)
- Sonnet deliberates between actions
- Opus completes in single deliberation

---

## Decision Points and Branches

### Key Decision: "Should I create files?"

```
┌─────────────────────────────────┐
│  Prompt received                │
└────────────┬────────────────────┘
             │
             ▼
      ┌──────────────┐
      │ Which model? │
      └──────┬───────┘
             │
        ┌────┴────┬────────┐
        │         │        │
        ▼         ▼        ▼
    ┌─────┐  ┌────────┐ ┌───────┐
    │Opus │  │Sonnet  │ │Haiku  │
    └──┬──┘  └───┬────┘ └───┬───┘
       │         │           │
       ▼         ▼           ▼
      NO        NO      Is prompt
                         clear?
                            │
                       ┌────┴─────┐
                       │          │
                      YES        NO
                       │          │
                       ▼          ▼
                      NO         YES
                               (create
                                files)
```

---

## Iteration Patterns

### Opus: No Iteration
```
Think → Write → Done
[■■■■■■■■■■] Single deliberation
```

### Sonnet: Moderate Iteration
```
Research → Explore → Synthesize → Document
[■■■]      [■■■]      [■■■]        [■■]
```

### Haiku (short prompt): Heavy Iteration
```
Scan → Build → Read → Refine → Read → Update → Verify
[■]    [■■■]   [■]     [■■]     [■]     [■■]    [■]
```

---

## Parallel vs Sequential Tool Usage

### Sonnet: Parallel Research
```
WebSearch(query1) ─┐
WebSearch(query2) ─┼─→ [Wait for all] → Synthesize
WebSearch(query3) ─┘
```

### Haiku: Sequential Build
```
Write(file1) → Write(file2) → Write(file3) → ...
     ↓             ↓              ↓
  [verify]      [verify]       [verify]
```

---

## Risk Profiles by Interaction Pattern

### Opus Pattern Risks
```
✅ Advantages:
   - Fast (2 messages)
   - Comprehensive analysis
   - Self-aware limitations

❌ Risks:
   - No validation against reality
   - May miss practical constraints
   - Over-analyzes vs. acts
```

### Sonnet Pattern Risks
```
✅ Advantages:
   - Research-backed
   - Validates assumptions
   - Explores codebase

❌ Risks:
   - Slower (16-29 messages)
   - May over-research vs. act
   - Conservative (never builds)
```

### Haiku Pattern Risks
```
✅ Advantages:
   - Action-oriented
   - Iterates quickly
   - Learns by building

❌ Risks:
   - May build wrong things
   - Creates 6 files from 10 words
   - Less research-backed
   - Risk of over-building
```

---

## Interaction Depth Metrics

### Average Depth of Exploration

**File System Exploration:**
- Opus: 0 levels (no exploration)
- Sonnet: 2-3 levels (ls, find, read)
- Haiku: 3-4 levels (ls, find, read, verify created)

**Research Depth:**
- Opus: 0 queries (pure reasoning)
- Sonnet: 3-4 queries (broad → specific)
- Haiku: 0-3 queries (only for long prompts)

**Iteration Depth:**
- Opus: 1 pass (single deliberation)
- Sonnet: 2-3 passes (research → explore → synthesize)
- Haiku: 4-6 passes (build → read → refine → verify → update)

---

## Tool Call Patterns by Phase

### Information Gathering Phase

| Model | Tools Used | Pattern |
|-------|------------|---------|
| Opus | None | Pure reasoning |
| Sonnet | WebSearch (3-4x), Bash (1x) | External research |
| Haiku | Bash (3-5x), Read (1-2x) | Local exploration |

### Validation Phase

| Model | Tools Used | Pattern |
|-------|------------|---------|
| Opus | None | Self-critique only |
| Sonnet | Read (2-5x) | Validate against codebase |
| Haiku | Read (3-5x), Bash verify | Verify created files |

### Creation Phase

| Model | Tools Used | Pattern |
|-------|------------|---------|
| Opus | None | Document hypotheticals |
| Sonnet | None | Document plans |
| Haiku | Write (6x)* | Create files |

*10/14-word prompts only

---

## Behavioral Signatures

### How to Identify Each Model from Logs

**Opus signature:**
```
- Total messages: 2
- Tool calls: 0
- Output: Markdown with tables and assessments
- Keywords: "Quality Assessment", "Critical Gaps", "I would need to ask"
```

**Sonnet signature:**
```
- Total messages: 15-30
- WebSearch early in sequence
- Read multiple files
- NO Write calls
- Keywords: "best practices", "research", "patterns"
```

**Haiku signature (short prompts):**
```
- Total messages: 60+
- Bash commands: 10+
- Write calls: 6
- Filename pattern: COPILOT_*.md, SIMULATION_*.md
- Heavy iteration loops
```

**Haiku signature (long prompts):**
```
- Total messages: 25-30
- WebSearch appears
- NO Write calls
- Tactical research queries
```

---

## Key Process Insights

### 1. Tool Call Clustering

**Opus:** No clusters (no tools)
**Sonnet:** Research cluster at start → Exploration cluster → No creation
**Haiku:** Quick exploration → Creation burst → Verification cluster

### 2. Feedback Loop Detection

**Opus:** No feedback loops (single-pass)
**Sonnet:** Weak loops (research → read → synthesize, but rarely cycles back)
**Haiku:** Strong loops (write → read → update → read → refine)

### 3. Exploration vs Exploitation

| Model | Exploration | Exploitation |
|-------|-------------|--------------|
| Opus | 0% | 100% (of reasoning space) |
| Sonnet | 70% | 30% |
| Haiku | 40% | 60% |

### 4. Risk-Taking Behavior

**Opus:** Risk-averse (won't commit to specifics without info)
**Sonnet:** Moderately risk-averse (research before recommending)
**Haiku:** Risk-tolerant (build first, validate later)

---

## Recommendations for Agent Orchestration

### Serial vs Parallel Agent Usage

**Serial: Opus → Sonnet → Haiku**
```
1. Opus analyzes prompt quality
2. Sonnet researches best approach
3. Haiku implements (if prompt >= 35 words for analysis, or <20 for building)
```

**Parallel: All three simultaneously**
```
├─ Opus: Gap analysis (10 sec)
├─ Sonnet: Research-backed plan (2 min)
└─ Haiku: Rapid prototype (3 min)

Then compare outputs
```

### Optimal Agent Selection by Task Type

| Task Type | Best Model | Why |
|-----------|------------|-----|
| Prompt quality check | Opus | Fast, comprehensive analysis |
| Research synthesis | Sonnet | Web research + validation |
| Rapid prototyping (ambiguous) | Haiku | Will build exploratively |
| Validated recommendation | Sonnet | Research-backed, won't over-commit |
| Meta-analysis | Opus | Self-reflective reasoning |
| Iterative refinement | Haiku | Iteration loops built-in |

---

## Conclusion

The interaction patterns reveal three distinct agent **process philosophies**:

1. **Opus = Deliberative reasoning** (think deeply, act never)
2. **Sonnet = Empirical validation** (research, validate, recommend)
3. **Haiku = Pragmatic iteration** (build, measure, refine)

**Key finding:** Model selection is not about capability—it's about **process philosophy alignment** with your task requirements.

For the @copilot simulation:
- **Opus** showed what analysis looks like
- **Sonnet** showed what research looks like
- **Haiku** showed what building looks like

All are valid simulations of different agent types.

---

**Related documents:**
- Full analysis: `/Users/bln/play/agentic-primer/AGENT_PROCESS_ANALYSIS.md`
- Quick reference: `/Users/bln/play/agentic-primer/AGENT_PROCESS_SUMMARY.md`
