# Agent Process Summary - Quick Reference

Analysis date: Mon Jan 5 22:00 EST 2026

---

## The Three Archetypes

```
┌─────────────────────────────────────────────────────────────┐
│                     OPUS: "The Philosopher"                  │
│                                                              │
│  Strategy: Pure reasoning, zero tool usage                   │
│  Process:  Analyze → Document → Self-critique               │
│  Output:   Comprehensive gap analysis in single response     │
│  Tools:    NONE                                              │
│  Speed:    ⚡⚡⚡ (2 messages, ~10 seconds)                  │
│  Depth:    ⭐⭐⭐ (thoughtful but no validation)            │
│                                                              │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                    SONNET: "The Researcher"                  │
│                                                              │
│  Strategy: Research + validate before building               │
│  Process:  WebSearch → Read → Bash → Document → Repeat      │
│  Output:   Research-backed recommendations, no files         │
│  Tools:    WebSearch (2-4x), Read (1-5x), Bash (1-6x)       │
│  Speed:    ⚡⚡ (16-29 messages, ~2-3 minutes)              │
│  Depth:    ⭐⭐⭐⭐ (thorough, validated)                   │
│                                                              │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                      HAIKU: "The Builder"                    │
│                                                              │
│  Strategy: Action-first for ambiguous prompts                │
│  Process:  Bash → Read → Write → Read → Update → Repeat     │
│  Output:   6 files created for 10/14-word prompts            │
│  Tools:    Write (6x), Read (4-7x), Bash (3-11x)             │
│  Speed:    ⚡ (26-68 messages, ~3-5 minutes)                │
│  Depth:    ⭐⭐⭐ (extensive but may miss context)          │
│                                                              │
│  EXCEPTION: 35-word prompt → switched to analysis mode!      │
└─────────────────────────────────────────────────────────────┘
```

---

## Process Comparison Matrix

| Characteristic | Opus | Sonnet | Haiku |
|----------------|------|--------|-------|
| **Avg Messages** | 2 | 23 | 52 |
| **Avg Tool Calls** | 0 | 8 | 19 |
| **Web Research** | ❌ Never | ✅ Often | ⚠️ Rarely |
| **File Exploration** | ❌ Never | ✅ Always | ✅ Always |
| **File Creation** | ❌ Never | ❌ Never | ✅ Often* |
| **Self-Critique** | ✅ Strong | ⚠️ Moderate | ❌ Weak |
| **Iteration** | ❌ Single-pass | ✅ Moderate | ✅✅ Heavy |

*Except for 35-word prompt

---

## Tool Usage Breakdown

### Read Tool Usage
- **Opus:** 0/0/0 (10w/14w/35w)
- **Sonnet:** 3/1/5
- **Haiku:** 7/4/4

**Pattern:** Sonnet and Haiku explore codebase, Opus doesn't.

### Write Tool Usage
- **Opus:** 0/0/0
- **Sonnet:** 0/0/0
- **Haiku:** 6/6/0 (!!)

**Pattern:** Only Haiku creates files, and ONLY for short prompts.

### WebSearch Tool Usage
- **Opus:** 0/0/0
- **Sonnet:** 3/4/0
- **Haiku:** 0/0/3

**Pattern:** Sonnet researches proactively, Haiku only for long prompts.

### Bash Tool Usage
- **Opus:** 0/0/0
- **Sonnet:** 2/1/6
- **Haiku:** 10/11/3

**Pattern:** Everyone except Opus uses bash for exploration.

---

## Prompt Length Impact

```
10-word prompt:
├─ Opus:   Analysis-only (2 msgs)
├─ Sonnet: Research + Explore (23 msgs, 3 web searches)
└─ Haiku:  CREATE 6 FILES (68 msgs, 10 bash cmds)

14-word prompt:
├─ Opus:   Analysis-only (2 msgs)
├─ Sonnet: Research + Explore (16 msgs, 4 web searches)
└─ Haiku:  CREATE 6 FILES (62 msgs, 11 bash cmds)

35-word prompt:
├─ Opus:   Analysis-only (2 msgs)
├─ Sonnet: Explore only (29 msgs, 0 web searches)
└─ Haiku:  ANALYSIS-ONLY (!!) (26 msgs, 3 web searches)
            ^^^^^^^^^^^^^^^^
            BEHAVIOR FLIP AT 35 WORDS
```

---

## Strategic Differences

### How Each Model Interpreted "Simulation"

**Opus:**
> "Simulate what you would create" = "Analyze what creating would require"

**Sonnet:**
> "Simulate what you would create" = "Research what should be created, then document it"

**Haiku:**
> "Simulate what you would create" = "Actually create it (if prompt is ambiguous)"

### Information Gathering Strategies

| Stage | Opus | Sonnet | Haiku |
|-------|------|--------|-------|
| **1. Understand task** | Read prompt | Read prompt | Read prompt |
| **2. Gather context** | ❌ Skip | WebSearch + Read | Bash + Read |
| **3. Validate approach** | ❌ Skip | WebSearch patterns | ❌ Skip |
| **4. Take action** | Write analysis | Write analysis | Write files |
| **5. Verify** | Self-critique | ❌ Skip | Read created files |
| **6. Iterate** | ❌ Single-pass | Moderate | Heavy |

---

## Research Patterns

### Sonnet's Web Searches

**10-word prompt:**
1. "GitHub Copilot issue automation auto-review 2026"
2. "GitHub Copilot knowledge base integration best practices 2026"
3. "GitHub issue automation workflows copilot patterns 2026"

**14-word prompt:**
1. "GitHub Copilot workspace agent bootstrap capabilities 2026"
2. "issue-driven development workflow best practices 2026"
3. "GitHub auto-assign pull requests CODEOWNERS 2026"
4. "knowledge base integration development workflow 2026"

**Pattern:** Broad → specific → integration

### Haiku's Web Searches (35-word only!)

1. "GitHub issue templates best practices 2026"
2. "CODEOWNERS file patterns setup guide"
3. "knowledge base documentation best practices engineering teams"

**Pattern:** More tactical, implementation-focused

---

## Exploration Patterns

### Bash Commands by Model

**Sonnet (exploratory):**
```bash
ls -la /path/to/repo
find .github -type f
```
Goal: Understand existing structure

**Haiku (systematic):**
```bash
date                        # Temporal context
pwd && git log --oneline -5 # Git context
ls -la /path/to/repo/       # Structure scan
find . -type f -name "*.md" # File discovery
ls -lah /path/COPILOT*.md   # Verify created files
```
Goal: Full context + verification loop

---

## Files Created by Haiku

### 10-word and 14-word prompts only:

1. `COPILOT_BOOTSTRAP_SIMULATION.md` - Main simulation output
2. `COPILOT_SIMULATION_QUICK_REFERENCE.md` - Quick reference
3. `COPILOT_PRESSURE_TEST_SUMMARY.md` - Pressure test results
4. `COPILOT_SIMULATION_INDEX.md` - Navigation index
5. `COPILOT_FILES_CREATED.md` - File manifest
6. `SIMULATION_RESULTS.md` - Final results

**Note:** All meta-documentation ABOUT the simulation, not actual implementation files.

### 35-word prompt:
**ZERO FILES CREATED** - Haiku switched to analysis mode!

---

## Quality vs Speed Tradeoffs

```
┌─────────────┬──────────┬─────────────┬──────────────┐
│    Model    │  Speed   │ Thoroughness│   Quality    │
├─────────────┼──────────┼─────────────┼──────────────┤
│    Opus     │   ⚡⚡⚡  │      ⭐⭐   │   ⭐⭐⭐⭐   │
│   Sonnet    │   ⚡⚡    │    ⭐⭐⭐⭐  │   ⭐⭐⭐⭐   │
│   Haiku     │   ⚡      │    ⭐⭐⭐⭐⭐ │   ⭐⭐⭐     │
└─────────────┴──────────┴─────────────┴──────────────┘
```

- **Opus:** Fast analysis, may miss practical details
- **Sonnet:** Balanced, research-backed, thorough
- **Haiku:** Most exploration, may over-build or miss nuance

---

## Key Findings

### 1. Model > Prompt Length

Model architecture predicted behavior better than prompt specificity:
- Opus: Always analysis-only (all 3 prompts)
- Sonnet: Always research-first (all 3 prompts)
- Haiku: Varied by prompt length

### 2. Haiku's Clarity Threshold

Haiku behavior flipped at ~35 words:
- **Short prompts (10-14w):** Build aggressively → 6 files, 60+ messages
- **Long prompt (35w):** Analyze first → 0 files, web research

**Implication:** Haiku uses ambiguity as a signal to explore via building.

### 3. Sonnet is the "Safe Simulator"

Sonnet never created files, always researched, always validated.
- Best for: Risk-averse simulation, validation-first approaches
- Worst for: Rapid prototyping, exploratory building

### 4. Opus Over-Analyzes

Opus interpreted "simulate" as "meta-analyze the simulation task."
- Best for: Prompt quality assessment, gap identification
- Worst for: Actual execution, practical outputs

---

## Recommendations by Use Case

### Choose Opus For:
- ✅ Rapid prompt analysis (10 seconds)
- ✅ Gap identification in requirements
- ✅ Self-aware reasoning about limitations
- ❌ NOT for: Actual file creation or research

### Choose Sonnet For:
- ✅ Research-backed recommendations
- ✅ Validation before action
- ✅ Exploring unfamiliar domains safely
- ❌ NOT for: Rapid prototyping or iteration

### Choose Haiku For:
- ✅ Rapid prototyping with short prompts
- ✅ Exploratory building and iteration
- ✅ When verification can happen externally
- ❌ NOT for: High-stakes decisions or ambiguous tasks

---

## Prompt Engineering Guidance

| Your Goal | Model Choice | Prompt Length | Expected Behavior |
|-----------|--------------|---------------|-------------------|
| Analyze prompt quality | Opus | Any | Single-turn analysis |
| Research best practices | Sonnet | 10-20 words | Multi-turn research |
| Build prototype quickly | Haiku | 10-15 words | Aggressive file creation |
| Validated recommendation | Sonnet | 20-30 words | Research + document |
| Meta-analysis of task | Opus | Any | Gap identification |
| Exploratory building | Haiku | <20 words | Build → iterate loop |
| Analysis without building | Haiku | 35+ words | Analysis mode |

---

## Summary Insight

The simulation revealed **three distinct agent philosophies**:

1. **Opus = Cartesian thinker** - "I think, therefore I analyze"
2. **Sonnet = Empiricist** - "I research, therefore I validate"
3. **Haiku = Pragmatist** - "I build, therefore I learn"

None is universally better - they represent different tradeoffs between speed, thoroughness, and risk.

**For @copilot simulation specifically:**
- Sonnet most accurately simulated cautious automation
- Haiku most accurately simulated action-oriented automation
- Opus most accurately simulated self-aware refusal to over-commit

**Meta-lesson:** When launching agents, model selection is a STRATEGIC choice about process philosophy, not just capability.

---

**Full analysis:** See `/Users/bln/play/agentic-primer/AGENT_PROCESS_ANALYSIS.md`
