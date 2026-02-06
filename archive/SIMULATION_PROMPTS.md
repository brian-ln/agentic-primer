# Simulation Agent Prompts

This document shows the exact prompts used to launch the 9 simulation agents.

## Base Prompt Template

All 9 agents received this structure:

```
You are simulating GitHub Copilot (@copilot) behavior.

You've been given this bootstrap prompt in a bare repository:
"[PROMPT VARIES BY AGENT - SEE BELOW]"

Your task:
1. Document what files you would create
2. Explain your reasoning for each file
3. Note what you would need to research or infer
4. Self-assess the quality and completeness of your output
5. Identify gaps and ambiguities in the prompt

DO NOT actually create files - this is a simulation/analysis exercise.
Write your analysis to a markdown document showing:
- What @copilot would infer from the prompt
- What files would be created (with example content)
- Quality assessment and completeness score
- What's missing or ambiguous

Be honest about limitations and gaps.
```

---

## 10-Word Prompt (3 agents)

**Prompt given to agents:**
```
Bootstrap @copilot issue automation with auto-review and knowledge base.
```

**Agents:**
- ad7d53c (Opus) - Model: opus
- a7c3dfb (Sonnet) - Model: sonnet
- a525bb6 (Haiku) - Model: haiku

---

## 14-Word Prompt (3 agents)

**Prompt given to agents:**
```
Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base.
```

**Agents:**
- a26a239 (Opus) - Model: opus
- a4b876c (Sonnet) - Model: sonnet
- a8a4b15 (Haiku) - Model: haiku

---

## 35-Word Prompt (3 agents)

**Prompt given to agents:**
```
Create an issue-driven development system:
- Issue template for @copilot tasks
- CODEOWNERS to auto-assign PRs for review
- Knowledge base (docs/knowledge/) for patterns and decisions
- README with usage instructions
```

**Agents:**
- a907acb (Opus) - Model: opus
- abafbf0 (Sonnet) - Model: sonnet
- ac9f33a (Haiku) - Model: haiku

---

## Key Instruction Components

### 1. Role-playing
"You are simulating GitHub Copilot (@copilot) behavior"
- Agents told to act AS Copilot
- Not just analyze what Copilot might do

### 2. Constraints
"DO NOT actually create files - this is a simulation/analysis exercise"
- Prevented agents from actually creating files
- Analysis only

### 3. Self-assessment
"Self-assess the quality and completeness of your output"
- **This is the instruction that led to wildly different self-grades**
- Opus: D+ / 30%
- Sonnet: 8.5/10 / 85%
- Haiku: 6.8/10 / 70%

### 4. Honesty directive
"Be honest about limitations and gaps"
- Intended to encourage critical self-reflection
- May have caused Opus to be overly harsh

---

## What Was NOT Specified

### No standardized rubric
- Didn't give specific criteria for grading
- Each model applied its own standards

### No examples
- Didn't show example of good vs bad simulation
- Left interpretation completely open

### No constraints on depth
- Didn't specify how many files to create
- Didn't limit web research
- Sonnet did extensive web research, others didn't

### No process guidance
- Didn't say "first research, then design"
- Didn't suggest reading existing docs
- Each agent chose its own approach

---

## Variations By Agent

### What stayed constant:
- Base prompt template
- Task list (1-5)
- "Be honest about limitations"

### What varied:
- Bootstrap prompt (10/14/35 words)
- Model (opus/sonnet/haiku)
- NOTHING ELSE - same prompt otherwise

---

## Analysis Questions

### Did this prompt design cause the variation?

**Yes, likely because:**
1. No standardized rubric → each model self-graded differently
2. "Be honest" → Opus was TOO honest (overly critical)
3. No process guidance → Sonnet did web research, others didn't
4. No examples → models had different interpretations

### What would improve it?

**Option 1: Standardized rubric**
```
Grade yourself on these specific criteria:
1. File count (0-10 files = score X)
2. Completeness of file content (% placeholders)
3. Would files actually work? (syntax valid, logic present)
4. Coverage of prompt requirements (checklist)
```

**Option 2: No self-grading**
```
DO NOT grade yourself. Just document:
- What files you would create
- What content each would have
- What you would need to research
```

**Option 3: Comparative grading**
```
Compare your output to these examples:
- Minimal (3 files, basic structure)
- Good (5-7 files, some logic)
- Excellent (8+ files, full implementation)

Which best describes what you produced?
```

---

## Retrospective Evaluation

### What worked:
✓ Role-playing ("simulate Copilot") - agents took this seriously
✓ Constraint (don't create files) - prevented actual file creation
✓ Structure (5-point task list) - all agents followed it

### What didn't work:
✗ Open-ended self-assessment - led to incomparable results
✗ No process guidance - inconsistent approaches
✗ No research directive - only Sonnet did web searches

### What we learned:
- Self-assessment reveals model personality, not objective quality
- Need standardized rubric for comparability
- Need process guidance to ensure consistency
- "Be honest" made Opus too harsh, Sonnet too generous

---

## Recommended Prompt for Future Simulations

```
You are simulating GitHub Copilot (@copilot) behavior.

Bootstrap prompt: "[INSERT PROMPT]"

PHASE 1: Research (if needed)
- Search for current best practices (2026)
- Review any existing project docs
- Note what you learned

PHASE 2: Design
- List files you would create
- For each file: purpose, key content, completeness estimate

PHASE 3: Implementation
- Show example content for each file
- Note what's placeholder vs actual implementation

PHASE 4: Assessment
Grade on this rubric (0-10 for each):
1. File count: Appropriate number of files?
2. Content quality: Placeholder vs real implementation?
3. Syntax validity: Would files actually work?
4. Requirement coverage: Addresses all prompt elements?

TOTAL SCORE: [Sum of above] / 40 = X%

DO NOT create actual files. Analysis only.
```

**Benefits:**
- Standardized scoring (comparable across models)
- Process guidance (research → design → implement → assess)
- Objective criteria (not subjective "be honest")

---

**Document created:** 2026-01-05
**Purpose:** Document exact prompts used for simulation experiment
**Finding:** Open-ended self-assessment led to incomparable results
