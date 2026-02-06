# /bg Command Validation Demonstration

## How the Enhanced Validation Works

This document demonstrates the validation logic in action with concrete examples from the enhanced `/bg` command.

---

## Example 1: BAD PROMPT - Minimal Context

### User Input
```
/bg Analyze the codebase
```

### Validation Process

**Step 1: Extract Task Prompt**
```
Task Prompt: "Analyze the codebase"
```

**Step 2: Run Context Quality Checklist**

1. **Clear Context** - Does the agent know what they need to know?
   - [ ] ❌ Relevant background information provided
   - [ ] ❌ File paths, domains, or systems identified (which codebase?)
   - [ ] ❌ Current state or starting point explained
   - [ ] ❌ Constraints or boundaries specified

2. **Explicit Goals** - Does the agent know WHY and WHAT?
   - [ ] ❌ Clear purpose statement (why analyze?)
   - [ ] ❌ Specific outcomes defined (what does "analyze" mean?)
   - [ ] ❌ Deliverables enumerated (what to produce?)
   - [ ] ❌ Scope boundaries set (what NOT to do)

3. **Success Metrics** - Can the agent measure completion?
   - [ ] ❌ Concrete success criteria defined
   - [ ] ❌ Observable/measurable outcomes
   - [ ] ❌ Quality standards specified
   - [ ] ❌ Completion checkpoints identified

4. **Actionability** - Can the agent start immediately?
   - [ ] ❌ All necessary information present
   - [ ] ❌ No ambiguous terms or vague requirements ("analyze" is vague)
   - [ ] ❌ Clear starting point identified
   - [ ] ❌ First action is obvious

**Step 3: Validation Decision**
```
Is context clear? ────NO───> CLARIFY: "What context does the agent need?"
```

**Step 4: Block Launch and Clarify**

Command uses AskUserQuestion:

```
I need to clarify the context before launching this agent:

- What files, systems, or domains should the agent focus on?
- What is the current state or starting point?
- Are there constraints or boundaries I should specify?
- What background information would help the agent work independently?
```

**Step 5: User Responds**
```
User: Focus on the experiments/iteration-2 directory.
      Analyze the run structure and data completeness.
      Starting point is the directory tree.
      Don't modify any files.
```

**Step 6: Gather Goals**

Command asks:
```
I need to clarify the goals before launching this agent:

- What is the primary purpose of this task (why does it matter)?
- What specific outcomes define success?
- What deliverables should the agent produce?
- What is explicitly OUT of scope?
```

**Step 7: User Responds**
```
User: Purpose is to inventory experimental runs for tracking.
      Success is a complete list of runs with completeness status.
      Deliverable: EXPERIMENTS_INVENTORY.md
      Out of scope: Don't analyze the actual experiment results.
```

**Step 8: Gather Metrics**

Command asks:
```
I need to clarify success metrics before launching this agent:

- How will you know when this task is complete?
- What observable outcomes should I measure?
- What quality standards must be met?
- What checkpoints indicate progress?
```

**Step 9: User Responds**
```
User: Complete when all run directories are documented.
      Observable: Table with columns for run name, date, completeness.
      Quality: All runs accounted for, completeness accurately assessed.
      Checkpoints: Directory scanned, data assessed, table generated.
```

**Step 10: Refine Task Prompt**

Command shows refined prompt:
```
Refined Task Prompt:

Analyze the experiments/iteration-2 directory structure and document:
- What experimental runs exist (scan directory tree)
- What results files are present for each run
- Which runs have complete vs incomplete data
- DO NOT modify any files
- DO NOT analyze experiment results (out of scope)

Purpose: Inventory experimental runs for tracking
Success: Generate EXPERIMENTS_INVENTORY.md with a table showing:
  - Run name
  - Date
  - Data completeness status

Quality standards:
  - All run directories accounted for
  - Completeness accurately assessed
  - Table format with specified columns

Checkpoints:
  1. Directory scanned
  2. Data completeness assessed
  3. Table generated
```

**Step 11: Re-validate**

```
✓ Clear Context (experiments/iteration-2, starting point, constraints)
✓ Explicit Goals (inventory for tracking, specific outcomes)
✓ Success Metrics (table with columns, quality standards)
✓ Actionability (clear starting point, first action obvious)
```

**Step 12: Confirm and Launch**

Command asks:
```
Ready to launch with this refined prompt. Proceed? (y/n)
```

User confirms, and agent launches with complete context.

---

## Example 2: GOOD PROMPT - Immediate Launch

### User Input
```
/bg Review all .md files in the root directory for outdated references to old architecture:
- Search for mentions of "BOOTLOADER", "BOOTSTRAP_SEED_V1", or "session-compact.jsonl"
- These files were deleted per git status
- List each reference with file path and line number
- DO NOT modify any files
Success: Generate OUTDATED_REFERENCES.md with findings
```

### Validation Process

**Step 1: Extract Task Prompt**
```
Task Prompt: [full prompt above]
```

**Step 2: Run Context Quality Checklist**

1. **Clear Context** - Does the agent know what they need to know?
   - [x] ✓ Relevant background information (files were deleted)
   - [x] ✓ File paths, domains, or systems identified (root .md files)
   - [x] ✓ Current state or starting point explained (git status shows deletions)
   - [x] ✓ Constraints or boundaries specified (DO NOT modify)

2. **Explicit Goals** - Does the agent know WHY and WHAT?
   - [x] ✓ Clear purpose statement (find outdated references)
   - [x] ✓ Specific outcomes defined (list references)
   - [x] ✓ Deliverables enumerated (OUTDATED_REFERENCES.md)
   - [x] ✓ Scope boundaries set (don't modify)

3. **Success Metrics** - Can the agent measure completion?
   - [x] ✓ Concrete success criteria (generate findings document)
   - [x] ✓ Observable/measurable outcomes (file path + line number list)
   - [x] ✓ Quality standards (complete list of references)
   - [x] ✓ Completion checkpoints (search, list, generate)

4. **Actionability** - Can the agent start immediately?
   - [x] ✓ All necessary information present
   - [x] ✓ No ambiguous terms (specific file names, clear search terms)
   - [x] ✓ Clear starting point (root .md files)
   - [x] ✓ First action is obvious (search for specific terms)

**Step 3: Validation Decision**
```
Is context clear? ────YES
     │
Are goals explicit? ──YES
     │
Are metrics defined? ─YES
     │
Is it actionable? ────YES
     │
  LAUNCH ✓
```

**Step 4: Generate Short Description**
```
Short Description: "Find outdated architecture references"
```

**Step 5: Enhance Agent Instructions**
```
## Agent Context and Control

You have been launched as a background task. Here's what you need to know:

**Context Provided:**
- Target: All .md files in root directory
- Search terms: "BOOTLOADER", "BOOTSTRAP_SEED_V1", "session-compact.jsonl"
- Background: These files were deleted (per git status)
- Constraint: DO NOT modify any files

**Goals and Success Metrics:**
- Goal: Find and list outdated references to deleted architecture files
- Success: Generate OUTDATED_REFERENCES.md with findings
- Format: List each reference with file path and line number
- Completeness: All references found and documented

**If You Need Clarification During Execution:**
1. STOP work immediately when you encounter missing information
2. Use AskUserQuestion through your parent agent
3. Wait for the response before proceeding
4. Document what was clarified in your output

**Quality Standards:**
- Work autonomously within the provided context
- Ask for clarification rather than guessing
- Validate your work against the success metrics
- Document decisions and rationale
```

**Step 6: Launch Task Tool**
```
Task tool launched with:
- subagent_type: "general-purpose"
- description: "Find outdated architecture references"
- prompt: [enhanced prompt with agent instructions]
- run_in_background: true
```

**Step 7: Post-Launch Communication**
```
✓ Background task launched successfully

**Agent ID:** agent-12345

**How to Control This Agent:**

Check Progress:
  TaskOutput agent-id=agent-12345 block=false

Wait for Completion:
  TaskOutput agent-id=agent-12345 block=true

Stop Agent:
  Background agents run until completion or error.
  No explicit stop mechanism currently available.
  Workarounds:
    1. Let the agent complete (recommended)
    2. Restart Claude Code session (nuclear option)
    3. Answer any blocking AskUserQuestion prompts to unblock

Resume After Clarification:
  If agent blocks on AskUserQuestion, answer the question and use:
    TaskOutput agent-id=agent-12345 block=false
  to check if agent resumed.

**Expected Deliverables:**
- OUTDATED_REFERENCES.md with list of references

**Success Metrics:**
- All .md files in root searched
- All references to deleted files found
- Each reference documented with file path and line number
- File generated successfully
```

---

## Validation Flow Comparison

### BAD PROMPT FLOW
```
User Input
    ↓
Extract Prompt
    ↓
Validate ──FAIL──> Block Launch
    ↓
AskUserQuestion (Context)
    ↓
Gather Response
    ↓
AskUserQuestion (Goals)
    ↓
Gather Response
    ↓
AskUserQuestion (Metrics)
    ↓
Gather Response
    ↓
Refine Prompt
    ↓
Re-validate ──PASS──> Confirm with User
    ↓
Enhance Agent Instructions
    ↓
Launch Task Tool
    ↓
Return Agent ID + Control Info
```

**Time Investment:** 3-5 clarification rounds
**Agent Quality:** High (complete context)
**Waste Prevented:** Agent doesn't start without context

### GOOD PROMPT FLOW
```
User Input
    ↓
Extract Prompt
    ↓
Validate ──PASS──> Generate Short Description
    ↓
Enhance Agent Instructions
    ↓
Launch Task Tool
    ↓
Return Agent ID + Control Info
```

**Time Investment:** Immediate launch
**Agent Quality:** High (complete context from start)
**Efficiency:** Maximum (no clarification needed)

---

## Key Insights

### Why Validation Matters

**Without Validation (Old /bg):**
```
User: /bg Analyze the codebase
Agent: [Launched] ... What codebase? What to analyze?
       [Agent asks clarifying questions mid-execution]
       [User not monitoring, agent blocks]
       [Wasted time until user checks back]
```

**With Validation (New /bg):**
```
User: /bg Analyze the codebase
Command: [Validates] Context unclear. What codebase?
User: experiments/iteration-2
Command: [Validates] Goals unclear. What outcomes?
User: Inventory runs with completeness status
Command: [Validates] Metrics unclear. What defines success?
User: Table with run name, date, completeness
Command: [Re-validates] All clear. Launch?
User: Yes
Agent: [Launched with complete context]
       [Works autonomously without blocking]
       [Completes successfully]
```

### Cost-Benefit Analysis

**Upfront Clarification Cost:**
- 1-3 minutes of Q&A
- User thinks through requirements
- Clear success criteria defined

**Agent Execution Benefit:**
- No mid-execution blocks
- Autonomous operation
- Higher success rate
- Clear deliverables

**Net Result:** 5-10x better outcomes with minimal upfront investment

### Quality Gate Pattern

The enhanced /bg command implements a **quality gate pattern**:

```
┌─────────────────┐
│  User Request   │
└────────┬────────┘
         │
    ┌────▼─────┐
    │ VALIDATE │  ◄── Quality Gate
    └────┬─────┘
         │
    ┌────▼─────┐
    │  BLOCK?  │
    └─┬─────┬──┘
      │     │
     YES   NO
      │     │
      │  ┌──▼────────┐
      │  │  LAUNCH   │
      │  └───────────┘
      │
  ┌───▼────────┐
  │  CLARIFY   │
  └───┬────────┘
      │
  ┌───▼────────┐
  │  REFINE    │
  └───┬────────┘
      │
  ┌───▼────────┐
  │ RE-VALIDATE│ ◄── Quality Gate (again)
  └───┬────────┘
      │
  ┌───▼────────┐
  │  LAUNCH    │
  └────────────┘
```

**Principle:** Never launch without passing validation

**Enforcement:** Strict blocking (no bypass mechanism)

**Result:** High-quality agent launches with complete context

---

## Practical Examples for Testing

### Test Case 1: Validation Catches Vague Terms
```
Input:  /bg Make the system better
Expect: Block on "better" (vague), ask for specific goals
```

### Test Case 2: Validation Catches Missing Context
```
Input:  /bg Generate a report
Expect: Block on missing context (report about what?), ask for domain
```

### Test Case 3: Validation Catches Missing Metrics
```
Input:  /bg Research best practices for testing
Expect: Block on missing metrics (how to measure "best"?), ask for criteria
```

### Test Case 4: Validation Passes on Complete Prompt
```
Input:  /bg Extract common rubric criteria from experiments/*/RUBRIC.md:
        - Scan all RUBRIC.md files in experiment run directories
        - Identify repeated evaluation criteria
        - Synthesize unified template
        Success: UNIFIED_RUBRIC.md with merged criteria
Expect: Pass validation, launch immediately
```

### Test Case 5: Validation Catches Scope Creep
```
Input:  /bg Analyze the entire codebase, refactor everything, write tests
Expect: Block on unbounded scope, ask for prioritization and phasing
```

---

## Conclusion

The enhanced `/bg` command validation framework:

1. **Prevents bad launches** through strict validation
2. **Gathers missing information** through structured clarification
3. **Refines prompts** iteratively until validation passes
4. **Enhances agent context** with explicit instructions
5. **Documents lifecycle** with clear control patterns

**Key Principle:** Strict blocking on insufficient context prevents wasted agent time and ensures high-quality autonomous operation.

**Result:** Agents launch with complete context, work autonomously, and deliver expected outcomes with high success rates.
