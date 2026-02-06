# /bg Command Enhancement Summary

## Overview

Enhanced the `/bg` command at `~/.claude/commands/bg.md` to enforce intelligent task validation and enable clarification workflows before launching background agents.

**File:** `/Users/bln/.claude/commands/bg.md`
**Date:** 2026-01-11
**Size Change:** 35 lines → 342 lines (9.7x increase)

## What Changed

### Before: Simple Launch-Only Command
The original `/bg` command was a minimal wrapper that:
- Extracted task description from arguments
- Generated short description
- Launched Task tool immediately
- Returned agent ID

**Problems:**
- No validation of task quality
- Agents launched with incomplete context
- No way to clarify missing information
- No guidance on agent control
- No examples of good vs bad prompts

### After: Intelligent Validation Framework
The enhanced `/bg` command now includes:

1. **Pre-Launch Validation (Phase 1)**
   - Context Quality Checklist (4 dimensions)
   - Validation Decision Tree
   - Clarification Workflow with AskUserQuestion templates
   - Task prompt refinement process

2. **Enhanced Launch (Phase 2)**
   - Agent instruction embedding
   - Context summary injection
   - Agent-side clarification capability

3. **Post-Launch Communication (Phase 3)**
   - Clear agent control documentation
   - Stop/resume patterns
   - Best practices guide

4. **Validation Examples**
   - 5 concrete examples (2 bad, 3 good)
   - Demonstrates validation catching issues
   - Shows clarification workflow

## Key Features

### 1. Context Quality Checklist

Four validation dimensions that MUST pass before launch:

**Clear Context:**
- Relevant background information provided
- File paths, domains, or systems identified
- Current state or starting point explained
- Constraints or boundaries specified

**Explicit Goals:**
- Clear purpose statement (why this matters)
- Specific outcomes defined (what success looks like)
- Deliverables enumerated (what to produce)
- Scope boundaries set (what NOT to do)

**Success Metrics:**
- Concrete success criteria defined
- Observable/measurable outcomes
- Quality standards specified
- Completion checkpoints identified

**Actionability:**
- All necessary information present
- No ambiguous terms or vague requirements
- Clear starting point identified
- First action is obvious

### 2. Validation Decision Tree

```
Is context clear? ────NO───> CLARIFY: "What context does the agent need?"
     │
    YES
     │
Are goals explicit? ──NO───> CLARIFY: "What outcomes define success?"
     │
    YES
     │
Are metrics defined? ─NO───> CLARIFY: "How will you measure completion?"
     │
    YES
     │
Is it actionable? ────NO───> CLARIFY: "What information is missing?"
     │
    YES
     │
  LAUNCH ✓
```

### 3. Clarification Workflow

When validation fails, the command:
1. **BLOCKS launch** (no agent started)
2. Uses **AskUserQuestion** to gather missing information
3. Provides **context-specific question templates**:
   - For missing context
   - For missing goals
   - For missing metrics
   - For missing actionability
4. **Refines the task prompt** with gathered information
5. **Re-validates** against checklist
6. **Confirms with user** before launching

### 4. Agent-Side Clarification

Launched agents receive enhanced instructions:

```
## Agent Context and Control

You have been launched as a background task. Here's what you need to know:

**Context Provided:**
[Summarize the validated context here]

**Goals and Success Metrics:**
[Summarize the validated goals and metrics here]

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

### 5. Agent Lifecycle Documentation

Clear guidance on:
- How to check progress (`TaskOutput block=false`)
- How to wait for completion (`TaskOutput block=true`)
- Current limitations (no explicit stop mechanism)
- Workarounds (let complete, restart session, answer blocking questions)
- Resume patterns (after AskUserQuestion)
- Best practices (complete context upfront, monitor progress)

## Validation Examples

### Example 1: BAD - Missing Everything
```
User: /bg Analyze the codebase
```

**Validation Result:** ❌ FAILED
- Missing: Which codebase? What to analyze? Why?
- Missing: Success criteria undefined
- Missing: Deliverables not specified
- **Action:** BLOCK and clarify

---

### Example 2: BAD - Vague Goals
```
User: /bg Look at the experiments folder and tell me what's interesting
```

**Validation Result:** ❌ FAILED
- Context: ✓ (experiments folder)
- Goals: ❌ (What counts as "interesting"?)
- Metrics: ❌ (How to measure "interesting"?)
- **Action:** BLOCK and clarify

---

### Example 3: GOOD - Clear Context, Goals, Metrics
```
User: /bg Analyze the experiments/iteration-2 directory structure and document:
- What experimental runs exist
- What results files are present for each run
- Which runs have complete vs incomplete data
Success: Generate EXPERIMENTS_INVENTORY.md with a table showing run name, date, and data completeness
```

**Validation Result:** ✓ PASSED
- Context: ✓ (experiments/iteration-2 directory)
- Goals: ✓ (Inventory runs and assess completeness)
- Metrics: ✓ (Table with specific columns)
- Deliverables: ✓ (EXPERIMENTS_INVENTORY.md)
- **Action:** LAUNCH

---

### Example 4: GOOD - With Constraints
```
User: /bg Review all .md files in the root directory for outdated references to old architecture:
- Search for mentions of "BOOTLOADER", "BOOTSTRAP_SEED_V1", or "session-compact.jsonl"
- These files were deleted per git status
- List each reference with file path and line number
- DO NOT modify any files
Success: Generate OUTDATED_REFERENCES.md with findings
```

**Validation Result:** ✓ PASSED
- Context: ✓ (Root .md files, deleted files list)
- Goals: ✓ (Find outdated references)
- Metrics: ✓ (List with file paths and line numbers)
- Constraints: ✓ (No modifications)
- Deliverables: ✓ (OUTDATED_REFERENCES.md)
- **Action:** LAUNCH

---

### Example 5: GOOD - With Quality Standards
```
User: /bg Extract reusable patterns from experiments/iteration-2/runs/*/ENHANCED_RUBRIC.md:
- Read all ENHANCED_RUBRIC.md files across run directories
- Identify common evaluation criteria patterns
- Compare rubrics for consistency
- Synthesize a unified rubric template
Quality: Must preserve all unique criteria, merge duplicates, maintain scoring scales
Success: Generate UNIFIED_RUBRIC_TEMPLATE.md with merged criteria and usage examples
```

**Validation Result:** ✓ PASSED
- Context: ✓ (Run directories, rubric files)
- Goals: ✓ (Extract and synthesize patterns)
- Metrics: ✓ (Preserve unique, merge dupes, maintain scales)
- Quality: ✓ (Explicit quality standards)
- Deliverables: ✓ (UNIFIED_RUBRIC_TEMPLATE.md)
- **Action:** LAUNCH

## Before/After Comparison

### Before (35 lines)
```markdown
---
description: Launch a background task using the Task tool
argument-hint: [task-description]
---

# Launch Background Task

You must launch a background task with the Task tool using these exact steps:

## 1. Extract the Task Prompt
The full task prompt is: $ARGUMENTS

## 2. Generate Short Description
[...]

## 3. Launch the Task Tool
[...]

## 4. Return the Agent ID
[...]
```

**Structure:**
- Single-phase (launch only)
- No validation
- No examples
- No agent control guidance

### After (342 lines)
```markdown
---
description: Launch a background task using the Task tool with intelligent validation
argument-hint: [task-description]
---

# Launch Background Task

You must launch a background task with the Task tool using these exact steps:

## PHASE 1: PRE-LAUNCH VALIDATION (CRITICAL - DO NOT SKIP)

### Context Quality Checklist
[4 validation dimensions with checklists]

### Validation Decision Tree
[Clear decision flow]

### When Validation Fails: Clarification Workflow
[4 question templates for different failure modes]

### Refining the Task Prompt
[Refinement process]

## PHASE 2: PREPARE THE LAUNCH

### 1. Extract the Task Prompt
[...]

### 2. Generate Short Description
[...]

### 3. Enhance Agent Instructions
[Agent context and clarification capability]

### 4. Launch the Task Tool
[...]

## PHASE 3: POST-LAUNCH COMMUNICATION

### Return the Agent ID
[Enhanced feedback with control guidance]

## AGENT LIFECYCLE MANAGEMENT

### Stopping Agents
[Current behavior and workarounds]

### Resuming Agents
[After clarification patterns]

## VALIDATION EXAMPLES

[5 examples: 2 bad, 3 good]

## SUMMARY

[Key enforcement points]
```

**Structure:**
- Three-phase (validate → launch → communicate)
- Comprehensive validation framework
- 5 concrete examples
- Agent lifecycle documentation
- Clarification workflow integration
- Quality enforcement

## Success Metrics Achieved

✓ **bg.md updated with validation logic**
- 4-dimension checklist (Context, Goals, Metrics, Actionability)
- Decision tree for validation flow
- Clear pass/fail criteria

✓ **Can detect missing context/goals/metrics in prompts**
- Examples 1-2 demonstrate detection
- Validation checklist catches issues
- Specific failure modes identified

✓ **AskUserQuestion integration for clarifications**
- 4 question templates for different gaps
- Clear workflow: block → clarify → refine → validate
- User confirmation before launch

✓ **Stop/resume agent patterns documented**
- Current behavior explained
- Workarounds provided
- Best practices included
- TaskOutput usage clarified

✓ **At least 5 examples demonstrating validation**
- 2 bad examples (showing validation catching issues)
- 3 good examples (showing successful validation)
- Each example includes validation result breakdown

✓ **Documentation is clear enough for users to understand requirements**
- Progressive disclosure (checklist → decision tree → templates)
- Concrete examples for each concept
- Clear phase structure (validate → launch → communicate)

✓ **Agents receive necessary context to work autonomously**
- Enhanced agent instructions embedded in prompt
- Context and goals summarized
- Agent-side clarification capability enabled
- Quality standards specified

## Implementation Notes

### Design Decisions

1. **Strict Blocking Approach**
   - Validation failures BLOCK launch (no partial launches)
   - Forces complete context before agent starts
   - Prevents wasted agent time on unclear tasks

2. **AskUserQuestion Integration**
   - Context-specific question templates
   - Progressive refinement (gather → refine → re-validate)
   - User confirms before launch

3. **Agent Instruction Enhancement**
   - Context/goals embedded in agent prompt
   - Clarification capability built in
   - Quality standards made explicit

4. **Lifecycle Documentation**
   - Honest about current limitations (no stop mechanism)
   - Practical workarounds provided
   - Best practices emphasized

### What's NOT Included

1. **Automated Context Extraction**
   - Could analyze codebase to suggest context
   - Deferred (requires more infrastructure)

2. **Agent Stop Mechanism**
   - Not available in current Claude Code
   - Documented as limitation
   - Workarounds provided

3. **Success Metric Templates**
   - Could provide domain-specific metric templates
   - Deferred (user can specify custom metrics)

4. **Validation Bypass**
   - Intentionally NOT included
   - All launches must pass validation
   - Enforces quality standards

## Usage Guide

### How to Use Enhanced /bg Command

1. **Prepare Your Task Prompt**
   - Include context (what the agent needs to know)
   - Specify goals (why and what outcomes)
   - Define metrics (how to measure success)
   - Make it actionable (clear starting point)

2. **Run the Command**
   ```
   /bg [your-task-prompt]
   ```

3. **If Validation Fails**
   - Read the clarification questions
   - Provide missing information
   - Command will refine prompt and re-validate

4. **If Validation Passes**
   - Agent launches with enhanced context
   - Receive agent ID and control instructions
   - Monitor progress with TaskOutput

5. **Monitor and Control**
   - Check status: `TaskOutput agent-id=<id> block=false`
   - Wait for completion: `TaskOutput agent-id=<id> block=true`
   - Answer blocking questions if agent needs clarification

### Tips for Writing Good Prompts

**DO:**
- ✓ Specify file paths, directories, or domains
- ✓ Explain why the task matters
- ✓ List concrete deliverables
- ✓ Define what counts as "done"
- ✓ Include constraints (what NOT to do)
- ✓ Specify quality standards

**DON'T:**
- ✗ Use vague terms ("interesting", "good", "improve")
- ✗ Omit context (which codebase? what state?)
- ✗ Skip deliverables (agent won't know what to produce)
- ✗ Forget metrics (how will you measure completion?)
- ✗ Leave scope unbounded (what's in vs out?)

## Testing Recommendations

To test the enhanced command:

1. **Test Bad Prompts (Should Block)**
   ```
   /bg Analyze the code
   /bg Make the system better
   /bg Look at the experiments
   ```
   Expected: Validation fails, clarification questions asked

2. **Test Good Prompts (Should Launch)**
   ```
   /bg Analyze experiments/iteration-2 directory and create INVENTORY.md listing all runs with completeness status

   /bg Review root .md files for references to deleted files (BOOTLOADER.md, session-compact.jsonl) and generate OUTDATED_REFS.md

   /bg Extract common patterns from experiments/*/RUBRIC.md files and synthesize UNIFIED_RUBRIC.md with merged criteria
   ```
   Expected: Validation passes, agent launches

3. **Test Clarification Workflow**
   ```
   /bg Analyze the experiments
   ```
   Expected: Questions about what to analyze, what outcomes, what metrics

   Answer questions, then:
   Expected: Refined prompt, re-validation, launch

4. **Test Agent Control**
   After launch:
   ```
   TaskOutput agent-id=<id> block=false  # Check progress
   TaskOutput agent-id=<id> block=true   # Wait for completion
   ```
   Expected: Clear status updates

## Related Work

This enhancement complements:

- **Beads Issue Tracker**: Similar context/goals/metrics validation
- **Activity System**: Context propagation patterns
- **Agent SDK**: Clarification and human-in-the-loop patterns

## Future Enhancements

Potential improvements:

1. **Context Templates by Domain**
   - Code analysis context template
   - Documentation task template
   - Testing task template

2. **Metric Suggestions**
   - Auto-suggest metrics based on task type
   - Provide metric examples library

3. **Agent Portfolio**
   - Track launched agents
   - Aggregate status checks
   - Batch control operations

4. **Validation Scoring**
   - Score prompt quality (0-100)
   - Suggest improvements even when passing
   - Track quality over time

5. **Agent Stop Mechanism**
   - If/when Claude Code adds stop capability
   - Update lifecycle documentation

## Conclusion

The enhanced `/bg` command transforms background task launching from a simple wrapper into an intelligent validation framework that:

- **Enforces quality** through strict validation
- **Prevents wasted effort** by blocking unclear tasks
- **Enables clarification** through AskUserQuestion integration
- **Empowers agents** with context and clarification capability
- **Documents lifecycle** with clear control patterns
- **Demonstrates validation** with concrete examples

**Key Innovation:** Shift from "launch and hope" to "validate, clarify, then launch with confidence."

The command now serves as a **quality gate** that ensures agents have everything they need to work autonomously and successfully.
