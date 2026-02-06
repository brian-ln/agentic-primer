# Subagent Protocol Implementation Guide

**Version:** 1.0
**Date:** 2026-01-11
**Purpose:** Practical guide for implementing parent-subagent communication

---

## Quick Start

### For Parent Agents (You)

**1. Launch with Enhanced Prompt:**
```bash
/bg "Task description with clear context, goals, and metrics"
```

The /bg command now automatically adds:
- Subagent identity statement
- Communication protocol instructions
- Signal format templates

**2. Monitor the Subagent:**
```bash
# Poll every 10-30 seconds
TaskOutput agent-id=<agent-id> block=false
```

**3. Watch for Protocol Signals:**
```
Look for these markers in output:
- [CLARIFICATION_NEEDED]
- [STOP_WORK]
- [DELEGATE_WORK]
- [COMPLETION_REPORT]
```

**4. Respond to Signals:**
Follow the response workflows in bg.md (PHASE 4)

---

## Protocol Signal Examples

### Example 1: Clarification Protocol in Action

**User Request:**
```
/bg Analyze the authentication system
```

**Subagent Output:**
```
Starting authentication analysis...
Discovered multiple authentication strategies in codebase.

[CLARIFICATION_NEEDED]
agent_id: bg-task-a1b2c3
timestamp: 2026-01-11T09:00:00-05:00
blocked_at: Analyzing src/auth/ directory structure
reason: Multiple authentication strategies found, unclear which to analyze
questions:
  - question_id: Q1
    text: Should I analyze OAuth2, JWT, or both authentication strategies?
    context: Found separate implementations in oauth.ts and jwt.ts
  - question_id: Q2
    text: What depth of analysis is expected (surface review vs deep dive)?
    context: Deep dive would include dependency analysis, taking 2-3x longer
can_resume_with: Answers specifying auth strategies to analyze and analysis depth
current_state: Discovered 2 auth strategies, catalogued files, ready to analyze
[/CLARIFICATION_NEEDED]
```

**Parent Response:**
```bash
# Ask user (per CLAUDE.md, one by one)
# Then resume:

Task resume="bg-task-a1b2c3" prompt="
## CLARIFICATION RESPONSE

Your questions have been answered:

Q1: Should I analyze OAuth2, JWT, or both authentication strategies?
A1: Analyze both OAuth2 and JWT. They are both in active use.

Q2: What depth of analysis is expected?
A2: Deep dive - include dependency analysis, security patterns, and integration points.

## RESUME INSTRUCTIONS
Continue from: Analyzing src/auth/ directory
Current state: Discovered 2 auth strategies, catalogued files
Next step: Deep dive analysis of both OAuth2 and JWT implementations
Expected deliverables: AUTH_ANALYSIS.md with comprehensive findings
"
```

**Subagent Acknowledgment:**
```
## CLARIFICATION RECEIVED

Answers processed:
- Q1: Analyze both OAuth2 and JWT implementations
- Q2: Perform deep dive analysis (including dependencies and security)

Resuming authentication analysis with expanded scope...
Estimated completion: 2-3 hours
```

---

### Example 2: Stop/Resume Protocol with External Dependency

**User Request:**
```
/bg Run comprehensive security audit including dependency vulnerability scan
```

**Subagent Output:**
```
Security audit started...

Phase 1: Secret scanning
✓ Scanned 120 files
✓ Found 0 hardcoded secrets

Phase 2: SQL injection analysis
✓ Reviewed 23 query locations
✓ All using parameterized queries

Phase 3: Dependency vulnerability scan
✗ Cannot proceed - node_modules directory missing

[STOP_WORK]
agent_id: bg-task-d4e5f6
timestamp: 2026-01-11T09:30:00-05:00
stop_reason: blocker
blocker_type: external_dependency
details: |
  Dependency vulnerability scan requires npm audit, but node_modules is missing.
  Need to run: npm install
completed_work: |
  ✓ Phase 1: Secret scanning complete (0 secrets found)
  ✓ Phase 2: SQL injection analysis complete (no risks)
  ✗ Phase 3: Dependency scan blocked
blocked_work: |
  - npm audit vulnerability scan
  - License compliance check
  - Outdated dependency analysis
state_snapshot: |
  Files scanned: 120/120
  Reports generated: SECRET_SCAN.md, SQL_ANALYSIS.md
  Current directory: /Users/bln/play/agentic-primer
  Checkpoint: Completed phases 1-2, blocked at phase 3
resume_requirements: |
  1. Run: npm install (in parent session)
  2. Verify: node_modules directory exists
  3. Resume with: "Dependencies installed, continue with npm audit"
[/STOP_WORK]
```

**Parent Resolution:**
```bash
# Install dependencies
cd /Users/bln/play/agentic-primer
npm install

# Resume subagent
Task resume="bg-task-d4e5f6" prompt="
## BLOCKER RESOLVED

Issue: node_modules directory missing
Resolution: Ran 'npm install' successfully - 147 packages installed

## STATE RESTORATION
Files scanned: 120/120
Reports generated: SECRET_SCAN.md, SQL_ANALYSIS.md
Current directory: /Users/bln/play/agentic-primer
Checkpoint: Completed phases 1-2, ready for phase 3

## RESUME INSTRUCTIONS
Continue with Phase 3: Dependency vulnerability scan
- Run npm audit and analyze results
- Proceed to license compliance check
- Complete with outdated dependency analysis
Expected completion: 30-45 minutes
"
```

---

### Example 3: Delegation Protocol with Parallel Work

**User Request:**
```
/bg Comprehensive codebase modernization and documentation
```

**Subagent Output:**
```
Codebase modernization analysis started...

Analyzed project structure:
- 82 TypeScript files (3 years old on average)
- 14 documentation files (outdated)
- Build system: webpack 4 (EOL)

Identified 3 major work streams:
1. Code modernization (TS 4.x → 5.x, ESM conversion)
2. Documentation updates (architecture docs, API refs)
3. Build system upgrade (webpack 4 → vite)

These work streams are independent and can proceed in parallel.

[DELEGATE_WORK]
agent_id: bg-task-g7h8i9
timestamp: 2026-01-11T10:00:00-05:00
delegation_reason: |
  Documentation updates are independent from code modernization.
  Running in parallel would reduce total time by ~60%.
  This agent focuses on code, delegated agent handles docs.
new_task_description: |
  Update project documentation to reflect current architecture:

  **Context:**
  - Project: /Users/bln/play/agentic-primer
  - Current docs outdated by ~2 years
  - Focus areas: Architecture, API reference, deployment

  **Goals:**
  - Review all .md files in root and docs/
  - Update architecture diagrams to match current state
  - Regenerate API documentation from code
  - Update deployment guides for current CI/CD

  **Deliverables:**
  - Updated ARCHITECTURE.md
  - Refreshed API_REFERENCE.md
  - Current DEPLOYMENT.md
  - DOCUMENTATION_CHANGES.md (changelog)

  **Success Metrics:**
  - All docs reflect current codebase state
  - No references to deprecated systems
  - Architecture diagrams match actual structure
independence: can_proceed_parallel
priority: P1
context_required: |
  - Current architecture (see ARCHITECTURE_CURRENT_STATE.md - generated by this agent)
  - File structure and module organization
  - Active CI/CD pipeline configuration
coordination: |
  This agent (bg-task-g7h8i9): Code modernization (TS upgrade, ESM conversion)
  New agent: Documentation updates

  Shared resources:
  - NONE (docs in docs/, code in src/ - no overlap)

  Sync points:
  - Both complete independently
  - This agent generates final CODE_MODERNIZATION_REPORT.md
  - New agent generates DOCUMENTATION_CHANGES.md
  - Parent merges reports into MODERNIZATION_SUMMARY.md
estimated_duration: 2-3 hours
[/DELEGATE_WORK]
```

**Parent Evaluation and Response:**
```bash
# Evaluate delegation request
# - Justified? YES (independent work, 60% time savings)
# - Well-defined? YES (clear context, goals, metrics)
# - Truly parallel? YES (separate file trees)
# - Resources available? YES (can run 2 agents)

# APPROVE and launch
/bg "Update project documentation to reflect current architecture:
[paste new_task_description from signal]"

# Resume original agent
Task resume="bg-task-g7h8i9" prompt="
## DELEGATION APPROVED

New agent launched: bg-task-j0k1l2
Task: Documentation updates (architecture, API, deployment)
Expected duration: 2-3 hours

## COORDINATION CONFIRMED
Your work: Code modernization (TS upgrade, ESM conversion)
New agent work: Documentation updates
No resource conflicts (separate file trees)

## RESUME INSTRUCTIONS
Continue with code modernization work:
- TypeScript 4.x → 5.x upgrade
- ESM conversion for all modules
- Generate CODE_MODERNIZATION_REPORT.md when complete

When both agents complete:
- You: CODE_MODERNIZATION_REPORT.md
- bg-task-j0k1l2: DOCUMENTATION_CHANGES.md
- Parent will merge into MODERNIZATION_SUMMARY.md
"
```

**Both Agents Run in Parallel:**
```
Agent bg-task-g7h8i9: [===========60%=====     ] Code modernization
Agent bg-task-j0k1l2: [=============70%===     ] Documentation updates
```

**Both Complete:**
```
[COMPLETION_REPORT] from bg-task-g7h8i9
deliverables: CODE_MODERNIZATION_REPORT.md, src/ (upgraded)
status: success

[COMPLETION_REPORT] from bg-task-j0k1l2
deliverables: DOCUMENTATION_CHANGES.md, docs/ (updated), ARCHITECTURE.md
status: success
```

---

### Example 4: Completion Report

**User Request:**
```
/bg Catalog all TODO comments in codebase and generate prioritized action list
```

**Subagent Output:**
```
TODO cataloging task started...

Phase 1: Scanning codebase
✓ Scanned 120 files
✓ Found 37 TODO comments

Phase 2: Categorizing TODOs
✓ Categorized by type (bug, feature, refactor, docs)
✓ Extracted context and file locations

Phase 3: Prioritizing
✓ Assigned priorities based on context and age
✓ Generated action items

Phase 4: Deliverable generation
✓ Created TODO_LIST.md with prioritized actions
✓ Created TODO_SUMMARY.jsonl with structured data

[COMPLETION_REPORT]
agent_id: bg-task-m3n4o5
timestamp: 2026-01-11T11:00:00-05:00
status: success
deliverables: |
  Generated files:
  - TODO_LIST.md (prioritized action list with context)
  - TODO_SUMMARY.jsonl (structured data, 37 entries)
  - TODO_BY_FILE.md (grouped by file location)
  - TODO_TIMELINE.md (suggested implementation order)
summary: |
  Completed comprehensive TODO cataloging:
  ✓ Scanned 120 files across all modules
  ✓ Found 37 TODO comments
  ✓ Categorized: 12 bugs, 15 features, 7 refactors, 3 docs
  ✓ Prioritized using context and git blame age
  ✓ Generated 4 deliverable documents
metrics_achieved: |
  Success Criteria: [from original prompt]
  ✓ Catalog all TODO comments → 37 found and documented
  ✓ Generate prioritized action list → TODO_LIST.md created
  ✓ Include file locations and context → All entries include file:line and context
  ✓ Suggest implementation order → TODO_TIMELINE.md with sequencing
issues_encountered: |
  1. Found 3 ambiguous TODOs (unclear what action needed)
     → Documented in TODO_LIST.md as "NEEDS_CLARIFICATION"
  2. Found 5 TODOs referencing deleted code
     → Flagged for removal in TODO_LIST.md
recommendations: |
  Immediate actions (P0):
  1. Address 4 security-related TODOs (see TODO_LIST.md lines 15-28)
  2. Clarify 3 ambiguous TODOs with original authors

  Short-term (P1):
  3. Implement 8 bug-fix TODOs (estimated 2-3 days)
  4. Remove 5 obsolete TODOs referencing deleted code

  Long-term (P2):
  5. Feature TODOs can be converted to GitHub issues
  6. Set up automated TODO tracking in CI/CD
total_duration: 45 minutes
[/COMPLETION_REPORT]
```

**Parent Response:**
```
✓ Background task completed successfully

**Agent ID:** bg-task-m3n4o5
**Duration:** 45 minutes

**Deliverables:**
- TODO_LIST.md (prioritized action list)
- TODO_SUMMARY.jsonl (structured data)
- TODO_BY_FILE.md (grouped by location)
- TODO_TIMELINE.md (implementation order)

**Summary:**
Found and cataloged 37 TODO comments:
- 12 bugs
- 15 features
- 7 refactors
- 3 documentation

**Key Findings:**
- 4 security-related TODOs require immediate attention
- 3 TODOs need clarification with authors
- 5 obsolete TODOs can be removed

**Recommendations:**
See TODO_LIST.md for complete prioritized action plan.
Consider setting up automated TODO tracking in CI/CD.
```

---

## Common Patterns and Anti-Patterns

### Pattern 1: Progressive Clarification

**Good:**
```
Agent: Starts work with available context
Agent: Encounters ambiguity at step 3
Agent: Signals CLARIFICATION_NEEDED with specific questions
Parent: Gathers clarification, resumes agent
Agent: Continues from checkpoint
Agent: Completes successfully
```

**Bad:**
```
Agent: Encounters ambiguity at step 1
Agent: Guesses answer without asking
Agent: Produces incorrect results
Agent: Completes with wrong deliverables
```

### Pattern 2: Checkpoint-Resume

**Good:**
```
Agent: Completes phases 1-2
Agent: Hits blocker at phase 3
Agent: Creates checkpoint (saves state, generates reports)
Agent: Signals STOP_WORK with state_snapshot
Parent: Resolves blocker
Parent: Resumes with state restoration
Agent: Continues from phase 3
```

**Bad:**
```
Agent: Hits blocker
Agent: Signals STOP_WORK without checkpoint
Agent: Loses progress from phases 1-2
Parent: Resumes
Agent: Starts over from beginning
```

### Pattern 3: Smart Delegation

**Good:**
```
Agent: Analyzes work breakdown
Agent: Identifies truly independent parallel work
Agent: Signals DELEGATE_WORK with clear coordination plan
Parent: Validates independence claim
Parent: Launches delegated agent
Both: Work in parallel without conflicts
Both: Complete and merge results
```

**Bad:**
```
Agent: Signals DELEGATE_WORK for sequential work
Parent: Launches delegated agent
Both: Compete for same resources
Both: Create conflicts and duplicated work
```

### Pattern 4: Comprehensive Completion

**Good:**
```
Agent: Completes all phases
Agent: Validates against success metrics
Agent: Generates all deliverables
Agent: Signals COMPLETION_REPORT with:
  - All deliverables listed
  - Metrics achieved vs original criteria
  - Issues encountered and resolved
  - Recommendations for follow-up
Parent: Validates deliverables exist
Parent: Reviews completion report
```

**Bad:**
```
Agent: Completes most work
Agent: Signals COMPLETION_REPORT without:
  - Missing deliverables
  - Unchecked metrics
  - No follow-up recommendations
Parent: Accepts incomplete work
```

---

## Troubleshooting

### Problem: Subagent Doesn't Signal

**Symptoms:**
- Agent runs but never sends protocol signals
- Agent guesses instead of asking for clarification
- Agent completes without COMPLETION_REPORT

**Diagnosis:**
- Check if EXECUTION CONTEXT section was added to prompt
- Verify protocol instructions were included
- Look for AskUserQuestion usage (agent may be using wrong pattern)

**Solution:**
- Ensure /bg command includes full protocol instructions
- Re-launch with enhanced prompt
- Test with known-ambiguous task to verify signaling works

### Problem: Parent Can't Detect Signals

**Symptoms:**
- Subagent sends signals but parent doesn't respond
- TaskOutput shows signals but parent doesn't parse them

**Diagnosis:**
- Check signal format (delimiters, YAML syntax)
- Verify polling frequency (may be too slow)
- Look for signal detection logic in parent monitoring

**Solution:**
- Validate signal format against spec (SUBAGENT_PROTOCOLS.md)
- Increase polling frequency (10-30 seconds)
- Use grep/sed to extract signal blocks for parsing

### Problem: Resume Doesn't Restore State

**Symptoms:**
- Agent resumes but starts over from beginning
- Agent loses progress from before stop
- Agent re-does completed work

**Diagnosis:**
- Check if state_snapshot was included in STOP_WORK signal
- Verify resume prompt includes state restoration
- Look for checkpoint files/reports

**Solution:**
- Ensure agents create checkpoints before signaling
- Include full state_snapshot in resume prompt
- Reference completed work explicitly in resume instructions

### Problem: Delegated Agents Conflict

**Symptoms:**
- Multiple agents working on same files
- Duplicated work between agents
- Resource conflicts (file locks, directory conflicts)

**Diagnosis:**
- Check independence claim in DELEGATE_WORK signal
- Verify coordination plan specifies separate resources
- Look for overlapping file trees or tasks

**Solution:**
- Validate independence before approving delegation
- Ensure clear resource boundaries (separate directories)
- Use append-only coordination files for shared data
- Consider sequential dependency instead of parallel

---

## Advanced Usage

### Multi-Level Delegation

```
Parent Agent
  └─ Launch: Agent A (comprehensive analysis)
      └─ Delegate: Agent A1 (module 1 analysis)
      └─ Delegate: Agent A2 (module 2 analysis)
      └─ Delegate: Agent A3 (module 3 analysis)
```

**Coordination:**
- Agent A coordinates A1, A2, A3
- A1, A2, A3 write to shared FINDINGS.jsonl (append-only)
- Agent A merges results when A1, A2, A3 complete

### Conditional Delegation

```yaml
[DELEGATE_WORK]
...
delegation_reason: Only delegate if resource limit exceeded
independence: optional
...
```

**Parent Decision:**
```bash
if resource_limit_exceeded; then
  # Approve delegation
  /bg "delegated task"
else
  # Deny, agent can continue
  Task resume="..." prompt="
  DELEGATION DENIED
  Reason: Resources available, continue with current agent
  "
fi
```

### Error Recovery with Delegation

```yaml
[STOP_WORK]
stop_reason: error
blocker_type: error
details: |
  Encountered repeated errors in module X analysis
  5 retries failed, likely systemic issue
resume_requirements: |
  Option 1: Skip module X, delegate to specialized agent
  Option 2: Fix underlying issue, retry
```

**Parent Response:**
```bash
# Option 1: Delegate problematic module
/bg "Specialized analysis of module X with error handling"

Task resume="original-agent" prompt="
PARTIAL RESUME
Skip module X (delegated to bg-task-specialist)
Continue with modules Y and Z
"
```

---

## Appendix: Signal Parsing Code

### Bash Signal Extraction

```bash
#!/bin/bash
# extract-signal.sh - Extract protocol signal from TaskOutput

extract_signal() {
  local output="$1"
  local signal_type="$2"

  # Extract signal block between delimiters
  echo "$output" | sed -n "/\[$signal_type\]/,/\[\/$signal_type\]/p"
}

parse_yaml_field() {
  local signal="$1"
  local field="$2"

  # Simple YAML field extraction (basic implementation)
  echo "$signal" | grep "^$field:" | sed "s/^$field: *//"
}

# Usage example
output=$(TaskOutput agent-id=bg-task-abc123 block=false)

if echo "$output" | grep -q "\[CLARIFICATION_NEEDED\]"; then
  signal=$(extract_signal "$output" "CLARIFICATION_NEEDED")

  agent_id=$(parse_yaml_field "$signal" "agent_id")
  reason=$(parse_yaml_field "$signal" "reason")
  blocked_at=$(parse_yaml_field "$signal" "blocked_at")

  echo "Agent $agent_id needs clarification:"
  echo "Blocked at: $blocked_at"
  echo "Reason: $reason"
fi
```

### Python Signal Parsing

```python
#!/usr/bin/env python3
# parse-signal.py - Parse protocol signals with YAML

import re
import yaml
from subprocess import run, PIPE

def extract_signal(output, signal_type):
    """Extract signal block from TaskOutput"""
    pattern = f"\[{signal_type}\](.*?)\[/{signal_type}\]"
    match = re.search(pattern, output, re.DOTALL)
    return match.group(1).strip() if match else None

def parse_signal(signal_block):
    """Parse YAML signal block"""
    try:
        return yaml.safe_load(signal_block)
    except yaml.YAMLError as e:
        print(f"YAML parse error: {e}")
        return None

def get_agent_output(agent_id):
    """Get TaskOutput for agent"""
    result = run(
        ["claude", "task", "output", f"agent-id={agent_id}", "block=false"],
        capture_output=True,
        text=True
    )
    return result.stdout

# Usage example
agent_id = "bg-task-abc123"
output = get_agent_output(agent_id)

if "[CLARIFICATION_NEEDED]" in output:
    signal_block = extract_signal(output, "CLARIFICATION_NEEDED")
    signal_data = parse_signal(signal_block)

    if signal_data:
        print(f"Agent {signal_data['agent_id']} needs clarification:")
        print(f"Blocked at: {signal_data['blocked_at']}")
        print(f"Reason: {signal_data['reason']}")

        for q in signal_data['questions']:
            print(f"\n{q['question_id']}: {q['text']}")
            print(f"Context: {q['context']}")
```

---

**End of Implementation Guide**
