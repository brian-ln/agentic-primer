# Subagent Communication Protocols

**Version:** 1.0
**Date:** 2026-01-11
**Purpose:** Standardize parent-subagent communication for /bg background tasks

---

## Overview

Background subagents run in isolated processes and cannot directly interact with users. This document defines structured protocols for:

1. **Subagent identity awareness** - Making subagents aware of their constraints
2. **Clarification protocol** - Requesting missing information from parent
3. **Stop/resume protocol** - Pausing and resuming work with additional context
4. **Delegation protocol** - Requesting new background agents for additional work

---

## Protocol Design Principles

### Signal Format Requirements
- **Machine-parsable**: Structured format with clear delimiters
- **Human-readable**: Easy to understand in task output logs
- **Action-oriented**: Each signal triggers specific parent behavior
- **Stateless**: Signals are self-contained and context-free

### Parent Monitoring Strategy
- **Periodic polling**: Use `TaskOutput agent-id=<id> block=false` to check status
- **Signal detection**: Parse output for protocol signal markers
- **Immediate response**: React to signals within 1-2 polling cycles
- **State preservation**: Maintain context across stop/resume cycles

---

## Protocol 1: Subagent Identity Statement

### Purpose
Inform subagents of their execution context and constraints from the start.

### Implementation
The parent agent MUST add this section to every background task prompt:

```markdown
## EXECUTION CONTEXT: BACKGROUND SUBAGENT

**CRITICAL AWARENESS:**
You are executing as a BACKGROUND SUBAGENT in an isolated process.

**Your Constraints:**
- ❌ You CANNOT directly interact with the user
- ❌ You CANNOT access the user's interactive shell
- ❌ You CANNOT use interactive prompts or menus
- ✅ All user communication MUST go through your parent agent
- ✅ Use the protocols below when you need help

**Available Communication Protocols:**
1. CLARIFICATION_NEEDED - Request missing information
2. STOP_WORK - Signal that you cannot proceed
3. DELEGATE_WORK - Request another background agent
4. COMPLETION_REPORT - Signal successful completion

**Your Parent Agent:**
- Monitors your output periodically
- Will respond to protocol signals
- Can resume you with additional context
- Coordinates with the user on your behalf
```

### Example Integration
```markdown
User request: "Analyze the codebase for security issues"

Enhanced prompt sent to subagent:
"""
## EXECUTION CONTEXT: BACKGROUND SUBAGENT
[Identity statement above]

## YOUR TASK
Analyze the codebase in /Users/bln/play/agentic-primer for security issues:
- Scan for hardcoded secrets
- Check for SQL injection vulnerabilities
- Review authentication patterns
...
"""
```

---

## Protocol 2: Clarification Protocol

### Purpose
Enable subagents to request missing information and block until it arrives.

### Signal Format

```yaml
[CLARIFICATION_NEEDED]
agent_id: <agent-id>
timestamp: <ISO-8601-timestamp>
blocked_at: <specific task/step/file where work stopped>
reason: <why clarification is needed>
questions:
  - question_id: Q1
    text: <question text>
    context: <why this matters>
  - question_id: Q2
    text: <question text>
    context: <why this matters>
can_resume_with: <what info would unblock>
current_state: <what was completed before blocking>
[/CLARIFICATION_NEEDED]
```

### Example Signal

```yaml
[CLARIFICATION_NEEDED]
agent_id: bg-task-abc123
timestamp: 2026-01-11T08:45:00-05:00
blocked_at: Analyzing authentication patterns in src/auth/
reason: Multiple authentication strategies found, unclear which to analyze
questions:
  - question_id: Q1
    text: Should I analyze OAuth2, JWT, or both authentication strategies?
    context: Found separate implementations in src/auth/oauth.ts and src/auth/jwt.ts
  - question_id: Q2
    text: What security framework version should I assume (2.0 or 3.0)?
    context: Dependencies show both @auth/core@2.x and @auth/core@3.x-beta
can_resume_with: Answers specifying which auth strategies to analyze and framework version assumptions
current_state: Completed secret scanning (found 0 hardcoded secrets), started auth analysis
[/CLARIFICATION_NEEDED]
```

### Parent Detection and Response

**Detection:**
```bash
# Parent polls subagent output
output=$(TaskOutput agent-id=bg-task-abc123 block=false)

# Check for clarification signal
if echo "$output" | grep -q "\[CLARIFICATION_NEEDED\]"; then
  # Extract questions and context
  # Present to user via AskUserQuestion
fi
```

**Response Workflow:**
1. **Detect signal** in TaskOutput
2. **Extract questions** from signal block
3. **Ask user** via AskUserQuestion (one by one per CLAUDE.md)
4. **Resume subagent** with answers via Task resume:

```bash
# Parent resumes with clarification
Task resume="bg-task-abc123" prompt="
## CLARIFICATION RESPONSE

Your questions have been answered:

Q1: Should I analyze OAuth2, JWT, or both authentication strategies?
A1: Analyze both OAuth2 and JWT. They are both in active use.

Q2: What security framework version should I assume (2.0 or 3.0)?
A2: Assume 3.0-beta. We are migrating to 3.x.

## RESUME INSTRUCTIONS
Continue from: Analyzing authentication patterns in src/auth/
Current state: Completed secret scanning, started auth analysis
Next step: Analyze both OAuth2 and JWT implementations with 3.0-beta assumptions
"
```

### Subagent Responsibilities

**When to signal:**
- Missing information prevents proceeding
- Ambiguous requirements have multiple valid interpretations
- User preference needed for subjective decisions

**When NOT to signal:**
- You can make reasonable assumptions
- Information is discoverable in the codebase
- Decision is purely technical with clear best practice

**After resuming:**
```markdown
## CLARIFICATION RECEIVED

[Acknowledge what was clarified]
- Q1 answered: Analyze both OAuth2 and JWT
- Q2 answered: Assume framework version 3.0-beta

[Continue work with new context]
Resuming authentication analysis with both strategies...
```

---

## Protocol 3: Stop/Resume Protocol

### Purpose
Enable subagents to signal they cannot proceed and need parent intervention.

### Signal Format

```yaml
[STOP_WORK]
agent_id: <agent-id>
timestamp: <ISO-8601-timestamp>
stop_reason: <why stopping - blocker|error|completion>
blocker_type: <missing_info|external_dependency|error|resource_limit>
details: <description of blocker>
completed_work: <what was finished>
blocked_work: <what could not be started>
state_snapshot: <checkpoint for resume>
resume_requirements: <what is needed to continue>
[/STOP_WORK]
```

### Example Signal: Missing External Dependency

```yaml
[STOP_WORK]
agent_id: bg-task-def456
timestamp: 2026-01-11T09:15:00-05:00
stop_reason: blocker
blocker_type: external_dependency
details: Security analysis requires running npm audit, but node_modules is missing
completed_work: |
  ✓ Scanned for hardcoded secrets (0 found)
  ✓ Analyzed SQL queries (no injection risks)
  ✓ Reviewed authentication (see AUTH_ANALYSIS.md)
blocked_work: |
  ✗ Dependency vulnerability scan (requires npm audit)
  ✗ License compliance check (requires npm ls)
state_snapshot: |
  Files analyzed: 47/120
  Reports generated: AUTH_ANALYSIS.md, SQL_REVIEW.md
  Current directory: /Users/bln/play/agentic-primer/src
resume_requirements: |
  Run: npm install (in parent session)
  Then resume with: "Dependencies installed, continue with npm audit scan"
[/STOP_WORK]
```

### Example Signal: Resource Limit

```yaml
[STOP_WORK]
agent_id: bg-task-ghi789
timestamp: 2026-01-11T09:30:00-05:00
stop_reason: blocker
blocker_type: resource_limit
details: Analysis of 10,000+ files will exceed reasonable task duration
completed_work: |
  ✓ Analyzed file structure (10,247 files discovered)
  ✓ Identified file types (47% .ts, 23% .md, 30% other)
  ✓ Created analysis strategy (see ANALYSIS_PLAN.md)
blocked_work: |
  ✗ Full file-by-file analysis (estimated 6+ hours)
state_snapshot: |
  Discovered: 10,247 files in scope
  Strategy: Created batched analysis plan
  Recommendation: Use multiple parallel agents
resume_requirements: |
  Option 1: Reduce scope (specify file patterns to exclude)
  Option 2: Delegate to multiple agents (see DELEGATE_WORK protocol)
  Option 3: Continue with single agent (accept 6+ hour runtime)
[/STOP_WORK]
```

### Parent Response Workflow

**1. Detect Stop Signal:**
```bash
output=$(TaskOutput agent-id=bg-task-def456 block=false)
if echo "$output" | grep -q "\[STOP_WORK\]"; then
  # Extract blocker details
  # Determine response action
fi
```

**2. Resolve Blocker:**
```bash
# Example: Install dependencies for subagent
cd /Users/bln/play/agentic-primer
npm install
```

**3. Resume with Context:**
```bash
Task resume="bg-task-def456" prompt="
## BLOCKER RESOLVED

Issue: node_modules missing
Resolution: Ran npm install successfully

## STATE RESTORATION
Files analyzed: 47/120
Reports generated: AUTH_ANALYSIS.md, SQL_REVIEW.md
Working directory: /Users/bln/play/agentic-primer/src

## RESUME INSTRUCTIONS
Continue with dependency vulnerability scan (npm audit)
Then proceed to license compliance check (npm ls)
"
```

### State Preservation Between Stop/Resume

**Subagent checkpoint requirements:**
```markdown
Before signaling STOP_WORK, create checkpoint:
1. Document completed work (files, reports, findings)
2. Save intermediate results to files
3. Record current working directory and context
4. List next steps in resume_requirements
```

**Parent resume requirements:**
```markdown
When resuming subagent:
1. Acknowledge what was completed
2. Confirm blocker resolution
3. Restore state context (files, directories, progress)
4. Provide explicit next step
```

---

## Protocol 4: Delegation Protocol

### Purpose
Enable subagents to request new background agents for additional work.

### Signal Format

```yaml
[DELEGATE_WORK]
agent_id: <requesting-agent-id>
timestamp: <ISO-8601-timestamp>
delegation_reason: <why delegate vs continue>
new_task_description: <what new agent should do>
independence: <can_proceed_parallel|blocks_current_work|optional>
priority: <P0|P1|P2>
context_required: <what context new agent needs>
coordination: <how agents should coordinate>
estimated_duration: <time estimate for new task>
[/DELEGATE_WORK]
```

### Example Signal: Parallel Work

```yaml
[DELEGATE_WORK]
agent_id: bg-task-jkl012
timestamp: 2026-01-11T10:00:00-05:00
delegation_reason: Discovered large-scale refactoring opportunity that should run in parallel
new_task_description: |
  Refactor authentication module to use unified AuthProvider pattern
  - Consolidate OAuth2 and JWT implementations
  - Create single AuthProvider interface
  - Update all auth consumers to use new interface
independence: can_proceed_parallel
priority: P1
context_required: |
  - Current authentication analysis (see AUTH_ANALYSIS.md)
  - Unified pattern specification (see UNIFIED_AUTH_PATTERN.md)
  - Do NOT modify files in src/auth/ (this agent will continue analyzing)
coordination: |
  This agent: Continue security analysis of remaining modules
  New agent: Refactor auth module (separate work, no conflicts)
  Sync point: Both complete before final security report
estimated_duration: 2-3 hours
[/DELEGATE_WORK]
```

### Example Signal: Blocking Work

```yaml
[DELEGATE_WORK]
agent_id: bg-task-mno345
timestamp: 2026-01-11T10:30:00-05:00
delegation_reason: Discovered missing test infrastructure needed before security testing
new_task_description: |
  Set up security testing infrastructure:
  - Install and configure OWASP ZAP
  - Create test fixtures for auth scenarios
  - Set up mock API server
  - Document testing workflow
independence: blocks_current_work
priority: P0
context_required: |
  - Security analysis plan (see SECURITY_PLAN.md)
  - Required test scenarios (see TEST_SCENARIOS.md)
  - Target system architecture (see ARCHITECTURE.md)
coordination: |
  This agent: Will STOP_WORK until infrastructure ready
  New agent: Set up testing infrastructure
  Resume: When new agent completes, resume with "Infrastructure ready, continue testing"
estimated_duration: 1-2 hours
[/DELEGATE_WORK]
```

### Parent Response Workflow

**1. Detect Delegation Signal:**
```bash
output=$(TaskOutput agent-id=bg-task-jkl012 block=false)
if echo "$output" | grep -q "\[DELEGATE_WORK\]"; then
  # Extract delegation details
  # Decide: approve, deny, or modify
fi
```

**2. Evaluate Delegation Request:**
```markdown
Parent agent evaluation checklist:
- [ ] Is delegation justified? (vs original agent continuing)
- [ ] Is new task well-defined? (context, goals, metrics)
- [ ] Are independence claims accurate? (parallel vs blocking)
- [ ] Is priority appropriate? (P0/P1/P2)
- [ ] Are resources available? (time, compute, parallel slots)
```

**3A. Approve and Launch:**
```bash
# Extract new task description and context
new_task="[extracted from DELEGATE_WORK signal]"

# Launch new background agent
/bg "$new_task"

# Acknowledge to original agent
Task resume="bg-task-jkl012" prompt="
## DELEGATION APPROVED

New agent launched: bg-task-xyz789
Task: Refactor authentication module
Coordination: Proceed with your security analysis, new agent handling refactor

## RESUME INSTRUCTIONS
Continue security analysis of remaining modules
Avoid src/auth/ (being refactored by bg-task-xyz789)
"
```

**3B. Deny and Redirect:**
```bash
Task resume="bg-task-jkl012" prompt="
## DELEGATION DENIED

Reason: Refactoring should be deferred until security analysis complete
Alternative: Document refactoring recommendations in final report

## RESUME INSTRUCTIONS
Continue security analysis without refactoring
Add refactoring recommendations to SECURITY_RECOMMENDATIONS.md
"
```

### Subagent Coordination Patterns

**Pattern 1: Parallel Independence**
```yaml
Agent A: Analyzes frontend security
Agent B: Analyzes backend security
Coordination: No overlap, merge reports at end
```

**Pattern 2: Sequential Dependency**
```yaml
Agent A: Sets up test infrastructure → STOP_WORK
Agent B: Runs security tests (waits for A completion)
Coordination: Agent B launches after Agent A completes
```

**Pattern 3: Shared Resource**
```yaml
Agent A: Analyzes modules 1-50
Agent B: Analyzes modules 51-100
Coordination: Both write to shared SECURITY_FINDINGS.jsonl (append-only)
```

---

## Protocol 5: Completion Report

### Purpose
Signal successful completion with deliverables and summary.

### Signal Format

```yaml
[COMPLETION_REPORT]
agent_id: <agent-id>
timestamp: <ISO-8601-timestamp>
status: <success|partial_success|failed>
deliverables: <list of files/outputs created>
summary: <what was accomplished>
metrics_achieved: <how success criteria were met>
issues_encountered: <problems solved during execution>
recommendations: <follow-up actions>
total_duration: <execution time>
[/COMPLETION_REPORT]
```

### Example Signal: Success

```yaml
[COMPLETION_REPORT]
agent_id: bg-task-pqr678
timestamp: 2026-01-11T12:00:00-05:00
status: success
deliverables: |
  Generated files:
  - SECURITY_ANALYSIS.md (comprehensive report)
  - SECURITY_FINDINGS.jsonl (structured findings, 47 entries)
  - REMEDIATION_PLAN.md (prioritized action items)
  - AUTH_REFACTOR_PROPOSAL.md (architecture recommendations)
summary: |
  Completed comprehensive security analysis of agentic-primer codebase:
  ✓ Scanned 120 files across 8 modules
  ✓ Found 0 critical vulnerabilities
  ✓ Identified 3 medium-priority issues (documented)
  ✓ Validated authentication patterns (OAuth2 + JWT)
  ✓ Analyzed dependency security (0 known CVEs)
metrics_achieved: |
  Success Criteria: [from original prompt]
  ✓ Scan for hardcoded secrets → 0 found
  ✓ Check SQL injection risks → 0 found (parameterized queries used)
  ✓ Review authentication → OAuth2 and JWT validated, recommendations provided
  ✓ Generate structured report → SECURITY_ANALYSIS.md created
issues_encountered: |
  1. Missing node_modules → STOPPED, resolved by parent, resumed
  2. Ambiguous auth strategy → CLARIFIED with user (analyze both)
  3. Large file count → Optimized with batched analysis
recommendations: |
  Follow-up actions:
  1. [P1] Implement auth refactoring proposal (see AUTH_REFACTOR_PROPOSAL.md)
  2. [P2] Add automated security scanning to CI/CD
  3. [P2] Document security review process for future audits
total_duration: 3h 45m (including 1 stop/resume cycle)
[/COMPLETION_REPORT]
```

---

## Parent Monitoring Implementation

### Polling Strategy

```bash
#!/bin/bash
# poll-subagent.sh - Monitor background agent for signals

agent_id="$1"
poll_interval=10  # seconds

while true; do
  # Get current output
  output=$(TaskOutput agent-id="$agent_id" block=false 2>&1)

  # Check for signals
  if echo "$output" | grep -q "\[CLARIFICATION_NEEDED\]"; then
    handle_clarification "$agent_id" "$output"
    break
  elif echo "$output" | grep -q "\[STOP_WORK\]"; then
    handle_stop "$agent_id" "$output"
    break
  elif echo "$output" | grep -q "\[DELEGATE_WORK\]"; then
    handle_delegation "$agent_id" "$output"
    # Don't break - agent may continue
  elif echo "$output" | grep -q "\[COMPLETION_REPORT\]"; then
    handle_completion "$agent_id" "$output"
    break
  fi

  sleep "$poll_interval"
done
```

### Signal Extraction

```bash
extract_signal() {
  local output="$1"
  local signal_type="$2"

  # Extract signal block
  echo "$output" | sed -n "/\[$signal_type\]/,/\[\/$signal_type\]/p"
}

parse_yaml_field() {
  local signal="$1"
  local field="$2"

  # Parse YAML field from signal
  echo "$signal" | grep "^$field:" | sed "s/^$field: *//"
}
```

### Integration with Parent Agent Workflow

```markdown
## Parent Agent Workflow

1. **Launch Background Agent:**
   - Validate task prompt (context, goals, metrics)
   - Add EXECUTION CONTEXT section
   - Launch with Task tool (run_in_background=true)
   - Record agent_id

2. **Monitor Agent Progress:**
   - Poll TaskOutput every 10-30 seconds
   - Parse output for protocol signals
   - React to signals immediately

3. **Respond to Signals:**
   - CLARIFICATION_NEEDED → Ask user, resume with answers
   - STOP_WORK → Resolve blocker, resume with resolution
   - DELEGATE_WORK → Evaluate, approve/deny, launch if approved
   - COMPLETION_REPORT → Validate deliverables, notify user

4. **State Management:**
   - Track active agent_ids
   - Maintain signal history
   - Preserve context for resume operations
   - Coordinate multiple parallel agents
```

---

## Error Handling and Edge Cases

### Subagent Errors

**Signal format for errors:**
```yaml
[STOP_WORK]
agent_id: bg-task-stu901
stop_reason: error
blocker_type: error
details: |
  Unexpected error during analysis:
  TypeError: Cannot read property 'length' of undefined
  at analyzeFile (/tmp/analysis.js:142)
completed_work: [what was finished before error]
state_snapshot: [checkpoint before error]
error_recovery: |
  Potential fixes:
  1. Skip problematic file and continue
  2. Fix error in analysis script
  3. Restart analysis from checkpoint
[/STOP_WORK]
```

### Parent Response to Errors

```bash
# Option 1: Resume with error skip
Task resume="bg-task-stu901" prompt="
## ERROR RECOVERY

Skip the problematic file (analysis.js:142) and continue with remaining files.
Log the skipped file to SKIPPED_FILES.md for manual review.
"

# Option 2: Fix and restart
# [Fix error in code]
Task resume="bg-task-stu901" prompt="
## ERROR FIXED

Applied fix to analysis script (undefined check added)
Resume from checkpoint: [state_snapshot]
Retry the file that caused error
"
```

### Multiple Parallel Agents

**Coordination file pattern:**
```bash
# agents write to append-only coordination files
bg-task-abc123 → SECURITY_FINDINGS.jsonl (append)
bg-task-def456 → SECURITY_FINDINGS.jsonl (append)
bg-task-ghi789 → SECURITY_FINDINGS.jsonl (append)

# parent merges at end
cat SECURITY_FINDINGS.jsonl | jq -s '.' > MERGED_FINDINGS.json
```

### Agent Timeout

```yaml
[STOP_WORK]
agent_id: bg-task-timeout
stop_reason: blocker
blocker_type: resource_limit
details: Task exceeding expected duration (4h vs 1h estimate)
completed_work: [progress so far]
state_snapshot: [checkpoint]
resume_requirements: |
  Option 1: Continue (accept longer runtime)
  Option 2: Reduce scope (specify what to skip)
  Option 3: Delegate remaining work to new agent
[/STOP_WORK]
```

---

## Testing and Validation

### Protocol Testing Checklist

**Test 1: Clarification Protocol**
```bash
# Launch agent with intentionally ambiguous prompt
/bg "Analyze the codebase"

# Expected: CLARIFICATION_NEEDED signal
# Verify: Questions are clear and actionable
# Verify: Resume with answers succeeds
```

**Test 2: Stop/Resume Protocol**
```bash
# Launch agent with known blocker (missing dependencies)
/bg "Run npm audit on codebase"

# Expected: STOP_WORK signal (external_dependency)
# Verify: Blocker details are accurate
# Verify: Resume after npm install succeeds
```

**Test 3: Delegation Protocol**
```bash
# Launch agent with task that should delegate
/bg "Comprehensive security audit and refactoring"

# Expected: DELEGATE_WORK signal (refactoring as separate task)
# Verify: Independence claim is accurate
# Verify: Launching delegated agent succeeds
```

**Test 4: Completion Report**
```bash
# Launch agent with well-defined task
/bg "Scan codebase for TODO comments, generate TODO_LIST.md"

# Expected: COMPLETION_REPORT signal
# Verify: Deliverables exist
# Verify: Metrics align with success criteria
```

### Validation Criteria

**Signal Format Validation:**
- [ ] All required fields present
- [ ] YAML syntax is valid
- [ ] Timestamps are ISO-8601
- [ ] Delimiters are properly matched

**Parent Detection Validation:**
- [ ] Signals detected in TaskOutput
- [ ] Signal extraction works correctly
- [ ] Field parsing is accurate
- [ ] Parent response is appropriate

**State Preservation Validation:**
- [ ] Checkpoints capture full state
- [ ] Resume operations restore state
- [ ] No work is lost during stop/resume
- [ ] Coordination between agents is maintained

---

## Future Enhancements

### Potential Protocol Extensions

1. **Progress Reporting Protocol**
   - Periodic progress updates (% complete)
   - ETA estimates
   - Resource usage monitoring

2. **Interactive Approval Protocol**
   - Request approval before destructive operations
   - Preview changes before applying
   - User veto power

3. **Agent-to-Agent Communication**
   - Direct messaging between parallel agents
   - Shared state synchronization
   - Conflict resolution

4. **Enhanced Delegation**
   - Agent pools and task queues
   - Dynamic resource allocation
   - Automatic load balancing

---

## Appendix: Protocol Grammar

### ABNF Specification

```abnf
clarification-signal = "[CLARIFICATION_NEEDED]" CRLF
                       signal-fields CRLF
                       "[/CLARIFICATION_NEEDED]"

stop-signal = "[STOP_WORK]" CRLF
              signal-fields CRLF
              "[/STOP_WORK]"

delegate-signal = "[DELEGATE_WORK]" CRLF
                  signal-fields CRLF
                  "[/DELEGATE_WORK]"

completion-signal = "[COMPLETION_REPORT]" CRLF
                    signal-fields CRLF
                    "[/COMPLETION_REPORT]"

signal-fields = yaml-document  ; Valid YAML with required fields
```

### JSON Schema (Alternative Format)

For structured logging and programmatic parsing:

```json
{
  "signal_type": "CLARIFICATION_NEEDED",
  "agent_id": "bg-task-abc123",
  "timestamp": "2026-01-11T08:45:00-05:00",
  "payload": {
    "blocked_at": "...",
    "reason": "...",
    "questions": [...],
    "can_resume_with": "..."
  }
}
```

---

**End of Protocol Specification**
