# Subagent Communication Protocols - Implementation Summary

**Date:** 2026-01-11
**Status:** ✅ Complete and ready for testing

---

## What Was Delivered

### 1. Enhanced /bg Command (`~/.claude/commands/bg.md`)

**Added:**
- **PHASE 4:** Complete subagent monitoring and protocol handling workflow
- **PHASE 5:** Protocol testing and validation procedures
- **Subagent identity statement** in EXECUTION CONTEXT section
- **Four communication protocols** with full signal format templates
- **Protocol response workflows** for parent agents

**Before:** Basic clarification workflow (AskUserQuestion only)
**After:** Comprehensive structured protocol system with 4 signal types

### 2. Complete Protocol Specification (`SUBAGENT_PROTOCOLS.md`)

**Contents:**
- Protocol design principles
- Detailed signal formats (YAML specifications)
- Parent monitoring strategies
- State management patterns
- Coordination strategies (parallel and sequential)
- Error handling and edge cases
- Testing and validation criteria
- Future enhancement roadmap
- ABNF grammar and JSON schema alternatives

**Size:** ~15,000 words, comprehensive reference

### 3. Implementation Guide (`SUBAGENT_IMPLEMENTATION_GUIDE.md`)

**Contents:**
- Quick start instructions
- 4 detailed protocol examples with full workflows
- Common patterns and anti-patterns
- Troubleshooting guide
- Advanced usage (multi-level delegation, conditional delegation)
- Signal parsing code (Bash and Python)

**Size:** ~8,000 words, practical examples

### 4. Quick Reference (`SUBAGENT_PROTOCOLS_QUICK_REF.md`)

**Contents:**
- One-page protocol overview
- Signal format templates
- Monitoring workflow
- Response templates for each protocol
- Common scenarios
- Troubleshooting quick fixes
- Do/Don't table

**Size:** ~1,500 words, fast lookup

---

## The Four Communication Protocols

### Protocol 1: CLARIFICATION_NEEDED

**Purpose:** Subagent requests missing information

**Signal Format:**
```yaml
[CLARIFICATION_NEEDED]
agent_id: <id>
timestamp: <ISO-8601>
blocked_at: <where stopped>
reason: <why>
questions:
  - question_id: Q1
    text: <question>
    context: <why this matters>
can_resume_with: <what info needed>
current_state: <what completed>
[/CLARIFICATION_NEEDED]
```

**Parent Response:** Ask user questions, resume with answers

**Use Case:** Authentication analysis encounters multiple strategies (OAuth2, JWT) - which to analyze?

---

### Protocol 2: STOP_WORK

**Purpose:** Subagent cannot proceed due to blocker

**Signal Format:**
```yaml
[STOP_WORK]
agent_id: <id>
stop_reason: blocker|error|completion
blocker_type: missing_info|external_dependency|error|resource_limit
details: <description>
completed_work: <what finished>
state_snapshot: <checkpoint>
resume_requirements: <what needed>
[/STOP_WORK]
```

**Parent Response:** Resolve blocker (install deps, fix error, reduce scope), resume with resolution

**Use Case:** Security audit needs npm audit but node_modules missing - install and resume

---

### Protocol 3: DELEGATE_WORK

**Purpose:** Subagent requests new background agent for parallel/specialized work

**Signal Format:**
```yaml
[DELEGATE_WORK]
agent_id: <id>
delegation_reason: <why delegate>
new_task_description: <what new agent should do>
independence: can_proceed_parallel|blocks_current_work|optional
priority: P0|P1|P2
context_required: <what new agent needs>
coordination: <how agents coordinate>
estimated_duration: <time>
[/DELEGATE_WORK]
```

**Parent Response:** Evaluate justification, approve/deny, launch if approved

**Use Case:** Modernization project identifies independent work streams (code upgrade vs documentation) - parallelize

---

### Protocol 4: COMPLETION_REPORT

**Purpose:** Subagent signals successful completion

**Signal Format:**
```yaml
[COMPLETION_REPORT]
agent_id: <id>
status: success|partial_success|failed
deliverables: <files created>
summary: <what accomplished>
metrics_achieved: <vs success criteria>
issues_encountered: <problems solved>
recommendations: <follow-ups>
total_duration: <time>
[/COMPLETION_REPORT]
```

**Parent Response:** Validate deliverables, notify user, no resume needed

**Use Case:** TODO cataloging completes successfully with 4 deliverable files and recommendations

---

## Before/After Comparison

### Before: Basic Clarification

**Subagent capabilities:**
- Could use AskUserQuestion (not ideal for background)
- No structured communication
- No state management
- No delegation support
- No completion reporting

**Parent capabilities:**
- Launch and wait
- No monitoring workflow
- No signal detection
- No resume protocol

**Problems:**
- Subagents guessed when information missing
- No way to signal blockers
- Lost progress on errors
- No parallel work coordination
- No completion notifications

### After: Comprehensive Protocol System

**Subagent capabilities:**
- **4 structured signal types** for all communication needs
- **State checkpointing** before blocking
- **Delegation requests** for parallel work
- **Completion reports** with metrics validation
- **Clear identity awareness** (knows it's a subagent)

**Parent capabilities:**
- **Active monitoring** (polling workflow)
- **Signal detection** (parse 4 signal types)
- **Protocol responses** (templates for each signal)
- **State preservation** (resume with checkpoints)
- **Multi-agent coordination** (parallel and sequential)

**Benefits:**
- **No guessing** - subagents stop and ask when blocked
- **Preserved progress** - checkpoints prevent lost work
- **Parallel execution** - delegation protocol enables speedup
- **Quality assurance** - completion reports validate metrics
- **Clear communication** - structured signals, not ad-hoc messages

---

## Integration with /bg Command

### Phase Flow

**PHASE 1: Pre-launch Validation**
- Context, goals, metrics validation (unchanged)
- Clarification workflow if validation fails (unchanged)

**PHASE 2: Prepare Launch**
- Extract task prompt (unchanged)
- Generate short description (unchanged)
- **NEW:** Add EXECUTION CONTEXT with protocols
- **NEW:** Include 4 signal format templates
- Launch Task tool (unchanged)

**PHASE 3: Post-launch Communication**
- Return agent ID (unchanged)
- **NEW:** Explain monitoring expectations

**PHASE 4: Subagent Monitoring (NEW)**
- Periodic polling strategy
- Signal detection workflow
- 4 protocol response procedures
- State management
- Multi-agent coordination

**PHASE 5: Protocol Testing (NEW)**
- Test procedures for each protocol
- Validation checklists
- Success criteria

### Backward Compatibility

**Existing /bg usage:**
- All existing functionality preserved
- Pre-launch validation unchanged
- Task tool parameters unchanged
- User-facing workflow unchanged

**Enhanced /bg usage:**
- Protocols automatically added to subagent instructions
- Parent agents get monitoring workflow
- Subagents can use protocols (but not required)
- Graceful degradation if protocols not used

---

## Testing Strategy

### Test 1: Clarification Protocol
```bash
/bg "Analyze the authentication system"
# Expected: CLARIFICATION_NEEDED (which auth strategies?)
# Test: Resume with answer, verify continuation
```

### Test 2: Stop/Resume Protocol
```bash
/bg "Run npm audit and security report"
# Expected: STOP_WORK (node_modules missing)
# Test: Install deps, resume, verify completion
```

### Test 3: Delegation Protocol
```bash
/bg "Comprehensive modernization and documentation"
# Expected: DELEGATE_WORK (separate doc updates)
# Test: Approve, launch delegated agent, verify both complete
```

### Test 4: Completion Report
```bash
/bg "Catalog all TODO comments, generate TODO_LIST.md"
# Expected: COMPLETION_REPORT with deliverables
# Test: Verify files exist, metrics match criteria
```

### Validation Criteria

**For each test:**
- [ ] Signal format matches specification
- [ ] All required fields present
- [ ] YAML syntax is valid
- [ ] Parent detects signal correctly
- [ ] Parent response is appropriate
- [ ] State is preserved across stop/resume
- [ ] Agent resumes successfully
- [ ] Work completes as expected

---

## Example Workflow: End-to-End

### Scenario: Security Audit with Parallel Work

**User Request:**
```
/bg Comprehensive security audit of codebase - scan for secrets,
vulnerabilities, and authentication issues
```

**Execution Flow:**

**1. Launch (PHASE 1-3):**
```
Parent: Validates prompt (context ✓, goals ✓, metrics ✓)
Parent: Adds EXECUTION CONTEXT with protocols
Parent: Launches bg-task-abc123
Parent: Returns agent ID to user
```

**2. Initial Execution (Subagent):**
```
Agent: Scans for secrets (0 found)
Agent: Analyzes SQL queries (no injection risks)
Agent: Starts authentication analysis
Agent: Discovers OAuth2 and JWT implementations
Agent: STOPS - which to analyze?
```

**3. Clarification (Protocol 1):**
```
Agent: Signals CLARIFICATION_NEEDED
  Q1: Analyze OAuth2, JWT, or both?
  Q2: What depth (surface vs deep dive)?

Parent: Detects signal (PHASE 4)
Parent: Asks user (per CLAUDE.md)
User: "Both, deep dive including dependencies"
Parent: Resumes with answers
```

**4. Continued Execution (Subagent):**
```
Agent: Acknowledges clarification
Agent: Deep dive analysis of OAuth2 (2 hours)
Agent: Deep dive analysis of JWT (1.5 hours)
Agent: Identifies refactoring opportunity
Agent: STOPS - should delegate refactoring?
```

**5. Delegation (Protocol 3):**
```
Agent: Signals DELEGATE_WORK
  Task: Refactor auth to unified pattern
  Independence: can_proceed_parallel

Parent: Evaluates (justified ✓, independent ✓, resources ✓)
Parent: Launches bg-task-def456 (refactoring)
Parent: Resumes bg-task-abc123 (continue audit)
```

**6. Parallel Execution:**
```
Agent abc123: Finishes security audit
Agent def456: Implements auth refactoring
Both: Running in parallel
```

**7. Completion (Protocol 4):**
```
Agent abc123: Signals COMPLETION_REPORT
  Deliverables: SECURITY_ANALYSIS.md, FINDINGS.jsonl
  Metrics: All criteria achieved
  Duration: 4.5 hours

Agent def456: Signals COMPLETION_REPORT
  Deliverables: Refactored src/auth/, AUTH_REFACTOR_REPORT.md
  Metrics: All auth unified under AuthProvider
  Duration: 3 hours

Parent: Validates both deliverables
Parent: Notifies user of completion
```

**Total Time:** 4.5 hours (vs 7.5 hours sequential)

---

## Key Design Decisions

### 1. YAML Signal Format

**Why YAML:**
- Human-readable in TaskOutput logs
- Machine-parsable for automation
- Supports nested structures (questions array)
- Standard format with good tool support

**Alternative considered:** JSON
- Rejected: Less readable in raw output
- Kept as alternative in appendix

### 2. Delimiter Markers

**Why `[SIGNAL_TYPE]...[/SIGNAL_TYPE]`:**
- Easy to detect with grep/regex
- Clear visual boundaries
- Works in unstructured text output
- Resistant to false positives

**Alternative considered:** Special characters (###, ---, etc.)
- Rejected: Too common, false positives

### 3. Polling vs Event-Driven

**Why polling:**
- Simple to implement (no infrastructure)
- Works with current Task tool API
- Parent controls check frequency
- Stateless (no connection management)

**Alternative considered:** Event-driven callbacks
- Rejected: Requires Task tool changes
- Future enhancement candidate

### 4. Four Protocols (not more, not fewer)

**Why 4:**
- CLARIFICATION_NEEDED: Questions (pull info)
- STOP_WORK: Blockers (external help)
- DELEGATE_WORK: Parallelization (resource request)
- COMPLETION_REPORT: Success (notification)

**Covers all communication needs without protocol explosion**

---

## Usage Recommendations

### When to Use Protocols

**Always use:**
- /bg command automatically includes protocols
- Subagents should signal when blocked
- Parents should monitor actively

**Don't use:**
- For trivial single-step tasks
- When subagent can proceed autonomously
- For tasks < 5 minutes (overhead not worth it)

### Best Practices

**For Subagents:**
1. Signal early (don't wait until critical)
2. Create checkpoints before signaling
3. Include full context in signals
4. Use DELEGATE_WORK for truly independent work

**For Parents:**
1. Poll regularly (10-30 seconds)
2. Respond to signals within 1-2 cycles
3. Validate delegation requests
4. Preserve state when resuming
5. Track multiple agents separately

**For Users:**
1. Provide clear context upfront (reduces CLARIFICATION_NEEDED)
2. Expect parent to monitor actively
3. Answer clarifications promptly
4. Review completion reports for quality

---

## Files Modified/Created

### Modified
- **~/.claude/commands/bg.md** (enhanced with PHASE 4 & 5)
  - Added 200+ lines of protocol instructions
  - Added monitoring workflow
  - Added response templates
  - Added testing procedures

### Created
- **SUBAGENT_PROTOCOLS.md** (15,000 words)
  - Complete protocol specification
  - Design principles
  - Signal formats
  - Error handling
  - Future enhancements

- **SUBAGENT_IMPLEMENTATION_GUIDE.md** (8,000 words)
  - Practical examples
  - Common patterns
  - Troubleshooting
  - Code samples

- **SUBAGENT_PROTOCOLS_QUICK_REF.md** (1,500 words)
  - One-page quick reference
  - Signal templates
  - Response workflows
  - Common scenarios

- **SUBAGENT_PROTOCOLS_SUMMARY.md** (this file)
  - Implementation summary
  - Before/after comparison
  - Testing strategy
  - Design decisions

---

## Next Steps

### Immediate Testing (Recommended)

1. **Test basic clarification:**
   ```bash
   /bg "Analyze the codebase for issues"
   # Verify CLARIFICATION_NEEDED signal
   ```

2. **Test stop/resume:**
   ```bash
   /bg "Run npm audit security report"
   # Verify STOP_WORK on missing deps
   ```

3. **Test delegation:**
   ```bash
   /bg "Modernize codebase and update docs"
   # Verify DELEGATE_WORK for parallel work
   ```

4. **Test completion:**
   ```bash
   /bg "Find TODO comments, generate TODO_LIST.md"
   # Verify COMPLETION_REPORT with deliverables
   ```

### Production Deployment

1. **Validate in current project:**
   - Use /bg for real tasks
   - Monitor for signal usage
   - Verify protocols work as expected

2. **Iterate based on feedback:**
   - Adjust signal formats if needed
   - Refine response templates
   - Add new protocols if gaps found

3. **Document lessons learned:**
   - Which protocols used most?
   - What patterns emerged?
   - What improvements needed?

### Future Enhancements

**Short-term (mentioned in SUBAGENT_PROTOCOLS.md):**
- Progress reporting protocol (% complete, ETA)
- Interactive approval protocol (preview changes)
- Enhanced error recovery patterns

**Long-term:**
- Agent-to-agent direct messaging
- Shared state synchronization
- Task queues and agent pools
- Event-driven architecture (vs polling)

---

## Success Metrics

### For Protocols
- [x] All 4 protocols designed and documented
- [x] Signal formats specified (YAML)
- [x] Parent response workflows defined
- [x] State management documented
- [x] Testing procedures created

### For Documentation
- [x] Complete specification (SUBAGENT_PROTOCOLS.md)
- [x] Implementation guide with examples
- [x] Quick reference for lookup
- [x] /bg command enhanced
- [x] Before/after comparison

### For Usability
- [x] Subagent identity clearly stated
- [x] Protocol usage guidelines provided
- [x] Response templates included
- [x] Troubleshooting guide created
- [x] Code samples (Bash, Python)

### For Testing
- [x] Test procedures for each protocol (4)
- [x] Validation checklists created
- [x] Example scenarios documented
- [ ] **TODO:** Run actual tests (next step)
- [ ] **TODO:** Validate in production (after tests)

---

## Deliverable Summary

**What you asked for:**
1. Subagent awareness - ✅ EXECUTION CONTEXT statement
2. Clarification protocol - ✅ CLARIFICATION_NEEDED signal
3. Stop/resume protocol - ✅ STOP_WORK signal with state preservation
4. Delegation protocol - ✅ DELEGATE_WORK signal with coordination

**What you got:**
1. **4 complete protocols** (plus completion reporting)
2. **Enhanced /bg command** with full monitoring workflow
3. **3 documentation files** (spec, guide, quick ref)
4. **Testing strategy** with validation criteria
5. **Code samples** for signal parsing (Bash, Python)
6. **Examples** for every protocol (12+ scenarios)
7. **Design rationale** and future roadmap

**Ready to use:**
- /bg command enhanced and functional
- Protocols specified and documented
- Testing procedures defined
- Examples provided for all use cases

**Next action:** Test protocols with real background tasks

---

**End of Summary**
