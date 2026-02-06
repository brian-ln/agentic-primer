# Background Agent Clarification Protocol - Summary

**Status:** Protocol documented but not implemented
**Date:** 2026-01-11

---

## TL;DR

The bg.md command documents that agents should "Use AskUserQuestion through your parent agent" when clarification is needed, but **no mechanism exists for routing AskUserQuestion calls through a parent agent**. This is aspirational documentation, not operational protocol.

---

## What Works Today

1. **Pre-launch validation** (bg.md PHASE 1)
   - Checklist prevents launching ambiguous tasks
   - Parent agent asks clarifications BEFORE launch
   - Agent receives complete context upfront

2. **Fail-fast pattern**
   - Agent detects ambiguity
   - Outputs clear error message
   - Exits immediately
   - Parent re-launches with clarification

3. **Assumption documentation**
   - Agent makes reasonable assumptions
   - Documents all assumptions explicitly
   - Flags for human review in output

---

## What Doesn't Work

1. **Mid-execution clarification**
   - No way to route AskUserQuestion through parent
   - No blocking/resume mechanism
   - No answer delivery protocol

2. **Parent-side detection**
   - No standard format for clarification requests
   - TaskOutput is free-form text
   - No structured parsing mechanism

3. **State management**
   - No way for agent to "pause" while blocked
   - No way to resume after receiving answer

---

## Recommended Approach (Current)

### For Parent Agents Launching Tasks:

Use bg.md's pre-launch validation checklist:

```markdown
Before launching, validate:
✓ Clear context provided
✓ Explicit goals defined
✓ Success metrics specified
✓ Task is actionable

If ANY item missing: BLOCK and clarify with AskUserQuestion
```

### For Background Agents That Encounter Ambiguity:

Use the "clarification marker" pattern:

```markdown
🔴 CLARIFICATION REQUIRED 🔴

**Question ID:** clr-001-system-choice
**Question:** Which system should be documented?

**Options:**
A) Event System
B) spec-kit
C) BMAD

**Status:** BLOCKED - Waiting for answer
```

Then either:
- **Option A:** Exit and let parent re-launch
- **Option B:** Use file-based protocol (see test doc)
- **Option C:** Proceed with documented assumption

---

## Proposed Implementation

### File-Based Protocol (Works Now)

**Agent side:**
```python
# Write request
Path(f"/tmp/clarification-{id}.request").write_text(
    json.dumps({"question": q, "options": opts})
)

# Poll for answer
while not Path(f"/tmp/clarification-{id}.answer").exists():
    time.sleep(5)

answer = Path(f"/tmp/clarification-{id}.answer").read_text()
```

**Parent side:**
```python
# Monitor TaskOutput for "🔴 CLARIFICATION REQUIRED 🔴"
output = get_task_output(agent_id)

if "🔴 CLARIFICATION REQUIRED" in output:
    # Parse request from output or /tmp file
    request = parse_clarification(output)

    # Ask user
    answer = ask_user_question(request["question"])

    # Deliver answer
    Path(f"/tmp/clarification-{request['id']}.answer").write_text(answer)
```

---

## Future Platform Feature (Ideal)

```python
# Native clarification routing
Task(
    prompt="...",
    run_in_background=True,
    clarification_mode="route-to-parent",  # NEW
    parent_agent_id="parent-123"           # NEW
)

# TaskOutput shows clarification state
{
  "status": "blocked_on_clarification",
  "clarification": {
    "id": "clr-123",
    "question": "Which system?",
    "options": ["A", "B", "C"]
  }
}

# Answer mechanism
claude task answer clr-123 "Option B"
```

---

## Documentation Updates Needed

Update `/Users/bln/.claude/commands/bg.md` line 147:

**Current:**
```markdown
2. Use AskUserQuestion through your parent agent
```

**Should be:**
```markdown
2. Output a structured clarification request and exit:
   - Use format: "🔴 CLARIFICATION REQUIRED 🔴"
   - Include question ID, options, and blocking status
   - Parent will detect via TaskOutput and re-launch
3. [FUTURE] Use AskUserQuestion through parent (not yet implemented)
```

---

## Files Created

1. **CLARIFICATION_PROTOCOL_TEST.md** (comprehensive analysis)
   - Protocol gap analysis
   - Test execution results
   - Implementation recommendations
   - Code examples (Python)

2. **CLARIFICATION_PROTOCOL_EXAMPLES.md** (practical reference)
   - 6 concrete examples
   - Decision framework
   - Best practices
   - Copy-paste templates

3. **CLARIFICATION_PROTOCOL_SUMMARY.md** (this file)
   - Executive summary
   - Quick reference
   - Current best practices

---

## Key Takeaways

1. **Pre-launch validation is critical** - Use bg.md checklist religiously
2. **"Through parent agent" is not implemented** - Use fail-fast pattern instead
3. **File-based protocol works today** - Implement if mid-execution clarification is needed
4. **Platform feature would be ideal** - Native support for clarification routing

---

## Questions Answered

✓ **How should agents signal they need clarification?**
→ Output structured marker with "🔴 CLARIFICATION REQUIRED 🔴"

✓ **What format/convention should be used?**
→ Include question ID, category, options, and blocking status

✓ **How does parent agent detect these requests?**
→ Monitor TaskOutput for clarification marker

✓ **What works today?**
→ Pre-launch validation + fail-fast pattern

✓ **What doesn't work?**
→ Mid-execution routing through parent (not implemented)

---

**Status:** Research complete, protocol gaps identified, recommendations provided
