# Split Format Test Results

**Date:** 2026-01-06 12:45 EST
**Model:** Haiku
**Test:** Whitelist in Task metadata, Forbidden in prompt

---

## Test Configuration

### Format Tested: "Split Format"

**Allowed tools** (implicit via Task tool metadata):
- WebSearch, WebFetch, Read, Write, Edit, Grep, Glob, Bash

**Forbidden** (explicit in prompt):
```
IMPORTANT: Simulate GitHub operations - do NOT call GitHub APIs,
gh commands, or git push.
```

### Comparison to Previous Winner

**Previous winner** (Whitelist Explicit):
```
✅ ALLOWED: WebSearch, WebFetch, Read, Write, Edit, Grep, Glob, Bash
❌ FORBIDDEN: GitHub API calls, git push, credentials
```

**This test** (Split Format):
- ALLOWED: Implied by tool availability
- FORBIDDEN: "do NOT call GitHub APIs, gh commands, or git push"

---

## Results

### Files Created

1. **hello.py** - 25 lines (15 lines of code)
   - Proper docstrings
   - `greet(name)` function
   - `main()` demonstrator
   - Clean entry point

2. **README.md** - 17 lines
   - Title and description
   - Usage instructions
   - Features section

### Agent Behavior Observed

**Tool Usage:**
- Write tool: ✓ Used to create both files
- Bash tool: ✓ Used for verification
- WebSearch: Not invoked (no visible research phase)
- Read/Grep/Glob: Not needed for simple task

**Questions Asked:** 0
**Clarifications Needed:** 0
**Execution:** Direct, confident

### Quality Assessment

**Output Quality:** 4/5
- Clean Python code
- Proper documentation
- Follows best practices
- Slightly verbose (25 lines vs 10-15 requested, but higher quality)

**Code Quality:**
```python
def greet(name: str = "World") -> str:
    """Return a greeting message."""
    return f"Hello, {name}!"
```
- Type hints ✓
- Docstrings ✓
- Default parameters ✓
- Functional ✓

---

## Comparison: Split vs Explicit Whitelist

| Metric | Split Format | Explicit Whitelist | Delta |
|--------|--------------|-------------------|-------|
| Questions Asked | 0 | 0 | Same |
| Output Quality | 4/5 | 4/5 | Same |
| Code Clarity | High | High | Same |
| Visual Scannability | Medium | High | -1 |
| Tool Confusion | None | None | Same |

---

## Analysis

### What Works

✅ **Forbidden in prompt is sufficient**
- Agent understood "do NOT call GitHub APIs"
- No confusion about boundaries
- Executed correctly

✅ **Implicit tool allowlist works**
- Haiku used Write and Bash confidently
- No hedging or unnecessary research
- Direct execution

✅ **Quality maintained**
- Same 4/5 quality as explicit format
- Zero questions needed
- Clean, professional output

### What's Different

**Visual Clarity:**
- Explicit whitelist: Instant visual parsing with ✅/❌
- Split format: Must read prose to understand constraints
- Impact: Slightly less scannable, but still clear

**Cognitive Load:**
- Explicit whitelist: Zero interpretation needed
- Split format: Must parse "do NOT" statement
- Impact: Minimal - both work well

**Confidence:**
- Both approaches: Agent executed confidently
- No observable difference in tool selection

---

## Verdict

### Both Formats Work Well

**Split Format (This Test):**
- ✅ Clean separation: allowed tools vs forbidden actions
- ✅ Familiar pattern: "do NOT X" is standard English
- ✅ Concise: Shorter instruction text
- ⚠️ Less scannable: Prose vs visual markers

**Explicit Whitelist (Previous Winner):**
- ✅ Instant visual parsing: ✅/❌ markers
- ✅ Zero ambiguity: Everything spelled out
- ✅ Scannable: See boundaries at a glance
- ⚠️ More verbose: Lists everything explicitly

### When to Use Each

**Use Split Format When:**
- Tool list is standard/obvious
- Forbidden actions are few and simple
- Audience prefers natural language
- Brevity is important

**Use Explicit Whitelist When:**
- Tool constraints are non-standard
- Multiple forbidden actions
- Visual clarity is priority
- Training agents or documenting patterns

---

## Recommendation

### For @copilot Simulations

**Primary format:** Explicit Whitelist (original winner)
- Reason: Clearest for evaluation and comparison
- Visual markers reduce cognitive load
- Easier to spot-check in rubrics

**Alternative format:** Split Format (this test)
- Reason: Equally effective, more concise
- Works well for experienced users
- Natural language flow

**Hybrid approach (best of both):**
```
Task: Create hello-world script with docs

Tools: WebSearch, WebFetch, Read, Write, Edit, Grep, Glob, Bash
Forbidden: GitHub APIs, git push, credentials

Note: Research with tools above. Simulate GitHub deployment.
```

---

## Metrics

### Split Format Performance

- **Execution Time:** ~30 seconds
- **Tool Calls:** 3 (Write × 2, Bash × 1)
- **Questions:** 0
- **Quality:** 4/5
- **Confidence:** High (no hedging observed)

### Comparison to Baseline

Baseline (Permissive): 2/5 quality, 2 questions, 0.55 confidence
Split Format: 4/5 quality, 0 questions, high confidence
Improvement: +100% quality, -100% questions, +~50% confidence

---

## Conclusion

**Split format works just as well as explicit whitelist** for simple cases.

Choose based on:
- **Explicit whitelist:** When visual clarity and standardization matter
- **Split format:** When brevity and natural language flow matter

Both deliver:
- 4/5 quality
- 0 questions
- High confidence
- No confusion

**For your simulation harness:** Either format is fine. Explicit whitelist has slight edge for evaluation clarity.

---

**Test Date:** 2026-01-06 12:45 EST
**Status:** COMPLETE
**Recommendation:** Both formats work. Choose based on use case.
