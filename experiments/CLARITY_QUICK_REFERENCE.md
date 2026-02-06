# Instruction Clarity - Quick Reference

## TL;DR

**Use this format for best clarity:**

```
✅ ALLOWED: [specific tools]
❌ FORBIDDEN: [specific actions]
```

**Why**: 2x quality, 0 questions, 0.85 confidence

---

## Quick Comparison

```
┌────────────────────┬────────┬────────────┬─────────┬────────────┐
│ Format             │ Search │ Questions  │ Quality │ Confidence │
├────────────────────┼────────┼────────────┼─────────┼────────────┤
│ Baseline           │   Yes  │     2      │  2/5    │    0.55    │
│ Whitelist ✅       │   No   │     0      │  4/5    │    0.85    │
│ Ultra-minimal      │   Yes  │     1      │  3/5    │    0.75    │
└────────────────────┴────────┴────────────┴─────────┴────────────┘
```

---

## Winning Pattern

```markdown
Task: Create hello-world script with documentation

✅ ALLOWED:
   - WebSearch, WebFetch
   - Read, Write, Edit
   - Grep, Glob
   - AskUserQuestion
   - Bash (validation only)

❌ FORBIDDEN:
   - GitHub API calls
   - git push / git commands
   - Credential exposure
   - File deletion
```

**Result**: 4/5 quality, 0 questions, instant execution

---

## Key Metrics

| Aspect | Whitelist | Baseline | Delta |
|--------|-----------|----------|-------|
| Questions asked | 0 | 2 | -100% |
| Output quality | 4/5 | 2/5 | +100% |
| Confidence | 0.85 | 0.55 | +55% |
| Web research | No | Yes | ✓ Reduced |
| Tool activation | 4 calls | 3 calls | +33% |

---

## Apply This Today

Replace this:
```
Use web search and any tools @copilot would use. DO NOT call GitHub APIs.
```

With this:
```
✅ ALLOWED: WebSearch, WebFetch, Read, Write, Edit, Grep, Glob, AskUserQuestion, Bash
❌ FORBIDDEN: GitHub API calls, git push, credentials, file deletion
```

**Expected improvement**: 2x output quality, 0 clarifications needed

---

## When to Use Each Format

| Situation | Format | Why |
|-----------|--------|-----|
| Simulation/testing | Whitelist (Explicit) | Best clarity, fastest |
| Production deployments | Whitelist (Explicit) | Safety + confidence |
| One-off tasks | Whitelist (Explicit) | No questions asked |
| Brainstorming | Maybe Baseline | More freedom |
| Exploration | Ultra-minimal | Suggests research |

**Bottom line**: Default to Whitelist. You'll know if you need different.

---

Test date: 2026-01-06
