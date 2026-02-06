# Instruction Clarity Micro-Experiments

Testing 3 instruction formats for agent clarity. Each experiment simulates: "@copilot creates a hello-world script with documentation."

## Experiment Setup

**Task**: Agent writes a simple hello-world script (10-15 lines) + minimal README (5-10 lines)

**Metrics**:
- WebSearch usage (yes/no)
- Clarifying questions asked (yes/no)
- Output quality (1-5 scale)
- Time taken (seconds)

---

## Experiment 1: Baseline (Permissive)

**Instruction**:
```
Use web search and any tools @copilot would use. DO NOT call GitHub APIs.
```

**Test Case**:
Task: Create `hello.py` with documentation

**Results**:
- WebSearch used: [PENDING]
- Questions asked: [PENDING]
- Quality: [PENDING]
- Time: [PENDING]

---

## Experiment 2: Clear Whitelist (Explicit)

**Instruction**:
```
✅ ALLOWED: WebSearch, WebFetch, Read, Write, Edit, Grep, Glob, AskUserQuestion, Bash (validation)
❌ FORBIDDEN: GitHub API calls, GitHub commands (gh, git push)
```

**Test Case**:
Task: Create `hello.py` with documentation

**Results**:
- WebSearch used: [PENDING]
- Questions asked: [PENDING]
- Quality: [PENDING]
- Time: [PENDING]

---

## Experiment 3: Ultra-Minimal (Simulation)

**Instruction**:
```
Research with WebSearch/WebFetch. Simulate GitHub deployment.
```

**Test Case**:
Task: Create `hello.py` with documentation

**Results**:
- WebSearch used: [PENDING]
- Questions asked: [PENDING]
- Quality: [PENDING]
- Time: [PENDING]

---

## Comparison Table

| Format | WebSearch | Questions | Quality | Time (s) |
|--------|-----------|-----------|---------|----------|
| Baseline (Permissive) | [PENDING] | [PENDING] | [PENDING] | [PENDING] |
| Whitelist (Explicit) | [PENDING] | [PENDING] | [PENDING] | [PENDING] |
| Ultra-minimal (Simulation) | [PENDING] | [PENDING] | [PENDING] | [PENDING] |

---

## Winner: [TBD]

Clarity ranking: [TBD]
