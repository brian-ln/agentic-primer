# Manual Evaluation Prompts: Expert Agent Scoring

**Date:** 2026-01-08
**Purpose:** Expert agent prompts for scoring the 3 manual dimensions of the Enhanced Rubric (40 points)
**Context:** These prompts enable human evaluators to systematically score Correctness, Research Quality, and Insight Quality

---

## Expert Agent Evaluation Prompt: Manual Dimensions (40 points)

### Context
You are evaluating agent-generated bootstrap implementations from simulation runs. You have access to:
- The original prompt (P1/P2/P3)
- Success criteria (S1/S2/S3)
- Implementation files (in `P#-S#-model/` directory)
- Agent session logs (compact JSONL format, if available)
- Self-reflection analysis (from Phase 1b)
- Automated scoring results (80 points already scored)

### Your Task
Score the remaining **3 manual dimensions** that require human judgment:

---

## Dimension 5: CORRECTNESS (20 points)

**What you're evaluating:** Would this implementation actually work if deployed to GitHub?

### 5.1 Logical Correctness (15 points)

**Instructions:**
1. **Read all generated files** (YAML workflows, shell scripts, configs)
2. **Trace the execution path** step-by-step:
   - If a user creates an issue, what happens?
   - Does each workflow step have required inputs?
   - Are GitHub API calls syntactically valid?
   - Would CODEOWNERS syntax be parsed correctly?
   - Would issue templates render in GitHub UI?

3. **Check for logic errors:**
   - Missing dependencies between steps
   - Incorrect API endpoints or parameters
   - Invalid YAML syntax or structure
   - Broken cross-references between files

**Scoring:**
| Points | Criteria |
|--------|----------|
| 15 | **ALL FILES ARE CORRECT** - Logic traces successfully, would execute without errors, all API calls valid, no missing dependencies |
| 12 | **MINOR LOGICAL ISSUES** - 1-2 fixable issues: overly broad triggers (e.g., all issue events instead of just "opened"), CODEOWNERS pattern too generic (`*` instead of specific paths), outdated action versions (v2 instead of v4) |
| 9 | **MODERATE ISSUES** - 3-4 issues: workflow missing required steps, knowledge base structure incomplete, missing error handling |
| 6 | **MAJOR ISSUES** - 5+ issues: workflow would fail on execution, CODEOWNERS syntax invalid, missing critical file dependencies |
| 0 | **FUNDAMENTALLY BROKEN** - Unparseable YAML, incorrect GitHub API usage, system would not function at all |

**Example logic trace (for workflows):**
```
Step 1: Issue created → ✅ triggers workflow
Step 2: Check label → ✅ conditional correct
Step 3: Checkout repo → ✅ uses actions/checkout@v4
Step 4: Call GitHub API → ❌ missing required parameter 'assignees'
Score: 12/15 (minor issue)
```

**What to look for:**
- ✅ Actions use current versions (`@v4`, not `@v2`)
- ✅ GitHub API calls match REST API spec
- ✅ YAML syntax: `on:`, `jobs:`, `steps:` structure correct
- ✅ File paths referenced actually exist
- ✅ Variables passed between steps correctly

---

### 5.2 Edge Case Handling (5 points)

**Instructions:**
1. **Look for edge case handling** in workflows, scripts, or docs
2. **Common edge cases:**
   - Issue created without required fields
   - Multiple agents replying simultaneously
   - CODEOWNERS file missing or malformed
   - Knowledge base directories not created yet
   - Workflow failures or timeouts
   - Empty issue body or title
   - Concurrent PR creation

**Scoring:**
| Points | Criteria |
|--------|----------|
| 5 | **EXCELLENT** - Handles 3+ edge cases explicitly: validates inputs before processing, uses concurrency control, creates missing directories, handles errors gracefully |
| 4 | **GOOD** - Handles 2 edge cases or has general error handling patterns |
| 2 | **MINIMAL** - Assumes happy path only, no explicit edge case handling |
| 0 | **NONE** - No error handling, would fail on any edge case |

**Evidence to look for:**
```yaml
# Good: Input validation
if: github.event.issue.body != ''

# Good: Error handling
continue-on-error: true

# Good: Concurrency control
concurrency:
  group: copilot-${{ github.event.issue.number }}
  cancel-in-progress: false

# Good: Create if missing
mkdir -p docs/knowledge/patterns
```

---

## Dimension 6: RESEARCH QUALITY (15 points)

**What you're evaluating:** Did the agent research current best practices, or just guess from training data?

### 6.1 WebSearch Tool Usage (8 points)

**Instructions:**
1. **Check agent session logs** (compact JSONL format) for `WebSearch` or `WebFetch` tool calls
2. **Count distinct research queries**:
   - "GitHub Actions best practices 2026"
   - "CODEOWNERS syntax specification"
   - "GitHub issue template YAML format"
   - "yamllint configuration"

3. **Evaluate search quality:**
   - Relevant to the task? ✅
   - Specific enough? (NOT "what is github")
   - Current year? (2026 or 2025, NOT 2023)

**Scoring:**
| Points | Criteria |
|--------|----------|
| 8 | **EXCELLENT RESEARCH** - Used WebSearch 3+ times for critical components (workflow syntax, CODEOWNERS format, current action versions), searches were specific and relevant |
| 6 | **GOOD RESEARCH** - Used WebSearch 1-2 times for key components, searches were relevant |
| 3 | **NO RESEARCH BUT CURRENT** - No WebSearch usage BUT implementation follows 2026 best practices (lucky guess or good training data) |
| 0 | **NO RESEARCH AND OUTDATED** - No WebSearch AND uses outdated patterns (`actions/checkout@v2`, Node.js 12, old syntax) |

**How to check agent logs:**
```bash
# Extract WebSearch calls from compact agent logs
grep '"tool":"WebSearch"' agent-session.jsonl | jq .
```

**Red flags (score 0):**
- No WebSearch calls AND uses `actions/checkout@v2` (outdated, should be @v4)
- No WebSearch calls AND uses markdown templates (outdated, should be issue forms)
- No research for unfamiliar syntax (CODEOWNERS, issue template YAML)

---

### 6.2 Source Citation & Currency (7 points)

**Instructions:**
1. **Search generated docs** for source citations:
   ```bash
   grep -ri "source:\|per:\|according to:\|https://" docs/ README.md
   ```

2. **Check for:**
   - URLs to official GitHub docs
   - Dates mentioned (2025-2026 = current, 2023 = stale)
   - Attribution (e.g., "per GitHub Actions documentation")
   - References to current tool versions

**Scoring:**
| Points | Criteria |
|--------|----------|
| 7 | **EXCELLENT CITATIONS** - Cites 2+ sources from 2025-2026 with URLs, research clearly informed specific design decisions |
| 5 | **GOOD CITATIONS** - Cites 1-2 sources from 2024-2026, mentions sources but no URLs |
| 3 | **IMPLICIT CURRENCY** - No explicit citations BUT implementation is current (actions@v4, issue forms, 2026 best practices) |
| 0 | **NO CITATIONS AND STALE** - No source attribution AND uses outdated practices |

**Example excellent citation:**
```markdown
## Design Decision: Use GitHub Issue Forms

Based on GitHub's 2025 documentation, we use issue forms (.yml)
instead of markdown templates for better validation and UX.

Source: https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests/syntax-for-issue-forms
Accessed: 2026-01-08
```

**Example poor (score 0):**
```markdown
We use issue forms because they're better.
(No source, no justification, no date)
```

**Current 2026 standards to verify:**
- Actions: `actions/checkout@v4`, `actions/setup-node@v4`
- Node.js: v20 LTS (v18 acceptable, v16 outdated)
- Issue templates: `.yml` issue forms (not `.md` templates)
- CODEOWNERS: Proper glob syntax (`docs/* @team`, not `docs/ @team`)

---

## Dimension 7: INSIGHT QUALITY (5 points)

**What you're evaluating:** Did the agent think deeply about the problem, or just generate boilerplate?

### 7.1 Assumptions Documented (3 points)

**Instructions:**
1. **Search generated docs** for explicit assumptions:
   ```bash
   grep -ri "assumption\|assumes\|constraint\|limitation" docs/ README.md
   ```

2. **Look for design rationale:**
   - "Why we chose X over Y"
   - "Assumes single repository model"
   - "Assumes GitHub.com (not Enterprise)"
   - "Constraint: Workflow timeout set to 5 minutes"

**Scoring:**
| Points | Criteria |
|--------|----------|
| 3 | **EXCELLENT** - Documents 3+ assumptions explicitly (e.g., "Assumes single-owner CODEOWNERS", "GitHub.com not Enterprise", "Agents respond within 5min"), includes rationale for key decisions |
| 2 | **GOOD** - Documents 1-2 assumptions, some design rationale |
| 1 | **IMPLICIT** - Assumptions visible in implementation but not documented |
| 0 | **NONE** - No assumptions documented, no design rationale, risky assumptions made without acknowledgment |

**Example excellent (score 3):**
```markdown
## Assumptions

1. **Single repository model** - CODEOWNERS uses `*` pattern for one primary owner
2. **GitHub.com (not Enterprise)** - Uses public GitHub Actions, not enterprise features
3. **Agent response time** - 5min timeout assumes fast agent responses
4. **English-only** - No i18n support in issue templates
5. **Branch protection off** - Workflow pushes directly without PR status checks

## Design Rationale

We chose GitHub issue forms (.yml) over markdown templates because:
- Better input validation (required fields enforced)
- Structured data (easier to parse in workflows)
- Modern GitHub standard (2021+)
```

---

### 7.2 Edge Cases Identified (2 points)

**Instructions:**
1. **Search docs** for edge case discussion:
   ```bash
   grep -ri "edge case\|failure mode\|error handling\|what if" docs/ README.md
   ```

2. **Look for troubleshooting sections**, error handling docs, or "what could go wrong" analysis

**Scoring:**
| Points | Criteria |
|--------|----------|
| 2 | **EXCELLENT** - Documents 2+ edge cases or failure modes (e.g., "What if issue has no body?", "What if multiple agents reply?", "What if CODEOWNERS deleted?"), includes troubleshooting guidance |
| 1 | **GOOD** - Documents 1 edge case or has basic troubleshooting section |
| 0 | **NONE** - No edge case discussion, no troubleshooting, no "what if" analysis |

**Example excellent (score 2):**
```markdown
## Edge Cases & Troubleshooting

### Empty Issue Body
Workflow validates `github.event.issue.body != ''` before processing.

### Concurrent Agents
Uses GitHub concurrency control to prevent race conditions:
```yaml
concurrency:
  group: copilot-${{ github.event.issue.number }}
```

### Missing Knowledge Base
Script creates `docs/knowledge/` if missing: `mkdir -p docs/knowledge/{patterns,decisions,insights}`
```

---

## Scoring Template

Use this template to record your manual scoring:

```markdown
# Manual Evaluation: [P#-S#-model]

**Evaluator:** [Your name]
**Date:** [Date]
**Time spent:** [Minutes]

---

## 5. CORRECTNESS (20 points)

### 5.1 Logical Correctness (15 points)

**Score:** __/15

**Logic trace:**
- [ ] Workflow triggers correctly
- [ ] All workflow steps have required inputs
- [ ] GitHub API calls are valid
- [ ] CODEOWNERS syntax correct
- [ ] Issue template would render

**Issues found:**
1. [Describe issue 1]
2. [Describe issue 2]

**Justification:**
[Explain why you gave this score]

---

### 5.2 Edge Case Handling (5 points)

**Score:** __/5

**Edge cases handled:**
- [ ] Input validation
- [ ] Error handling (continue-on-error, try/catch)
- [ ] Missing files/directories
- [ ] Concurrency control
- [ ] Other: ___

**Justification:**
[Explain]

---

## 6. RESEARCH QUALITY (15 points)

### 6.1 WebSearch Tool Usage (8 points)

**Score:** __/8

**WebSearch calls found:** [Count]
**Example queries:**
1. [Query 1]
2. [Query 2]

**Implementation currency:**
- [ ] Actions use @v4 (not @v2)
- [ ] Issue forms (.yml, not .md)
- [ ] Current CODEOWNERS syntax
- [ ] Modern best practices

**Justification:**
[Explain]

---

### 6.2 Source Citation & Currency (7 points)

**Score:** __/7

**Sources cited:**
1. [Source 1 with URL]
2. [Source 2 with URL]

**Currency check:**
- Dates mentioned: [List dates]
- Tool versions: [List versions]

**Justification:**
[Explain]

---

## 7. INSIGHT QUALITY (5 points)

### 7.1 Assumptions Documented (3 points)

**Score:** __/3

**Assumptions found:**
1. [Assumption 1]
2. [Assumption 2]
3. [Assumption 3]

**Design rationale found:**
- [ ] Yes, comprehensive
- [ ] Yes, minimal
- [ ] No

**Justification:**
[Explain]

---

### 7.2 Edge Cases Identified (2 points)

**Score:** __/2

**Edge cases documented:**
1. [Edge case 1]
2. [Edge case 2]

**Troubleshooting section:**
- [ ] Yes, comprehensive
- [ ] Yes, basic
- [ ] No

**Justification:**
[Explain]

---

## MANUAL DIMENSIONS TOTAL

| Dimension | Score | Max |
|-----------|-------|-----|
| 5. Correctness | __ | 20 |
| 6. Research Quality | __ | 15 |
| 7. Insight Quality | __ | 5 |
| **MANUAL TOTAL** | **__** | **40** |

---

## OVERALL SCORE

| Category | Score | Max |
|----------|-------|-----|
| Automated (from Phase 1) | __ | 80 |
| Manual (this evaluation) | __ | 40 |
| **TOTAL** | **__** | **120** |

**Grade:**
- 110-120 (92-100%): A (Outstanding)
- 100-109 (83-91%): B (Excellent)
- 80-99 (67-82%): C (Pass)
- <80 (<67%): F (Fail, needs rework)

**Pass/Fail:** [PASS / FAIL]

---

## Summary

**Strengths:**
1. [Strength 1]
2. [Strength 2]

**Weaknesses:**
1. [Weakness 1]
2. [Weakness 2]

**Recommendations:**
1. [Recommendation 1]
2. [Recommendation 2]
```

---

## Key Evaluation Tips

### For Correctness (20 points):
1. **Actually trace the logic** - Don't just skim, follow the execution path step-by-step
2. **Check GitHub API docs** - Verify endpoint names and parameters
3. **Look for action versions** - `@v2` = outdated (deduct points), `@v4` = current (full points)
4. **Test YAML syntax mentally** - Does indentation look right? Are required fields present?

### For Research Quality (15 points):
1. **Check agent logs first** - WebSearch usage is objective (either used it or didn't)
2. **Verify implementation currency** - Even without research, current practices = partial credit
3. **Look for URLs in docs** - Presence of source URLs = strong signal of research
4. **Check dates** - 2025-2026 = current, 2023 = stale

### For Insight Quality (5 points):
1. **Search for keywords** - "assumption", "constraint", "limitation", "edge case", "what if"
2. **Look beyond implementation** - Check design docs, READMEs, architecture decision records
3. **Credit thoughtfulness** - "Why we chose X over Y" shows deeper thinking
4. **Don't expect perfection** - This dimension is only 5 points, not heavily weighted

---

## Calibration Examples

### Example 1: High-scoring implementation (38/40)
- Correctness: 20/20 (perfect logic, handles 3+ edge cases)
- Research: 13/15 (used WebSearch 2x, cites sources, current practices)
- Insight: 5/5 (documents 4 assumptions, identifies 2 edge cases)

### Example 2: Mid-scoring implementation (28/40)
- Correctness: 15/20 (minor issues: outdated action versions, generic CODEOWNERS)
- Research: 8/15 (no WebSearch, but implementation is current)
- Insight: 5/5 (excellent documentation despite no research)

### Example 3: Low-scoring implementation (18/40)
- Correctness: 12/20 (several logical issues, missing error handling)
- Research: 3/15 (no research, uses outdated patterns)
- Insight: 3/5 (documents 2 assumptions, no edge cases)

---

## Usage Notes

**Time Investment:** 15-20 minutes per scenario (vs 2+ hours for full functional testing)

**When to Use:**
- Representative scenarios from each prompt/criteria/model combination
- High-stakes evaluations requiring human judgment
- Research validation or publication
- Comparing multiple simulation runs

**When to Skip:**
- Quick pass/fail checks (use automated scoring only)
- Large batch evaluations (sample instead of evaluating all)
- Low-priority scenarios

**Integration with Enhanced Rubric:**
- Automated dimensions (80 pts) run first using scripts
- Manual dimensions (40 pts) use these prompts
- Combined score out of 120 points

---

**Document Version:** 1.0
**Last Updated:** 2026-01-08
**Status:** Production-ready