# Full Automation Roadmap: 120 Points

**Date:** 2026-01-08
**Current:** 65-80 points automated (54-67%)
**Goal:** 120 points automated (100%)
**Gap:** 40-55 points to automate

---

## Executive Summary

To reach full 120-point automation, we need:

**Approach A: Deterministic Scripts Only**
- Complexity: HIGH (requires sophisticated static analysis)
- Reliability: VERY HIGH (deterministic, reproducible)
- Cost: Time investment to build tools
- Timeline: 2-4 weeks development
- Achievable: ~95-105 points (79-88%)

**Approach B: LLM Agent Scoring**
- Complexity: MEDIUM (use existing Claude API)
- Reliability: HIGH (consistent if well-prompted)
- Cost: API calls (~$0.50-2.00 per scenario)
- Timeline: 1-3 days development
- Achievable: 120 points (100%) ✅

**Recommended: Hybrid Approach**
- Use deterministic scripts where possible (speed, cost)
- Use LLM agents for semantic evaluation (quality, correctness)
- Best of both worlds: Fast + Accurate + Cost-effective

---

## Current Automation Status

| Dimension | Points | Automated | Method | Remaining |
|-----------|--------|-----------|--------|-----------|
| 1. Functional Verification | 30 | ✅ 30 | yamllint, shellcheck, grep | 0 |
| 2. Completeness Calibration | 25 | ✅ 25 | File counting, size analysis | 0 |
| 3. Correctness | 20 | ⚠️ 5 | Basic validation | **15** |
| 4. Actionability | 15 | ✅ 10 | Placeholder count | **5** |
| 5. Research Quality | 15 | ✅ 15 | Log + doc analysis | 0 |
| 6. Specificity | 10 | ✅ 7 | Placeholder density | **3** |
| 7. Insight Quality | 5 | ⚠️ 0 | None | **5** |
| **TOTAL** | **120** | **92** | | **28** |

**Note:** Aggressive estimate assumes we can deterministically check basic correctness (5pts)

---

## Dimension-by-Dimension Roadmap

### ✅ Already Automated (92 points)

**No work needed:**
1. Functional Verification: 30 points
2. Completeness Calibration: 25 points
3. Research Quality: 15 points
4. Actionability (placeholder count): 10 points
5. Specificity (placeholder density): 7 points
6. Correctness (basic syntax): 5 points

---

### 🔧 Dimension 3: Correctness (15 points remaining)

**Current:** 5 points (basic validation)
**Target:** 20 points (full correctness)
**Gap:** 15 points

#### Option A: Deterministic Static Analysis

**Tools needed:**
```bash
# 1. Workflow linting (catches ~30% of errors)
actionlint .github/workflows/*.yml  # Validates GitHub Actions syntax
# npm install -g actionlint

# 2. CODEOWNERS validation
codeowners-validator .github/CODEOWNERS  # Checks syntax
# go install github.com/mszostok/codeowners-validator@latest

# 3. Cross-reference checking
./scripts/validate-references.sh  # Check if referenced files exist
# Custom script to parse workflow, check paths

# 4. API endpoint validation
./scripts/check-github-api.sh  # Verify GitHub API calls
# Parse workflows, check against GitHub API spec

# 5. Trigger validation
./scripts/validate-triggers.sh  # Check event triggers
# Verify on.issues.opened, on.pull_request, etc.
```

**Estimated automation:**
- Syntax/structure: 5 points ✅ (already done)
- Static analysis: +5 points (actionlint, validators)
- Cross-references: +3 points (custom scripts)
- API validation: +2 points (GitHub API spec checking)
- **Total: 15 of 20 points (75%)**

**Remaining 5 points require:**
- Semantic logic trace (human or LLM)
- "Does this workflow actually do what it's supposed to?"

**Development effort:** ~3-5 days

#### Option B: LLM Agent Evaluation

**Prompt-based scoring:**
```bash
./scripts/agent-score-correctness.sh <scenario-dir> <agent-id>
# Uses Claude API to read workflows and score correctness
```

**Agent task:**
1. Read all workflow files
2. Trace execution path step-by-step
3. Check for logical errors (missing steps, wrong API calls, etc.)
4. Check for edge case handling
5. Output JSON score with justification

**Estimated automation:**
- Logic trace: 15 points (LLM can trace execution)
- Edge case handling: 5 points (LLM can identify patterns)
- **Total: 20 of 20 points (100%)**

**Development effort:** ~1 day (write prompt + parsing script)

**Cost:** ~$0.15-0.50 per scenario (depends on file size)

**Reliability:** HIGH (if prompt is well-calibrated, scores are consistent)

---

### 🔧 Dimension 4: Actionability (5 points remaining)

**Current:** 10 points (placeholder count)
**Target:** 15 points (full actionability)
**Gap:** 5 points (documentation quality)

#### Option A: Deterministic Heuristics

**Metrics to check:**
```bash
# 1. README presence and size
test -f README.md && wc -l README.md
# Score: >50 lines = 2pts, >100 lines = 3pts, >200 lines = 5pts

# 2. Quickstart section presence
grep -qi "quick.?start\\|getting.?started" README.md
# Score: +2pts if found

# 3. Usage examples
grep -c '```' README.md  # Count code blocks
# Score: 3+ examples = 3pts, 1-2 examples = 1pt

# 4. Step-by-step instructions
grep -c '^[0-9]\.' README.md  # Count numbered lists
# Score: 5+ steps = 2pts, 1-4 steps = 1pt

# 5. Troubleshooting section
grep -qi "troubleshoot\\|faq\\|common.?issues" *.md
# Score: +1pt if found
```

**Estimated automation:**
- README quality: 3 points (heuristics)
- Examples/instructions: 2 points (code block count)
- **Total: 5 of 5 points (100%)**

**Development effort:** ~2 hours

#### Option B: LLM Agent Evaluation

**Agent task:**
1. Read README and documentation
2. Assess clarity and completeness
3. Check for quickstart guide
4. Verify usage examples
5. Score 0-5 points

**Estimated automation:** 5 of 5 points (100%)

**Development effort:** ~2 hours (simpler prompt than Correctness)

**Cost:** ~$0.05-0.10 per scenario

---

### 🔧 Dimension 6: Specificity (3 points remaining)

**Current:** 7 points (placeholder density)
**Target:** 10 points (full specificity)
**Gap:** 3 points (prompt-specific implementation)

#### Option A: Deterministic Keyword Matching

**Check for prompt-specific details:**
```bash
# Extract key terms from prompt
PROMPT_KEYWORDS="copilot issue-driven CODEOWNERS knowledge-base"

# Check if implementation mentions these
for keyword in $PROMPT_KEYWORDS; do
    grep -qi "$keyword" "$SCENARIO_DIR"/**/*.{md,yml}
done

# Score based on matches
# 4/4 keywords = 3pts, 3/4 = 2pts, 2/4 = 1pt, <2 = 0pts
```

**Estimated automation:** 2-3 of 3 points (67-100%)

**Development effort:** ~1 hour

#### Option B: LLM Agent Evaluation

**Agent task:**
1. Read prompt (P1/P2/P3)
2. Read implementation
3. Assess how specifically the implementation addresses prompt details
4. Score 0-3 points

**Estimated automation:** 3 of 3 points (100%)

**Development effort:** ~1 hour

**Cost:** ~$0.05-0.10 per scenario

---

### 🔧 Dimension 7: Insight Quality (5 points remaining)

**Current:** 0 points
**Target:** 5 points
**Gap:** 5 points

#### Option A: Deterministic Keyword Detection

**Check for indicators of thoughtfulness:**
```bash
# 1. Assumptions documented (3 points)
ASSUMPTION_COUNT=$(grep -riE "assumption|assumes|constraint|limitation" docs/ README.md | wc -l)
# Score: 3+ mentions = 3pts, 1-2 = 2pts, 0 = 0pts

# 2. Edge cases identified (2 points)
EDGE_CASE_COUNT=$(grep -riE "edge.?case|failure.?mode|error.?handling|what.?if" docs/ README.md | wc -l)
# Score: 2+ mentions = 2pts, 1 = 1pt, 0 = 0pts

# 3. Design rationale (bonus if present)
grep -qi "design.?decision\\|rationale\\|why.?we.?chose" docs/ README.md
# Score: +1pt if found (already included above)
```

**Estimated automation:** 3-4 of 5 points (60-80%)

**Development effort:** ~2 hours

**Challenge:** Can detect keywords but not assess quality

#### Option B: LLM Agent Evaluation

**Agent task:**
1. Read all documentation
2. Look for explicitly documented assumptions
3. Look for edge case discussion
4. Assess depth of design rationale
5. Score 0-5 points

**Estimated automation:** 5 of 5 points (100%)

**Development effort:** ~2 hours

**Cost:** ~$0.05-0.10 per scenario

---

## Implementation Plans

### Plan A: Pure Deterministic (95-105 points)

**What to build:**
```bash
# 1. Enhanced correctness validator
./scripts/validate-correctness-static.sh
# - actionlint integration
# - CODEOWNERS validator
# - Cross-reference checker
# - GitHub API validator
# Development: 3-5 days

# 2. Documentation quality scorer
./scripts/score-documentation.sh
# - README analysis (length, structure)
# - Example counting
# - Quickstart detection
# Development: 4 hours

# 3. Specificity checker
./scripts/check-specificity.sh
# - Keyword extraction from prompt
# - Implementation matching
# Development: 2 hours

# 4. Insight quality detector
./scripts/detect-insights.sh
# - Assumption keyword counting
# - Edge case keyword counting
# Development: 2 hours
```

**Total development:** 4-6 days

**Pros:**
- ✅ No API costs
- ✅ Fast execution (<1 second per scenario)
- ✅ Deterministic (same input = same output)
- ✅ No rate limits

**Cons:**
- ❌ Only reaches 95-105 points (not full 120)
- ❌ Keyword matching misses semantic quality
- ❌ Can't truly assess "correctness" of logic
- ❌ Time investment to build tools

**Best for:** Cost-sensitive, high-volume evaluation (100s of scenarios)

---

### Plan B: LLM Agent Scoring (120 points) ✅

**What to build:**
```bash
# 1. Agent-based correctness scorer
./scripts/agent-score-correctness.sh <scenario-dir> <agent-id>
# Sends workflows to Claude API for logic trace
# Development: 1 day

# 2. Agent-based documentation scorer
./scripts/agent-score-documentation.sh <scenario-dir>
# Sends docs to Claude API for quality assessment
# Development: 3 hours

# 3. Agent-based specificity scorer
./scripts/agent-score-specificity.sh <scenario-dir> <prompt-id>
# Sends prompt + implementation to Claude API
# Development: 2 hours

# 4. Agent-based insight scorer
./scripts/agent-score-insights.sh <scenario-dir>
# Sends docs to Claude API for thoughtfulness assessment
# Development: 2 hours

# 5. Master scoring script
./scripts/agent-score-all.sh <scenario-dir> <agent-id> <prompt-id>
# Combines all agent scores into final 120-point score
# Development: 2 hours
```

**Total development:** 2-3 days

**Pros:**
- ✅ Full 120-point automation (100%)
- ✅ Semantic understanding (can assess quality)
- ✅ Can trace workflow logic correctly
- ✅ Fast development (just write prompts)
- ✅ Flexible (easy to adjust scoring)

**Cons:**
- ❌ API costs (~$0.30-0.70 per scenario)
- ❌ Slower (~30-60 seconds per scenario vs <1 second)
- ❌ Rate limits (need to batch requests)
- ❌ Non-deterministic (small score variance)

**Best for:** Research, publication-quality evaluation, moderate volume (10-100 scenarios)

---

### Plan C: Hybrid Approach (120 points, optimal) ⭐

**Combine deterministic + LLM agents:**

| Dimension | Method | Cost | Speed | Accuracy |
|-----------|--------|------|-------|----------|
| Functional Verification | Deterministic | $0 | <1s | 100% |
| Completeness | Deterministic | $0 | <1s | 100% |
| Research Quality | Deterministic | $0 | <5s | 95% |
| Actionability (10pts) | Deterministic | $0 | <1s | 100% |
| Specificity (7pts) | Deterministic | $0 | <1s | 100% |
| **Correctness** | **LLM Agent** | **$0.20** | **~15s** | **95%** |
| **Actionability (5pts)** | **LLM Agent** | **$0.05** | **~5s** | **90%** |
| **Specificity (3pts)** | **LLM Agent** | **$0.05** | **~5s** | **95%** |
| **Insight Quality** | **LLM Agent** | **$0.05** | **~5s** | **90%** |

**Total per scenario:**
- Cost: ~$0.35
- Time: ~30 seconds
- Accuracy: ~96%
- Coverage: 120/120 points ✅

**Development:**
- Deterministic parts: Already done ✅
- LLM agent scripts: 2-3 days

**Best for:** Most use cases (balanced cost/speed/accuracy)

---

## Sample Implementation: LLM Agent Correctness Scorer

**File:** `scripts/agent-score-correctness.sh`

```bash
#!/bin/bash
# agent-score-correctness.sh
# Uses Claude API to score Correctness dimension (20 points)

set -e

SCENARIO_DIR="$1"
AGENT_ID="$2"

# Read files
WORKFLOWS=$(find "$SCENARIO_DIR/.github/workflows" -name "*.yml" 2>/dev/null | head -3 | xargs cat)
CODEOWNERS=$(cat "$SCENARIO_DIR/.github/CODEOWNERS" 2>/dev/null || echo "")
ISSUE_TPL=$(find "$SCENARIO_DIR/.github/ISSUE_TEMPLATE" -name "*.yml" 2>/dev/null | head -1 | xargs cat || echo "")

# Create evaluation prompt
PROMPT="You are evaluating GitHub automation for Correctness (20 pts).

## Files:

### Workflow:
\`\`\`yaml
$WORKFLOWS
\`\`\`

### CODEOWNERS:
\`\`\`
$CODEOWNERS
\`\`\`

### Issue Template:
\`\`\`yaml
$ISSUE_TPL
\`\`\`

## Task:

1. **Trace execution** (15 pts): What happens when issue is created?
2. **Check edge cases** (5 pts): Error handling, validation, concurrency?

## Output JSON:

{
  \"logical_correctness\": {
    \"score\": 0-15,
    \"issues\": [\"issue 1\", \"issue 2\"],
    \"trace\": \"Execution description\"
  },
  \"edge_cases\": {
    \"score\": 0-5,
    \"cases_found\": [\"case 1\"]
  },
  \"total\": 0-20
}

Score strictly. Be critical of missing steps, invalid syntax, or logical errors."

# Call Claude API (using claude-code or direct API call)
echo "$PROMPT" | claude-3-5-sonnet --json > "/tmp/correctness-result.json"

# Display result
cat "/tmp/correctness-result.json" | jq '{
  scenario: "'$(basename $SCENARIO_DIR)'",
  correctness: .
}'
```

**Usage:**
```bash
./scripts/agent-score-correctness.sh P1-S1-opus a715b99
```

**Output:**
```json
{
  "scenario": "P1-S1-opus",
  "correctness": {
    "logical_correctness": {
      "score": 13,
      "issues": [
        "Workflow missing error handling for empty issue body",
        "CODEOWNERS uses * pattern (too broad, should be specific paths)"
      ],
      "trace": "Issue created → workflow triggered → checks label → assigns to owner → creates PR. Missing: input validation."
    },
    "edge_cases": {
      "score": 3,
      "cases_found": [
        "Concurrency control present (group: copilot-$issue)",
        "Missing: validation for empty fields",
        "Missing: directory creation for knowledge base"
      ]
    },
    "total": 16
  }
}
```

---

## Cost Analysis

### Scenario 1: Evaluate 27 scenarios (full matrix)

**Plan A: Pure Deterministic**
- Development: 4-6 days (~$4,000-6,000 if hiring)
- Execution: <1 minute total
- Recurring cost: $0
- Points achieved: 95-105/120 (79-88%)

**Plan B: LLM Agents**
- Development: 2-3 days (~$2,000-3,000 if hiring)
- Execution: ~15 minutes total
- Recurring cost: 27 × $0.35 = ~$9.45
- Points achieved: 120/120 (100%)

**Plan C: Hybrid**
- Development: 2-3 days (~$2,000-3,000 if hiring)
- Execution: ~15 minutes total
- Recurring cost: 27 × $0.35 = ~$9.45
- Points achieved: 120/120 (100%)

### Scenario 2: Evaluate 1000 scenarios (large-scale research)

**Plan A: Pure Deterministic**
- Development: $4,000-6,000 (one-time)
- Execution: ~15 minutes
- Recurring cost: $0
- Total: $4,000-6,000

**Plan B: LLM Agents**
- Development: $2,000-3,000 (one-time)
- Execution: ~8 hours
- Recurring cost: 1000 × $0.35 = $350
- Total: $2,350-3,350

**Plan C: Hybrid**
- Development: $2,000-3,000 (one-time)
- Execution: ~8 hours
- Recurring cost: 1000 × $0.35 = $350
- Total: $2,350-3,350

**Breakeven:** Plan A becomes cost-effective at ~12,000+ scenarios

---

## Recommended Next Steps

### For Your Project (27 scenarios)

**Recommended: Plan C (Hybrid)** ⭐

**Immediate next steps:**

1. **Write LLM agent prompts** (Day 1, 4 hours)
   - Correctness evaluation prompt
   - Documentation quality prompt
   - Specificity assessment prompt
   - Insight quality prompt

2. **Create agent scoring scripts** (Day 1-2, 6 hours)
   - `agent-score-correctness.sh`
   - `agent-score-documentation.sh`
   - `agent-score-specificity.sh`
   - `agent-score-insights.sh`
   - `agent-score-all.sh` (master script)

3. **Test and calibrate** (Day 2, 3 hours)
   - Run on 3-5 scenarios
   - Compare LLM scores to manual scores
   - Adjust prompts if needed
   - Verify consistency

4. **Batch process all 27 scenarios** (Day 2, 1 hour)
   ```bash
   ./scripts/batch-agent-score-all.sh
   ```

5. **Generate final report** (Day 2, 1 hour)
   - Combine deterministic + agent scores
   - Create comprehensive 120-point breakdown
   - Generate statistics and insights

**Total time:** 2-3 days
**Total cost:** ~$10 in API calls
**Result:** Full 120/120 point automation ✅

---

## Alternative: Use Existing Agent Infrastructure

**Even faster approach:**

You already have the Task tool with general-purpose agents. You can use this immediately:

```bash
# For each scenario, launch agent with scoring task
claude-code task "Evaluate this scenario for Correctness (20pts):
- Read files in experiments/iteration-2/runs/.../P1-S1-opus/
- Trace workflow logic
- Check edge case handling
- Output JSON score with justification
"
```

**Benefits:**
- ✅ No script development needed
- ✅ Use existing Task infrastructure
- ✅ Can start immediately

**Drawbacks:**
- ❌ More expensive (full agent vs targeted API call)
- ❌ Slower (agent includes overhead)
- ❌ Less structured output

---

## Conclusion

**To reach full 120-point automation:**

**Quickest path:** LLM Agent Scoring (Plan B)
- 2-3 days development
- $0.35 per scenario
- 100% coverage

**Most cost-effective at scale:** Pure Deterministic (Plan A)
- 4-6 days development
- $0 per scenario
- 79-88% coverage
- Breakeven at 12,000+ scenarios

**Best balanced approach:** Hybrid (Plan C) ⭐
- 2-3 days development
- $0.35 per scenario
- 100% coverage
- Fast execution

**For your 27 scenarios:** I recommend Plan C (Hybrid) - you'll get full 120-point automation in 2-3 days for ~$10 total cost.

---

**Ready to implement?** I can help build the LLM agent scoring scripts right now if you want to proceed.
