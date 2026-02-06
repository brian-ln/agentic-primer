# Evaluation Automation Summary

**Date:** 2026-01-08
**Purpose:** Document automated scoring techniques for the Enhanced 120-Point Rubric
**Related:** ENHANCED_RUBRIC.md, RESEARCH_QUALITY_SCORING_AUTOMATION.md, MANUAL_EVALUATION_PROMPTS.md

---

## Overview

This document summarizes the automation capabilities for evaluating agent-generated bootstrap implementations using the Enhanced 120-Point Rubric.

### Automation Coverage

| Dimension | Points | Automation | Time | Method |
|-----------|--------|------------|------|--------|
| 1. Functional Verification | 30 | ✅ Full | ~30s | yamllint, shellcheck, grep |
| 2. Completeness Calibration | 25 | ✅ Full | ~5s | File count, du -sk |
| 3. Correctness | 20 | ⚠️ Partial | ~15min | Human logic trace |
| 4. Actionability | 15 | ✅ Mostly | ~10s | Placeholder count + manual doc review |
| 5. Research Quality | 15 | ✅ Full | ~5s | Log analysis (NEW!) |
| 6. Specificity | 10 | ✅ Mostly | ~5s | Placeholder density |
| 7. Insight Quality | 5 | ⚠️ Minimal | ~5min | Human review of assumptions |
| **TOTAL** | **120** | **~65-80pts** | **~1min** | **Automated** |
| | | **~40pts** | **~15-20min** | **Manual** |

---

## Fully Automated Dimensions

### 1. Functional Verification (30 points)

**Script:** Embedded in Enhanced Rubric automation example
**Method:**
```bash
# Syntax validation (10 points)
yamllint .github/**/*.yml
shellcheck scripts/**/*.sh

# Workflow triggers (10 points)
grep -A 5 "^on:" .github/workflows/*.yml

# Structure correctness (10 points)
test -d .github && test -f .github/CODEOWNERS
```

**Output:** Pass/fail + point allocation
**Time:** ~30 seconds

### 2. Completeness Calibration (25 points)

**Script:** Embedded in Enhanced Rubric automation example
**Method:**
```bash
# Count files (excluding .git)
file_count=$(find . -type f | grep -v "^./.git" | wc -l)

# Compare to prompt-specific expectations
# P1: 4-8 files = 100%
# P2: 6-12 files = 100%
# P3: 8-16 files = 100%
```

**Output:** File count + completeness percentage + score
**Time:** ~5 seconds

### 5. Research Quality (15 points) ⭐ NEW

**Script:** `/Users/bln/play/agentic-primer/scripts/analyze-research-quality.sh`
**Method:**
```bash
# WebSearch usage (8 points)
grep "WebSearch\|WebFetch" <agent-log> | wc -l

# Citation detection (7 points)
grep -rhE "https?://[^\s)]+" docs/ *.md
grep -rhE "202[456]" docs/ *.md

# Implementation currency
grep -E "actions/checkout@v[34]" .github/workflows/*.yml
```

**Output:** Detailed JSON or human-readable report
**Time:** ~5 seconds

**Example Usage:**
```bash
# Single scenario
./scripts/analyze-research-quality.sh P1-S1-opus a715b99

# All 27 scenarios
./scripts/batch-analyze-research-quality.sh

# View summary
cat RESEARCH_QUALITY_SUMMARY.md
```

---

## Partially Automated Dimensions

### 4. Actionability (15 points)

**Automated Part (10 points):**
```bash
# Placeholder count
TODO_COUNT=$(grep -riE "TODO|FIXME|PLACEHOLDER|REPLACE_ME" . --exclude-dir=.git | wc -l)

# Score based on count
# 0-2 TODOs: 10 points
# 3-5 TODOs: 7 points
# 6-10 TODOs: 4 points
# 11+ TODOs: 0 points
```

**Manual Part (5 points):**
- Documentation quality assessment
- Quickstart guide presence
- Usage examples clarity

**Time:** ~10s automated + ~5min manual

### 6. Specificity (10 points)

**Automated Part (7 points):**
```bash
# Same placeholder count as Actionability
# Cross-referenced with implementation specificity heuristics
```

**Manual Part (3 points):**
- Implementation specificity to prompt
- Generic vs customized content
- Prompt detail matching

**Time:** ~5s automated + ~3min manual

---

## Manual-Only Dimensions

### 3. Correctness (20 points)

**Why not automated:**
- Requires semantic understanding of workflow logic
- Must trace execution paths step-by-step
- Needs GitHub API knowledge
- Edge case analysis requires judgment

**Human tasks:**
- Trace workflow execution mentally
- Verify CODEOWNERS syntax
- Check cross-file dependencies
- Assess edge case handling

**Time:** ~15 minutes

### 7. Insight Quality (5 points)

**Why not automated:**
- Requires understanding of assumptions
- Must assess design rationale quality
- Edge case identification needs context
- Thoughtfulness is subjective

**Human tasks:**
- Read design documentation
- Assess assumption documentation
- Evaluate edge case identification
- Judge design rationale depth

**Time:** ~5 minutes

---

## Complete Automation Workflow

### Step 1: Automated Scoring (1 minute)

```bash
# Run all automated checks
SCENARIO_DIR="P1-S1-opus"
AGENT_ID="a715b99"

# 1. Functional verification (30 pts)
yamllint $SCENARIO_DIR/.github/**/*.yml
shellcheck $SCENARIO_DIR/scripts/**/*.sh
# ... (check structure)

# 2. Completeness (25 pts)
find $SCENARIO_DIR -type f | grep -v .git | wc -l

# 3. Research quality (15 pts)
./scripts/analyze-research-quality.sh $SCENARIO_DIR $AGENT_ID

# 4. Actionability (10 pts automated)
grep -riE "TODO|FIXME" $SCENARIO_DIR --exclude-dir=.git | wc -l

# 5. Specificity (7 pts automated)
# (Same as actionability placeholder count)

# TOTAL AUTOMATED: ~65-80 points
```

### Step 2: Manual Review (15-20 minutes)

```bash
# 3. Correctness (20 pts) - Manual
# - Read workflows
# - Trace logic
# - Check CODEOWNERS

# 7. Insight Quality (5 pts) - Manual
# - Read design docs
# - Assess assumptions

# 4. Actionability (5 pts manual) - Documentation quality
# - Review README
# - Check quickstart guide

# TOTAL MANUAL: ~30-40 points
```

### Step 3: Final Scoring

```bash
# Combine scores
AUTOMATED_SCORE=75  # From step 1
MANUAL_SCORE=35     # From step 2
TOTAL=110           # Automated + Manual

# Grade
# 110-120: Outstanding (A)
# 100-109: Excellent (B)
# 80-99: Pass (C)
# <80: Fail (F)
```

---

## Research Quality Automation Details

### Why This Works

Agent a4abbd8 suggested checking agent logs for WebSearch usage when creating manual evaluation prompts:

> **Instructions:**
> 1. **Check agent session logs** (compact JSONL format) for `WebSearch` or `WebFetch` tool calls
> 2. **Count distinct research queries**
> 3. **Evaluate search quality**
>
> **How to check agent logs:**
> ```bash
> grep '"tool":"WebSearch"' agent-session.jsonl | jq .
> ```

We expanded this into a full automation script that:
1. ✅ Counts WebSearch calls
2. ✅ Extracts query strings
3. ✅ Checks implementation currency (actions@v4, .yml forms, etc.)
4. ✅ Scans docs for citations and URLs
5. ✅ Detects year references (2025-2026 = current)
6. ✅ Identifies official documentation URLs

### Results from Full Matrix

**27 scenarios analyzed automatically:**

- **WebSearch usage:** 0 calls across all scenarios (agents relied on training data)
- **Current practices:** 16/27 (59%) used current standards despite no research
- **Citations:** Most scenarios included URLs and 2025-2026 references
- **Average scores:**
  - Opus: 7.1/15 (47%)
  - Sonnet: 7.7/15 (51%) - best at citations
  - Haiku: 5.9/15 (39%) - lowest overall

### Key Insight

Agents didn't use WebSearch but still produced current implementations, suggesting:
- Training data is recent (circa 2025-2026)
- Models "know" current standards without explicit research
- WebSearch would likely improve scores further (6-8 points vs 0-3 points)

---

## Automation Scripts Reference

| Script | Purpose | Input | Output | Time |
|--------|---------|-------|--------|------|
| `analyze-research-quality.sh` | Score single scenario | Scenario dir + agent ID | JSON or human report | ~5s |
| `batch-analyze-research-quality.sh` | Score all 27 scenarios | None (hardcoded paths) | JSONL + MD summary | ~30s |
| `auto-score.sh` (example in rubric) | Score functional + completeness | Scenario dir + P/S labels | Point breakdown | ~1min |

### Installation

All scripts are located in `/Users/bln/play/agentic-primer/scripts/`:

```bash
# Make executable (if not already)
chmod +x scripts/*.sh

# Test research quality automation
./scripts/analyze-research-quality.sh --help

# Run batch analysis
./scripts/batch-analyze-research-quality.sh

# View results
cat experiments/iteration-2/runs/run-20260106-003027-full-matrix/RESEARCH_QUALITY_SUMMARY.md
```

---

## Benefits of Automation

### Speed

- **Manual evaluation:** ~2-3 hours per scenario (functional testing, logic trace, etc.)
- **Automated + manual:** ~20-25 minutes per scenario
- **Speedup:** ~6-8x faster

### Consistency

- **Manual:** Subjective judgments vary between evaluators
- **Automated:** Same inputs always produce same outputs
- **Benefit:** Reproducible scores, easier to compare across scenarios

### Scale

- **Manual:** Evaluating 27 scenarios = 54-81 hours (2-3 days of work)
- **Automated:** Batch script runs in ~30 seconds + ~6-8 hours of manual review
- **Benefit:** Can evaluate full matrix in 1 day instead of 3

### Insight

Automated analysis revealed patterns not visible in manual review:
- No WebSearch usage across all scenarios
- Consistent use of current standards despite no research
- Model-specific patterns (Sonnet best at citations, Opus best at current practices)

---

## Future Automation Opportunities

### Dimension 3: Correctness (20 points)

**Potential automation:**
```bash
# Static workflow validation
actionlint .github/workflows/*.yml  # Lints GitHub Actions workflows

# CODEOWNERS syntax check
codeowners-validator .github/CODEOWNERS

# Cross-reference validation
# Check if workflow references files that exist
grep "uses:.*/" .github/workflows/*.yml | extract file paths | check existence
```

**Estimated automation:** 10-15 of 20 points
**Challenge:** Logic trace still requires human judgment

### Dimension 7: Insight Quality (5 points)

**Potential automation:**
```bash
# Assumption detection (keyword matching)
grep -riE "assumption|assumes|constraint|limitation" docs/ README.md | wc -l

# Edge case detection
grep -riE "edge case|failure mode|error handling|what if" docs/ README.md | wc -l
```

**Estimated automation:** 2-3 of 5 points
**Challenge:** Quality assessment requires human judgment

### Advanced: LLM-Assisted Scoring

```python
# Use LLM to assess correctness
prompt = f"""
Analyze this GitHub Actions workflow for logical correctness:

{workflow_content}

Check for:
1. Missing required steps
2. Invalid API calls
3. Incorrect variable passing
4. Edge case handling

Score: 0-20 points
"""

score = llm.complete(prompt)
```

**Potential:** Could automate Correctness and Insight Quality
**Risk:** LLM scoring may introduce new biases
**Recommendation:** Use as assist tool, not replacement for human review

---

## Recommendations

### For Quick Evaluation (5 minutes)

Run automated scripts only:
```bash
# Get 65-80 point baseline
./scripts/analyze-research-quality.sh <scenario> <agent-id>
# + run syntax validation
# + count files
# + count TODOs
```

**Use when:**
- Quick pass/fail check needed
- Comparing many scenarios
- Initial triage before deep evaluation

### For Full Evaluation (20-25 minutes)

Run automation + focused manual review:
```bash
# 1min: Automated scoring
./scripts/batch-analyze-research-quality.sh  # (if doing all scenarios)

# 15-20min: Manual review
# - Correctness (15min)
# - Insight Quality (5min)
```

**Use when:**
- Publication-quality results needed
- High-stakes decisions
- Representative samples from experiment

### For Research/Analysis

Use batch automation to find patterns:
```bash
# Score all scenarios
./scripts/batch-analyze-research-quality.sh

# Analyze results
cat RESEARCH_QUALITY_SCORES.jsonl | jq -s '
  group_by(.scenario | split("-")[2]) |
  map({model: .[0].scenario | split("-")[2], avg: (map(.research_quality.total_score) | add / length)})
'
```

**Use when:**
- Comparing model performance
- Testing prompt variations
- Identifying systematic issues

---

## Conclusion

We've successfully automated **65-80 of 120 points** (54-67%) in the Enhanced Rubric:

✅ **Fully automated (65 points):**
- Functional Verification: 30 points
- Completeness Calibration: 25 points
- Research Quality: 15 points (NEW!)

✅ **Partially automated (15 points):**
- Actionability: 10 of 15 points
- Specificity: 7 of 10 points

⚠️ **Manual only (40 points):**
- Correctness: 20 points
- Insight Quality: 5 points
- Documentation quality: 5 points

This represents a **6-8x speedup** in evaluation time while maintaining consistency and reproducibility.

The Research Quality automation was suggested by agent a4abbd8 and successfully implemented to enable batch analysis of all 27 simulation scenarios.

---

**Key Files:**
- `/Users/bln/play/agentic-primer/scripts/analyze-research-quality.sh`
- `/Users/bln/play/agentic-primer/scripts/batch-analyze-research-quality.sh`
- `RESEARCH_QUALITY_SCORES.jsonl` (27 scenario scores)
- `RESEARCH_QUALITY_SUMMARY.md` (statistical analysis)
- `RESEARCH_QUALITY_SCORING_AUTOMATION.md` (detailed methodology)

**Related Documentation:**
- ENHANCED_RUBRIC.md (120-point scoring system)
- MANUAL_EVALUATION_PROMPTS.md (human evaluation guidance)

**Date:** 2026-01-08
**Version:** 1.0
