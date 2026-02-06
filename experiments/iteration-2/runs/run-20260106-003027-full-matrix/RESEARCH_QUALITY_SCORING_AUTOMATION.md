# Research Quality Scoring Automation

**Date:** 2026-01-08
**Purpose:** Automate Research Quality scoring (Dimension 6, 15 points) using deterministic log analysis
**Related:** ENHANCED_RUBRIC.md (Dimension 6), MANUAL_EVALUATION_PROMPTS.md (Section 6)

---

## Overview

This document describes the automated approach to scoring Research Quality (15 points) by analyzing agent logs and generated documentation. This technique was suggested by agent a4abbd8 when creating the manual evaluation prompts.

### Why Automation?

The manual evaluation prompts (MANUAL_EVALUATION_PROMPTS.md) suggested checking agent logs for WebSearch usage:

```bash
# Extract WebSearch calls from compact agent logs
grep '"tool":"WebSearch"' agent-session.jsonl | jq .
```

We've formalized this into a deterministic scoring script that:
1. **Analyzes agent logs** for WebSearch/WebFetch tool calls
2. **Scans generated docs** for source citations and URLs
3. **Checks implementation currency** (2026 standards: actions@v4, .yml issue forms, etc.)
4. **Scores objectively** based on evidence found

---

## Scoring Methodology

### 6.1 WebSearch Tool Usage (8 points)

**Data Source:** Agent logs at `/tmp/claude/-Users-bln-play-agentic-primer/tasks/<agent-id>.output`

**Detection Method:**
```bash
# Count WebSearch/WebFetch calls
grep "WebSearch\|WebFetch" <agent-log> | wc -l

# Extract query parameters
grep -o '"query":"[^"]*"' <agent-log> | cut -d'"' -f4
```

**Scoring Logic:**
- **8 points:** 3+ WebSearch calls for critical components
- **6 points:** 1-2 WebSearch calls
- **3 points:** No WebSearch BUT implementation uses current standards (actions@v4, .yml forms, Node.js 18+)
- **0 points:** No WebSearch AND uses outdated patterns (actions@v2, markdown templates, Node.js 12-16)

**Current Standards Checked:**
- GitHub Actions: `actions/checkout@v4`, `actions/setup-node@v4` (not @v2)
- Issue templates: `.yml` issue forms (not `.md` markdown templates)
- Node.js: v18 or v20 LTS (not v12/v14/v16)

### 6.2 Source Citation & Currency (7 points)

**Data Source:** All markdown files in scenario directory

**Detection Method:**
```bash
# Find URLs in documentation
grep -rhE "https?://[^\s)]+" docs/ *.md

# Check for source attributions
grep -rhE "source:|per:|according to:|based on:" docs/ *.md

# Find year references
grep -rhE "202[456]" docs/ *.md

# Count official GitHub documentation URLs
grep "docs.github.com\|github.blog\|actions/" <urls>
```

**Scoring Logic:**
- **7 points:** 2+ URLs from official sources (docs.github.com), year references (2025-2026), clear attribution
- **5 points:** 1-2 citations or source attributions but no URLs
- **3 points:** No explicit citations BUT implementation is current (implicit research)
- **0 points:** No citations AND uses outdated practices

---

## Tools

### 1. analyze-research-quality.sh

**Purpose:** Score a single scenario

**Usage:**
```bash
/Users/bln/play/agentic-primer/scripts/analyze-research-quality.sh <scenario-dir> <agent-id>

# Example
/Users/bln/play/agentic-primer/scripts/analyze-research-quality.sh \
  experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S1-opus \
  a715b99
```

**Options:**
- `-v, --verbose`: Show detailed analysis (action versions, URLs found, etc.)
- `-j, --json`: Output JSON for automation
- `-h, --help`: Show usage information

**Output Example:**
```
==========================================
  RESEARCH QUALITY ANALYSIS
==========================================

Scenario: P1-S1-opus
Agent ID: a715b99

----------------------------------------
6.1 WebSearch Tool Usage (8 points)
----------------------------------------
Score: 3/8 - NO RESEARCH BUT CURRENT
  WebSearch calls: 0
  No WebSearch queries found in agent log

Implementation Currency:
  ✅ current action versions
  ✅ issue forms
  ✅ current nodejs

----------------------------------------
6.2 Source Citation & Currency (7 points)
----------------------------------------
Score: 7/7 - EXCELLENT CITATIONS
  URLs in documentation: 15
  Source attributions: 0
  Official GitHub docs: 8
  Year references: 2026

==========================================
  TOTAL SCORE: 10/15 (66.7%)
==========================================

✅ GOOD - Adequate research or current practices
```

**JSON Output:**
```json
{
  "scenario": "P1-S1-opus",
  "agent_id": "a715b99",
  "research_quality": {
    "total_score": 10,
    "max_score": 15,
    "percentage": 66.7,
    "websearch": {
      "score": 3,
      "max_score": 8,
      "count": 0,
      "label": "NO RESEARCH BUT CURRENT",
      "queries": []
    },
    "citations": {
      "score": 7,
      "max_score": 7,
      "count": 15,
      "label": "EXCELLENT CITATIONS",
      "urls_found": 15,
      "source_attributions": 0,
      "official_docs_count": 8,
      "year_references": ["2026"]
    },
    "currency": {
      "current_practices": ["current_action_versions", "issue_forms", "current_nodejs"],
      "outdated_patterns": []
    }
  }
}
```

### 2. batch-analyze-research-quality.sh

**Purpose:** Score all 27 scenarios in the full matrix

**Usage:**
```bash
/Users/bln/play/agentic-primer/scripts/batch-analyze-research-quality.sh
```

**Outputs:**
1. **RESEARCH_QUALITY_SCORES.jsonl** - JSONL file with one JSON object per scenario
2. **RESEARCH_QUALITY_SUMMARY.md** - Markdown summary with table and statistics

**Summary Report Includes:**
- Score table for all 27 scenarios
- Statistics by model (Opus, Sonnet, Haiku)
- Key findings (WebSearch usage, implementation currency)
- Interpretation of results

---

## Results from Full Matrix Analysis

**Date:** 2026-01-08
**Scenarios:** 27 (P1-P3 × S1-S3 × Opus/Sonnet/Haiku)

### Key Findings

1. **No WebSearch Usage**
   - 0 WebSearch calls across all 27 scenarios
   - All agents relied on training data instead of active research

2. **Current Implementation Standards**
   - 16/27 scenarios (59%) used current practices
   - 0/27 scenarios used outdated patterns
   - Most implementations used actions@v4, .yml issue forms, and current Node.js

3. **Citation Patterns**
   - URLs found in most scenarios
   - Official GitHub docs referenced frequently
   - Year references (2025-2026) common

4. **Average Scores by Model**
   - **Opus:** 7.1/15 (47%), driven by current practices (3pts WebSearch)
   - **Sonnet:** 7.7/15 (51%), best citation scores (6.3/7 avg)
   - **Haiku:** 5.9/15 (39%), lowest overall scores

### Score Distribution

- **10/15 (B grade):** 4 scenarios (all Opus or Sonnet)
- **7-8/15 (C grade):** 18 scenarios
- **5/15 (F grade):** 4 scenarios (3 Haiku, 1 Sonnet)
- **0/15 (F grade):** 1 scenario (P1-S1-haiku)

---

## Integration with Enhanced Rubric

### Automated vs Manual Scoring

The Enhanced 120-Point Rubric splits Dimension 6 (Research Quality) evaluation:

**Automated Components (via analyze-research-quality.sh):**
- ✅ WebSearch call counting (objective)
- ✅ URL detection in docs (objective)
- ✅ Year reference detection (objective)
- ✅ Action version checking (objective)
- ✅ Implementation currency patterns (objective)

**Manual Components (human judgment still required):**
- ⚠️ Search query quality assessment (Are queries relevant? Specific enough?)
- ⚠️ Source attribution verification (Is attribution meaningful or just a URL?)
- ⚠️ Research impact on design decisions (Did research inform specific choices?)

**Recommended Workflow:**
1. **Run automated script first** - Get objective score (covers ~80% of Dimension 6)
2. **Manually review edge cases** - If score is borderline (5-6 or 9-10 points)
3. **Spot-check high-value scenarios** - P3-S3 combinations warrant extra scrutiny

### Score Calibration

The automated script is **conservative by design**:
- If no WebSearch found, checks for current practices (gives 3pts instead of 0pts)
- If URLs found but no official docs, gives partial credit (5pts instead of 7pts)
- Requires both conditions for maximum scores

**Human evaluators can override** if manual review reveals:
- Exceptional research quality not captured by automation (upgrade score)
- Citations are superficial or unrelated (downgrade score)
- Implementation is current by coincidence, not research (downgrade score)

---

## Example Scenarios

### High Score: P1-S1-opus (10/15, 66.7%)

**WebSearch:** 3/8 (no searches, but current implementation)
**Citations:** 7/7 (15 URLs, 8 official GitHub docs, 2026 references)

**Why this scored well:**
- Used `actions/checkout@v4` (current)
- Used `.yml` issue forms (current)
- Used Node.js v20 (current)
- Cited docs.github.com URLs
- Mentioned 2026 in documentation

**Automated scoring:** Accurate - implementation is current despite no research

### Low Score: P1-S1-haiku (0/15, 0.0%)

**WebSearch:** 0/8 (no searches, no current practices detected)
**Citations:** 0/7 (no URLs, no attributions, no year references)

**Why this scored poorly:**
- Flat file structure (implementation issue, not research issue)
- No documentation generated
- No URLs or citations found
- No year references found

**Automated scoring:** Accurate - no evidence of research or currency

### Mid Score: P2-S2-haiku (8/15, 53.3%)

**WebSearch:** 3/8 (no searches, but current implementation)
**Citations:** 5/7 (URLs found but no official docs)

**Why this scored mid-range:**
- Used current action versions
- Used `.yml` issue forms
- Included URLs in docs but not from official sources
- Mentioned 2025-2026 years

**Automated scoring:** Appropriate - shows currency but lacks authoritative citations

---

## Limitations & Future Improvements

### Current Limitations

1. **WebSearch Detection:**
   - Only detects explicit tool calls in agent logs
   - Cannot detect "mental" research (agent considering knowledge without WebSearch)
   - Cannot assess quality of search queries automatically

2. **Citation Quality:**
   - Counts URLs but doesn't verify they're still valid
   - Doesn't check if citation is meaningful or just link spam
   - Doesn't verify if cited source actually informed design decisions

3. **Training Data Currency:**
   - Assumes current practices (actions@v4) indicate good training data
   - Cannot distinguish "lucky guess" from "well-trained model"
   - Model training cutoff dates affect what's considered "current"

### Potential Improvements

1. **WebSearch Quality Scoring:**
   ```bash
   # Check query specificity
   - "GitHub Actions best practices 2026" → specific ✅
   - "what is github" → too broad ❌

   # Check query relevance
   - Searches for actual project needs (CODEOWNERS syntax) ✅
   - Unrelated searches ❌
   ```

2. **Citation Impact Analysis:**
   ```bash
   # Cross-reference citations with implementation
   - Find URL for "actions@v4" in docs
   - Check if implementation actually uses actions@v4
   - Correlation = research informed decision ✅
   ```

3. **Semantic Analysis:**
   ```python
   # Use LLM to assess research quality
   - Does attribution explain WHY decision was made?
   - Does implementation match cited best practices?
   - Is design rationale grounded in sources?
   ```

---

## Usage in Evaluation Workflow

### Phase 1: Automated Batch Analysis

```bash
# Score all 27 scenarios
./scripts/batch-analyze-research-quality.sh

# Review summary
cat experiments/iteration-2/runs/run-20260106-003027-full-matrix/RESEARCH_QUALITY_SUMMARY.md

# Identify scenarios needing manual review
cat RESEARCH_QUALITY_SCORES.jsonl | jq 'select(.research_quality.total_score < 6 or .research_quality.total_score > 12)'
```

### Phase 2: Spot-Check Manual Review

For scenarios flagged in Phase 1:

1. **Read agent log** - Look for WebSearch calls manually
2. **Review generated docs** - Assess citation quality and design rationale
3. **Check implementation** - Verify currency claims are accurate
4. **Adjust score** - Override automated score if justified

### Phase 3: Final Scoring

Combine automated Research Quality scores (15 points) with:
- Automated scores from Phase 1 (Functional Verification, Completeness, etc.)
- Manual scores (Correctness, Insight Quality)
- Human-reviewed overrides for Research Quality edge cases

**Final 120-Point Score = Automated (80-95pts) + Manual (25-40pts)**

---

## Conclusion

The Research Quality automation achieves:

✅ **Deterministic scoring** - Same inputs always produce same outputs
✅ **Fast analysis** - 27 scenarios analyzed in ~30 seconds
✅ **Objective evidence** - Based on log files and generated code, not subjective judgment
✅ **Partial automation** - Covers ~80% of Research Quality dimension
✅ **Human-in-the-loop** - Manual review for edge cases and quality assessment

This approach was suggested by the evaluation prompt creation agent (a4abbd8) and successfully implemented to scale Research Quality assessment across all simulation scenarios.

---

**Scripts:**
- `/Users/bln/play/agentic-primer/scripts/analyze-research-quality.sh`
- `/Users/bln/play/agentic-primer/scripts/batch-analyze-research-quality.sh`

**Data:**
- RESEARCH_QUALITY_SCORES.jsonl (27 scenario scores)
- RESEARCH_QUALITY_SUMMARY.md (statistical summary)

**Related Documentation:**
- ENHANCED_RUBRIC.md (Dimension 6: Research Quality)
- MANUAL_EVALUATION_PROMPTS.md (Section 6: Research Quality)
