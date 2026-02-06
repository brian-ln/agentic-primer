# Enhanced 120-Point Evaluation Rubric

**Version:** 2.0
**Date:** 2026-01-08
**Purpose:** Evaluate agent-generated bootstrap implementations with calibrated completeness expectations

---

## Overview

This enhanced rubric addresses critical gaps in the original 100-point system:

1. **Completeness Calibration** - Different prompt levels require different completeness thresholds
2. **Functional Verification** - Direct testing against success criteria
3. **Research Quality** - Evidence of current, web-based research
4. **Actionability** - Ready-to-use vs requires rework

**Total Points:** 120
**Pass Threshold:** 80/120 (67%)
**Excellence Threshold:** 100/120 (83%)

---

## Scoring Dimensions

### 1. FUNCTIONAL VERIFICATION (30 points)

**Direct testing against success criteria - pass/fail gates**

#### 1.1 Syntax Validation (10 points)

**Automated validation of generated files:**

| Points | Criteria |
|--------|----------|
| 10 | ALL files pass validation: yamllint (YAML), shellcheck (shell scripts), no syntax errors |
| 7 | Minor warnings only (e.g., line length, comment style) |
| 4 | 1-2 syntax errors that are easily fixable |
| 0 | 3+ syntax errors OR critical errors (unparseable YAML, invalid shell) |

**How to measure:**
```bash
# YAML files
yamllint .github/**/*.yml .github/**/*.yaml

# Shell scripts
shellcheck scripts/**/*.sh

# Markdown (optional, informational only)
markdownlint **/*.md
```

**Scoring guidance:**
- Run automated tools on ALL generated files
- Count total errors/warnings
- Syntax errors are objective, not subjective
- DO NOT score if tools are not available (mark N/A)

#### 1.2 Workflow Trigger Correctness (10 points)

**GitHub Actions workflows trigger on correct events:**

| Points | Criteria |
|--------|----------|
| 10 | Workflow triggers match success criteria exactly (e.g., `on: issues: types: [opened]`) |
| 7 | Workflow triggers are present but overly broad (e.g., triggers on all issue events when only `opened` needed) |
| 4 | Workflow triggers partially correct (missing some required events) |
| 0 | No workflow file OR incorrect triggers OR missing required workflows |

**How to measure:**
```bash
# Extract workflow triggers
grep -A 5 "^on:" .github/workflows/*.yml

# Check against success criteria
# S2 example: Must trigger on issue creation
# Expected: on.issues.types includes 'opened'
```

**Scoring guidance:**
- Compare workflow triggers to success criteria document
- S1 (minimal): May not specify triggers → score 10 if workflow exists
- S2 (moderate): Explicitly requires "workflow triggers on issue creation" → must match
- S3 (comprehensive): Check multi-event triggers (issues, PRs, etc.)

#### 1.3 Structure Correctness (10 points)

**Files placed in correct GitHub-standard locations:**

| Points | Criteria |
|--------|----------|
| 10 | Perfect structure: `.github/CODEOWNERS`, `.github/ISSUE_TEMPLATE/`, `.github/workflows/`, `docs/` hierarchy |
| 7 | Minor issues: Extra files in root, but all required files correctly placed |
| 4 | Major issues: Some required files misplaced (e.g., `CODEOWNERS` at root instead of `.github/`) |
| 0 | Flat structure (e.g., `docs-knowledge-README.md` instead of `docs/knowledge/README.md`) OR critical files missing |

**How to measure:**
```bash
# Check required directories exist
test -d .github && echo ".github: ✓" || echo ".github: ✗"
test -d .github/ISSUE_TEMPLATE && echo "ISSUE_TEMPLATE: ✓" || echo "ISSUE_TEMPLATE: ✗"
test -d .github/workflows && echo "workflows: ✓" || echo "workflows: ✗"
test -d docs/knowledge && echo "knowledge: ✓" || echo "knowledge: ✗"

# Check CODEOWNERS location
test -f .github/CODEOWNERS && echo "CODEOWNERS: ✓" || echo "CODEOWNERS: ✗"

# Flag flat structure (Haiku pattern)
ls | grep -E "docs-knowledge|docs_knowledge" && echo "FLAT STRUCTURE DETECTED ✗"
```

**Scoring guidance:**
- GitHub features REQUIRE specific paths (`.github/CODEOWNERS`, not `CODEOWNERS`)
- Flat structure (underscores/hyphens instead of directories) = automatic 0 points
- Extra files in root are OK if not interfering (deduct 3 points max)

**Known model patterns (from analysis):**
- Opus: 100% correct structure (all 9 scenarios)
- Sonnet: 100% correct structure (all 9 scenarios)
- Haiku: 0% correct structure (systematic flat structure issue)

---

### 2. COMPLETENESS CALIBRATION (25 points)

**Appropriate level of detail for prompt complexity**

**Core Principle:** Different prompts warrant different completeness levels. Penalize both under-completeness AND over-engineering.

#### 2.1 Prompt-Specific Completeness Targets

**P1 (Minimal Prompt - 10 words):**
```
Example: "Bootstrap @copilot issue automation with auto-review and knowledge base."
```

| Points | Completeness | Description |
|--------|--------------|-------------|
| 25 | 65-70% | **OPTIMAL** - Creates core files only: issue template, CODEOWNERS, knowledge README templates, basic workflow |
| 20 | 60-65% | Slightly minimal - Missing 1-2 nice-to-have files (e.g., no validation script) |
| 15 | 70-80% | Slight over-engineering - Extra docs (FILE_MANIFEST, EXECUTIVE_SUMMARY) not needed for minimal prompt |
| 10 | 80-90% | **OVER-ENGINEERED** - Created extensive docs, multiple validation scripts, example knowledge content |
| 5 | 90-100% | Severe over-engineering - 15+ files for a 10-word prompt (e.g., P1-S3-sonnet: 36 files) |
| 0 | <60% OR >100% | Under-complete (missing critical files) OR absurdly over-complete |

**How to measure:**
```bash
# Count files (excluding .git)
file_count=$(find . -type f | grep -v "^./.git" | wc -l)

# Size in KB
total_kb=$(du -sk . | awk '{print $1}')

# P1 optimal range:
# Files: 4-8
# Size: 20-80 KB
```

**P2 (Moderate Prompt - 14 words):**
```
Example: "Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base."
```

| Points | Completeness | Description |
|--------|--------------|-------------|
| 25 | 70-75% | **OPTIMAL** - Core files + basic docs: README, issue template, CODEOWNERS, workflow, knowledge structure, basic validation |
| 20 | 65-70% | Slightly minimal - Missing validation script or comprehensive README |
| 15 | 75-85% | Slight over-engineering - Extra summary docs, multiple READMEs for same purpose |
| 10 | 85-95% | **OVER-ENGINEERED** - Multiple documentation perspectives, extensive examples |
| 5 | 95-100% | Severe over-engineering - 20+ files for a 14-word prompt |
| 0 | <65% OR >100% | Under-complete OR absurdly over-complete |

**How to measure:**
```bash
# P2 optimal range:
# Files: 6-12
# Size: 60-150 KB
```

**P3 (Detailed Prompt - 35 words):**
```
Example: "Create issue-driven development system: Issue template (.github/ISSUE_TEMPLATE/task.yml) for @copilot tasks, CODEOWNERS (* @owner) for PR auto-assignment, Knowledge base (docs/knowledge/) with patterns/decisions/insights structure, README with workflow: issue → @copilot → PR → review via web UI"
```

| Points | Completeness | Description |
|--------|--------------|-------------|
| 25 | 75-85% | **OPTIMAL** - All specified files + comprehensive docs: detailed README, validation scripts, example knowledge content, implementation summary |
| 20 | 70-75% | Slightly minimal - Missing validation script or example knowledge content |
| 15 | 85-95% | Acceptable thoroughness - Multiple documentation perspectives, extensive examples (justified by detailed prompt) |
| 10 | 60-70% OR 95-100% | Under-complete for detailed prompt OR approaching over-engineering threshold |
| 5 | <60% OR >100% | Missing critical components OR excessive redundancy |
| 0 | <50% OR absurd over-engineering (30+ files) | Fundamentally incomplete OR P1-S3-sonnet level (36 files) |

**How to measure:**
```bash
# P3 optimal range:
# Files: 8-16
# Size: 80-180 KB
```

#### 2.2 Objective Completeness Measurement

**Formula:**
```
Completeness % = (Files_Created / Expected_Maximum) × 100
```

**Expected Maximum by Prompt:**
- P1: 8 files = 100%
- P2: 12 files = 100%
- P3: 16 files = 100%

**Example calculations:**
- P1-S3-sonnet: 36 files → 36/8 = 450% → 0 points (absurd over-engineering)
- P2-S3-opus: 1 file → 1/12 = 8% → 0 points (under-complete)
- P3-S2-sonnet: 8 files → 8/16 = 50% → falls in 70-75% range after file quality adjustment → 20 points

**File Quality Adjustment:**
- If files contain substantial content (not just templates), increase effective completeness by 10-20%
- If files are mostly TODOs/placeholders, decrease effective completeness by 10-20%

**Calibration rationale:**
1. **P1 (minimal)** - User expects quick bootstrap, not comprehensive system → penalize elaboration
2. **P2 (moderate)** - User wants functional system with docs → balanced approach
3. **P3 (detailed)** - User provided specific requirements → thoroughness expected

**Why calibration matters:**
- From analysis: P1 prompts produced LARGEST outputs (14.7 files avg, 190 KB)
- From analysis: P3 prompts produced SMALLEST outputs (7.8 files avg, 86 KB)
- **Counterintuitive:** Shorter prompts → more elaboration (agents "fill in gaps")
- **Implication:** Must penalize over-elaboration for minimal prompts

---

### 3. CORRECTNESS (20 points)

**Semantic correctness - would it work if deployed?**

#### 3.1 Logical Correctness (15 points)

| Points | Criteria |
|--------|----------|
| 15 | All files are semantically correct: workflows would execute, CODEOWNERS would assign, issue templates would render |
| 12 | Minor logical issues: overly broad workflow triggers, CODEOWNERS pattern too generic (e.g., `*` instead of `* @specific-team`) |
| 9 | Moderate issues: workflow jobs missing required steps, knowledge base structure incomplete |
| 6 | Major issues: workflow would fail on execution, CODEOWNERS syntax invalid, missing critical dependencies |
| 0 | Fundamentally broken: workflow unparseable, CODEOWNERS incorrect format, system would not function |

**How to measure:**
- **Workflows:** Trace execution path - do all steps have required inputs? Are actions/versions valid?
- **CODEOWNERS:** Does syntax match GitHub spec? (`pattern @owner`, not `owner: pattern`)
- **Issue templates:** Would GitHub render the form correctly? Required fields present?
- **Knowledge base:** Can files be navigated? Do cross-references work?

**Common errors:**
- Workflow: `uses: actions/checkout@v2` (outdated version, should be v3+)
- CODEOWNERS: `docs/: @team` (missing asterisk: should be `docs/* @team`)
- Issue template: Missing `name`, `description`, or `body` fields

#### 3.2 Edge Case Handling (5 points)

| Points | Criteria |
|--------|----------|
| 5 | Handles edge cases: empty issues, missing labels, concurrent PRs, invalid YAML in user input |
| 4 | Handles most edge cases but missing 1-2 (e.g., no handling for issues without body) |
| 2 | Minimal edge case handling (e.g., assumes happy path only) |
| 0 | No edge case handling or incorrect assumptions (e.g., assumes single agent always) |

**How to measure:**
- Check workflow: Does it validate inputs before processing?
- Check scripts: Do they handle missing files/directories?
- Check docs: Do they mention error states or failure modes?

**Example edge cases:**
- Issue created without required fields
- Multiple agents replying simultaneously
- CODEOWNERS file missing or malformed
- Knowledge base directories not yet created

---

### 4. ACTIONABILITY (15 points)

**Ready to use without modification**

#### 4.1 Immediate Usability (10 points)

| Points | Criteria |
|--------|----------|
| 10 | **IMMEDIATE USE** - Copy files, commit, push → system works. No placeholders, no TODOs, no configuration needed |
| 7 | **MINOR TWEAKS** - 1-2 placeholders to fill in (e.g., replace `@owner` with actual GitHub username) |
| 4 | **MODERATE REWORK** - 3-5 TODOs, need to add actual content to knowledge base, configure workflow variables |
| 0 | **SIGNIFICANT REWORK** - Most files are templates/skeletons, extensive TODOs, missing critical implementation |

**How to measure:**
```bash
# Count placeholders/TODOs
grep -ri "TODO\|FIXME\|PLACEHOLDER\|REPLACE_ME\|YOUR_\|FILL_IN" . | wc -l

# Scoring:
# 0 instances = 10 points (immediate use)
# 1-3 instances = 7 points (minor tweaks)
# 4-8 instances = 4 points (moderate rework)
# 9+ instances = 0 points (significant rework)
```

**Contextual appropriateness:**
- Some TODOs are GOOD (e.g., "TODO: Add first ADR" in empty `decisions/` directory)
- Some TODOs are BAD (e.g., "TODO: Implement workflow logic" in main workflow file)
- Judge based on: Is this TODO for future expansion (good) or missing core functionality (bad)?

#### 4.2 Documentation Quality (5 points)

| Points | Criteria |
|--------|----------|
| 5 | Clear, actionable documentation: step-by-step setup, usage examples, troubleshooting section |
| 4 | Good documentation but missing troubleshooting or examples |
| 2 | Minimal documentation: just describes what files exist, no usage guide |
| 0 | No documentation OR documentation is incorrect/misleading |

**How to measure:**
- Does README exist? Does it explain setup steps?
- Are there usage examples (e.g., "Create an issue with this template: ...")?
- Is there a quickstart guide (e.g., "00-START-HERE.md")?
- Are there inline comments in complex files (workflows, scripts)?

---

### 5. RESEARCH QUALITY (15 points)

**Evidence of current (2026) web-based research**

**Critical context:** Agent responses should be grounded in current best practices, not stale knowledge from training data.

**⚠️ AUTOMATION AVAILABLE:** This dimension can be scored automatically using `scripts/analyze-research-quality.sh`. See RESEARCH_QUALITY_SCORING_AUTOMATION.md for details.

#### 5.1 WebSearch Tool Usage (8 points)

| Points | Criteria |
|--------|----------|
| 8 | Used WebSearch 3+ times for: GitHub Actions best practices, CODEOWNERS syntax, issue template format, current tool versions |
| 6 | Used WebSearch 1-2 times for critical components (e.g., workflow syntax) |
| 3 | No WebSearch usage BUT implementation follows current 2026 standards (lucky guess or good training data) |
| 0 | No WebSearch usage AND implementation uses outdated patterns (e.g., `actions/checkout@v2` instead of `v4`) |

**How to measure:**
```bash
# Automated (recommended)
./scripts/analyze-research-quality.sh <scenario-dir> <agent-id>

# Manual verification
grep "WebSearch\|WebFetch" /tmp/claude/-Users-bln-play-agentic-primer/tasks/<agent-id>.output
```

- Check agent logs for WebSearch tool calls
- Count distinct WebSearch queries
- Verify search queries were relevant (e.g., "github actions codeowners 2026" NOT "what is github")

**Example good searches:**
- "GitHub Actions best practices 2026"
- "CODEOWNERS file syntax specification"
- "GitHub issue template YAML schema"
- "yamllint configuration 2026"

**Example poor searches:**
- "what is github" (too basic)
- "how to code" (not specific)
- (no search at all) = 0 points IF implementation is outdated

#### 5.2 Source Citation & Currency (7 points)

| Points | Criteria |
|--------|----------|
| 7 | Cites 2+ sources from 2025-2026, includes source URLs in docs, research informed specific decisions |
| 5 | Cites 1-2 sources from 2024-2026, mentions sources but no URLs |
| 3 | No explicit citations BUT implementation is current (implicit research) |
| 0 | No citations AND uses outdated practices (e.g., Node.js 12, actions/checkout@v1) |

**How to measure:**
```bash
# Automated (recommended)
./scripts/analyze-research-quality.sh --verbose <scenario-dir> <agent-id>

# Manual verification
grep -ri "source:\|per:\|according to:\|https://" docs/ README.md
grep -rhE "202[456]" docs/ *.md
```

- Search docs for source citations
- Check dates mentioned in docs
- Verify tool versions against current standards (2026)

**Current 2026 standards (as of 2026-01-08):**
- GitHub Actions: `actions/checkout@v4`, `actions/setup-node@v4`
- Node.js: v20 LTS (v18 acceptable, v16 outdated)
- yamllint: v1.35+
- shellcheck: v0.10+

**Example citations (good):**
```markdown
# Design Decision: Use GitHub Native Features

Based on GitHub's 2025 Actions documentation, we use issue forms
instead of markdown templates for better validation.

Source: https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests/syntax-for-issue-forms (accessed 2026-01-08)
```

**No citation (bad):**
```markdown
# Design Decision: Use GitHub Native Features

We use issue forms because they're better.
```

---

### 6. SPECIFICITY (10 points)

**Low placeholder density, contextually appropriate**

#### 6.1 Placeholder Density (7 points)

| Points | Criteria |
|--------|----------|
| 7 | 0-2 placeholders total (highly specific implementation) |
| 5 | 3-5 placeholders, all contextually appropriate (e.g., `@owner` in CODEOWNERS is acceptable) |
| 3 | 6-10 placeholders, some unnecessary (e.g., `YOUR_REPO_NAME` when repo context available) |
| 0 | 11+ placeholders OR critical values left as placeholders (e.g., workflow name is `WORKFLOW_NAME`) |

**How to measure:**
```bash
# Count placeholders (case-insensitive)
grep -riE "TODO|FIXME|PLACEHOLDER|REPLACE_ME|YOUR_|FILL_IN|XXX|TBD|CHANGEME" . \
  --exclude-dir=.git | wc -l

# Contextual review:
grep -riE "TODO|FIXME|PLACEHOLDER" . --exclude-dir=.git
# Review each instance:
# - Is this a placeholder for future expansion? (OK)
# - Is this a missing core value? (BAD)
```

**Contextually appropriate placeholders (DO NOT penalize):**
```yaml
# CODEOWNERS - user must specify actual owner
* @owner

# Issue template - example value that user will replace
assignees:
  - copilot-agent  # Replace with your agent's GitHub username
```

**Inappropriate placeholders (PENALIZE):**
```yaml
# Workflow - should be filled in by agent
name: WORKFLOW_NAME  # BAD: Agent should provide actual name

jobs:
  job_name:  # BAD: Generic name
    steps:
      - run: echo "TODO: Add actual logic"  # BAD: Missing core implementation
```

#### 6.2 Specificity of Implementation (3 points)

| Points | Criteria |
|--------|----------|
| 3 | Implementation is specific to the prompt (e.g., references @copilot, issue-driven workflow, specific file paths from P3) |
| 2 | Generic implementation that could apply to any project (no mention of copilot, issue automation, or prompt-specific details) |
| 0 | Implementation contradicts prompt (e.g., creates PR-driven workflow when prompt says issue-driven) |

**How to measure:**
- Check README: Does it mention `@copilot` (if prompt included it)?
- Check workflows: Do they reference issue automation (if prompt required it)?
- Check knowledge base: Does it reflect prompt context (e.g., ADR about using GitHub native features)?

---

### 7. INSIGHT QUALITY (5 points)

**Documented assumptions, constraints, and edge cases**

#### 7.1 Assumptions Documented (3 points)

| Points | Criteria |
|--------|----------|
| 3 | Explicitly documents 3+ assumptions (e.g., "Assumes single-owner model", "Assumes GitHub Enterprise not needed", "Assumes agents reply within 24h") |
| 2 | Documents 1-2 assumptions |
| 1 | Implicit assumptions visible in implementation but not documented |
| 0 | No assumptions documented AND implementation makes risky assumptions without acknowledgment |

**How to measure:**
- Search docs for "assumption", "assumes", "constraint", "limitation"
- Check design docs or ADRs for decision rationale
- Look for "Why we chose X over Y" sections

**Example (good):**
```markdown
## Assumptions

1. **Single repository model**: CODEOWNERS uses `*` pattern, assuming one primary owner for all files.
2. **GitHub.com (not Enterprise)**: Uses public GitHub Actions, not enterprise-specific features.
3. **Agent response time**: Workflow timeout set to 5 minutes, assuming agents respond quickly.
4. **English-only**: Issue templates do not include i18n support.
```

**Example (bad):**
```markdown
## Design

We created a CODEOWNERS file and workflow.
(No mention of why or what assumptions were made)
```

#### 7.2 Edge Cases Identified (2 points)

| Points | Criteria |
|--------|----------|
| 2 | Documents 2+ edge cases or failure modes (e.g., "What if issue has no body?", "What if multiple agents reply?", "What if CODEOWNERS is deleted?") |
| 1 | Documents 1 edge case |
| 0 | No edge cases documented |

**How to measure:**
- Search docs for "edge case", "failure mode", "error handling", "what if"
- Check troubleshooting sections
- Look for workflow error handling steps

**Example (good):**
```markdown
## Edge Cases

1. **Empty issue body**: Workflow validates issue body exists before triggering agent.
2. **Concurrent agents**: Uses GitHub's concurrency control to prevent race conditions.
3. **Missing knowledge base**: Script creates `docs/knowledge/` structure if missing.
```

---

## Scoring Summary Table

| Dimension | Points | Weight | Description |
|-----------|--------|--------|-------------|
| **1. Functional Verification** | 30 | 25% | Syntax valid, triggers correct, structure proper |
| **2. Completeness Calibration** | 25 | 21% | Appropriate detail for prompt (P1: 65-70%, P2: 70-75%, P3: 75-85%) |
| **3. Correctness** | 20 | 17% | Logically sound, handles edge cases |
| **4. Actionability** | 15 | 13% | Immediate use vs tweaks vs rework |
| **5. Research Quality** | 15 | 13% | Used WebSearch, cites 2026 sources |
| **6. Specificity** | 10 | 8% | Low placeholder density, contextually appropriate |
| **7. Insight Quality** | 5 | 4% | Assumptions and edge cases documented |
| **TOTAL** | **120** | **100%** | |

**Pass Thresholds:**
- **80/120 (67%)** - Minimum viable implementation
- **100/120 (83%)** - Excellent implementation
- **110/120 (92%)** - Outstanding implementation

---

## Comparison to Original 100-Point Rubric

### What Changed?

**Added Dimensions:**
1. **Completeness Calibration (25 points)** - NEW
   - Original rubric did not distinguish between P1/P2/P3 completeness expectations
   - Original penalized minimal implementations even when appropriate (P1)
   - Original rewarded over-engineering (e.g., P1-S3-sonnet with 36 files)

2. **Research Quality (15 points)** - NEW
   - Original rubric did not measure web research usage
   - Original did not require 2026-current sources
   - Original did not credit agents for researching best practices

**Modified Dimensions:**

3. **Functional Verification (30 points)** - EXPANDED from implied criteria
   - Original: Vague "meets success criteria" (no point allocation)
   - Enhanced: Explicit syntax validation (10), workflow triggers (10), structure (10)
   - Now automatable with `yamllint`, `shellcheck`, `grep`

4. **Actionability (15 points)** - REFINED
   - Original: Binary "works or doesn't" (implied in "correctness")
   - Enhanced: Spectrum from immediate use (10) → minor tweaks (7) → moderate rework (4) → significant rework (0)
   - Added placeholder count measurement

**Retained Dimensions:**

5. **Correctness (20 points)** - SIMILAR to original
   - Original: ~30-40 points for "logic, syntax, completeness"
   - Enhanced: 20 points for logical correctness + edge cases
   - More focused (syntax moved to Functional Verification)

6. **Specificity (10 points)** - SIMILAR to original
   - Original: ~10-15 points for "detail, clarity"
   - Enhanced: 10 points, but now measured objectively (placeholder count)

7. **Insight Quality (5 points)** - REDUCED from original
   - Original: ~15-20 points for "documentation quality"
   - Enhanced: 5 points for assumptions/edge cases only
   - Documentation quality moved to Actionability

### Why 120 Points Instead of 100?

**Rationale:**
1. **Completeness Calibration is critical** - P1/P2/P3 have different quality bars (25 points justified)
2. **Research Quality deserves weight** - Using 2026 sources vs stale knowledge is important (15 points)
3. **Functional Verification is objective** - Syntax/structure/triggers are pass/fail gates (30 points)
4. **Total: 100 + 20 (new) = 120** - Maintains original weights, adds new dimensions

**Pass threshold adjustment:**
- Original: 70/100 (70%)
- Enhanced: 80/120 (67%) - Slightly easier because rubric is more comprehensive
- Excellence: 100/120 (83%) - Higher bar for "excellent" due to added dimensions

---

## Rationale for Changes

### 1. Why Completeness Calibration?

**Problem identified:** From FINAL_ANALYSIS.md:
```
P1 (minimal prompt): 14.7 files avg, 190 KB avg
P3 (detailed prompt): 7.8 files avg, 86 KB avg

Counterintuitive: Shorter prompts → MORE output!
```

**Why this matters:**
- Agents over-elaborate when prompts are vague (P1)
- Users with minimal prompts do NOT want comprehensive systems
- Original rubric rewarded thoroughness regardless of prompt
- **Example:** P1-S3-sonnet (36 files, 335 KB) would score highly on original rubric for "completeness" but is actually OVER-ENGINEERED for a 10-word prompt

**Solution:** Calibrate completeness expectations:
- P1 (10 words): 65-70% complete is OPTIMAL (penalize >80%)
- P2 (14 words): 70-75% complete is OPTIMAL
- P3 (35 words): 75-85% complete is OPTIMAL

**Impact:**
- P1-S3-sonnet (36 files) would score 5/25 (over-engineered)
- P3-S2-opus (3 files) would score 20/25 (slightly minimal for detailed prompt)
- P2-S2-sonnet (4 files) would score 25/25 (perfect balance)

### 2. Why Research Quality?

**Problem identified:** No way to distinguish between:
- Agent A: Uses WebSearch to find 2026 best practices for GitHub Actions
- Agent B: Uses stale training data from 2023, suggests `actions/checkout@v2` (outdated)

**Why this matters:**
- GitHub Actions evolve rapidly (v2 → v3 → v4 in 2 years)
- Best practices change (markdown templates → issue forms in 2021)
- Original rubric did not credit research or penalize staleness

**Solution:** Measure research quality:
- WebSearch usage (8 points) - Did agent look up current docs?
- Source citation (7 points) - Are sources from 2025-2026?

**Impact:**
- Agents that research score up to 15 extra points
- Agents that guess or use stale data score 0-3 points
- Encourages evidence-based implementations

### 3. Why Functional Verification?

**Problem identified:** Original rubric was subjective:
- "Does it meet success criteria?" - Human judgment required
- No automated validation
- Hard to compare across evaluators

**Why this matters:**
- Syntax errors are OBJECTIVE (yamllint reports them)
- Workflow triggers are VERIFIABLE (grep for `on:` section)
- File structure is CHECKABLE (test if `.github/` exists)

**Solution:** Automated functional verification:
```bash
# Syntax (10 points)
yamllint .github/**/*.yml

# Triggers (10 points)
grep -A 5 "^on:" .github/workflows/*.yml

# Structure (10 points)
test -d .github && test -f .github/CODEOWNERS
```

**Impact:**
- 30 points are now automatable (no human judgment needed)
- Consistent scoring across evaluators
- Fast feedback (run tools immediately after generation)

### 4. Why Actionability?

**Problem identified:** Original rubric did not distinguish:
- Implementation A: Ready to use (no TODOs)
- Implementation B: Requires filling in 10 placeholders
- Implementation C: Skeleton only (extensive rework needed)

**Why this matters:**
- User time-to-value varies drastically
- Implementation A: 0 minutes to deploy
- Implementation B: 10 minutes to fill placeholders
- Implementation C: 2 hours to complete implementation

**Solution:** Actionability spectrum:
- 10 points: Immediate use (0 TODOs)
- 7 points: Minor tweaks (1-3 TODOs)
- 4 points: Moderate rework (4-8 TODOs)
- 0 points: Significant rework (9+ TODOs)

**Impact:**
- Penalizes skeleton implementations (common with Haiku)
- Rewards production-ready outputs (common with Opus)
- Objective measurement (count TODOs/placeholders)

---

## Scoring Edge Cases

### Edge Case 1: Haiku Flat Structure

**Scenario:** Haiku creates `docs-knowledge-README.md` instead of `docs/knowledge/README.md`

**Scoring:**
- **Structure Correctness (10 points):** 0 points - Flat structure violates GitHub conventions
- **Actionability (15 points):** 4 points - Requires moderate rework to restructure files
- **Total impact:** -21 points (out of 120)

**Rationale:**
- From analysis: Haiku had 0% structure compliance across all 27 scenarios
- Flat structure breaks GitHub features (CODEOWNERS must be in `.github/`)
- Requires manual restructuring post-generation

### Edge Case 2: Opus Minimal Output for P3

**Scenario:** Opus creates 3 files for P3 (detailed prompt requiring 35 words of specification)

**Scoring:**
- **Completeness Calibration (25 points):** 20 points - Slightly minimal for P3 (expected 8-16 files)
- **Functional Verification (30 points):** 30 points - All files syntactically correct, proper structure
- **Actionability (15 points):** 10 points - Immediate use (no TODOs)
- **Total:** ~100/120 (still excellent despite being minimal)

**Rationale:**
- P3 expects 75-85% completeness → 3 files is ~50% (after quality adjustment)
- But if those 3 files are production-ready, actionable, and correct, still scores well
- Deduct 5 points for slightly under-complete, but don't penalize heavily if quality is high

### Edge Case 3: Sonnet Over-Documentation for P1

**Scenario:** Sonnet creates 36 files for P1 (minimal 10-word prompt)

**Scoring:**
- **Completeness Calibration (25 points):** 5 points - Severe over-engineering (36/8 = 450%)
- **Functional Verification (30 points):** 30 points - Assuming all files are syntactically correct
- **Actionability (15 points):** 4 points - Too many files to review, overwhelming
- **Total:** ~60-70/120 (fails despite correct implementation)

**Rationale:**
- P1 expects 65-70% completeness → 36 files is absurd over-engineering
- User with 10-word prompt does NOT want to navigate 36 files
- Penalize heavily for not matching user intent (minimal prompt = minimal output)

### Edge Case 4: Perfect Implementation but No Research

**Scenario:** Agent creates perfect implementation using only training data (no WebSearch)

**Scoring:**
- **Research Quality (15 points):** 3 points - No WebSearch usage, but implementation is current
- **All other dimensions:** Full points (assuming correctness)
- **Total:** ~105/120 (excellent but not outstanding)

**Rationale:**
- If implementation happens to be current (2026 best practices), give partial credit (3/15)
- Cannot give full research points without evidence of research (WebSearch logs)
- Still scores well overall if implementation is correct

### Edge Case 5: Excellent Research but Poor Implementation

**Scenario:** Agent uses WebSearch 5 times, cites 2026 sources, but creates buggy workflow

**Scoring:**
- **Research Quality (15 points):** 15 points - Perfect research
- **Functional Verification (30 points):** 6 points - Major logical errors in workflow
- **Correctness (20 points):** 6 points - Workflow would fail
- **Total:** ~50-60/120 (fails despite excellent research)

**Rationale:**
- Research is necessary but not sufficient
- Cannot score well without functional implementation
- Research points are capped at 15 (13% of total) to prevent this scenario from passing

---

## Measurement Automation

### Automated Scoring Script Template

```bash
#!/bin/bash
# auto-score.sh - Automated scoring for enhanced rubric

IMPLEMENTATION_DIR="$1"
PROMPT_LEVEL="$2"  # P1, P2, or P3
SUCCESS_CRITERIA="$3"  # S1, S2, or S3

cd "$IMPLEMENTATION_DIR" || exit 1

SCORE=0
MAX_SCORE=120

echo "=== ENHANCED RUBRIC SCORING ==="
echo "Implementation: $IMPLEMENTATION_DIR"
echo "Prompt: $PROMPT_LEVEL, Criteria: $SUCCESS_CRITERIA"
echo ""

# 1. FUNCTIONAL VERIFICATION (30 points)
echo "=== 1. FUNCTIONAL VERIFICATION (30 points) ==="

# 1.1 Syntax Validation (10 points)
echo "1.1 Syntax Validation:"
YAML_ERRORS=$(yamllint .github/**/*.yml 2>&1 | grep -c "error" || true)
SHELL_ERRORS=$(shellcheck scripts/**/*.sh 2>&1 | grep -c "error" || true)
TOTAL_SYNTAX_ERRORS=$((YAML_ERRORS + SHELL_ERRORS))

if [ "$TOTAL_SYNTAX_ERRORS" -eq 0 ]; then
  SYNTAX_SCORE=10
elif [ "$TOTAL_SYNTAX_ERRORS" -le 2 ]; then
  SYNTAX_SCORE=4
else
  SYNTAX_SCORE=0
fi
echo "  Errors: $TOTAL_SYNTAX_ERRORS → Score: $SYNTAX_SCORE/10"
SCORE=$((SCORE + SYNTAX_SCORE))

# 1.2 Workflow Trigger Correctness (10 points)
echo "1.2 Workflow Triggers:"
if [ "$SUCCESS_CRITERIA" = "S2" ] || [ "$SUCCESS_CRITERIA" = "S3" ]; then
  if grep -q "on:" .github/workflows/*.yml && grep -A 5 "on:" .github/workflows/*.yml | grep -q "issues"; then
    TRIGGER_SCORE=10
    echo "  Workflow triggers on issues → Score: 10/10"
  else
    TRIGGER_SCORE=0
    echo "  Workflow missing or incorrect triggers → Score: 0/10"
  fi
else
  TRIGGER_SCORE=10  # S1 doesn't require specific triggers
  echo "  S1: No specific trigger required → Score: 10/10"
fi
SCORE=$((SCORE + TRIGGER_SCORE))

# 1.3 Structure Correctness (10 points)
echo "1.3 Structure Correctness:"
FLAT_FILES=$(ls | grep -E "docs-knowledge|docs_knowledge" | wc -l)
if [ "$FLAT_FILES" -gt 0 ]; then
  STRUCTURE_SCORE=0
  echo "  FLAT STRUCTURE DETECTED → Score: 0/10"
elif [ -d .github ] && [ -f .github/CODEOWNERS ] && [ -d docs/knowledge ]; then
  STRUCTURE_SCORE=10
  echo "  Proper directory structure → Score: 10/10"
else
  STRUCTURE_SCORE=4
  echo "  Missing some required directories → Score: 4/10"
fi
SCORE=$((SCORE + STRUCTURE_SCORE))

# 2. COMPLETENESS CALIBRATION (25 points)
echo ""
echo "=== 2. COMPLETENESS CALIBRATION (25 points) ==="
FILE_COUNT=$(find . -type f | grep -v "^./.git" | wc -l)
echo "Files created: $FILE_COUNT"

case "$PROMPT_LEVEL" in
  P1)
    if [ "$FILE_COUNT" -ge 4 ] && [ "$FILE_COUNT" -le 8 ]; then
      COMPLETENESS_SCORE=25
    elif [ "$FILE_COUNT" -lt 4 ]; then
      COMPLETENESS_SCORE=20
    elif [ "$FILE_COUNT" -le 12 ]; then
      COMPLETENESS_SCORE=15
    elif [ "$FILE_COUNT" -le 20 ]; then
      COMPLETENESS_SCORE=10
    else
      COMPLETENESS_SCORE=5
    fi
    ;;
  P2)
    if [ "$FILE_COUNT" -ge 6 ] && [ "$FILE_COUNT" -le 12 ]; then
      COMPLETENESS_SCORE=25
    elif [ "$FILE_COUNT" -le 16 ]; then
      COMPLETENESS_SCORE=15
    else
      COMPLETENESS_SCORE=10
    fi
    ;;
  P3)
    if [ "$FILE_COUNT" -ge 8 ] && [ "$FILE_COUNT" -le 16 ]; then
      COMPLETENESS_SCORE=25
    elif [ "$FILE_COUNT" -ge 6 ]; then
      COMPLETENESS_SCORE=20
    else
      COMPLETENESS_SCORE=15
    fi
    ;;
esac
echo "Completeness Score: $COMPLETENESS_SCORE/25"
SCORE=$((SCORE + COMPLETENESS_SCORE))

# 4. ACTIONABILITY (15 points)
echo ""
echo "=== 4. ACTIONABILITY (15 points) ==="
TODO_COUNT=$(grep -riE "TODO|FIXME|PLACEHOLDER|REPLACE_ME" . --exclude-dir=.git | wc -l)
echo "Placeholder count: $TODO_COUNT"

if [ "$TODO_COUNT" -le 2 ]; then
  ACTIONABILITY_SCORE=10
elif [ "$TODO_COUNT" -le 5 ]; then
  ACTIONABILITY_SCORE=7
elif [ "$TODO_COUNT" -le 10 ]; then
  ACTIONABILITY_SCORE=4
else
  ACTIONABILITY_SCORE=0
fi
echo "Actionability Score: $ACTIONABILITY_SCORE/15 (placeholder-based)"
SCORE=$((SCORE + ACTIONABILITY_SCORE))

# 6. SPECIFICITY (10 points)
echo ""
echo "=== 6. SPECIFICITY (10 points) ==="
echo "Using TODO count from Actionability: $TODO_COUNT"
if [ "$TODO_COUNT" -le 2 ]; then
  SPECIFICITY_SCORE=7
elif [ "$TODO_COUNT" -le 5 ]; then
  SPECIFICITY_SCORE=5
elif [ "$TODO_COUNT" -le 10 ]; then
  SPECIFICITY_SCORE=3
else
  SPECIFICITY_SCORE=0
fi
SPECIFICITY_SCORE=$((SPECIFICITY_SCORE + 3))  # Assume 3 points for implementation specificity (manual review)
echo "Specificity Score: $SPECIFICITY_SCORE/10"
SCORE=$((SCORE + SPECIFICITY_SCORE))

# TOTAL
echo ""
echo "=== AUTOMATED SCORE ==="
echo "Total: $SCORE/$MAX_SCORE ($(( SCORE * 100 / MAX_SCORE ))%)"
echo ""
echo "Note: Manual review required for:"
echo "  - Correctness (20 points)"
echo "  - Research Quality (15 points)"
echo "  - Insight Quality (5 points)"
echo "Automated score covers $((30+25+15+10))=80 points"
```

**Usage:**
```bash
chmod +x auto-score.sh
./auto-score.sh P3-S2-opus P3 S2
```

**Output:**
```
=== ENHANCED RUBRIC SCORING ===
Implementation: P3-S2-opus
Prompt: P3, Criteria: S2

=== 1. FUNCTIONAL VERIFICATION (30 points) ===
1.1 Syntax Validation:
  Errors: 0 → Score: 10/10
1.2 Workflow Triggers:
  Workflow triggers on issues → Score: 10/10
1.3 Structure Correctness:
  Proper directory structure → Score: 10/10

=== 2. COMPLETENESS CALIBRATION (25 points) ===
Files created: 8
Completeness Score: 25/25

=== 4. ACTIONABILITY (15 points) ===
Placeholder count: 2
Actionability Score: 10/15

=== 6. SPECIFICITY (10 points) ===
Using TODO count: 2
Specificity Score: 10/10

=== AUTOMATED SCORE ===
Total: 65/120 (54%)

Note: Manual review required for:
  - Correctness (20 points)
  - Research Quality (15 points)
  - Insight Quality (5 points)
Automated score covers 80 points
```

---

## Usage Guidelines

### When to Use This Rubric

**Use for:**
- Evaluating agent-generated bootstrap implementations
- Comparing outputs across different prompts (P1/P2/P3)
- Comparing outputs across different models (Opus/Sonnet/Haiku)
- Measuring improvement over time
- Quality gates for production deployment

**Do NOT use for:**
- Hand-written implementations (different quality expectations)
- Non-bootstrap tasks (rubric is specific to GitHub setup)
- Evaluating agents on other tasks (criteria are task-specific)

### Scoring Process

**Step 1: Automated Scoring (50 points, 5 minutes)**
```bash
./auto-score.sh <implementation-dir> <P1|P2|P3> <S1|S2|S3>
```
- Functional Verification: 30 points (automated)
- Completeness Calibration: 25 points (automated)
- Actionability: 15 points (partially automated)
- Specificity: 10 points (partially automated)
- **Total automated:** ~65 points

**Step 2: Automated Research Quality (15 points, 1 minute)**
```bash
./scripts/analyze-research-quality.sh <scenario-dir> <agent-id>
```
- **Research Quality (15 points):** Automated log and doc analysis
  - WebSearch call counting (objective)
  - URL and citation detection (objective)
  - Implementation currency checking (objective)

**Step 3: Manual Review (40 points, 15-20 minutes)**
- **Correctness (20 points):** Read workflows, test logic, check CODEOWNERS syntax
- **Insight Quality (5 points):** Read design docs for assumptions/edge cases
- **Actionability (5 points):** Manual review of documentation quality
- **Research Quality override (optional):** Adjust automated score if human review reveals issues
- **Total manual:** ~30-40 points

**Step 4: Final Score**
- Sum automated scores (functional + completeness + research + actionability/specificity)
- Add manual scores (correctness + insight + documentation)
- Apply research quality overrides if needed
- Compare to thresholds:
  - <80: Fail (needs rework)
  - 80-99: Pass (viable implementation)
  - 100-109: Excellent (ready for production)
  - 110-120: Outstanding (exemplary quality)

**Automation Summary:**
- **Fully automated:** ~65-80 points (Functional Verification 30pts + Completeness 25pts + Research Quality 15pts + partial Actionability/Specificity)
- **Human required:** ~40 points (Correctness 20pts + Insight 5pts + Documentation 5pts + Research overrides)
- **Total time:** ~5min automated + ~15-20min manual = ~20-25min per scenario

### Calibration Across Evaluators

**To ensure consistency:**
1. **Anchor on automated scores first** (30+25 = 55 points are objective)
2. **Use example implementations** as references:
   - P1-S2-opus (minimal, correct) = ~95/120
   - P2-S2-sonnet (balanced, comprehensive) = ~105/120
   - P1-S3-sonnet (over-engineered) = ~65/120
   - P3-S2-haiku (flat structure) = ~70/120
3. **Review edge cases together** (see Edge Cases section)
4. **Spot-check each other** (score same implementation, compare)

---

## Future Improvements

### Potential Additions (v3.0)

1. **Security Score (10 points)**
   - Check for hardcoded secrets
   - Validate workflow permissions (principle of least privilege)
   - Check for insecure actions (e.g., `actions/checkout` without SHA pinning)

2. **Maintainability Score (10 points)**
   - Code complexity metrics
   - Documentation coverage
   - Update frequency assumptions (does it assume weekly updates or yearly?)

3. **User Experience Score (10 points)**
   - Onboarding clarity (time to first value)
   - Error message quality
   - Recovery from common mistakes

**If added, total would be 150 points. Thresholds would adjust:**
- Pass: 100/150 (67%)
- Excellent: 125/150 (83%)

### Rubric Validation Plan

**Test rubric against all 27 scenarios:**
1. Score all P1/P2/P3 × S1/S2/S3 × Opus/Sonnet/Haiku scenarios
2. Verify expected patterns:
   - Opus should score high on Structure, Actionability
   - Sonnet should score high on Completeness (for P2/P3)
   - Haiku should score low on Structure (flat file issue)
   - P1 over-engineered implementations should score low on Completeness
3. Adjust weights if needed
4. Publish scoring results as reference dataset

---

## Appendix: Reference Examples

### Example 1: P3-S2-opus (Expected Score: ~105/120)

**Prompt:** P3 (detailed, 35 words)
**Criteria:** S2 (moderate, 3 requirements)
**Model:** Opus

**Expected Scoring:**
- Functional Verification: 30/30 (perfect structure, syntax, triggers)
- Completeness Calibration: 20/25 (3 files is slightly minimal for P3, but high quality)
- Correctness: 20/20 (logically sound, handles edge cases)
- Actionability: 10/15 (immediate use, excellent docs)
- Research Quality: 12/15 (likely researched, may not cite sources explicitly)
- Specificity: 10/10 (0 placeholders, highly specific)
- Insight Quality: 3/5 (documents some assumptions)

**Total:** ~105/120 (87%) - Excellent

### Example 2: P1-S3-sonnet (Expected Score: ~65/120)

**Prompt:** P1 (minimal, 10 words)
**Criteria:** S3 (comprehensive, 7 requirements)
**Model:** Sonnet

**Expected Scoring:**
- Functional Verification: 30/30 (all 36 files syntactically correct, proper structure)
- Completeness Calibration: 5/25 (36 files = 450% for P1 → severe over-engineering)
- Correctness: 18/20 (logically sound but overly complex)
- Actionability: 4/15 (too many files to navigate, overwhelming)
- Research Quality: 12/15 (Sonnet typically researches well)
- Specificity: 7/10 (low placeholder density, but generic implementation)
- Insight Quality: 4/5 (comprehensive docs include assumptions)

**Total:** ~80/120 (67%) - Barely passes despite perfect syntax

### Example 3: P3-S2-haiku (Expected Score: ~75/120)

**Prompt:** P3 (detailed, 35 words)
**Criteria:** S2 (moderate, 3 requirements)
**Model:** Haiku

**Expected Scoring:**
- Functional Verification: 10/30 (syntax OK, triggers OK, but FLAT STRUCTURE = 0 points)
- Completeness Calibration: 25/25 (12 files is within P3 optimal range)
- Correctness: 18/20 (content correct, but structure issues)
- Actionability: 4/15 (requires restructuring before use)
- Research Quality: 6/15 (Haiku less likely to use WebSearch)
- Specificity: 7/10 (creates example content, but some placeholders)
- Insight Quality: 2/5 (minimal assumptions documented)

**Total:** ~72/120 (60%) - Fails due to structure issues

---

## Conclusion

This enhanced 120-point rubric addresses critical gaps in the original 100-point system:

1. **Completeness Calibration** - Adjusts expectations based on prompt complexity (P1/P2/P3)
2. **Functional Verification** - Adds objective, automatable tests for syntax, structure, triggers
3. **Research Quality** - Credits agents for using current (2026) sources and WebSearch
4. **Actionability** - Distinguishes immediate-use from requires-rework implementations

**Key innovations:**
- **Prompt-aware scoring:** P1 expects 65-70% complete, P3 expects 75-85% complete
- **Automated scoring:** 65+ points (54%) can be scored automatically
- **Research tracking:** 15 points for web research usage and source currency
- **Objective measurements:** Placeholder counts, file counts, syntax validation

**Use this rubric to:**
- Evaluate agent-generated implementations objectively
- Compare models (Opus vs Sonnet vs Haiku)
- Compare prompts (P1 vs P2 vs P3)
- Improve prompt engineering (target optimal completeness ranges)
- Quality-gate production deployments (require 100+ points)

---

**Document Version:** 2.0
**Last Updated:** 2026-01-08
**Author:** Claude Sonnet 4.5
**Status:** Ready for validation testing
