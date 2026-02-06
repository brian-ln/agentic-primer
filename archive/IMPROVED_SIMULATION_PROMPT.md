# Improved Simulation Prompt Template

## Overview

This document provides an improved simulation prompt based on lessons learned from the 9-agent simulation experiment. Use this template for future bootstrap prompt testing.

---

## Two-Phase Methodology

### Phase 1: Simulation (Agent Task)

**Goal:** Generate files without self-assessment bias

**Prompt Template:**

```
You are analyzing what GitHub Copilot (@copilot) would generate in response to a bootstrap request.

CONTEXT:
- Bare git repository (no existing files except .git/)
- Current year: 2026
- Bootstrap prompt: "[PROMPT_HERE]"

RESEARCH POLICY: [Choose ONE]
- Option A: "Do NOT search the web. Work from training data only."
- Option B: "You MAY search the web for current best practices (2026)."
- Option C: "Check the repository context first, then infer from training data."

YOUR TASK:
1. List all files Copilot would create (file paths only)
2. For each file, provide:
   - Purpose (1 sentence)
   - Example content (20-100 lines, complete and functional)
   - Assumptions made (what did you infer from the prompt?)
   - Confidence level (High/Medium/Low for this file)

3. Identify what information is missing from the prompt:
   - Stack/language not specified?
   - Mechanism unclear?
   - Success criteria undefined?

4. Note what you would need to research:
   - Current APIs or best practices
   - Framework-specific patterns
   - Integration requirements

OUTPUT FORMAT:
- Markdown document
- One section per file with example content
- "Assumptions" subsection for each file
- "Missing Information" section at end
- "Research Needs" section at end

CONSTRAINTS:
- DO NOT create actual files (this is a simulation/analysis exercise)
- DO NOT self-assess quality (external evaluation will handle this)
- DO NOT grade your output or assign scores
- If uncertain, state assumptions explicitly with confidence level
- Provide complete, functional code (not placeholders like [INSERT_CODE_HERE])

GOAL:
Simulate authentic Copilot behavior. What would it actually generate given this prompt?
```

### Phase 2: Evaluation (Separate Evaluator)

**Goal:** Standardized assessment with rubric

**Evaluator Prompt:**

```
You are evaluating a simulated bootstrap output against a standardized rubric.

SIMULATION OUTPUT: [Paste agent output here]

BOOTSTRAP PROMPT: "[Original prompt]"

YOUR TASK: Score the simulation using this rubric.

## RUBRIC

### 1. Completeness (30 points)

**File Coverage (15 points):**
Expected files for this prompt type: [List expected files]

Score = (files_created / files_expected) * 15
- 15 points: All expected files present
- 10 points: 70-99% of expected files
- 5 points: 40-69% of expected files
- 0 points: <40% of expected files

**Content Depth (15 points):**
For each file, assess content quality:
- 0 points: File path only, no content
- 1 point: Content is placeholder/TODO
- 2 points: Partial working content
- 3 points: Complete working content

Average content score across all files, scale to 15 points.

**Completeness Score:** [File Coverage] + [Content Depth] = __/30

---

### 2. Correctness (25 points)

**Syntax Validity (15 points):**
Check each file:
- YAML files: Valid YAML syntax (yamllint)
- Markdown: Proper formatting
- Code files: Syntactically correct (language linter)

Score = (valid_files / total_files) * 15

**Semantic Correctness (10 points):**
- Would workflows actually run? (5 points)
- Are file paths correct? (3 points)
- Are dependencies specified correctly? (2 points)

**Correctness Score:** __/25

---

### 3. Actionability (20 points)

**Question:** Can a developer use this output immediately without modification?

Assessment:
- 20 points: Ready to use, runs successfully out-of-the-box
- 15 points: Minor tweaks needed (env vars, API keys)
- 10 points: Some working parts, some placeholders
- 5 points: Requires significant rework
- 0 points: Mostly placeholders, not functional

**Actionability Score:** __/20

---

### 4. Specificity (15 points)

**Placeholder Density:**
Search for: `PLACEHOLDER`, `FIXME`, `TODO`, `TBD`, `[INSERT`, `CHANGEME`

Score = (1 - placeholder_count / total_lines) * 15

**Contextual Appropriateness:**
- Are choices appropriate for the prompt? (5 points)
- Are patterns idiomatic for the stack? (5 points)

**Specificity Score:** __/15

---

### 5. Insight Quality (10 points)

**Qualitative Assessment:**
- Did simulation identify novel approaches? (3 points)
- Were assumptions made explicit and reasonable? (3 points)
- Were edge cases or limitations noted? (2 points)
- Was reasoning clearly documented? (2 points)

**Insight Score:** __/10

---

## TOTAL SCORE

| Dimension | Score | Weight | Weighted Score |
|-----------|-------|--------|----------------|
| Completeness | __/30 | 30% | __ |
| Correctness | __/25 | 25% | __ |
| Actionability | __/20 | 20% | __ |
| Specificity | __/15 | 15% | __ |
| Insight Quality | __/10 | 10% | __ |
| **TOTAL** | **__/100** | | **__/100** |

---

## GRADE INTERPRETATION

- 90-100: Excellent (production-ready with minimal tweaking)
- 75-89: Good (solid foundation, some refinement needed)
- 60-74: Acceptable (useful scaffolding, requires customization)
- 40-59: Needs Work (significant gaps, mostly generic)
- 0-39: Insufficient (placeholders, not actionable)

---

## COMMENTARY

**Strengths:**
[List 2-3 specific strengths]

**Weaknesses:**
[List 2-3 specific weaknesses]

**Recommendations:**
[What would improve this to the next grade tier?]

**Notable Insights:**
[Any particularly clever or novel approaches?]
```

---

## Calibration Examples

Show these to agents BEFORE simulation to establish shared quality standards.

### Example A: Poor (Score: 35/100)

**Prompt:** "Bootstrap issue automation"

**Output:**
- `.github/workflows/issues.yml` (15 lines, mostly TODOs)
- `README.md` (5 lines, generic)

**Issues:**
- 2 files (expected 6+) → Low completeness
- Heavy placeholder usage → Low specificity
- Workflows wouldn't run → Low actionability

**Grade:** F (Insufficient)

---

### Example B: Good (Score: 75/100)

**Prompt:** "Bootstrap TypeScript issue automation with auto-labeling"

**Output:**
- `.github/workflows/auto-label.yml` (40 lines, working)
- `.github/labeler.yml` (20 lines, config)
- `src/label-logic.ts` (80 lines, partial placeholders for label rules)
- `README.md` (50 lines, clear instructions)
- `package.json` (30 lines, dependencies listed)

**Issues:**
- 5 files (expected 6) → Good completeness
- Workflow functional, some placeholder label rules → Medium specificity
- Mostly actionable, needs label customization → Good actionability

**Grade:** B (Good)

---

### Example C: Excellent (Score: 92/100)

**Prompt:** "Bootstrap Node.js GitHub Actions workflow: parse issue titles → auto-label (bug/feature/docs), assign to CODEOWNERS, validate format, post comments. Include Jest tests, ESLint config. Target: <30s execution."

**Output:**
- `.github/workflows/issue-automation.yml` (75 lines, complete)
- `src/parser.ts` (120 lines, regex logic for title parsing)
- `src/labeler.ts` (90 lines, GitHub API calls)
- `src/validator.ts` (80 lines, format checks)
- `__tests__/parser.test.ts` (150 lines, comprehensive)
- `__tests__/labeler.test.ts` (130 lines, mocked API)
- `package.json` (45 lines, all deps + scripts)
- `.eslintrc.json` (30 lines, configured)
- `tsconfig.json` (25 lines, strict mode)
- `README.md` (120 lines, examples + troubleshooting)
- `.github/CODEOWNERS` (15 lines, path-based)

**Strengths:**
- 11 files (expected 8-10) → Excellent completeness
- All files functional, no placeholders → Excellent specificity
- Includes tests, linting, types → Excellent correctness
- Ready to run immediately → Excellent actionability

**Grade:** A (Excellent)

---

## Usage Instructions

### For Running Simulations

1. **Choose research policy** based on what you're testing:
   - Testing model knowledge? → "No web search"
   - Testing current best practices? → "Web search allowed"
   - Testing inference from existing code? → "Check repo first"

2. **Run Phase 1 (Simulation):**
   - Use simulation prompt with chosen research policy
   - Save output to file (don't let agent self-assess)

3. **Run Phase 2 (Evaluation):**
   - Use different agent or human evaluator
   - Apply standardized rubric
   - Document scores and commentary

4. **Optional: Validation Step:**
   - Actually create the files in test repository
   - Run linters, tests, workflows
   - Measure what works out-of-the-box
   - Compare actual functionality to rubric scores

### For Comparing Prompts

**Controlled variables:**
- Same research policy across all runs
- Same evaluator (or same rubric)
- Same model (to isolate prompt effect)

**Vary:**
- Prompt length (10, 20, 30, 40, 50 words)
- Prompt specificity (vague → detailed)
- Prompt structure (imperative vs. declarative)

**Measure:**
- Completeness score
- Actionability score
- Actual validation results (if executing files)

---

## Key Improvements Over Original Prompt

| Aspect | Original | Improved |
|--------|----------|----------|
| **Self-assessment** | Agent grades own work | Separate evaluator |
| **Research policy** | Ambiguous | Explicit choice |
| **Quality standard** | Invented by agent | Standardized rubric |
| **Scoring scale** | Inconsistent (D+ to 8.5/10) | 100-point scale |
| **Validation** | None | Optional execution step |
| **Calibration** | None | 3 examples provided |
| **Comparability** | Low (subjective) | High (rubric-based) |

---

## Expected Outcomes

**Using this improved methodology, you should get:**

1. **Comparable scores** across different runs (same rubric)
2. **Reduced bias** (external evaluator, not self-assessment)
3. **Clearer signal** (controlled research policy)
4. **Actionable insights** (rubric identifies specific gaps)
5. **Validation data** (if executing generated files)

**This enables:**
- Quantitative comparison of prompt variations
- Model capability assessment (apples-to-apples)
- Bootstrap prompt optimization (test hypotheses)
- Quality prediction (rubric correlates with actual usability)

---

## Customization Guide

### Adjusting the Rubric

**For different artifact types:**

**CLI tool bootstrap:**
- Add: Argument parsing completeness
- Add: Help text quality
- Add: Installation instructions

**API bootstrap:**
- Add: OpenAPI spec completeness
- Add: Authentication handling
- Add: Error response standards

**Workflow bootstrap:**
- Add: Trigger configuration correctness
- Add: Secret handling security
- Add: Concurrency management

### Adjusting Expected Files

**For 10-word prompt:**
- Expected files: 3-5 (minimal viable)

**For 30-word prompt:**
- Expected files: 6-10 (comprehensive)

**For 50-word prompt:**
- Expected files: 10-15 (production-ready)

### Adjusting Weights

**For research-focused evaluation:**
- Increase: Insight Quality (20%)
- Decrease: Actionability (10%)

**For production-readiness evaluation:**
- Increase: Actionability (30%)
- Increase: Correctness (30%)
- Decrease: Insight Quality (5%)

---

## Conclusion

This improved prompt addresses the key flaws identified in the original simulation:

1. ✅ Separates simulation from evaluation (removes self-grading bias)
2. ✅ Standardizes research policy (controls a key variable)
3. ✅ Provides calibration examples (shared quality standard)
4. ✅ Uses quantitative rubric (enables comparison)
5. ✅ Includes validation option (ground-truth testing)

**Result:** More reliable, comparable, and actionable simulation data for optimizing bootstrap prompt design.
