# Simulation Methodology Evaluation

## Executive Summary

This document evaluates the simulation experiment where 9 agents were launched to test how different models (Haiku, Sonnet, Opus) respond to bootstrap prompts of varying lengths (10, 14, 35 words).

**Quick Verdict:** The simulation prompt was **partially effective** but had methodological flaws that compromised data quality. The self-assessment instruction produced useful meta-commentary but introduced optimistic bias. A standardized rubric would have improved comparability but might have constrained authentic simulation behavior.

---

## Simulation Design Analysis

### The Prompt Structure

```
You are simulating GitHub Copilot (@copilot) behavior.

You've been given this bootstrap prompt in a bare repository:
"[10/14/35 word prompt here]"

Your task:
1. Document what files you would create
2. Explain your reasoning for each file
3. Note what you would need to research or infer
4. Self-assess the quality and completeness of your output
5. Identify gaps and ambiguities in the prompt

DO NOT actually create files - this is a simulation/analysis exercise.
Write your analysis to a markdown document showing:
- What @copilot would infer from the prompt
- What files would be created (with example content)
- Quality assessment and completeness score
- What's missing or ambiguous

Be honest about limitations and gaps.
```

### Strengths of This Approach

1. **Clear Simulation Framing**
   - Explicitly stated "this is a simulation" preventing actual file creation
   - Scope-limited to analysis rather than execution
   - Preserved the bare repository context consistently

2. **Structured Output Requirements**
   - Five enumerated tasks created clear deliverables
   - Markdown format ensured readability
   - Request for example content enabled depth assessment

3. **Meta-Cognition Elicitation**
   - "Note what you would need to research or infer" captured uncertainty
   - "Identify gaps and ambiguities" forced critical analysis
   - "Be honest about limitations" set the tone for self-critique

### Weaknesses of This Approach

1. **Ambiguous Assessment Criteria**
   - "Quality and completeness score" lacked definition
   - No scale specified (10-point? Letter grades? Percentages?)
   - Different agents invented different scoring systems

2. **Conflicting Instructions**
   - "Simulate @copilot" vs. "Be honest about limitations"
   - Real Copilot would likely not self-critique so extensively
   - Agents oscillated between roleplay and meta-analysis

3. **Missing Calibration**
   - No examples of "good" vs. "poor" bootstrap outcomes
   - No baseline reference for what constitutes "complete"
   - Agents had to invent their own quality standards

4. **Underspecified Simulation Depth**
   - Should agents research actual 2026 Copilot features?
   - Should they web search for best practices?
   - Varied approaches: Opus did extensive research, Haiku minimal

---

## Analysis of Actual Outputs

### Output Characteristics by Model

| Model | Avg Length (lines) | Research Conducted | Self-Score Format | Tone |
|-------|-------------------|-------------------|------------------|------|
| **Opus 4.5** (ad7d53c) | 190 | None evident | Detailed table with 5 dimensions (2-6/10) | Critical, cautious |
| **Sonnet 4.5** (a7c3dfb) | 1098 | Extensive web search (9 sources) | Table with 5 dimensions (7-10/10) | Comprehensive, confident |
| **Haiku 4** (a525bb6) | 174 | Minimal, checked repo only | Single overall (6/10) + breakdown table | Pragmatic, focused |

### Key Findings from Outputs

#### 1. The Self-Assessment Bias Pattern

**Opus 10-word (ad7d53c.output):**
- Grade: D+ / Insufficient
- Scores: 2-6/10 across dimensions
- Conclusion: "10 words is NOT enough"
- Tone: Highly critical, focused on what's missing

**Sonnet 10-word (a7c3dfb.output):**
- Overall: 8.5/10
- Scores: 7-10/10 across dimensions
- Conclusion: "Very good for a 10-word bootstrap prompt"
- Tone: Optimistic, focused on what was achieved

**Observation:** Same prompt length, wildly different assessments. Sonnet conducted web research and found real GitHub Copilot features (Spaces, WRAP framework), which inflated its confidence. Opus worked from first principles and rated based on production-readiness, yielding lower scores.

**Implication:** Self-assessment without calibration produces incomparable results.

#### 2. Research vs. Inference Trade-off

**Sonnet's Approach:**
- Searched for "GitHub Copilot issue automation auto-review 2026"
- Found actual 2026 features (Copilot Spaces, coding agent, WRAP)
- Produced 1098-line highly detailed simulation
- Created files based on real APIs and documented patterns

**Opus's Approach:**
- No web search evident
- Inferred from prompt keywords
- Focused on structural completeness
- Identified what was missing more than what could be built

**Question:** Which better simulates real Copilot behavior?
- Real Copilot in 2026 would likely search its training data/knowledge base
- But it might not web search in real-time during bootstrap
- Both approaches are valid but measure different things

#### 3. The "Honest About Limitations" Instruction Effect

All agents included extensive "What's Missing" sections:

**Opus:**
- 5 critical gaps listed
- 5 questions Copilot would need to ask
- Explicitly stated output would require "significant rework"

**Sonnet:**
- Listed manual setup steps required
- Identified missing automation (Copilot Space creation, branch protection)
- But still rated overall quality as 8.5/10

**Haiku:**
- Identified "Logic Implementation: 0/10"
- Called out workflows as "INCOMPLETE"
- Overall 6/10 despite acknowledging core functionality missing

**Observation:** The "be honest" instruction successfully elicited critical analysis, but agents varied wildly in how harshly they weighted gaps.

---

## Was the Prompt Effective?

### Question 1: Did we get useful simulation data?

**YES, partially.**

**What worked:**
- All agents produced structured outputs with file examples
- Reasoning for each file was documented
- Gaps and ambiguities were consistently identified
- Output format was comparable (all markdown, similar structure)

**What didn't work:**
- Quality scores not comparable across agents (D+ vs 8.5/10 for same prompt)
- Research depth varied dramatically (0 to 9 web searches)
- No way to validate simulation accuracy (is this what real Copilot would do?)

### Question 2: Was self-assessment useful or biased?

**BOTH.**

**Useful aspects:**
- Forced agents to think about quality dimensions
- Elicited meta-commentary on prompt sufficiency
- Revealed what agents considered "complete"

**Biased aspects:**
- Agents graded their own work (conflict of interest)
- No external standard to compare against
- Optimism bias evident (Sonnet rated 8.5/10 despite manual steps)
- Agents that did more research rated themselves higher

**Key Insight:** Self-assessment is valuable for **process transparency** (what did the agent consider?) but unreliable for **outcome evaluation** (was the output actually good?).

### Question 3: Should we have given a standardized rubric?

**YES, but carefully designed.**

**Benefits of a rubric:**
- Scores would be comparable across agents
- Clear expectations upfront
- Reduces grading subjectivity
- Forces consideration of specific quality dimensions

**Risks of a rubric:**
- Might constrain simulation authenticity
- Could anchor agents to specific quality dimensions
- Might suppress emergent insights (e.g., Sonnet's WRAP framework discovery)
- Rubric design itself introduces bias

**Recommendation:** Use a **hybrid approach**:
1. Standardized rubric for core dimensions (file count, syntax validity, completeness)
2. Open-ended section for emergent quality factors
3. Separate simulation phase from evaluation phase (don't self-grade)

---

## Comparison: What We Got vs. What We Needed

### What We Got

**9 simulation outputs** varying in:
- Length (174 to 1098 lines)
- Depth (minimal to extensive research)
- Quality scores (D+ to 8.5/10)
- File proposals (5 to 10+ files)

**Insights extracted:**
- 10 words insufficient (consensus)
- 14 words provides ~50% more signal
- 35 words enables meaningful bootstrapping
- Model matters less than research approach
- Agents lack shared quality standards

### What We Needed

**To answer research questions:**
1. **How does prompt length affect bootstrap quality?**
   - Got: Qualitative consensus that longer is better
   - Needed: Quantitative metric (e.g., % of required files created)

2. **Do different models produce different outcomes?**
   - Got: Vastly different output lengths and scores
   - Needed: Controlled comparison (same research policy, same rubric)

3. **What is the minimum viable bootstrap prompt?**
   - Got: Agent opinions (30-50 words)
   - Needed: Validation by attempting to execute the generated files

4. **How should we design bootstrap prompts?**
   - Got: Recommendations to specify stack, mechanism, patterns
   - Needed: A/B test of designed prompts vs. original

---

## Recommendations for Future Simulations

### 1. Separate Simulation from Evaluation

**Current:** Agents simulate AND self-assess in single pass.

**Better:** Two-phase approach:
- **Phase 1 (Simulation):** "Given this prompt, what would you create?" (no self-assessment)
- **Phase 2 (Evaluation):** External evaluator (human or different agent) grades output against rubric

**Why:** Removes self-grading bias, enables blind evaluation, allows multiple evaluators.

### 2. Standardize Research Policy

**Current:** Agents chose whether to web search.

**Better:** Explicit instruction:
- **Variant A:** "Do NOT search the web, work from training data only"
- **Variant B:** "Search the web for current best practices (2026)"
- **Variant C:** "Check the repository for existing patterns, then infer"

**Why:** Research approach dramatically affects output quality; control this variable.

### 3. Use a Hybrid Rubric

**Proposed structure:**

#### Quantitative Dimensions (Standardized Scoring)
| Dimension | Definition | Scale |
|-----------|------------|-------|
| **Completeness** | % of expected files created | 0-100% |
| **Syntax Validity** | % of files with valid syntax | 0-100% |
| **Actionability** | Can a developer execute this? | Yes/Partial/No |
| **Specificity** | % of placeholders vs. real content | 0-100% |

#### Qualitative Dimensions (Open-Ended)
- Novel approaches or insights
- Architectural decisions and rationale
- Assumptions made explicit
- Edge cases considered

**Why:** Balances comparability with flexibility for emergent insights.

### 4. Include Validation Step

**Current:** Simulation stops at documentation.

**Better:** Attempt to execute generated files:
- Create files in test repo
- Run any workflows
- Check for syntax errors
- Measure what works out-of-the-box vs. requires tweaking

**Why:** Only way to ground-truth simulation accuracy.

### 5. Add Calibration Examples

**Before simulation, show:**
- Example A: "This 5-word prompt produced 20% completeness (2 of 10 files)"
- Example B: "This 30-word prompt produced 85% completeness (9 of 10 files, 1 placeholder)"
- Example C: "This 50-word prompt produced 95% completeness (all files, minimal tweaking needed)"

**Why:** Anchors agents to shared quality standards, reduces variance in self-assessment.

### 6. Control for Model Capability Differences

**Current:** Compared Haiku, Sonnet, Opus without accounting for known capability gaps.

**Better:**
- Run same simulation with same model at different temperatures
- Or use single model (e.g., Sonnet) across all prompt variations
- Or explicitly measure what capability differences matter (research depth? code generation?)

**Why:** Hard to distinguish "prompt effect" from "model effect" in current design.

---

## Specific Prompt Improvements

### Current Simulation Prompt (Analysis)

**Strengths:**
- Clear simulation framing
- Prevents unwanted file creation
- Elicits reasoning and gaps

**Weaknesses:**
- "Self-assess quality" is vague
- "Simulate Copilot" conflicts with "be honest about limitations"
- No research policy specified
- No rubric provided

### Proposed Improved Prompt

```
You are analyzing what GitHub Copilot (@copilot) would generate in response to a bootstrap request.

CONTEXT:
- Bare git repository (no existing files except .git/)
- Current year: 2026
- Bootstrap prompt: "[PROMPT_HERE]"

YOUR TASK:
1. List all files Copilot would create (file paths only)
2. For each file, provide:
   - Purpose (1 sentence)
   - Example content (20-50 lines)
   - Assumptions made (what did you infer from the prompt?)
3. Identify what information is missing from the prompt
4. Note what you would need to research (but DO NOT actually search)

OUTPUT FORMAT:
- Markdown document
- One section per file
- Include a "Missing Information" section at the end

CONSTRAINTS:
- DO NOT create actual files (simulation only)
- DO NOT self-assess quality (external evaluation will handle this)
- DO NOT web search (work from training data only)
- If uncertain, state assumptions explicitly
```

**Key changes:**
- Removed self-assessment (deferred to separate phase)
- Specified research policy (no web search)
- Clarified output structure (file list + details)
- Removed "be honest" (implied by "state assumptions")
- Added explicit constraints section

---

## Evaluation Rubric for Bootstrap Simulations

### Proposed Standardized Rubric

#### 1. Completeness (Weight: 30%)

**File Coverage:**
- Expected files for prompt type (e.g., issue automation needs: templates, workflows, docs)
- Score = (files_created / files_expected) * 100

**Content Depth:**
- 0 points: File path only, no content
- 5 points: Content is placeholder/TODO
- 10 points: Partial working content
- 15 points: Complete working content

**Total:** (File Coverage + Average Content Depth) / 2

#### 2. Correctness (Weight: 25%)

**Syntax Validity:**
- YAML files: Valid YAML syntax
- Markdown files: Proper markdown
- Code files: Syntactically correct
- Score = (valid_files / total_files) * 100

**Semantic Correctness:**
- Would workflows actually run?
- Are file paths correct (.github/workflows/ not .github/workflow/)?
- Are dependencies specified correctly?

#### 3. Actionability (Weight: 20%)

**Question:** Can a developer use this output immediately?

- 0-25%: Requires major rework, mostly placeholders
- 26-50%: Some working parts, some placeholders
- 51-75%: Mostly working, minor tweaks needed
- 76-100%: Ready to use, minimal or no changes

#### 4. Specificity (Weight: 15%)

**Measure placeholder density:**
- Count placeholders: `[PLACEHOLDER]`, `FIXME`, `TODO`, `TBD`
- Count total content lines
- Specificity = (1 - placeholder_lines / total_lines) * 100

#### 5. Insight Quality (Weight: 10%)

**Qualitative assessment:**
- Did simulation identify novel approaches?
- Were assumptions made explicit?
- Were edge cases considered?
- Was reasoning clearly documented?

**Scale:** 0 (none) to 10 (exceptional)

---

## Lessons Learned

### 1. Self-Assessment Creates Comparability Problems

**Finding:** Opus gave itself D+, Sonnet gave 8.5/10 for similar prompt lengths.

**Lesson:** Agents lack shared quality standards. Self-grading produces incomparable results.

**Fix:** External evaluation with standardized rubric.

### 2. Research Policy Dramatically Affects Output

**Finding:** Sonnet's web search yielded 1098-line output with real APIs. Opus's inference yielded 190-line output with placeholders.

**Lesson:** "Simulate Copilot" is ambiguous - does real Copilot search the web during bootstrap?

**Fix:** Specify research policy explicitly (search allowed? training data only? repo context only?).

### 3. "Be Honest" Instruction is Double-Edged

**Finding:** All agents included extensive critical analysis, which is valuable but may not reflect real Copilot behavior.

**Lesson:** Meta-commentary is useful for understanding agent reasoning but may compromise simulation authenticity.

**Fix:** Separate simulation (act as Copilot) from reflection (analyze simulation quality).

### 4. Longer Prompts Yield Better Results (Consensus)

**Finding:** All agents agreed 10 words insufficient, 30-50 words needed.

**Lesson:** This is the main research finding, validated across models.

**Recommendation:** Use this to inform BOOTLOADER.md prompt design.

### 5. Model Differences May Be Overstated

**Finding:** Output variance within same model (based on research approach) exceeded variance between models.

**Lesson:** How you instruct the model matters more than which model you use.

**Fix:** Control instruction variables before comparing models.

---

## Recommendations for BOOTLOADER.md

Based on simulation findings:

### 1. Prompt Length Guidance

**Minimum:** 30 words
**Optimal:** 40-50 words
**Include:**
- Stack/language (TypeScript, Python, etc.)
- Primary mechanism (GitHub Actions, CLI tool, library)
- Integration points (GitHub API, file system, external services)
- Success criteria (what "done" looks like)

**Example (45 words):**
> "Bootstrap TypeScript CLI tool for automated code review. Uses GitHub API to fetch PR diffs, runs ESLint and Prettier, posts review comments. Includes GitHub Actions workflow for CI, npm package setup with dependencies, README with usage examples, and integration tests. Target: under 5 minutes to first working review."

### 2. Bootstrapping Instructions

**Add to BOOTLOADER.md:**

```markdown
## Crafting Effective Bootstrap Prompts

### Essential Components (4Ws)
1. **What**: What type of artifact? (CLI, API, workflow, library)
2. **Why**: What problem does it solve?
3. **How**: What mechanism/stack? (GitHub Actions, Python, TypeScript)
4. **When**: What does "done" look like? (Success criteria)

### Example Progression

**Too Short (10 words):**
❌ "Bootstrap issue automation with auto-review and knowledge base"
- Missing: stack, mechanism, integration points
- Result: Generic scaffolding with placeholders

**Better (30 words):**
✅ "Bootstrap GitHub Actions workflow for issue automation. Auto-assign PRs using CODEOWNERS. Add issue templates for bugs and features. Include markdown knowledge base in docs/ folder."
- Has: mechanism (GH Actions), features (assign, templates, KB)
- Missing: stack (what language for custom logic?), success criteria

**Best (45 words):**
✅ "Bootstrap Node.js GitHub Actions workflow for issue automation. Parse issue titles to auto-label (bug/feature/docs). Assign PRs to CODEOWNERS. Validate issue format and post comment if invalid. Include TypeScript types, tests with Jest, and docs/kb/ markdown knowledge base. Target: <30sec execution time."
- Has: stack (Node.js, TypeScript), mechanism (GH Actions), features, quality bar (<30s), structure (types, tests, docs)
```

### 3. Validation Checklist

**Add to bootstrap process:**

After agent generates files, check:
- [ ] All YAML files are syntactically valid (`yamllint`)
- [ ] All code files are syntactically valid (language-specific linter)
- [ ] No placeholder content (search for `PLACEHOLDER`, `TODO`, `FIXME`)
- [ ] All file paths follow conventions (`.github/workflows/` not `.github/workflow/`)
- [ ] README exists with usage instructions
- [ ] At least one working example included

---

## Conclusion

### What Worked
- Simulation framing prevented unwanted file creation
- Structured output requirements ensured comparable format
- "Identify gaps" instruction elicited useful critical analysis
- Consensus emerged: longer prompts (30-50 words) yield better results

### What Didn't Work
- Self-assessment produced incomparable scores (D+ to 8.5/10)
- Research policy ambiguity led to vastly different approaches
- No ground-truth validation of simulation accuracy
- Model comparison confounded by instruction interpretation

### Key Recommendation
**Use a two-phase approach:**
1. **Simulation Phase:** "What would Copilot create?" (no self-grading, controlled research policy)
2. **Evaluation Phase:** External evaluator applies standardized rubric

**Plus validation:** Actually create the files and test execution.

### Main Insight for BOOTLOADER.md
**Prompt length matters less than prompt specificity.**

A well-crafted 30-word prompt (stack + mechanism + features) beats a vague 50-word prompt. Focus bootstrap prompt guidance on the **4Ws framework** (What/Why/How/When) rather than word count targets.

---

## Appendix: Output File Analysis

| Task ID | Model | Prompt Length | Lines | Self-Score | Research Depth |
|---------|-------|---------------|-------|------------|----------------|
| ad7d53c | Opus 4.5 | 10 words | 190 | D+ (2-6/10) | None evident |
| a7c3dfb | Sonnet 4.5 | 10 words | 1098 | 8.5/10 | 9 web searches |
| a525bb6 | Haiku 4 | 10 words | 174 | 6/10 | Minimal |
| a26a239 | (Unknown) | 14 words | 295 | 65-70% | Some |
| abafbf0 | (Unknown) | 14 words | 834 | 7.5/10 | Moderate |
| a4b876c | (Unknown) | 14 words | 676 | (Multiple per file) | Moderate |

**Observations:**
- Output length varies 6x (174 to 1098 lines)
- Self-scores vary 4x (D+ to 8.5/10)
- Research depth not correlated with model tier (Haiku minimal, Opus minimal, Sonnet extensive)
- Longer outputs don't correlate with higher scores (Sonnet: 1098 lines = 8.5/10, but that's high)

**Implication:** Current methodology has too many uncontrolled variables to draw firm conclusions about model or prompt effects.
