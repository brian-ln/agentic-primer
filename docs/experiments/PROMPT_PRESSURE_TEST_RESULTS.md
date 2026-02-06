# Bootstrap Prompt Pressure Test Results

Simulation of @copilot behavior with 3 prompt lengths across 3 models.

## Executive Summary

| Prompt | Opus Rating | Key Finding |
|--------|-------------|-------------|
| **10 words** | D+ / Insufficient | Too much ambiguity, mostly placeholders |
| **14 words** | 65-70% complete | Meaningful improvement, key elements clear |
| **35 words** | 70% complete (7/10) | Marginal improvement over 14 words |

## Detailed Results

### 10-Word Prompt (Opus)
**Prompt:** "Bootstrap @copilot issue automation with auto-review and knowledge base."

**Rating:** D+ / Insufficient

**What was created:**
- `.github/copilot-instructions.md` - Quality: LOW (too generic)
- `.github/workflows/issue-triage.yml` - Quality: MEDIUM (missing labels)
- `.github/workflows/auto-review.yml` - Quality: LOW-MEDIUM (needs stack info)
- Issue templates - Quality: MEDIUM (standard patterns)
- `docs/knowledge-base/` - Quality: LOW (structure only, no content)

**Problems:**
- Almost everything was a placeholder
- High uncertainty about what "knowledge base" means
- Missing critical context (language, stack, team size)
- Workflows would run but do little useful

**Specificity:** 2/10
**Actionability:** 4/10
**Completeness:** 3/10

---

### 14-Word Prompt (Opus)
**Prompt:** "Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base."

**Rating:** 65-70% complete

**What was created:**
1. `.github/copilot-instructions.md` - Issue-driven dev guidelines
2. `.github/CODEOWNERS` - Auto-assign mechanism
3. `.github/workflows/auto-assign.yml` - PR assignment workflow
4. `.github/ISSUE_TEMPLATE/feature.yml` - Feature request template
5. `.github/ISSUE_TEMPLATE/bug.yml` - Bug report template
6. `.github/ISSUE_TEMPLATE/config.yml` - Template chooser
7. `docs/kb/README.md` - Knowledge base index
8-11. Knowledge base subdirectories (.gitkeep files)
12. `.github/labels.yml` - Issue workflow labels

**Improvements over 10 words:**
- Explicit mention of "auto-assign PRs" triggered CODEOWNERS creation
- "Knowledge base" now clearly interpreted as `/docs/kb/` structure
- Issue templates more specific to issue-driven workflow
- Labels defined for workflow states

**Still ambiguous:**
- "Owner" = PR author? Code owner? Repo owner?
- Knowledge base categories (assumed architecture/patterns/troubleshooting)
- Tech stack (unknown)
- Integration with external tools

**Verdict:** 14 words provides meaningfully more signal than 10. Explicit requirements reduce ambiguity.

---

### 35-Word Prompt (Opus)
**Prompt:**
```
Create an issue-driven development system:
- Issue template for @copilot tasks
- CODEOWNERS to auto-assign PRs for review
- Knowledge base (docs/knowledge/) for patterns and decisions
- README with usage instructions
```

**Rating:** 70% complete (7/10)

**What was created:**
1. `.github/ISSUE_TEMPLATE/copilot-task.yml` - Structured template
2. `.github/CODEOWNERS` - With path-based ownership
3. `docs/knowledge/README.md` - Index with structure
4. `docs/knowledge/patterns/` - Directory for patterns
5. `docs/knowledge/decisions/` - Directory for ADRs
6. `README.md` - Usage instructions

**Improvements over 14 words:**
- Explicit file paths given (`docs/knowledge/`)
- Clear categories specified (patterns, decisions)
- CODEOWNERS purpose clarified ("for review")
- README requirement ensures documentation

**Still missing:**
- PR template
- Contributing guide
- Specific patterns/decisions content
- CI/CD integration
- Branch protection rules

**Verdict:** Marginal improvement over 14 words. Specificity helps, but still requires inference for production-ready system.

---

## Optimal Prompt Length Analysis

### Diminishing Returns Curve

```
Completeness
100% ┤
 90% ┤                              ╭─────  (estimated plateau)
 80% ┤                         ╭────╯
 70% ┤                    ╭────╯  ← 35 words (70%)
 60% ┤              ╭─────╯
 50% ┤         ╭────╯  ← 14 words (65-70%)
 40% ┤    ╭────╯
 30% ┤╭───╯  ← 10 words (30%)
 20% ┤╯
 10% ┤
  0% └─────────────────────────────────────
     0   10   20   30   40   50   60  Words
```

**Key Insight:** Biggest jump is from 10→14 words. After ~35 words, adding more detail yields diminishing returns.

---

## Recommendations

### Minimum Viable Prompt: ~14-20 words
**Why:**
- Cuts through major ambiguities
- Specifies key mechanisms (CODEOWNERS, knowledge base)
- Enables ~65-70% completeness
- Still concise enough to use quickly

**Example (16 words):**
```
Setup issue-driven development with @copilot, CODEOWNERS for auto-review,
and docs/knowledge/ for patterns and decisions.
```

### Optimal Prompt: ~25-35 words
**Why:**
- Reaches 70%+ completeness
- Provides file paths and structure
- Specifies categories/purposes
- Actionable without being verbose

**Example (28 words):**
```
Create issue-driven development:
- Issue template for @copilot tasks with acceptance criteria
- CODEOWNERS for auto-review assignment
- Knowledge base in docs/knowledge/ (patterns, decisions, insights)
- README with workflow instructions
```

### Maximum Useful Length: ~50 words
**Why:**
- Beyond 50 words, you're essentially writing the spec yourself
- Defeats the purpose of AI inference
- Better to use structured format (YAML) at that point

---

## Comparison Across Models (In Progress)

Once all 9 simulations complete, we'll see:

| Model | 10-word | 14-word | 35-word |
|-------|---------|---------|---------|
| **Haiku** | TBD | TBD | TBD |
| **Sonnet** | TBD | TBD | TBD |
| **Opus** | 30% (D+) | 65-70% | 70% |

Hypothesis: Larger models (Opus) may extract more from minimal prompts than smaller models (Haiku).

---

## Implications for BOOTSTRAP_SEED_V2.md

**Current v2.0 is ~280 words** - way beyond optimal!

**Recommendation:** Create v2.1 that's ~25-35 words:

```markdown
# Bootstrap Seed v2.1 (Minimal)

Create issue-driven development system:
- Issue template for @copilot tasks (title, description, acceptance criteria)
- CODEOWNERS file to auto-assign PRs to repository owner
- Knowledge base in docs/knowledge/ with subdirs for patterns, decisions, insights
- README explaining: create issue → mention @copilot → PR auto-assigned → review → merge
```

**Word count:** 47 words (vs 280 in v2.0)

**Expected completeness:** 70-75%

**Trade-off:** Lose some detail but gain brevity and force AI to infer best practices.

---

## Next Steps

1. ✅ Complete all 9 simulations (6 remaining)
2. Compare model performance (Haiku vs Sonnet vs Opus)
3. Test optimal prompt (25-35 words) with real @copilot
4. Create BOOTSTRAP_SEED_V2.1.md with findings
5. Document which details are worth specifying vs letting AI infer

---

**Test Date:** 2026-01-05
**Status:** In progress (3/9 simulations complete)
