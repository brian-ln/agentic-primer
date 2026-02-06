# Simulation Evaluation: Document Index

**Evaluation Date:** January 5, 2026
**Evaluator:** Claude Sonnet 4.5
**Simulation Data:** 9 agent runs across 3 models (Haiku, Sonnet, Opus) and 3 prompt lengths (10, 14, 35 words)

---

## Quick Navigation

### 📊 Start Here
**[SIMULATION_FINDINGS_SUMMARY.md](SIMULATION_FINDINGS_SUMMARY.md)** (11K)
Executive summary with key takeaways, numbers, and recommendations by audience.

### 🔬 Deep Dive
**[SIMULATION_METHODOLOGY_EVALUATION.md](SIMULATION_METHODOLOGY_EVALUATION.md)** (21K)
Comprehensive analysis of simulation design, what worked, what didn't, and why.

### 🛠️ Actionable Tools
**[IMPROVED_SIMULATION_PROMPT.md](IMPROVED_SIMULATION_PROMPT.md)** (11K)
Two-phase methodology, standardized rubric, and calibration examples for future simulations.

### 📁 Reference
**[SIMULATION_COMPLETE_WRITEUP.md](SIMULATION_COMPLETE_WRITEUP.md)** (16K)
Original detailed writeup of simulation results.

**[SIMULATION_RESULTS.md](SIMULATION_RESULTS.md)** (11K)
Tabular summary of all 9 simulation runs.

**[SIMULATION_PROMPTS.md](SIMULATION_PROMPTS.md)** (6.1K)
The actual prompts used in the simulation.

---

## Key Findings at a Glance

### ✅ Validated
- **10 words insufficient** → use 30-50 words (consensus)
- **Specificity > length** → stack + mechanism + features matters most
- **Agents can self-critique** → "be honest" instruction worked

### ❌ Flawed
- **Self-assessment unreliable** → D+ to 8.5/10 for same prompt
- **Research policy ambiguous** → 0 to 9 web searches, uncontrolled
- **No ground-truth validation** → can't verify simulation accuracy

### 📈 Recommendations
1. **Two-phase approach:** Simulate → evaluate separately (remove self-grading bias)
2. **Standardize research policy:** Explicit instruction (control variable)
3. **Use rubric:** 100-point scale across 5 dimensions (enable comparison)
4. **Add validation:** Execute generated files (ground-truth testing)

---

## Document Summaries

### SIMULATION_FINDINGS_SUMMARY.md
**Purpose:** Quick reference for decision-makers
**Audience:** Anyone who needs the bottom line
**Length:** 11K (~15 min read)

**Contains:**
- ✅ What validated / ❌ What revealed as flawed
- The numbers (variance, consensus, recommendations)
- 3 evaluation questions answered (effective? useful? biased?)
- Recommendations by audience (researchers, BOOTLOADER.md, users)
- Key insights for bootstrap prompt design

**Read this if:** You want actionable takeaways without methodology details.

---

### SIMULATION_METHODOLOGY_EVALUATION.md
**Purpose:** Rigorous analysis of simulation design
**Audience:** Researchers, methodologists, future simulation designers
**Length:** 21K (~30 min read)

**Contains:**
- Simulation prompt structure analysis (strengths/weaknesses)
- Output characteristics by model (tables, comparisons)
- Deep dive into self-assessment bias
- Research vs. inference trade-offs
- Lessons learned (6 major insights)
- Proposed improved methodology
- Standardized rubric design

**Read this if:** You're designing future simulations or need to understand methodology flaws.

---

### IMPROVED_SIMULATION_PROMPT.md
**Purpose:** Template for future simulations
**Audience:** Anyone running bootstrap prompt experiments
**Length:** 11K (reference document)

**Contains:**
- Two-phase methodology (simulate → evaluate)
- Simulation prompt template (with research policy options)
- Evaluation rubric (100-point scale, 5 dimensions)
- Calibration examples (poor, good, excellent)
- Usage instructions (how to run, how to compare)
- Customization guide (adjust for different artifact types)

**Read this if:** You're about to run a simulation and want the improved process.

---

## How to Use These Documents

### Scenario 1: Designing BOOTLOADER.md Prompts
**Path:**
1. Read **SIMULATION_FINDINGS_SUMMARY.md** → "Key Insights for BOOTLOADER.md"
2. Use the 4Ws framework (What/Why/How/When)
3. Follow prompt progression examples (10 → 30 → 45 words)
4. Apply validation checklist

**Key takeaway:** Use 40-50 words with stack + mechanism + features.

---

### Scenario 2: Running Future Simulations
**Path:**
1. Read **IMPROVED_SIMULATION_PROMPT.md** → "Usage Instructions"
2. Choose research policy (no search / search allowed / repo-first)
3. Run Phase 1 (simulation) with template prompt
4. Run Phase 2 (evaluation) with rubric
5. Optional: Execute files for validation

**Key takeaway:** Two-phase approach with standardized rubric.

---

### Scenario 3: Understanding What Went Wrong
**Path:**
1. Read **SIMULATION_METHODOLOGY_EVALUATION.md** → "Weaknesses of This Approach"
2. Review "Analysis of Actual Outputs" section
3. Study "Lessons Learned" (6 insights)

**Key takeaway:** Self-assessment bias + uncontrolled research policy = incomparable results.

---

### Scenario 4: Comparing Models
**Path:**
1. Read **SIMULATION_RESULTS.md** → output tables
2. Read **SIMULATION_METHODOLOGY_EVALUATION.md** → "Output Characteristics by Model"
3. Note: Current data confounded by instruction interpretation

**Key takeaway:** Can't distinguish model effect from research policy effect. Need controlled comparison.

---

## Key Metrics from Evaluation

| Metric | Value | Interpretation |
|--------|-------|----------------|
| **Output length variance** | 6x (174-1098 lines) | Research approach matters more than model |
| **Self-score variance** | 4x (D+ to 8.5/10) | No shared quality standard |
| **Web searches** | 0 to 9 | Research policy uncontrolled |
| **Recommended prompt length** | 30-50 words | Consensus across all agents |
| **Completeness at 10 words** | ~40% | Opus estimate |
| **Completeness at 14 words** | ~65-70% | Multiple agent consensus |
| **Completeness at 45 words** | ~85-90% | Estimated from specificity |

---

## Research Questions Answered

### Q1: How does prompt length affect bootstrap quality?
**Answer:** 10 words → ~40% complete (generic scaffolding), 30 words → ~65% complete (working structure), 45 words → ~85% complete (production-ready). But specificity matters more than length.

**Source:** SIMULATION_FINDINGS_SUMMARY.md, SIMULATION_METHODOLOGY_EVALUATION.md

---

### Q2: Do different models produce different outcomes?
**Answer:** Within-model variance (based on research approach) exceeded between-model variance. Sonnet with web search >> Opus without search. Can't distinguish model capability from instruction interpretation.

**Source:** SIMULATION_METHODOLOGY_EVALUATION.md → "Key Findings"

---

### Q3: Was the simulation prompt effective?
**Answer:** Partially. Got useful directional findings (30-50 word consensus) but methodological flaws (self-assessment bias, uncontrolled research) compromised quantitative comparisons.

**Source:** SIMULATION_FINDINGS_SUMMARY.md → "Was the Simulation Prompt Effective?"

---

### Q4: Should future simulations use a rubric?
**Answer:** Yes. Hybrid approach: standardized rubric for core dimensions (completeness, correctness, actionability) + open-ended section for emergent insights. Separate simulation from evaluation.

**Source:** SIMULATION_METHODOLOGY_EVALUATION.md → "Recommendations for Future Simulations"

---

## Surprising Findings

### 1. Research Approach > Model Tier
Sonnet (with web search) produced 1098-line output with real APIs.
Opus (no search) produced 190-line output with placeholders.
**Insight:** Instruction matters more than model.

### 2. Self-Assessment Optimism Bias
Agents that researched more rated themselves higher.
Hypothesis: Finding real patterns increased confidence.
**Insight:** Self-assessment conflates effort with quality.

### 3. "Be Honest" Worked Too Well
Agents provided extensive meta-commentary on limitations.
Real Copilot wouldn't do this.
**Insight:** Simulation became analysis rather than roleplay.

---

## Next Steps

### Immediate Actions (Based on Evaluation)

1. **Update BOOTLOADER.md**
   - Add 4Ws framework (What/Why/How/When)
   - Include prompt progression examples
   - Add validation checklist
   - **Source:** SIMULATION_FINDINGS_SUMMARY.md

2. **Create Bootstrap Template**
   - 45-word template with blanks for [STACK], [MECHANISM], [FEATURES]
   - Example: "Bootstrap [STACK] [MECHANISM] for [PURPOSE]. Features: [LIST]. Include [TESTS], [LINTING], [DOCS]. Target: [SUCCESS_CRITERIA]."
   - **Source:** SIMULATION_METHODOLOGY_EVALUATION.md → "Recommendations for BOOTLOADER.md"

3. **Run Validation Experiment**
   - Take 3 simulated outputs (10-word, 30-word, 45-word)
   - Actually create the files in test repos
   - Measure what works out-of-the-box
   - Compare to rubric scores
   - **Source:** IMPROVED_SIMULATION_PROMPT.md → "Optional: Validation Step"

### Future Research Questions

1. **Does the 4Ws framework improve completeness?**
   - Test: Prompt with 4Ws vs. without (same word count)
   - Measure: Completeness score, actionability score
   - Hypothesis: 4Ws structure → +15-20% completeness

2. **What's the optimal word count per component?**
   - Test: Vary word allocation (What: 10, Why: 5, How: 20, When: 10)
   - Measure: Completeness, correctness, actionability
   - Hypothesis: "How" needs most words (mechanism clarity)

3. **Does validation correlate with rubric scores?**
   - Collect: Rubric scores + actual execution results
   - Measure: Correlation between predicted and actual quality
   - Hypothesis: Actionability score predicts execution success

---

## Conclusion

This evaluation identified critical flaws in the original simulation methodology (self-assessment bias, uncontrolled research policy) while validating the core finding (30-50 words optimal, specificity matters). The improved two-phase methodology with standardized rubric is ready for future experiments.

**Main recommendation:** Use the 4Ws framework in BOOTLOADER.md and run validation experiments to ground-truth the completeness estimates.

**Evaluation grade:** B+ (useful insights despite methodological limitations)

---

## Document Metadata

| Document | Lines | Size | Created |
|----------|-------|------|---------|
| SIMULATION_FINDINGS_SUMMARY.md | 430+ | 11K | Jan 5, 2026 |
| SIMULATION_METHODOLOGY_EVALUATION.md | 770+ | 21K | Jan 5, 2026 |
| IMPROVED_SIMULATION_PROMPT.md | 550+ | 11K | Jan 5, 2026 |
| SIMULATION_COMPLETE_WRITEUP.md | 590+ | 16K | Jan 5, 2026 |
| SIMULATION_RESULTS.md | 430+ | 11K | Jan 5, 2026 |
| SIMULATION_PROMPTS.md | 240+ | 6.1K | Jan 5, 2026 |

**Total analysis:** ~3000+ lines, ~76K of documentation

---

**Last updated:** January 5, 2026, 10:01 PM EST
**Evaluator:** Claude Sonnet 4.5
**Source data:** `/tmp/claude/-Users-bln-play-agentic-primer/tasks/*.output` (29 files)
