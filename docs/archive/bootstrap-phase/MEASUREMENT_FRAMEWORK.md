# Measurement Framework: Goals → Outcomes → Metrics

## How We Measure Success

This document maps the relationship between what we want to achieve (goals), what success looks like (outcomes), and how we measure it (metrics).

---

## The Measurement Hierarchy

```
VISION
  ↓
GOALS (What we want to achieve)
  ↓
OUTCOMES (What success looks like)
  ↓
KEY RESULTS (Specific, measurable targets)
  ↓
METRICS (How we track progress)
  ↓
VALIDATION (How we verify)
```

---

## Vision Statement

**Build a self-bootstrapping, self-optimizing git-native issue automation system that can be reliably created by any AI agent from a minimal specification.**

---

## Goal → Outcome → Metric Mapping

### GOAL 1: Create Minimal Bootstrap Seed

**What we want to achieve**: A single markdown file that any AI agent can execute to generate a complete issue automation system.

**Success looks like**:
- File is concise (under 500 words)
- Contains specific, actionable instructions
- Works with multiple AI agents (Claude, Copilot, Gemini, Aider)
- Produces consistent results

**Key Results**:
| Key Result | Target | How Measured |
|------------|--------|--------------|
| Seed word count | ≤500 words | Word count tool |
| Agent compatibility | ≥3 of 4 agents | Test execution with each agent |
| First-time success rate | ≥80% | Fresh repo tests |
| Result consistency | 100% file match | Compare generated files |

**Metrics**:
- **Quantitative**:
  - Word count: Current 287, Target ≤500
  - Agent pass rate: Target 75% (3/4)
  - Bootstrap success rate: Target 80%
  - File generation accuracy: Target 100%

- **Qualitative**:
  - Instructions are specific (not vague)
  - Requirements are verifiable (not subjective)
  - Language is imperative (commands, not descriptions)

**Validation Method**:
1. Execute seed with each agent
2. Compare generated files against expected
3. Run verification script (pass/fail)
4. Document results in EXECUTION_LOG

**Current Status**: Seed created (287 words), not yet tested

---

### GOAL 2: Build Automated Verification

**What we want to achieve**: Programmatic validation that confirms bootstrap success without human judgment.

**Success looks like**:
- Script runs on all platforms (macOS, Linux, Windows)
- Checks are comprehensive (files, syntax, content)
- Results are unambiguous (exit 0 or 1)
- Execution is fast (under 30 seconds)
- Zero false positives/negatives

**Key Results**:
| Key Result | Target | How Measured |
|------------|--------|--------------|
| Platform compatibility | 3/3 OS | Test on macOS, Linux, Windows |
| Verification accuracy | 100% | Manual audit vs script results |
| Execution time | ≤30 seconds | Time command |
| False positive rate | 0% | Test with intentionally broken setups |

**Metrics**:
- **Quantitative**:
  - Platforms supported: Target 3/3
  - Checks performed: 10+ (files, YAML, content)
  - Execution time: Target ≤30 sec
  - Accuracy: Target 100%

- **Qualitative**:
  - Error messages are clear and actionable
  - Script follows bash best practices
  - No dependencies beyond git/bash

**Validation Method**:
1. Run on macOS, Linux, Windows Git Bash
2. Test with correct setup (should pass)
3. Test with missing files (should fail)
4. Test with invalid YAML (should fail)
5. Test with incomplete content (should fail)
6. Compare script results to manual inspection (100% match)

**Current Status**: Not created yet, specification ready

---

### GOAL 3: Establish Optimization Process

**What we want to achieve**: Systematic way to improve the bootstrap seed over time based on execution data.

**Success looks like**:
- Execution results are logged in structured format
- Analysis scripts identify failure patterns automatically
- System proposes specific, actionable improvements
- Improvements can be tested and validated
- Eventually, system optimizes itself

**Key Results**:
| Key Result | Target | How Measured |
|------------|--------|--------------|
| Executions logged | 100% | Count in EXECUTION_LOG vs actual runs |
| Pattern detection | ≥5 patterns | Analysis script output |
| Improvement proposals | ≥2 per 10 runs | Count proposed changes |
| Proposal quality | ≥80% merged | Accepted / total proposals |

**Metrics**:
- **Quantitative**:
  - Executions tracked: Target 100%
  - Patterns identified: Target 5+
  - Improvements proposed: Target 2 per 10 runs
  - Improvements merged: Target 80%
  - Optimization cycle time: Target ≤1 hour

- **Qualitative**:
  - Proposals are specific (file/line changes)
  - Reasoning is clear (cause → effect → fix)
  - Changes are testable (can re-run verification)

**Validation Method**:
1. Run 10+ bootstrap executions
2. Log all results in structured format
3. Run analysis script
4. Verify pattern detection output
5. Test proposed improvements
6. Measure success rate change

**Current Status**: Not started, design complete

---

## Composite Success Metrics

These combine multiple goals to measure overall success:

### Overall System Health

```
System Health Score = (Reliability × 0.4) + (Performance × 0.3) + (Quality × 0.3)

Where:
- Reliability = Success rate across all agents
- Performance = 1 - (Actual time / Target time)
- Quality = (Files correct + YAML valid + Workflows functional) / 3
```

**Target**: ≥90% health score
**Current**: Not measured yet

### Bootstrap Maturity Index

```
Maturity = (Phase 1 + Phase 2 + Phase 3 + Phase 4) / 4

Where each phase is:
- 0% (not started)
- 25% (in progress)
- 75% (mostly complete)
- 100% (complete and validated)
```

**Current Maturity**: 25% (Phase 1: 100%, Phases 2-4: 0%)

### Self-Improvement Velocity

```
Velocity = Improvements merged / Weeks since first execution

Where:
- Improvements = System-proposed changes that merged
- Weeks = Time since first successful bootstrap
```

**Target**: ≥1 improvement per 2 weeks
**Current**: N/A (no executions yet)

---

## Measurement Cadence

### Real-Time (Continuous)
- Bootstrap execution results → EXECUTION_LOG
- Verification pass/fail → Immediate feedback
- File generation → Automated check

### Daily (Automated)
- Update METRICS_DASHBOARD with latest execution data
- Flag critical failures (success rate drop >10%)
- Alert on new patterns detected

### Weekly (Manual Review)
- Review METRICS_DASHBOARD
- Analyze execution patterns
- Update OKR progress
- Adjust targets if needed
- Plan next week's work

### Monthly (Strategic Review)
- Quarterly OKR assessment
- Success criteria validation
- Risk review and mitigation
- Roadmap adjustments
- Stakeholder updates

---

## How to Measure Each Metric

### Reliability Metrics

**Overall Success Rate**:
```bash
# Count passes and fails in EXECUTION_LOG
passes=$(grep -c "PASS" EXECUTION_LOG_V1.md)
total=$(grep -c "Test.*:" EXECUTION_LOG_V1.md)
success_rate=$((passes * 100 / total))
echo "Success Rate: $success_rate%"
```

**Per-Agent Success Rate**:
```bash
# For each agent
for agent in claude copilot gemini aider; do
  passes=$(grep "$agent.*PASS" EXECUTION_LOG_V1.md | wc -l)
  total=$(grep "$agent" EXECUTION_LOG_V1.md | wc -l)
  rate=$((passes * 100 / total))
  echo "$agent: $rate%"
done
```

### Performance Metrics

**Bootstrap Duration**:
```bash
# Extract from execution log
grep "Duration:" EXECUTION_LOG_V1.md | awk '{print $2}' | sort -n
# Calculate average, min, max
```

**Verification Time**:
```bash
# Time the verification script
time ./scripts/verify-bootstrap.sh
```

### Quality Metrics

**YAML Validity**:
```bash
# Run yamllint on all generated YAML files
yamllint .github/**/*.yml
# Count passes vs total
```

**File Generation Accuracy**:
```bash
# Compare expected vs actual
expected=("file1" "file2" "file3" "file4" "file5")
for file in "${expected[@]}"; do
  [[ -f "$file" ]] && echo "✓ $file" || echo "✗ $file"
done
```

---

## Success Validation Checklist

Use this checklist at each checkpoint:

### Checkpoint 1: Minimal Viability
- [ ] Bootstrap seed exists (≤500 words)
- [ ] Verification script exists and runs
- [ ] Executed with 1 agent successfully
- [ ] Verification script passes (exit 0)
- [ ] Results documented in EXECUTION_LOG
- [ ] METRICS_DASHBOARD updated
- **Success Threshold**: All 6 items checked

### Checkpoint 2: Agent Portability
- [ ] Tested with Claude Code (passed)
- [ ] Tested with Copilot (result: ___)
- [ ] Tested with Gemini (result: ___)
- [ ] Tested with Aider (result: ___)
- [ ] Success rate ≥75% (3/4 passing)
- [ ] Quirks documented in AGENT_COMPATIBILITY
- **Success Threshold**: ≥5 items checked, including success rate

### Checkpoint 3: Optimization Infrastructure
- [ ] scripts/run-optimization-cycle.sh created
- [ ] scripts/analyze-execution.sh created
- [ ] Optimization issue template created
- [ ] Analysis script identifies ≥3 patterns
- [ ] System proposes ≥1 improvement
- [ ] Proposed improvement is valid and specific
- [ ] Improvement tested and merged
- **Success Threshold**: All 7 items checked

### Checkpoint 4: Production Readiness
- [ ] All documentation complete (100%)
- [ ] QUICKSTART exists and tested
- [ ] Template repo published on GitHub
- [ ] ≥1 external user bootstrapped successfully
- [ ] External user took ≤5 minutes
- [ ] Known issues documented in FAQ
- [ ] Demo video created and published
- **Success Threshold**: ≥6 items checked, including external validation

---

## Reporting Templates

### Weekly Status Report

```markdown
# Weekly Status Report: Week [N]

## Metrics Summary
- Overall Success Rate: [X]% (Target: 90%)
- Bootstrap Duration: [X] min (Target: ≤10 min)
- System Health Score: [X]% (Target: ≥90%)
- Maturity Index: [X]% (Target: progress)

## This Week's Progress
- Executions: [N] total, [N] passed, [N] failed
- New agents tested: [list]
- Improvements merged: [N]
- Blockers resolved: [N]

## Next Week's Plan
- [ ] Target 1
- [ ] Target 2
- [ ] Target 3

## Risks and Issues
- [List any new risks or blockers]

## Decisions Needed
- [Any decisions requiring input]
```

### Monthly OKR Review

```markdown
# Monthly OKR Review: [Month]

## Q[N] Progress

### Objective: [Name]
- Key Result 1: [X]% complete (Target: [Y]%)
- Key Result 2: [X]% complete (Target: [Y]%)
- Key Result 3: [X]% complete (Target: [Y]%)
- Key Result 4: [X]% complete (Target: [Y]%)
- **Overall**: [X]% (On/Behind/Ahead of schedule)

## Achievements This Month
1. [Achievement 1]
2. [Achievement 2]
3. [Achievement 3]

## Challenges and Learnings
- Challenge: [X] → Learning: [Y]

## Adjustments for Next Month
- [Any scope, timeline, or target changes]
```

---

## Decision Framework

When metrics show problems, use this framework to decide next actions:

### If Success Rate < 75%
1. **Analyze**: Run scripts/analyze-execution.sh
2. **Identify**: Which agents are failing? What's the pattern?
3. **Decide**:
   - If 1 agent failing: Document quirk, provide guidance
   - If 2+ agents failing: Revise seed, re-test
   - If all agents failing: Fundamental issue, major revision
4. **Test**: Re-run after changes
5. **Document**: Update OPTIMIZATION_LOG

### If Bootstrap Duration > 15 min
1. **Analyze**: Check execution logs for bottlenecks
2. **Identify**: Is it agent thinking time or actual execution?
3. **Decide**:
   - Agent thinking: Simplify seed language
   - Execution time: Reduce file count or complexity
   - GitHub Actions: Check runner availability
4. **Test**: Measure before/after
5. **Document**: Update METRICS_DASHBOARD

### If Verification False Positives > 0
1. **Analyze**: What passed verification but shouldn't have?
2. **Identify**: Which check is missing or inadequate?
3. **Decide**: Add specific check for this failure mode
4. **Test**: Re-run with known failure cases
5. **Document**: Update verify script comments

---

## Success Criteria Reference Card

Print this and post it where you work:

```
┌────────────────────────────────────────────────────────────┐
│            AGENTIC PRIMER SUCCESS CRITERIA                 │
├────────────────────────────────────────────────────────────┤
│ RELIABILITY                                                │
│  ✓ 90%+ success across all agents                         │
│  ✓ 75%+ per-agent success rate                            │
│  ✓ 80%+ first-time success                                │
│                                                            │
│ PERFORMANCE                                                │
│  ✓ ≤10 min bootstrap duration                             │
│  ✓ ≤30 sec verification time                              │
│  ✓ ≤1 hour optimization cycle                             │
│                                                            │
│ QUALITY                                                    │
│  ✓ 100% YAML validity                                     │
│  ✓ 100% file generation                                   │
│  ✓ 100% workflow functionality                            │
│                                                            │
│ USABILITY                                                  │
│  ✓ ≤5 min time to first bootstrap                         │
│  ✓ ≤3 steps setup complexity                              │
│  ✓ ≥5 successful external users                           │
│                                                            │
│ DONE WHEN:                                                 │
│  • All above targets met                                   │
│  • Self-optimization working (3+ improvements)             │
│  • 100% verification accuracy                              │
│  • Complete documentation                                  │
└────────────────────────────────────────────────────────────┘
```

---

## Document Status

**Version**: v1.0
**Last Updated**: 2026-01-05
**Owner**: Project Lead
**Review Cycle**: Weekly
**Next Review**: 2026-01-12
