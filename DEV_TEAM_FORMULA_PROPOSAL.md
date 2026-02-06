# Formula-Based Agent Workflow Proposal

**Date:** 2026-01-11
**Status:** Ready for Experimentation

---

## The Opportunity

The dev-team agents (@agent-dev-team, @agent-dev-spec-modeler, @agent-dev-tester, @agent-dev-coder, @agent-dev-validator) implement a sophisticated workflow pattern called the "System Modeling Quad" - but this pattern is currently embedded in agent prompts rather than being formalized as a reusable workflow.

**Key Insight:** The dev-team workflow is already formula-ready. It has:
- Clear phases with dependencies
- Parallel execution with isolation constraints
- Quality gates requiring user approval
- Conditional branching based on validation results
- Observable, measurable outcomes

By modeling this as a workflow formula (like `spec-creation.formula.toml`), we can make the pattern reusable, composable, and optimizable.

---

## What is a Formula-Based Workflow?

A formula is a declarative specification of a multi-agent workflow:

```toml
# Example: Component Development Formula
formula = "component-dev"

[[steps]]
id = "specifications"
agent = "@agent-dev-spec-modeler"
needs = ["epic"]
description = "Create .spec.md + .model.lisp"

[[steps]]
id = "test-suite"
agent = "@agent-dev-tester"
needs = ["specifications"]
parallel_with = ["implementation"]  # Runs in parallel
isolation = true  # Cannot communicate with parallel steps

[[steps]]
id = "implementation"
agent = "@agent-dev-coder"
needs = ["specifications"]
parallel_with = ["test-suite"]
isolation = true

[[steps]]
id = "validation"
agent = "@agent-dev-validator"
needs = ["test-suite", "implementation"]
description = "Validate all 4 files, require user approval"
```

This describes the workflow structure, dependencies, and constraints without implementation details.

---

## The System Modeling Quad Workflow

### Current Pattern (Agent-Embedded)

**Phase 1:** Specifications
- Agent: @agent-dev-spec-modeler
- Creates: .spec.md (human) + .model.lisp (formal)
- Gate: User approval required

**Phase 2:** Parallel Development
- Agent: @agent-dev-tester (creates .test.js from specs)
- Agent: @agent-dev-coder (creates .js from specs)
- Constraint: Complete isolation - cannot communicate
- Both receive identical specification inputs

**Phase 3:** Validation
- Agent: @agent-dev-validator
- Validates all 4 files against each other
- Runs tests, generates comprehensive report
- Gate: User approval required (APPROVE/REQUEST_FIXES/RESTART)

**Phase 4:** Resolution
- Conditional on validation result
- APPROVED: Complete and integrate
- FIXES_REQUIRED: Re-run specific agents
- RESTART: Return to Phase 1

### As Formula (Reusable)

See the complete formula in `DEV_TEAM_FORMULA_ANALYSIS.md` - it captures this exact pattern as a declarative workflow that can be:
- Executed with different component names
- Varied (team vs solo mode, validation levels)
- Composed (multiple components in parallel)
- Measured (track metrics across executions)
- Optimized (identify and fix bottlenecks)

---

## Benefits

### 1. Reusability
Use the same workflow pattern across projects:
```bash
# Project A
bd mol wisp component-dev --var component_name="MessageRouter"

# Project B
bd mol wisp component-dev --var component_name="CacheManager"
```

### 2. Experimentation
Test different approaches without changing agents:
```bash
# Experiment: Team parallel vs solo sequential
bd mol wisp component-dev --var development_mode="team"
bd mol wisp component-dev --var development_mode="solo"
```

### 3. Quality Measurement
Track workflow performance:
- First-pass approval rate
- Validation findings per execution
- Phase durations
- Iteration counts

### 4. Workflow Composition
Combine workflows for complex tasks:
```toml
# Develop 3 components in parallel
[[steps]]
id = "router"
include_formula = "component-dev"
vars = { component_name = "MessageRouter" }

[[steps]]
id = "cache"
include_formula = "component-dev"
vars = { component_name = "ResultCache" }
parallel_with = ["router"]

[[steps]]
id = "validator"
include_formula = "component-dev"
vars = { component_name = "InputValidator" }
parallel_with = ["router", "cache"]
```

### 5. Automated Coordination
Generate beads automatically from formula:
```bash
bd mol wisp component-dev --var component_name="Router" --generate-beads
# Creates epic + phase beads with proper dependencies
```

---

## Comparison with Spec-Creation Formula

The existing `spec-creation.formula.toml` demonstrates several patterns:

| Pattern | Spec-Creation | Dev-Team | Formula Benefit |
|---------|--------------|----------|-----------------|
| **Parallel Work** | 3 agents (BDD, StateMachine, FIT) | 2 agents (tester, coder) | Explicit parallelism |
| **Dependencies** | Phase gates (specs → runners → integration) | Phase gates (specs → dev → validation) | Clear workflow structure |
| **Variables** | system_name, component_count | component_name, dev_mode | Parameterization |
| **Quality Gates** | File existence, format checks | Test pass rate, compliance | Measurable outcomes |

Both patterns benefit from formula-based orchestration:
- Explicit coordination rather than implicit agent knowledge
- Reusable across projects
- Measurable and optimizable

---

## Implementation Roadmap

### Phase 1: Proof of Concept (1 week)
**Goal:** Validate formula approach with basic execution

1. Extend formula schema for agent workflows
   - Add `agent` field for step execution
   - Add `parallel_with` for parallel execution
   - Add `isolation` for communication constraints
   - Add `condition` for conditional execution

2. Create `component-dev.formula.toml`
   - Full dev-team workflow
   - Both team and solo modes
   - Quality gates and outcomes

3. Build simple formula executor
   - Parse TOML
   - Execute steps with agents
   - Track completion and dependencies

4. Test with 2-3 real components
   - Measure vs manual coordination
   - Validate formula benefits

### Phase 2: Parallel Execution (1 week)
**Goal:** Enable true parallel agent coordination

1. Implement parallel step execution
2. Enforce isolation constraints
3. Add conditional branching
4. Test parallel tester + coder workflow

### Phase 3: Bead Integration (1 week)
**Goal:** Auto-generate beads from formulas

1. Build formula-to-beads generator
2. Create dependency graphs
3. Generate epic and task beads
4. Compare to manual bead creation

### Phase 4: Formula Library (2 weeks)
**Goal:** Create reusable workflow collection

1. Document 5 core workflows
2. Build workflow metrics tracking
3. Create formula testing framework
4. Validate with real projects

---

## Recommended Experiments

### Experiment 1: Manual vs Formula Coordination
**Question:** Does formula-based coordination improve on manual?

**Test:**
- Same component requirements
- Same agents
- Different coordination (manual vs formula)

**Measure:**
- Setup time
- Execution time
- Error rate
- Quality outcomes

### Experiment 2: Workflow Optimization
**Question:** Which workflow variant performs best?

**Variables:**
- Development mode (team vs solo)
- Validation level (standard vs strict)
- Agent model (Opus vs Sonnet vs Haiku)

**Measure:**
- First-pass approval rate
- Validation findings
- Total duration
- Cost

### Experiment 3: Workflow Composition
**Question:** Can we efficiently develop multiple components?

**Test:**
- Parallel component development (2, 4, 8 components)
- Independent vs dependent components
- Serial vs parallel execution

**Measure:**
- Total duration vs sequential
- Resource utilization
- Integration success rate
- Per-component quality

---

## Open Questions

1. **Formula Granularity:** Pattern-level vs phase-level vs task-level?
   - Recommendation: Start pattern-level, evolve to phase composition

2. **Agent Autonomy:** How much should formulas constrain agents?
   - Recommendation: Guided structure, agents adapt to context

3. **Failure Handling:** How to handle errors and edge cases?
   - Recommendation: Formula defines recovery strategies, agent executes

4. **State Management:** How to manage workflow state?
   - Recommendation: File-based for now (matches current pattern)

5. **Human-in-the-Loop:** How to model approval gates?
   - Recommendation: Blocking for quality gates, optional for development

---

## Next Steps

### Immediate (This Week)
1. Review this proposal and `DEV_TEAM_FORMULA_ANALYSIS.md`
2. Decide: Build proof-of-concept?
3. If yes: Define first experiment (recommend Experiment 1)

### Short-Term (1-2 Weeks)
1. Build basic formula executor
2. Create `component-dev.formula.toml`
3. Test with 2-3 real components
4. Measure results vs manual coordination

### Medium-Term (1 Month)
1. Add parallel execution support
2. Build bead integration
3. Create formula library (5 workflows)
4. Run optimization experiments

### Long-Term (2-3 Months)
1. Production formula system
2. Comprehensive workflow library
3. Metrics dashboard
4. Team adoption and training

---

## Conclusion

The dev-team agents implement a sophisticated workflow pattern that's ready for formula-based orchestration. By formalizing this pattern, we gain:

- **Reusability** across projects and contexts
- **Experimentation** with different workflow variants
- **Measurement** of quality and performance
- **Optimization** based on data
- **Composition** for complex multi-component tasks

This is not just documentation - it's executable workflow infrastructure that brings infrastructure-as-code principles to agent coordination.

**The pattern exists, the agents exist, the benefits are clear. The opportunity is to formalize what's working and make it systematic.**

---

**Recommended Decision:** Build proof-of-concept this week with `component-dev.formula.toml` to validate the approach with real component development.

See `DEV_TEAM_FORMULA_ANALYSIS.md` for complete technical analysis and formula implementation.
