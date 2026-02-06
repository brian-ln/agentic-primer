# Dev-Team Agent Workflow Formula Analysis

**Date:** 2026-01-11
**Status:** Investigation Complete
**Bead:** agentic-primer-0t6

---

## Executive Summary

This analysis explores how to model the dev-team agent workflows using the formula-based approach exemplified by `spec-creation.formula.toml`. The dev-team agents (@agent-dev-team, @agent-dev-spec-modeler, @agent-dev-tester, @agent-dev-coder, @agent-dev-validator) follow a sophisticated parallel specification-driven development pattern that maps naturally to formula-based workflow orchestration.

**Key Finding:** The dev-team workflow is already formula-ready - it has clear phases, parallel execution paths, dependency chains, and quality gates. What's missing is the formalization layer that makes these patterns reusable and executable as workflow orchestration.

**Opportunity:** By modeling dev-team workflows as formulas, we can:
1. Make the "System Modeling Quad" pattern (spec → parallel dev → validation) reusable across projects
2. Enable workflow composition and variation (solo vs team, different validation levels)
3. Create measurable quality gates and success criteria
4. Support workflow experimentation and optimization
5. Generate beads/tasks automatically from workflow definitions

---

## Part 1: Current Dev-Team Workflow Architecture

### The System Modeling Quad Pattern

The dev-team agents produce four synchronized artifacts:
1. `.spec.md` - Human-readable specification (dev-spec-modeler)
2. `.model.lisp` - Formal DSL model (dev-spec-modeler)
3. `.test.js` - Specification-based tests (dev-tester)
4. `.js` - Implementation (dev-coder)

These are created through a 4-phase workflow with built-in quality gates.

### Phase Structure Analysis

#### Phase 1: Specification Architecture
**Agent:** @agent-dev-spec-modeler
**Pattern:** Single-agent sequential work with user approval gate
**Outputs:** 2 files (spec.md, model.lisp)
**Critical Feature:** Specification verification (reads existing, only updates if needed)
**Gate:** User approval required before proceeding

**Formula Mapping:**
```toml
[[steps]]
id = "specifications"
title = "Create specifications for {{component_name}}"
agent = "@agent-dev-spec-modeler"
needs = ["epic"]
description = """
Verify existing specifications or create new ones...
"""
```

#### Phase 2: Parallel Development Launch
**Agents:** @agent-dev-tester + @agent-dev-coder (parallel, isolated)
**Pattern:** Fork-join parallelism with strict isolation constraints
**Outputs:** 2 files (test.js, component.js)
**Critical Feature:** Complete isolation - agents cannot communicate or see each other's work
**Coordination:** Both receive identical specification inputs

**Formula Mapping:**
```toml
[[steps]]
id = "test-suite"
title = "Create test suite from specifications"
agent = "@agent-dev-tester"
needs = ["specifications"]
parallel_with = ["implementation"]
isolation = true
description = """
Create tests from specs: {{component_name}}.spec.md, {{component_name}}.model.lisp
CRITICAL: NO test skipping - fail on missing dependencies
"""

[[steps]]
id = "implementation"
title = "Create implementation from specifications"
agent = "@agent-dev-coder"
needs = ["specifications"]
parallel_with = ["test-suite"]
isolation = true
description = """
Create implementation from specs: {{component_name}}.spec.md, {{component_name}}.model.lisp
ISOLATION: Work independently from tests
"""
```

#### Phase 3: Independent Validation
**Agent:** @agent-dev-validator
**Pattern:** Third-party validation with comprehensive reporting
**Inputs:** All 4 files from previous phases
**Outputs:** Validation report with recommendations
**Critical Feature:** Cannot modify code, cannot auto-approve
**Gate:** User decision required (APPROVE/REQUEST_FIXES/RESTART)

**Formula Mapping:**
```toml
[[steps]]
id = "validation"
title = "Validate all artifacts against specifications"
agent = "@agent-dev-validator"
needs = ["test-suite", "implementation"]
description = """
Validate all 4 files: specs, tests, implementation
Run tests, analyze compliance, generate report
Require user approval to proceed
"""
```

#### Phase 4: Resolution and Integration
**Coordinator:** @agent-dev-team
**Pattern:** Conditional workflow based on validation results
**Branching Logic:**
- APPROVED → Complete, archive, document
- FIXES_REQUIRED → Re-launch specific agents with corrections
- RESTART → Return to Phase 1 with updated requirements

**Formula Mapping:**
```toml
[[steps]]
id = "resolution"
title = "Resolve validation findings and integrate"
needs = ["validation"]
conditional = true
description = """
Based on validation report and user decision:
- APPROVED: Complete workflow
- FIXES_REQUIRED: Re-run specific agents (spec-modeler, tester, or coder)
- RESTART: Return to Phase 1 with updated requirements
"""
```

---

## Part 2: Formula Structure Analysis

### Comparison with Spec-Creation Formula

The existing `spec-creation.formula.toml` demonstrates several patterns relevant to dev-team:

#### Pattern 1: Type-Based Parallel Organization
**Spec-Creation Approach:**
- 3 parallel agents, one per specification type (BDD, State Machine, FIT)
- Each agent handles ALL components for their type
- Enables specialization and consistency

**Dev-Team Parallel:**
- 2 parallel agents, one per artifact type (test, implementation)
- Each agent handles their artifact from identical specifications
- Enables independence and unbiased validation

**Formula Insight:** Both use parallel specialization but at different granularities (type vs artifact)

#### Pattern 2: Phased Dependencies
**Spec-Creation Pattern:**
```
Epic → [BDD + StateMachine + FIT] → TestRunners → Integration → Documentation
```

**Dev-Team Pattern:**
```
Epic → Specifications → [Tests + Implementation] → Validation → Resolution
```

**Formula Insight:** Both use phase gates where next phase depends on completion of ALL parallel tasks

#### Pattern 3: Quality Gates
**Spec-Creation Gates:**
- File existence checks (minimum 5 scenarios per component)
- Format validation (proper Given/When/Then)
- Coverage criteria (all components covered)

**Dev-Team Gates:**
- Specification completeness (100% required fields)
- Test coverage (100% of spec requirements)
- Implementation compliance (interface, state schema, message handling)
- Test execution results (pass rate, failure categorization)

**Formula Insight:** Gates are more sophisticated in dev-team (semantic validation vs structural checks)

#### Pattern 4: Variable Interpolation
**Spec-Creation Variables:**
```toml
[vars.system_name]
description = "Name of the system being specified"
required = true

[vars.component_count]
description = "Number of components to specify"
default = "6"
```

**Dev-Team Variables (proposed):**
```toml
[vars.component_name]
description = "Name of component to develop"
required = true

[vars.development_mode]
description = "solo or team (parallel)"
default = "team"

[vars.validation_level]
description = "standard or strict"
default = "standard"
```

---

## Part 3: Formula-Based Dev-Team Workflow Design

### Core Formula: Component Development

```toml
# Component Development Formula v1
#
# Specification-driven component development using System Modeling Quad pattern
# (.spec.md + .model.lisp + .test.js + .js)
#
# Supports both team-based parallel development and solo sequential development
#
# Usage:
# ```bash
# bd mol wisp component-dev \
#   --var component_name="MessageRouter" \
#   --var component_type="message-handler" \
#   --var development_mode="team" \
#   --var validation_level="standard" \
#   --var output_dir="src/components"
# ```

formula = "component-dev"
description = """
Specification-driven component development workflow.

Creates System Modeling Quad artifacts:
1. Specifications (.spec.md + .model.lisp)
2. Tests (.test.js) - parallel development
3. Implementation (.js) - parallel development
4. Validation report

Coordination: Team mode uses parallel isolated development with third-party validation.
Solo mode uses sequential development by single agent.

Result: Complete validated component with comprehensive specifications and tests.
"""
type = "workflow"
version = 1

[vars.component_name]
description = "Name of component to develop (e.g., 'MessageRouter')"
required = true

[vars.component_type]
description = "Component type for DSL foundation"
default = "message-handler"
options = ["message-handler", "state-machine", "data-processor", "service-provider", "integration-adapter"]

[vars.development_mode]
description = "Development mode: team (parallel) or solo (sequential)"
default = "team"
options = ["team", "solo"]

[vars.validation_level]
description = "Validation thoroughness: standard or strict"
default = "standard"
options = ["standard", "strict"]

[vars.output_dir]
description = "Directory for component output"
required = true

[vars.requirements]
description = "Component requirements (user-provided or from analysis)"
required = true

# =============================================================================
# Epic: Parent for all development work
# =============================================================================

[[steps]]
id = "epic"
title = "Develop {{component_name}} component with System Modeling Quad"
description = """
Complete specification-driven development of {{component_name}}.

Goal: Create production-ready component with:
- Comprehensive specifications (spec.md + model.lisp)
- Specification-based tests (test.js)
- Compliant implementation (js)
- Independent validation and approval

Development mode: {{development_mode}}
Validation level: {{validation_level}}
"""

# =============================================================================
# Phase 1: Specification Architecture (P0)
# =============================================================================

[[steps]]
id = "specifications"
title = "Create or verify specifications for {{component_name}}"
needs = ["epic"]
agent = "@agent-dev-spec-modeler"
description = """
Create comprehensive specifications for {{component_name}}:

Requirements: {{requirements}}
Component Type: {{component_type}}
Output Directory: {{output_dir}}

VERIFY FIRST: Read existing {{component_name}}.spec.md and {{component_name}}.model.lisp
Compare against current requirements
ONLY UPDATE IF NECESSARY

Deliverables:
- {{output_dir}}/{{component_name}}.spec.md (human-readable)
- {{output_dir}}/{{component_name}}.model.lisp (formal DSL)

Success criteria:
- Intent clearly stated
- State schema fully defined
- Message interface complete
- Validation rules specified
- Integration points documented

**PAUSE for user approval before parallel development**

Agent workflow:
1. Analyze requirements and extract component intent
2. Check for existing specifications
3. Create or update specifications as needed
4. Present specifications for user approval
5. Wait for explicit approval before marking complete
"""

# =============================================================================
# Phase 2: Parallel Development (P0) - Team Mode
# Conditional: Only if development_mode = "team"
# =============================================================================

[[steps]]
id = "test-suite"
title = "Create specification-based test suite"
needs = ["specifications"]
agent = "@agent-dev-tester"
parallel_with = ["implementation"]
condition = "development_mode == 'team'"
description = """
Create comprehensive test suite from specifications ONLY:

Input Specifications:
- {{output_dir}}/{{component_name}}.spec.md
- {{output_dir}}/{{component_name}}.model.lisp

Output: {{output_dir}}/{{component_name}}.test.js

CRITICAL CONSTRAINTS:
- NEVER access implementation file
- NEVER coordinate with @agent-dev-coder
- Tests MUST fail when dependencies missing - NO skipping
- Test the contract, not implementation details

Success criteria:
- Every requirement from spec has a test
- Every element from model is validated
- No implementation details assumed
- Tests are based purely on specifications
- Proper test documentation linking to specs

Agent workflow:
1. Read and analyze specifications (spec.md + model.lisp)
2. Extract all testable requirements
3. Design test structure by specification categories
4. Implement comprehensive test suite
5. Verify quality checklist completion
6. Commit: `git add {{output_dir}}/{{component_name}}.test.js && git commit -m "feat: Add tests for {{component_name}}"`
7. Close bead: `bd close <bead-id>`
"""

[[steps]]
id = "implementation"
title = "Create specification-compliant implementation"
needs = ["specifications"]
agent = "@agent-dev-coder"
parallel_with = ["test-suite"]
condition = "development_mode == 'team'"
description = """
Create specification-compliant implementation from specifications ONLY:

Input Specifications:
- {{output_dir}}/{{component_name}}.spec.md
- {{output_dir}}/{{component_name}}.model.lisp

Output: {{output_dir}}/{{component_name}}.js

CRITICAL CONSTRAINTS:
- NEVER access test file
- NEVER coordinate with @agent-dev-tester
- Implement specification exactly as defined
- All state fields from state-schema initialized
- All message types handled

Success criteria:
- All state fields from state-schema initialized
- All message types from :accepts are handled
- All validation rules enforced
- FSM logic correctly implemented (if applicable)
- Integration contracts satisfied

Agent workflow:
1. Read and analyze specifications (spec.md + model.lisp)
2. Design implementation structure matching specs
3. Implement complete specification-compliant component
4. Verify quality checklist completion
5. Commit: `git add {{output_dir}}/{{component_name}}.js && git commit -m "feat: Implement {{component_name}}"`
6. Close bead: `bd close <bead-id>`
"""

# =============================================================================
# Phase 2 Alternative: Solo Development (P0) - Solo Mode
# Conditional: Only if development_mode = "solo"
# =============================================================================

[[steps]]
id = "solo-development"
title = "Complete test and implementation development"
needs = ["specifications"]
agent = "@agent-dev-solo"
condition = "development_mode == 'solo'"
description = """
Continue from Phase 3 with completed specifications:

Input Specifications:
- {{output_dir}}/{{component_name}}.spec.md
- {{output_dir}}/{{component_name}}.model.lisp

Outputs:
- {{output_dir}}/{{component_name}}.test.js
- {{output_dir}}/{{component_name}}.js

Development approach: Sequential (tests first, then implementation)

Agent workflow:
1. Start from Phase 3 of solo workflow (specs already created)
2. Create test suite from specifications
3. Create implementation from specifications
4. Run tests and iterate until passing
5. Commit both files
6. Close bead: `bd close <bead-id>`
"""

# =============================================================================
# Phase 3: Independent Validation (P0)
# =============================================================================

[[steps]]
id = "validation"
title = "Validate all artifacts against specifications"
needs = ["test-suite", "implementation", "solo-development"]
needs_any = true  # Either parallel completion OR solo completion
agent = "@agent-dev-validator"
description = """
Independent third-party validation of all development artifacts:

Files to Validate:
- {{output_dir}}/{{component_name}}.spec.md (specification)
- {{output_dir}}/{{component_name}}.model.lisp (formal model)
- {{output_dir}}/{{component_name}}.test.js (tests)
- {{output_dir}}/{{component_name}}.js (implementation)

Validation Level: {{validation_level}}

CRITICAL CONSTRAINTS:
- Cannot modify any files
- Cannot auto-approve results
- Must report all discrepancies
- Must require user approval

Validation process:
1. Specification analysis (extract all requirements)
2. Test suite validation (coverage, quality, structure)
3. Implementation validation (interface, state, messages)
4. Cross-validation (run tests, analyze results)
5. Report generation (comprehensive findings)

Success criteria:
- Comprehensive validation report generated
- Test execution results analyzed
- All discrepancies identified and categorized
- Specific recommendations provided
- User approval explicitly required

Agent workflow:
1. Read all 4 files
2. Build validation checklist from specifications
3. Validate test coverage and quality
4. Validate implementation compliance
5. Run test suite: `bun test {{output_dir}}/{{component_name}}.test.js`
6. Analyze test failures and categorize
7. Generate comprehensive validation report
8. Present report to user for decision
9. Wait for user approval (APPROVE/REQUEST_FIXES/RESTART)
10. Close bead based on decision: `bd close <bead-id>`
"""

# =============================================================================
# Phase 4: Resolution (P1)
# Conditional based on validation results
# =============================================================================

[[steps]]
id = "resolution-approved"
title = "Finalize and integrate approved component"
needs = ["validation"]
condition = "validation_result == 'APPROVED'"
description = """
Component approved - finalize and integrate:

Artifacts:
- {{output_dir}}/{{component_name}}.spec.md ✓
- {{output_dir}}/{{component_name}}.model.lisp ✓
- {{output_dir}}/{{component_name}}.test.js ✓
- {{output_dir}}/{{component_name}}.js ✓

Tasks:
1. Archive specifications as authoritative
2. Add component to integration tests
3. Update component registry
4. Generate usage documentation
5. Document lessons learned

**Component development complete!**
"""

[[steps]]
id = "resolution-fixes"
title = "Apply fixes based on validation findings"
needs = ["validation"]
condition = "validation_result == 'REQUEST_FIXES'"
description = """
Validation identified issues - apply targeted fixes:

Root cause analysis:
- Specification ambiguity → Re-run @agent-dev-spec-modeler
- Test issues → Re-run @agent-dev-tester
- Implementation issues → Re-run @agent-dev-coder

Process:
1. Identify root cause from validation report
2. Re-launch specific agent(s) with corrections
3. Return to Phase 3 (validation)
4. Iterate until user approval

Note: May require multiple iterations
"""

[[steps]]
id = "resolution-restart"
title = "Restart from specifications with updated requirements"
needs = ["validation"]
condition = "validation_result == 'RESTART'"
description = """
Fundamental issues require specification redesign:

Process:
1. Analyze validation report for root causes
2. Update requirements based on findings
3. Return to Phase 1 (specifications)
4. Apply lessons learned to prevent recurring issues

Note: This restarts the entire workflow
"""
```

---

## Part 4: Advanced Formula Patterns

### Pattern 1: Workflow Composition

The formula approach enables composition of workflows:

```toml
# Multi-component development formula
formula = "system-dev"
description = "Develop multiple related components in parallel"

[[steps]]
id = "component-router"
include_formula = "component-dev"
vars = { component_name = "MessageRouter", component_type = "message-handler" }

[[steps]]
id = "component-cache"
include_formula = "component-dev"
vars = { component_name = "ResultCache", component_type = "data-processor" }
parallel_with = ["component-router"]

[[steps]]
id = "integration-tests"
needs = ["component-router", "component-cache"]
description = "Test components working together"
```

### Pattern 2: Workflow Variants

Different development contexts need different workflows:

```toml
# Brownfield variant (reverse-engineering existing code)
[[steps]]
id = "specifications-brownfield"
agent = "@agent-dev-spec-modeler"
description = """
Reverse-engineer specifications from existing implementation:
Input: {{output_dir}}/{{component_name}}.js (existing code)
Output: .spec.md + .model.lisp (extracted specifications)
"""

# Greenfield variant (new development)
[[steps]]
id = "specifications-greenfield"
agent = "@agent-dev-spec-modeler"
description = """
Create specifications from requirements:
Input: {{requirements}} (user requirements)
Output: .spec.md + .model.lisp (new specifications)
"""
```

### Pattern 3: Parameterized Quality Gates

```toml
[validation.standard]
test_pass_rate_min = 95
coverage_min = 90
spec_compliance_min = 100

[validation.strict]
test_pass_rate_min = 100
coverage_min = 100
spec_compliance_min = 100
performance_validation = true
security_scan = true
```

### Pattern 4: Observable Outcomes

Each phase should define observable outcomes (inspired by SUCCESS_CRITERIA.md):

```toml
[[steps]]
id = "specifications"
observable_outcomes = [
  "Files {{component_name}}.spec.md and {{component_name}}.model.lisp exist",
  "Spec contains all required sections (Overview, Features, Usage, Integration)",
  "Model defines state-schema with all required fields",
  "Model defines :accepts with at least one message type",
  "User has approved specifications"
]
```

---

## Part 5: Implementation Roadmap

### Step 1: Extend Formula Schema (1-2 days)

Add new formula capabilities:

```toml
# New fields for formula schema
[steps]
agent = "@agent-name"  # Agent to execute step
parallel_with = ["step-id"]  # Parallel execution
isolation = true  # Cannot communicate with parallel steps
condition = "var == 'value'"  # Conditional execution
needs_any = true  # Needs ANY of the dependencies (OR logic)
observable_outcomes = ["outcome1", "outcome2"]  # Measurable results

[validation]
# Validation-specific configuration
level = "standard"  # or "strict"
gates = { test_pass_rate = 95, coverage = 90 }
```

### Step 2: Create Formula Execution Engine (2-3 days)

Build interpreter for formulas:

```javascript
class FormulaExecutor {
  // Parse formula TOML
  parseFormula(formulaPath)

  // Resolve dependencies and build execution graph
  buildExecutionGraph(steps)

  // Execute step with agent
  executeStep(step, vars, context)

  // Handle parallel execution
  executeParallel(steps, vars, context)

  // Manage isolation constraints
  enforceIsolation(agents)

  // Evaluate conditions
  evaluateCondition(condition, vars)

  // Check observable outcomes
  validateOutcomes(outcomes, context)

  // Generate beads from formula
  generateBeads(formula, vars)
}
```

### Step 3: Create Formula Library (1 week)

Build reusable formulas for common workflows:

1. `component-dev.formula.toml` - System Modeling Quad development
2. `system-dev.formula.toml` - Multi-component development
3. `brownfield-spec.formula.toml` - Reverse-engineering specifications
4. `spec-refinement.formula.toml` - Iterative specification improvement
5. `integration-test.formula.toml` - Component integration testing

### Step 4: Agent Integration (2-3 days)

Update agents to work with formula system:

```markdown
# Agent updates needed:
- Accept formula variables as input
- Report observable outcomes
- Support conditional execution
- Handle isolation constraints
- Provide structured status updates
```

### Step 5: Beads Integration (1-2 days)

Auto-generate beads from formulas:

```bash
# Generate beads from formula
bd mol wisp component-dev \
  --var component_name="MessageRouter" \
  --generate-beads

# Creates:
# - Epic bead
# - Phase beads
# - Step beads with dependencies
# - All with proper priority and status
```

---

## Part 6: Benefits and Use Cases

### Benefit 1: Reusable Workflows

The dev-team pattern becomes reusable across projects:

```bash
# Project A: Develop message router
bd mol wisp component-dev \
  --var component_name="MessageRouter" \
  --var component_type="message-handler"

# Project B: Develop cache system
bd mol wisp component-dev \
  --var component_name="CacheManager" \
  --var component_type="data-processor"

# Same workflow, different context
```

### Benefit 2: Workflow Experimentation

Test different development approaches:

```bash
# Experiment 1: Team parallel development
bd mol wisp component-dev \
  --var development_mode="team" \
  --var validation_level="standard"

# Experiment 2: Solo sequential development
bd mol wisp component-dev \
  --var development_mode="solo" \
  --var validation_level="standard"

# Experiment 3: Strict validation
bd mol wisp component-dev \
  --var development_mode="team" \
  --var validation_level="strict"
```

### Benefit 3: Quality Metrics

Formula-based workflows enable measurement:

```toml
# Track workflow performance
[metrics]
phase_1_duration = "specifications completion time"
phase_2_duration = "parallel development completion time"
phase_3_duration = "validation completion time"
iteration_count = "number of validation cycles"
first_pass_rate = "percentage approved without fixes"

# Quality metrics
spec_approval_rate = "percentage approved on first review"
test_pass_rate = "percentage of tests passing on first run"
validation_issues = "number of discrepancies found"
```

### Benefit 4: Workflow Optimization

Identify bottlenecks and optimize:

```bash
# Analyze workflow metrics
bd mol analyze component-dev --metrics

# Output:
# Phase 1 (Specifications): avg 45 min, 90% approval rate
# Phase 2 (Parallel Dev): avg 120 min, 85% success rate
# Phase 3 (Validation): avg 30 min, 3.2 avg issues found
# Phase 4 (Resolution): avg 60 min, 2.1 avg iterations
#
# Bottleneck: Phase 2 - parallel development
# Suggestion: Improve specification clarity to reduce issues
```

---

## Part 7: Proposal and Recommendations

### Proposal: Formula-Based Agent Workflow System

**Goal:** Create a formula-based workflow orchestration system that:
1. Formalizes the dev-team pattern as a reusable workflow
2. Enables composition and variation of agent workflows
3. Provides measurable quality gates and outcomes
4. Supports experimentation and optimization
5. Auto-generates beads from workflow definitions

### Recommended Approach

#### Phase 1: Proof of Concept (1 week)
1. Implement basic formula schema extensions
2. Create `component-dev.formula.toml` for dev-team pattern
3. Build simple formula executor (no parallelism yet)
4. Test with manual agent invocation
5. Validate formula approach with 2-3 real components

**Success Criteria:**
- Formula successfully describes dev-team workflow
- Manual execution follows formula steps
- Observable outcomes are measurable
- Formula is more maintainable than ad-hoc coordination

#### Phase 2: Parallel Execution (1 week)
1. Add parallel execution support to formula executor
2. Implement isolation constraints
3. Add conditional execution
4. Test parallel dev-tester + dev-coder workflow
5. Measure coordination overhead vs manual approach

**Success Criteria:**
- Parallel steps execute simultaneously
- Isolation prevents cross-contamination
- Conditional branching works correctly
- Performance matches manual coordination

#### Phase 3: Bead Integration (1 week)
1. Build formula-to-beads generator
2. Create dependency graph from formula
3. Generate epic and task beads automatically
4. Test with full component-dev workflow
5. Compare to manual bead creation

**Success Criteria:**
- Beads generated correctly from formula
- Dependencies match formula needs
- Execution follows formula structure
- Less manual coordination required

#### Phase 4: Formula Library (2 weeks)
1. Create 5 core workflow formulas
2. Document formula patterns and best practices
3. Build formula testing framework
4. Create workflow metrics tracking
5. Validate with real project usage

**Success Criteria:**
- Formula library covers common workflows
- Formulas are composable and reusable
- Metrics provide actionable insights
- Teams adopt formula-based workflows

### Recommended Experiments

#### Experiment 1: Workflow Comparison
Compare manual vs formula-based dev-team coordination:

**Variables:**
- Same component requirements
- Same agents
- Different coordination approaches (manual vs formula)

**Metrics:**
- Setup time (workflow definition vs manual coordination)
- Execution time (total workflow duration)
- Error rate (coordination mistakes)
- Quality (validation findings)
- Developer experience (subjective feedback)

#### Experiment 2: Workflow Optimization
Test different dev-team workflow variants:

**Variables:**
- Development mode (team vs solo)
- Validation level (standard vs strict)
- Specification detail level (minimal vs comprehensive)
- Agent model (Opus vs Sonnet vs Haiku)

**Metrics:**
- Success rate (first-pass approval)
- Issue count (validation findings)
- Iteration count (validation cycles)
- Duration (total workflow time)
- Cost (API usage)

#### Experiment 3: Workflow Composition
Test multi-component development with composed workflows:

**Variables:**
- Number of parallel components (2, 4, 8)
- Component dependencies (independent vs dependent)
- Resource constraints (serial vs parallel execution)

**Metrics:**
- Total duration vs sequential development
- Resource utilization (parallel efficiency)
- Integration success rate
- Quality metrics per component

---

## Part 8: Open Questions and Considerations

### Question 1: Formula Granularity
**How fine-grained should formulas be?**

Options:
A. One formula per workflow pattern (component-dev, system-dev, etc.)
B. Composable sub-formulas for phases (spec, dev, validate)
C. Atomic formulas for individual agent tasks

**Recommendation:** Start with A (pattern-level), evolve to B (phase composition) as patterns emerge

### Question 2: Agent Autonomy
**How much should formulas constrain agent behavior?**

Options:
A. Prescriptive: Formula defines exact steps and outputs
B. Guided: Formula provides structure, agents adapt
C. Autonomous: Formula defines outcomes, agents choose approach

**Recommendation:** B (guided) - balance structure with agent intelligence

### Question 3: Failure Handling
**How should formulas handle failures and edge cases?**

Options:
A. Explicit error paths in formula
B. Agent handles errors, reports to formula
C. Hybrid: Formula defines recovery strategies, agent executes

**Recommendation:** C (hybrid) - formula defines what to do, agent handles how

### Question 4: State Management
**How should workflow state be managed across phases?**

Options:
A. File-based: Artifacts on disk, agents read/write files
B. Context object: Structured state passed between phases
C. Database: Persistent workflow state in database

**Recommendation:** A (file-based) for now - matches current dev-team pattern, add B later for optimization

### Question 5: Human-in-the-Loop
**How should user approval gates be modeled?**

Options:
A. Blocking: Formula pauses execution, waits for user input
B. Asynchronous: Formula continues, user approves later
C. Optional: User can enable/disable gates per execution

**Recommendation:** A (blocking) for quality gates, C (optional) for development workflow flexibility

---

## Conclusion

The dev-team agent workflow is already formula-ready - it has clear structure, defined phases, parallel patterns, and quality gates. By formalizing this as a workflow formula, we can:

1. **Make patterns reusable** - The System Modeling Quad pattern becomes available across all projects
2. **Enable experimentation** - Test different development approaches (team vs solo, validation levels)
3. **Measure quality** - Track metrics like first-pass approval rate, validation findings
4. **Reduce coordination overhead** - Auto-generate beads and manage dependencies
5. **Improve workflows** - Identify bottlenecks and optimize over time

The formula approach is not just documentation - it's executable workflow orchestration that brings the benefits of infrastructure-as-code to agent coordination.

**Recommended Next Step:** Build proof-of-concept with `component-dev.formula.toml` to validate the approach with real component development.

---

**Analysis Complete**
**Bead Status:** Ready for close after user review
