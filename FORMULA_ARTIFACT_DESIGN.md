# Formula Artifact Design: First-Class Outputs in Workflow Orchestration

**Date:** 2026-01-11
**Status:** Design Complete
**Formula:** component-dev.formula.toml v1.0

---

## Executive Summary

This document introduces **artifact-aware workflow formulas** - an extension to the beads formula system that elevates outputs (files, links, artifacts) from implicit side-effects to first-class citizens in the workflow graph.

**Key Innovation:** Instead of workflows that simply "create files", artifact-aware formulas explicitly model:
- What artifacts each step produces and consumes
- Validation criteria for each artifact
- Links and references between artifacts
- How artifacts flow through the workflow
- Quality gates based on artifact state

**Result:** Workflows become traceable, verifiable, and composable at the artifact level, not just the task level.

---

## The Problem: Implicit Artifacts

### Current Approach (Task-Centric)

Traditional formulas focus on **tasks** and **dependencies**:

```toml
[[steps]]
id = "create-tests"
title = "Create test suite"
needs = ["specifications"]
description = "Create comprehensive test suite from specifications"
```

**What's missing:**
- Which files does this step produce?
- What validation ensures quality?
- How does the next step consume these artifacts?
- What happens if artifacts are incomplete or invalid?

### Problems This Creates

1. **No Artifact Traceability**: Files appear magically without explicit tracking
2. **Unclear Dependencies**: Next step depends on task completion, not artifact availability
3. **Missing Validation**: No explicit criteria for artifact quality
4. **Hard to Debug**: When workflows fail, unclear which artifact is problematic
5. **Limited Reusability**: Can't compose workflows at artifact level

---

## The Solution: Artifact-Aware Formulas

### Design Principle: Artifacts as First-Class Citizens

**Core Idea:** Workflows are fundamentally about **transforming artifacts**, not just completing tasks.

```toml
[[steps]]
id = "test-suite"
title = "Create specification-based test suite"
agent = "@agent-dev-tester"
needs = ["specifications"]

# EXPLICIT OUTPUT TRACKING
[[steps.outputs]]
artifact = "{{component_name}}.test.js"
path = "{{output_dir}}/{{component_name}}.test.js"
type = "test-suite"
required = true
description = "Comprehensive test suite validating all specification requirements"
validation = [
  "File exists",
  "Every requirement from spec.md has a test",
  "Every element from model.lisp is validated",
  "NO test skipping (conditional returns, silent warnings)",
  "Tests are based purely on specifications"
]
```

### Benefits of Explicit Artifact Tracking

1. **Traceability**: See exactly what artifacts each step produces
2. **Validation**: Explicit quality criteria for each artifact
3. **Dependencies**: Steps depend on artifact availability, not just task completion
4. **Debugging**: Identify which artifact is incomplete or invalid
5. **Composition**: Reuse artifacts across workflow variations
6. **Documentation**: Self-documenting workflow with clear inputs and outputs

---

## Artifact Schema Design

### Artifact Definition Structure

```toml
[[steps.outputs]]
artifact = "name"           # Logical artifact name (supports templates)
path = "path/to/file"       # Physical file path (supports templates)
type = "artifact-type"      # Semantic type (documentation, test-suite, implementation)
required = true|false       # Is this artifact mandatory?
description = "..."         # Human-readable purpose
validation = [              # Quality criteria
  "criterion 1",
  "criterion 2"
]
```

### Artifact Types

Semantic types enable type-based workflow composition:

- **documentation**: Human-readable specifications, guides, reports
- **formal-spec**: Machine-readable models (DSL, schema, contracts)
- **test-suite**: Test files validating specifications
- **implementation**: Working code satisfying specifications
- **validation-report**: Third-party analysis and recommendations
- **integration-guide**: Usage documentation and examples

### Input Artifacts

Steps can explicitly declare required inputs:

```toml
[[steps.inputs]]
artifact = "{{component_name}}.spec.md"
path = "{{output_dir}}/{{component_name}}.spec.md"
type = "documentation"
required = true
```

**Benefit:** Formula executor can verify all inputs exist before launching step.

---

## System Modeling Quad: Artifact Flow Example

The component-dev formula demonstrates artifact-aware workflow for the System Modeling Quad pattern.

### Phase 1: Specification Creation

**Agent:** @agent-dev-spec-modeler
**Outputs:** 2 artifacts

```toml
[[steps.outputs]]
artifact = "{{component_name}}.spec.md"
type = "documentation"
validation = [
  "File exists",
  "Contains all required sections: Overview, Features, Usage, Integration",
  "Intent clearly stated"
]

[[steps.outputs]]
artifact = "{{component_name}}.model.lisp"
type = "formal-spec"
validation = [
  "File exists",
  "Valid Lisp syntax",
  "Contains :state-schema with required fields",
  "Contains :accepts with at least one message type"
]
```

### Phase 2: Parallel Development (Isolated)

**Agents:** @agent-dev-tester + @agent-dev-coder (parallel, isolation enforced)
**Inputs:** Specifications from Phase 1
**Outputs:** Test suite + Implementation (2 artifacts)

```toml
# Test Suite Step
[[steps.inputs]]
artifact = "{{component_name}}.spec.md"
required = true

[[steps.inputs]]
artifact = "{{component_name}}.model.lisp"
required = true

[[steps.outputs]]
artifact = "{{component_name}}.test.js"
type = "test-suite"
validation = [
  "Every requirement from spec.md has a test",
  "NO test skipping"
]

# Implementation Step (parallel)
[[steps.inputs]]
artifact = "{{component_name}}.spec.md"
required = true

[[steps.inputs]]
artifact = "{{component_name}}.model.lisp"
required = true

[[steps.outputs]]
artifact = "{{component_name}}.js"
type = "implementation"
validation = [
  "All state fields from state-schema initialized",
  "All message types from :accepts are handled"
]
```

### Phase 3: Validation

**Agent:** @agent-dev-validator
**Inputs:** All 4 artifacts from previous phases
**Outputs:** Validation report (1 artifact)

```toml
# Inputs: Complete System Modeling Quad
[[steps.inputs]]
artifact = "{{component_name}}.spec.md"
type = "documentation"
required = true

[[steps.inputs]]
artifact = "{{component_name}}.model.lisp"
type = "formal-spec"
required = true

[[steps.inputs]]
artifact = "{{component_name}}.test.js"
type = "test-suite"
required = true

[[steps.inputs]]
artifact = "{{component_name}}.js"
type = "implementation"
required = true

# Output: Validation analysis
[[steps.outputs]]
artifact = "{{component_name}}-validation-report.md"
type = "validation-report"
validation = [
  "Specification compliance analysis complete",
  "Test coverage analysis complete",
  "Implementation compliance analysis complete",
  "All discrepancies categorized"
]
```

### Artifact Flow Visualization

```
Phase 1: Specifications
├─ spec.md ──────┬──────────┬──────────┐
└─ model.lisp ───┼──────────┼──────────┤
                 ↓          ↓          ↓
Phase 2: Parallel Development (Isolated)
                 ↓          ↓          ↓
        ┌────────┴──┐  ┌───┴──────┐   │
        │ test.js   │  │  impl.js │   │
        └────────┬──┘  └───┬──────┘   │
                 │         │          │
                 └────┬────┴──────────┘
                      ↓
Phase 3: Validation
                      ↓
              ┌───────────────┐
              │validation.md  │
              └───────────────┘
```

**Key Insight:** Artifacts have explicit lineage - each artifact's inputs and outputs are traceable.

---

## Artifact Validation: Quality Gates

### Validation Criteria Types

**1. Structural Validation** (File exists, correct format)
```toml
validation = [
  "File exists",
  "Valid Lisp syntax",
  "Contains required sections"
]
```

**2. Semantic Validation** (Content quality)
```toml
validation = [
  "Intent clearly stated",
  "All features documented with examples",
  "Every requirement has a test"
]
```

**3. Cross-Artifact Validation** (Consistency between artifacts)
```toml
validation = [
  "Every element from model.lisp is validated in test.js",
  "All state fields from state-schema initialized in impl.js",
  "All message types from :accepts are handled"
]
```

**4. Executable Validation** (Tests pass, builds succeed)
```toml
validation = [
  "Tests pass when executed",
  "No TODO or FIXME comments in production code"
]
```

### Quality Gates with Thresholds

Formula-level quality gates use artifact validation:

```toml
[quality_gates.standard]
test_pass_rate_min = 95
coverage_min = 90
spec_compliance_min = 100
require_user_approval = true

[quality_gates.strict]
test_pass_rate_min = 100
coverage_min = 100
spec_compliance_min = 100
performance_validation = true
security_scan = true
require_user_approval = true
```

**Usage in steps:**

```toml
[[steps.quality_gates]]
gate = "validation_thresholds"
description = "Validation must meet quality thresholds for {{validation_level}} level"
blocking = true
thresholds = "quality_gates.{{validation_level}}"
```

---

## Comparison: Current vs Artifact-Aware Approach

### Current Approach (Task-Centric)

```toml
[[steps]]
id = "test-suite"
title = "Create test suite from specifications"
agent = "@agent-dev-tester"
needs = ["specifications"]
description = """
Create tests from specs: component.spec.md, component.model.lisp
"""
```

**What we know:**
- Task name: "test-suite"
- Dependencies: Needs "specifications" step to complete
- Agent: @agent-dev-tester
- Description mentions file names

**What we DON'T know:**
- Which files are actually created?
- Where are they located?
- What makes a valid test file?
- How does next step find these files?
- What if files are incomplete?

### Artifact-Aware Approach

```toml
[[steps]]
id = "test-suite"
title = "Create specification-based test suite"
agent = "@agent-dev-tester"
needs = ["specifications"]

# EXPLICIT INPUTS
[[steps.inputs]]
artifact = "{{component_name}}.spec.md"
path = "{{output_dir}}/{{component_name}}.spec.md"
type = "documentation"
required = true

[[steps.inputs]]
artifact = "{{component_name}}.model.lisp"
path = "{{output_dir}}/{{component_name}}.model.lisp"
type = "formal-spec"
required = true

# EXPLICIT OUTPUTS
[[steps.outputs]]
artifact = "{{component_name}}.test.js"
path = "{{output_dir}}/{{component_name}}.test.js"
type = "test-suite"
required = true
description = "Comprehensive test suite validating all specification requirements"
validation = [
  "File exists",
  "Every requirement from spec.md has a test",
  "Every element from model.lisp is validated",
  "NO test skipping",
  "Tests are based purely on specifications"
]
```

**What we NOW know:**
- Exact input files required with paths
- Exact output file produced with path
- Artifact type (test-suite)
- Quality criteria (5 validation rules)
- Dependencies are artifact-based, not just task-based

### Benefits Summary

| Aspect | Task-Centric | Artifact-Aware |
|--------|-------------|----------------|
| **Traceability** | Implicit in descriptions | Explicit in schema |
| **Validation** | Manual/ad-hoc | Explicit criteria |
| **Dependencies** | Task completion | Artifact availability |
| **Debugging** | "Step failed" | "Artifact X missing/invalid" |
| **Composition** | Task-level reuse | Artifact-level reuse |
| **Documentation** | Narrative text | Structured metadata |

---

## Implementation: Formula Executor Extensions

### Artifact Registry

The formula executor maintains an artifact registry:

```javascript
class ArtifactRegistry {
  constructor() {
    this.artifacts = new Map(); // artifact_id -> ArtifactMetadata
  }

  register(stepId, artifactDef) {
    const artifact = {
      id: artifactDef.artifact,
      path: this.resolvePath(artifactDef.path),
      type: artifactDef.type,
      required: artifactDef.required,
      description: artifactDef.description,
      validation: artifactDef.validation,
      producedBy: stepId,
      status: 'pending' // pending, available, validated, invalid
    };
    this.artifacts.set(artifact.id, artifact);
    return artifact;
  }

  validate(artifactId) {
    const artifact = this.artifacts.get(artifactId);
    const results = [];

    for (const criterion of artifact.validation) {
      const result = this.evaluateCriterion(artifact, criterion);
      results.push({ criterion, passed: result.passed, details: result.details });
    }

    artifact.status = results.every(r => r.passed) ? 'validated' : 'invalid';
    return results;
  }

  checkInputsAvailable(stepId) {
    const step = this.getStep(stepId);
    const missing = [];

    for (const input of step.inputs) {
      const artifact = this.artifacts.get(input.artifact);
      if (!artifact || artifact.status !== 'validated') {
        missing.push(input.artifact);
      }
    }

    return { available: missing.length === 0, missing };
  }
}
```

### Execution Flow with Artifacts

```javascript
class FormulaExecutor {
  async executeStep(step) {
    // 1. Check inputs available
    const inputCheck = this.registry.checkInputsAvailable(step.id);
    if (!inputCheck.available) {
      throw new Error(`Step ${step.id} missing inputs: ${inputCheck.missing.join(', ')}`);
    }

    // 2. Execute step with agent
    await this.runAgent(step.agent, step.description);

    // 3. Validate outputs
    for (const outputDef of step.outputs) {
      const artifact = this.registry.register(step.id, outputDef);
      const validation = this.registry.validate(artifact.id);

      if (!validation.every(r => r.passed)) {
        console.error(`Artifact ${artifact.id} validation failed:`, validation);
        throw new Error(`Step ${step.id} produced invalid artifact: ${artifact.id}`);
      }
    }

    // 4. Check quality gates
    if (step.quality_gates) {
      await this.checkQualityGates(step.quality_gates);
    }
  }
}
```

### Artifact-Based Dependency Resolution

Instead of just task dependencies:

```javascript
// Old: Task-based
needs = ["specifications"]  // Wait for task to complete

// New: Artifact-based
needs_artifacts = [
  "{{component_name}}.spec.md",
  "{{component_name}}.model.lisp"
]  // Wait for specific artifacts to be validated
```

**Hybrid approach:** Use task dependencies for structure, artifact registry for validation.

---

## Advanced Patterns

### Pattern 1: Artifact Composition

Create workflows that compose existing artifacts:

```toml
[[steps]]
id = "integration-tests"
title = "Create integration tests for multiple components"

# Inputs from multiple previous workflows
[[steps.inputs]]
artifact = "MessageRouter.js"
type = "implementation"
required = true

[[steps.inputs]]
artifact = "CacheManager.js"
type = "implementation"
required = true

[[steps.outputs]]
artifact = "integration-suite.test.js"
type = "test-suite"
description = "Tests MessageRouter + CacheManager working together"
```

### Pattern 2: Artifact Variants

Create different variants of the same logical artifact:

```toml
# Production variant
[[steps.outputs]]
artifact = "{{component_name}}.js"
path = "src/{{component_name}}.js"
type = "implementation"
variant = "production"

# Test variant (with mocks)
[[steps.outputs]]
artifact = "{{component_name}}.js"
path = "test/mocks/{{component_name}}.js"
type = "implementation"
variant = "test-mock"
```

### Pattern 3: Artifact Lineage

Track artifact provenance:

```javascript
{
  artifact: "validation-report.md",
  producedBy: "validation",
  derivedFrom: [
    "spec.md",
    "model.lisp",
    "test.js",
    "impl.js"
  ],
  lineage: [
    "specifications -> test-suite -> validation",
    "specifications -> implementation -> validation"
  ]
}
```

### Pattern 4: Conditional Artifacts

Produce different artifacts based on workflow state:

```toml
[[steps.outputs]]
artifact = "{{component_name}}-fixes.md"
type = "documentation"
required = false
condition = "validation_result == 'REQUEST_FIXES'"
description = "Detailed fix recommendations when validation finds issues"
```

---

## Separation of Concerns: One Doc Per Concern

The artifact-aware approach reinforces the "separate docs for separate concerns" pattern the user liked from spec-creation.formula.toml:

### Concern 1: Workflow Structure (Formula File)

**File:** `component-dev.formula.toml`
**Purpose:** Declarative workflow specification
**Contains:**
- Step definitions
- Dependencies
- Artifact inputs/outputs
- Validation criteria
- Quality gates

**Benefit:** Machine-readable, version-controlled, composable

### Concern 2: Design Rationale (Design Doc)

**File:** `FORMULA_ARTIFACT_DESIGN.md` (this document)
**Purpose:** Explain the "why" and "how" of artifact-aware formulas
**Contains:**
- Problem statement
- Design principles
- Schema design
- Examples and patterns
- Implementation guidance

**Benefit:** Human-readable context for understanding and extending the approach

### Concern 3: Execution (Formula Executor)

**Component:** Formula executor engine
**Purpose:** Interpret and execute formulas
**Responsibilities:**
- Parse TOML formulas
- Resolve artifact dependencies
- Validate artifacts
- Enforce quality gates
- Coordinate agents

**Benefit:** Reusable execution infrastructure

### Concern 4: Results (Workflow Metrics)

**Output:** Metrics and observations
**Purpose:** Track workflow performance and quality
**Contains:**
- Phase durations
- Artifact validation results
- Quality gate outcomes
- Iteration counts
- Success rates

**Benefit:** Data-driven optimization

---

## Recommendations for Phase 2/3 Implementation

### Phase 2: Proof of Concept (1 Week)

**Goal:** Validate artifact-aware approach with basic execution

**Tasks:**
1. **Extend Formula Parser**
   - Parse `[[steps.outputs]]` and `[[steps.inputs]]` arrays
   - Parse validation criteria arrays
   - Support artifact templates with variable interpolation

2. **Build Artifact Registry**
   - Track artifact metadata (id, path, type, status)
   - Implement basic validation (file existence, syntax checks)
   - Support artifact status lifecycle (pending → available → validated)

3. **Modify Formula Executor**
   - Check inputs available before step execution
   - Register outputs after step completion
   - Validate artifacts against criteria
   - Report artifact status in execution logs

4. **Test with Component Development**
   - Run component-dev formula with real component
   - Validate all 4 artifacts (spec.md, model.lisp, test.js, impl.js)
   - Measure: Does artifact tracking improve debugging?
   - Measure: Are validation criteria actionable?

**Success Criteria:**
- Formula executor successfully tracks all artifacts
- Validation failures clearly identify problematic artifacts
- Artifact dependencies prevent steps from running prematurely
- Improved debugging: "Artifact X missing validation Y" vs "Step failed"

### Phase 3: Advanced Features (2 Weeks)

**Goal:** Add sophisticated artifact capabilities

**Tasks:**
1. **Semantic Validation**
   - Implement content-based validation (not just structural)
   - Parse specification files to extract requirements
   - Cross-validate artifacts (tests cover all spec requirements)
   - Executable validation (run tests, check pass rate)

2. **Artifact Lineage**
   - Track artifact provenance (derivedFrom, producedBy)
   - Visualize artifact flow through workflow
   - Enable artifact-based debugging (trace back to source)

3. **Artifact Composition**
   - Support workflows that consume artifacts from other workflows
   - Enable artifact reuse across formula variations
   - Build artifact library for common components

4. **Quality Gate Integration**
   - Use artifact validation results in quality gates
   - Support threshold-based gates (test pass rate ≥ 95%)
   - Enable conditional workflow branching based on artifact state

**Success Criteria:**
- Semantic validation catches real quality issues
- Artifact lineage enables root cause analysis
- Composed workflows reuse artifacts successfully
- Quality gates make data-driven approval decisions

### Phase 4: Production Readiness (2 Weeks)

**Goal:** Robust artifact-aware formula system

**Tasks:**
1. **Error Handling**
   - Graceful handling of missing artifacts
   - Clear error messages with remediation steps
   - Support for artifact recovery (re-run specific steps)

2. **Performance Optimization**
   - Lazy validation (only validate when needed)
   - Parallel artifact validation
   - Caching of validation results

3. **Documentation and Examples**
   - Formula author guide for artifact definitions
   - Validation criteria catalog (common patterns)
   - Example formulas demonstrating artifact patterns

4. **Integration with Beads**
   - Generate beads with artifact tracking
   - Update bead status based on artifact validation
   - Link beads to artifacts in task descriptions

**Success Criteria:**
- Production-ready artifact-aware formula system
- Formula authors can easily define artifacts and validation
- Integration with beads provides seamless tracking
- Team adopts artifact-aware formulas as standard practice

---

## Metrics: How to Measure Success

### Formula Quality Metrics

**Before Artifact-Aware (Baseline):**
- Workflow failures with vague errors: "Step failed"
- Manual artifact checking: Developer inspects files manually
- Debugging time: Average X minutes to identify problematic artifact

**After Artifact-Aware (Target):**
- Workflow failures with specific errors: "Artifact X missing validation criterion Y"
- Automatic artifact validation: Formula executor validates all criteria
- Debugging time: Reduced by 50% due to precise error messages

### Artifact Validation Metrics

Track validation effectiveness:
- **Validation Coverage**: % of artifacts with explicit validation criteria
- **Validation Accuracy**: % of validation failures that identify real issues
- **Validation Completeness**: % of artifact quality aspects covered by criteria
- **False Positives**: Validation failures that aren't real issues (minimize)
- **False Negatives**: Quality issues missed by validation (minimize)

### Workflow Composition Metrics

Track reusability gains:
- **Artifact Reuse Rate**: % of artifacts used in multiple workflows
- **Composition Success Rate**: % of composed workflows that execute successfully
- **Time to Compose**: Time to create new workflow from existing artifacts

### Developer Experience Metrics

Survey formula authors and users:
- **Clarity**: Are artifact definitions clear and actionable?
- **Debugging Speed**: How quickly can failures be diagnosed?
- **Confidence**: How confident are developers in workflow results?
- **Adoption**: What % of new formulas use artifact tracking?

---

## Conclusion

**Artifact-aware formulas** elevate workflow orchestration from task-centric to data-centric:

1. **Artifacts as First-Class Citizens**: Explicit tracking, validation, lineage
2. **Quality Gates Based on Artifact State**: Data-driven approval decisions
3. **Improved Debugging**: Precise error messages identifying problematic artifacts
4. **Enhanced Composition**: Reuse artifacts across workflow variations
5. **Self-Documenting Workflows**: Structured metadata replaces narrative descriptions

**The component-dev formula demonstrates this approach** with the System Modeling Quad pattern:
- 4 artifacts explicitly tracked (spec.md, model.lisp, test.js, impl.js)
- Validation criteria for each artifact (structural, semantic, cross-artifact)
- Quality gates based on validation results
- Clear artifact flow from specifications → parallel dev → validation

**Next Steps:**
1. Build proof-of-concept formula executor with artifact registry
2. Test with real component development workflow
3. Measure debugging improvements and validation effectiveness
4. Iterate based on findings
5. Expand to other workflow patterns (system-dev, brownfield-spec)

**This approach transforms formulas from documentation to executable infrastructure**, bringing the benefits of data lineage and quality validation to multi-agent workflows.

---

**Design Complete**
**Ready for Implementation**
