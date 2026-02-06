# Spec-Kit Formula Design Decisions

**Project**: GitHub spec-kit formula implementation
**Analysis Date**: 2026-01-11
**Purpose**: Document design decisions, rationale, and integration recommendations

## Executive Summary

This document explains the design decisions made in creating the spec-kit workflow formula and artifact schema for the beads ecosystem. The formula captures GitHub's Specification-Driven Development (SDD) methodology as an executable workflow with first-class artifact tracking.

## Design Decision Log

### Decision 1: Single Comprehensive Formula vs Multiple Formulas

**Question**: Should spec-kit be one formula or multiple (e.g., spec-kit-full, spec-kit-clarify, spec-kit-simple)?

**Decision**: **Single comprehensive formula** with conditional steps

**Rationale**:
1. **Workflow cohesion**: All 6 stages are tightly coupled with strict dependencies
2. **Artifact flow**: Each stage depends on outputs from previous stages
3. **Constitutional governance**: Constitution applies across entire workflow
4. **User experience**: Single invocation simpler than orchestrating multiple formulas
5. **Conditional execution**: TOML supports `condition` field for optional stages

**Implementation**:
```toml
[[steps]]
id = "clarification"
needs = ["specification"]
condition = "has_clarifications == true"  # Optional stage
description = "Resolve specification ambiguities..."

[[steps]]
id = "planning"
needs = ["specification", "clarification"]
needs_any = true  # Either spec or clarified spec
```

**Trade-offs**:
- ✓ Single source of truth
- ✓ Easier maintenance
- ✓ Complete workflow visibility
- ✗ Larger formula file (550 lines)
- ✗ Cannot easily run Planning stage standalone

**Alternative considered**: Modular formulas (spec-kit-spec, spec-kit-plan, spec-kit-impl)
- Would enable standalone execution of individual stages
- Would complicate artifact dependency management
- Would require manual orchestration
- Rejected: Cohesion > Modularity for this workflow

---

### Decision 2: How to Represent Constitutional Articles

**Question**: How should the 9 constitutional articles be represented in the formula?

**Decision**: **Hybrid approach** - Variables for thresholds + Quality gates for validation

**Rationale**:
1. **Parameterization**: Thresholds (max_projects) vary by project
2. **Validation**: Gates enforce compliance at runtime
3. **Documentation**: Quality gate descriptions explain each article
4. **Flexibility**: Users can override thresholds via variables

**Implementation**:
```toml
# Threshold as variable (Article VII: Simplicity)
[vars.max_projects]
description = "Constitutional constraint: maximum initial projects (Simplicity Gate)"
default = "3"

# Validation as quality gate
[[steps.quality_gates]]
gate = "constitutional_compliance"
description = "Plan must comply with constitutional gates or justify violations"
checks = [
  "Simplicity Gate: ≤{{max_projects}} projects OR justified",
  "Anti-Abstraction Gate: Framework used directly",
  "Integration-First Gate: Contracts defined"
]
```

**Trade-offs**:
- ✓ Parameterizable thresholds
- ✓ Explicit validation rules
- ✓ User-visible constraints
- ✗ Not a full rule engine (checks are strings, not executable)

**Alternative considered**: Embed constitution.md template in formula
- Would duplicate content (template exists separately)
- Would reduce flexibility (hard to customize articles)
- Rejected: Reference > Embed for templates

**Alternative considered**: External rule engine
- Would enable complex boolean logic
- Would require additional dependency
- Rejected: KISS principle - string checks sufficient for v1

---

### Decision 3: Artifact Attachment Syntax/Schema

**Question**: What syntax should formulas use to declare artifact inputs/outputs?

**Decision**: **TOML array tables** (`[[steps.inputs]]`, `[[steps.outputs]]`, `[[steps.quality_gates]]`)

**Rationale**:
1. **Consistency**: Matches existing `[[steps]]` pattern from component-dev.formula.toml
2. **First-class artifacts**: Elevates artifacts to workflow primitives
3. **Validation**: Each artifact can declare validation rules
4. **Type system**: Artifact types enable tool understanding
5. **Documentation**: Inline descriptions explain purpose

**Implementation**:
```toml
[[steps]]
id = "planning"

# Input artifacts
[[steps.inputs]]
artifact = "spec.md"
path = "{{spec_dir}}/specs/###-feature-name/spec.md"
type = "specification"
required = true

# Output artifacts
[[steps.outputs]]
artifact = "plan.md"
path = "{{spec_dir}}/specs/###-feature-name/plan.md"
type = "technical-plan"
required = true
description = "Technical implementation plan..."
validation = [
  "File exists",
  "Technical context complete",
  "Constitutional gates checked"
]

# Quality gates
[[steps.quality_gates]]
gate = "constitutional_compliance"
blocking = true
checks = [...]
```

**Schema Fields**:
- `artifact`: Logical name (human-readable)
- `path`: File path (supports variable interpolation)
- `type`: Artifact type from taxonomy
- `required`: Boolean or variable expression
- `description`: Human-readable purpose
- `validation`: Array of validation rules (strings)

**Trade-offs**:
- ✓ Explicit declaration
- ✓ Validation rules co-located
- ✓ Tool parseable
- ✗ Verbose (3 arrays per step)
- ✗ No automatic dependency inference

**Alternative considered**: Implicit artifacts (only in description)
- Simpler syntax (no extra TOML)
- Harder for tools to parse
- No validation specification
- Rejected: Explicit > Implicit for tooling

**Alternative considered**: Separate artifacts.toml file
- Centralized artifact definitions
- Harder to see inputs/outputs per step
- More files to maintain
- Rejected: Co-location > Separation

---

### Decision 4: How Validation Gates Attach Quality Metrics

**Question**: How should quality gates reference metrics and thresholds?

**Decision**: **Reference to quality_gates sections** via `thresholds` field

**Rationale**:
1. **DRY**: Thresholds defined once, referenced many times
2. **Validation levels**: Standard vs Strict configurations
3. **Clarity**: Separation of what to check vs acceptable values
4. **Parameterization**: Thresholds can vary by `{{validation_level}}` variable

**Implementation**:
```toml
# Define thresholds
[quality_gates.standard]
specification_quality_min = 90
constitutional_compliance_min = 100
task_completeness_min = 95
test_pass_rate_min = 95

[quality_gates.strict]
specification_quality_min = 100
constitutional_compliance_min = 100
task_completeness_min = 100
test_pass_rate_min = 100

# Reference in gate
[[steps.quality_gates]]
gate = "specification_quality"
blocking = true
thresholds = "quality_gates.{{validation_level}}"
checks = [
  "No implementation details",
  "Requirements testable"
]
```

**Trade-offs**:
- ✓ Reusable threshold definitions
- ✓ Multiple validation levels
- ✓ Clear separation of concerns
- ✗ Indirection (gate → thresholds → values)

**Alternative considered**: Inline thresholds in each gate
```toml
[[steps.quality_gates]]
gate = "specification_quality"
specification_quality_min = 90
```
- More direct
- Harder to maintain consistency
- No validation level abstraction
- Rejected: DRY > Direct

---

### Decision 5: How [P] Parallel Markers Translate to Formula Steps

**Question**: How should parallel task execution opportunities be represented in formulas?

**Decision**: **Parallel description + Documentation** (no explicit scheduling)

**Rationale**:
1. **Simplicity**: Parallel opportunities documented in step descriptions
2. **Human-readable**: Clear explanation of what can run in parallel
3. **No scheduler**: Formula doesn't dictate execution (agent/tool decides)
4. **Flexibility**: Implementation can choose sequential or parallel

**Implementation**:
```toml
[[steps]]
id = "planning"
description = """
Phase 1 - Design (can run in parallel):
5. Design data models → data-model.md
6. Generate API contracts → contracts/
7. Create validation scenarios → quickstart.md
"""

# All three outputs can be generated in parallel
[[steps.outputs]]
artifact = "data-model.md"
# ...

[[steps.outputs]]
artifact = "contracts"
# ...

[[steps.outputs]]
artifact = "quickstart.md"
# ...
```

**For tasks.md**:
```toml
[[steps]]
id = "implementation"
description = """
6. Respect [P] markers for parallel execution
7. Follow TDD: tests before implementation
"""

# tasks.md artifact contains [P] markers
# Formula doesn't interpret them (agent does)
```

**Trade-offs**:
- ✓ Simple formula structure
- ✓ Flexible execution
- ✓ Human-readable
- ✗ No automatic parallel scheduling
- ✗ Parallel opportunities not machine-parseable

**Alternative considered**: Explicit parallel_with field
```toml
[[steps]]
id = "generate-data-model"
parallel_with = ["generate-contracts", "generate-quickstart"]
```
- Machine-parseable
- Explicit parallelism
- More complex formula (one step per parallel task)
- Rejected: Simplicity > Explicit parallelism (v1)

**Alternative considered**: Parallel step groups
```toml
[[step_group]]
parallel = true
steps = ["generate-data-model", "generate-contracts", "generate-quickstart"]
```
- Clean grouping
- Not supported in current formula schema
- Rejected: Requires schema extension

**Recommendation for future**: Add `parallel_with` field in formula schema v2

---

### Decision 6: Artifact Schema Extensions for Beads

**Question**: How should beads track artifact state and validation?

**Decision**: **Extend issue YAML with artifact section** (proposed)

**Rationale**:
1. **Integration**: Artifacts are part of issue state
2. **Traceability**: Link artifacts to specific issues/steps
3. **Validation tracking**: Record validation status per artifact
4. **Immutability**: Artifact hashes ensure integrity

**Proposed Schema**:
```yaml
# .beads/issues/ISSUE-001.yaml
id: ISSUE-001
title: "Create specification for real-time chat"
status: in_progress
formula: spec-kit-workflow
formula_step: specification

artifacts:
  inputs:
    - artifact: constitution.md
      path: .specify/memory/constitution.md
      type: governance
      status: exists
      hash: abc123...

  outputs:
    - artifact: spec.md
      path: specs/001-real-time-chat/spec.md
      type: specification
      status: draft
      validation:
        - check: "No implementation details"
          status: pass
        - check: "Requirements testable"
          status: pass

  quality_gates:
    - gate: specification_quality
      status: pending
```

**Benefits**:
- ✓ Complete artifact provenance
- ✓ Validation status tracked
- ✓ Hash-based integrity
- ✓ Query artifacts by type/status

**Trade-offs**:
- ✗ Larger issue files
- ✗ Requires beads schema extension
- ✗ Manual tracking if not automated

**Alternative considered**: Separate artifact database
- Centralized artifact tracking
- Complex to maintain consistency with issues
- Rejected: Co-location > Centralization

---

### Decision 7: Template Artifacts - Reference vs Embed

**Question**: Should templates be embedded in formulas or referenced externally?

**Decision**: **Reference external templates** (not embedded)

**Rationale**:
1. **Reusability**: Templates used across multiple formulas
2. **Maintainability**: Single source of truth
3. **Size**: Templates are large (spec-template.md is 116 lines)
4. **Flexibility**: Users can customize templates independently
5. **Separation of concerns**: Formulas orchestrate, templates constrain

**Implementation**:
```toml
[[steps]]
id = "specification"
description = """
Transform feature description into structured specification.

Input:
- feature_description: {{feature_description}}
- constitution.md
- spec-template.md  # Referenced, not embedded
"""
```

**Template location**: `spec-kit/templates/spec-template.md` (external)

**Trade-offs**:
- ✓ DRY (single template source)
- ✓ Smaller formula files
- ✓ Independent template evolution
- ✗ External dependency
- ✗ Template location must be known

**Alternative considered**: Embed templates in formula
```toml
[templates.spec]
content = """
# Feature Specification: [FEATURE NAME]
...
"""
```
- Self-contained formula
- Large formula file (>1000 lines)
- Hard to maintain
- Rejected: Reference > Embed

---

### Decision 8: Variable Interpolation Syntax

**Question**: What syntax should be used for variable interpolation in paths/descriptions?

**Decision**: **Mustache-style `{{variable_name}}`**

**Rationale**:
1. **Consistency**: Matches existing beads/formula conventions
2. **Readability**: Clear visual distinction from literal text
3. **Tooling**: Standard template syntax, widely supported
4. **Nesting**: Supports nested references `quality_gates.{{validation_level}}`

**Implementation**:
```toml
[vars.spec_dir]
default = ".specify"

[[steps.outputs]]
artifact = "spec.md"
path = "{{spec_dir}}/specs/###-feature-name/spec.md"  # Variable interpolation
type = "specification"

[[steps.quality_gates]]
thresholds = "quality_gates.{{validation_level}}"  # Nested reference
```

**Special case**: `###-feature-name`
- Not a variable (no braces)
- Placeholder for script-generated name
- Convention from spec-kit scripts (create-new-feature.sh)

**Trade-offs**:
- ✓ Standard syntax
- ✓ Clear visual distinction
- ✓ Tool-parseable
- ✗ Manual interpolation required (no automatic)

**Alternative considered**: Shell-style `$variable_name`
- Common in bash
- Less visual distinction
- Rejected: Mustache > Shell for templates

---

### Decision 9: Validation Rule Representation

**Question**: How should validation rules be represented - strings vs structured data?

**Decision**: **String arrays for v1** (with recommendation for structured v2)

**Rationale**:
1. **Simplicity**: Easy to read and write
2. **Human-readable**: Natural language descriptions
3. **Flexibility**: Free-form text, no schema constraints
4. **Gradual typing**: Can evolve to structured format later

**Implementation**:
```toml
[[steps.outputs]]
artifact = "spec.md"
validation = [
  "File exists",
  "No implementation details (tech stack, frameworks, APIs)",
  "User stories prioritized (P1, P2, P3)",
  "Requirements testable (FR-001, FR-002, ...)",
  "Success criteria measurable and tech-agnostic"
]
```

**Trade-offs**:
- ✓ Simple syntax
- ✓ Human-readable
- ✓ No schema enforcement needed
- ✗ Not machine-executable (requires NLP or regex)
- ✗ No type safety

**Alternative considered**: Structured validation rules
```toml
[[steps.outputs.validation]]
check = "file_exists"
path = "{{spec_dir}}/specs/###-feature-name/spec.md"

[[steps.outputs.validation]]
check = "contains_section"
section = "User Scenarios & Testing"

[[steps.outputs.validation]]
check = "regex_absent"
pattern = "(Express|React|PostgreSQL|AWS)"
reason = "No implementation details allowed"
```
- Machine-executable
- Type-safe
- More verbose
- Requires validation engine
- Recommended for v2

**Recommendation**: Start with strings, evolve to structured as validation automation matures

---

### Decision 10: Metrics Definition and Collection

**Question**: How should workflow metrics be defined in formulas?

**Decision**: **Declarative metrics section** (documentation, not enforcement)

**Rationale**:
1. **Visibility**: Makes observable outcomes explicit
2. **Documentation**: Explains what should be measured
3. **Guidance**: Helps tools know what to collect
4. **No enforcement**: Metrics are optional, not required for workflow completion

**Implementation**:
```toml
[metrics]
constitution_duration = "Time to create and approve constitution"
specification_duration = "Time to create and approve specification"
total_duration = "Time from epic creation to implementation approval"

[metrics.quality]
specification_quality_score = "Specification quality score (0-100)"
test_pass_rate = "Percentage of tests passing"

[metrics.artifacts]
user_story_count = "Number of user stories defined"
task_count = "Total number of tasks generated"
```

**Trade-offs**:
- ✓ Explicit expectations
- ✓ Tool guidance
- ✓ No collection overhead
- ✗ Not automatically collected
- ✗ No validation that metrics are recorded

**Alternative considered**: Enforce metrics collection
```toml
[metrics.total_duration]
required = true
collector = "time_tracker"
```
- Automatic collection
- Requires integration with time tracking
- Adds complexity
- Rejected: Documentation > Enforcement (v1)

---

## Integration with Existing Formulas

### Comparison: spec-kit-workflow vs component-dev

**Common Patterns**:
1. Both use `[[steps.outputs]]` for artifact declaration
2. Both use `validation` arrays
3. Both use `quality_gates` for validation
4. Both support `required` flag for conditional artifacts
5. Both use `type` for artifact classification

**Differences**:

| Aspect | spec-kit-workflow | component-dev |
|--------|-------------------|---------------|
| Stages | 6 sequential stages | 4 phases (some parallel) |
| Governance | Constitutional (9 articles) | None (ad-hoc) |
| Artifacts | 15+ types | 5 types (spec, model, test, impl, report) |
| Parallelism | Documentation-based | Explicit `parallel_with` |
| Validation | Multi-level (standard/strict) | Single level |
| User approval | Multiple gates | Two gates (spec, final) |
| TDD | Constitutional (Article III) | Optional |

**Harmonization Opportunities**:
1. **Artifact types**: Align type taxonomy (spec, model, test, implementation)
2. **Validation schema**: Standardize validation rule format
3. **Quality gates**: Unify gate declaration syntax
4. **Metrics**: Common metrics section format

---

### Comparison: spec-kit-workflow vs spec-creation

**Common Patterns**:
1. Both create specifications
2. Both support parallel execution
3. Both use epic → steps hierarchy
4. Both produce multiple artifact types

**Differences**:

| Aspect | spec-kit-workflow | spec-creation |
|--------|-------------------|---------------|
| Scope | Full SDD (spec → code) | Specs only (BDD, State, FIT) |
| Governance | Constitutional | None |
| Implementation | Included (stage 6) | Not included |
| Organization | User story-based | Type-based (BDD, State, FIT) |
| Artifacts | 15+ types | 3 types (feature, state-machine, fit) |

**Complementarity**:
- spec-creation could be **stage 2b** of spec-kit-workflow
- Add comprehensive specs (BDD, State, FIT) to spec.md
- Reuse spec-creation agents for richer specifications

**Integration Opportunity**:
```toml
[[steps]]
id = "specification"
# Option 1: Simple spec (current)
# Option 2: Comprehensive spec (invoke spec-creation formula)
advanced_specs = "{{use_bdd_state_fit}}"  # Variable to choose
```

---

## Recommendations for Beads Formula Schema

### Extension 1: First-Class Artifact Support

**Add to beads formula schema**:
```toml
[[steps.inputs]]
artifact = "artifact-name"
path = "path/to/artifact"
type = "artifact-type"
required = true|false
validation = [...]

[[steps.outputs]]
artifact = "artifact-name"
path = "path/to/artifact"
type = "artifact-type"
required = true|false
description = "Human-readable purpose"
validation = [...]
```

**Benefits**:
- Explicit artifact dependencies
- Validation specification
- Tool-parseable inputs/outputs
- Type-based queries

---

### Extension 2: Parallel Execution Markers

**Add to beads formula schema**:
```toml
[[steps]]
id = "generate-data-model"
parallel_with = ["generate-contracts", "generate-quickstart"]
```

**Benefits**:
- Machine-parseable parallelism
- Automatic parallel scheduling
- Performance optimization opportunities

---

### Extension 3: Conditional Execution

**Already supported** (good!):
```toml
[[steps]]
id = "clarification"
condition = "has_clarifications == true"
```

**Enhancement opportunity**: Support expression evaluation
```toml
condition = "{{validation_level}} == 'strict' AND test_count > 0"
```

---

### Extension 4: Structured Validation Rules

**Add to beads formula schema**:
```toml
[[steps.outputs.validation]]
type = "file_exists"
path = "{{artifact_path}}"

[[steps.outputs.validation]]
type = "contains_section"
section = "User Scenarios & Testing"

[[steps.outputs.validation]]
type = "regex_absent"
pattern = "(Express|React|PostgreSQL)"
message = "No implementation details allowed"
```

**Benefits**:
- Machine-executable validation
- Type-safe checks
- Automated validation reports

---

### Extension 5: Quality Gate Thresholds

**Already supported** (good!):
```toml
[quality_gates.standard]
specification_quality_min = 90

[quality_gates.strict]
specification_quality_min = 100

[[steps.quality_gates]]
thresholds = "quality_gates.{{validation_level}}"
```

**Enhancement opportunity**: Support threshold expressions
```toml
[[steps.quality_gates]]
thresholds = "quality_gates.{{validation_level}}"
overrides = { test_pass_rate_min = "{{user_defined_threshold}}" }
```

---

## Design Philosophy

### Principles Applied

1. **Explicit over Implicit**
   - Artifact inputs/outputs declared explicitly
   - Validation rules visible in formula
   - Quality gates clear about requirements

2. **Declarative over Imperative**
   - Formula declares what should happen, not how
   - Agent/tool decides execution details
   - Parallel opportunities documented, not scheduled

3. **Separation of Concerns**
   - Templates constrain structure
   - Formulas orchestrate workflow
   - Agents execute steps
   - Quality gates validate outcomes

4. **Progressive Disclosure**
   - Simple features use simple paths (no clarification)
   - Complex features get full workflow (clarification, multiple gates)
   - Validation levels (standard vs strict) adapt to needs

5. **Constitutional Governance**
   - Immutable principles (Article III: Test-First)
   - Enforceable constraints (Article VII: Simplicity)
   - Justifiable violations (complexity tracking)

6. **First-Class Artifacts**
   - Artifacts are workflow primitives
   - Types enable tool understanding
   - Validation ensures quality
   - Dependencies explicit

7. **DRY (Don't Repeat Yourself)**
   - Templates referenced, not embedded
   - Thresholds defined once, referenced many
   - Artifact types reused across formulas

8. **YAGNI (You Aren't Gonna Need It)**
   - No complex scheduling (v1)
   - No full rule engine (v1)
   - String validation rules (not structured)
   - Can evolve later based on actual needs

---

## Future Enhancements

### Priority 1: Validation Automation

**Goal**: Enable programmatic validation of artifacts

**Approach**:
1. Extend validation rule syntax to structured format
2. Implement validation engine in beads CLI
3. Auto-generate validation reports
4. Fail fast on validation errors

**Example**:
```bash
bd formula validate spec-kit-workflow --step specification
# Checks all validation rules for spec.md
# Outputs: PASS/FAIL with details
```

---

### Priority 2: Artifact State Tracking

**Goal**: Track artifact lifecycle in beads issues

**Approach**:
1. Extend issue YAML schema with `artifacts` section
2. Record artifact state (planned → draft → validated → approved)
3. Track validation status per artifact
4. Enable artifact-based queries

**Example**:
```bash
bd artifacts list --type specification --status validated
# Lists all validated specifications across issues
```

---

### Priority 3: Parallel Execution Engine

**Goal**: Automatically execute parallel tasks

**Approach**:
1. Extend formula schema with `parallel_with` field
2. Build task scheduler respecting dependencies
3. Execute parallel tasks concurrently
4. Aggregate results

**Example**:
```bash
bd formula execute spec-kit-workflow --parallel
# Automatically runs parallel tasks concurrently
# Reports speedup factor
```

---

### Priority 4: Template Registry

**Goal**: Centralized template management

**Approach**:
1. Create template repository (local or remote)
2. Version templates (spec-template.md v1.0, v2.0)
3. Formula references templates by name+version
4. Auto-download templates if missing

**Example**:
```toml
[[steps]]
id = "specification"
template = "spec-template@1.0"  # Name + version
```

---

### Priority 5: Metrics Dashboard

**Goal**: Visualize workflow metrics

**Approach**:
1. Collect metrics during workflow execution
2. Store in beads database
3. Generate dashboard showing:
   - Phase durations
   - Quality scores
   - Artifact counts
   - Parallel efficiency

**Example**:
```bash
bd metrics dashboard spec-kit-workflow
# Opens web dashboard with charts
```

---

## Conclusion

The spec-kit formula design captures GitHub's SDD methodology as an executable workflow with comprehensive artifact tracking. Key design decisions prioritize:

1. **Explicitness**: Artifact dependencies declared, not inferred
2. **Validation**: Quality gates at every stage
3. **Flexibility**: Conditional steps, validation levels
4. **Governance**: Constitutional principles enforced
5. **Reusability**: Templates referenced, types standardized
6. **Simplicity**: String validation rules (v1), structured later (v2)

The design integrates well with existing formulas (component-dev, spec-creation) and provides a foundation for future enhancements (validation automation, artifact tracking, parallel execution).

**Deliverables**:
1. ✅ spec-kit-workflow.formula.toml (550 lines, complete workflow)
2. ✅ SPEC_KIT_ARTIFACT_SCHEMA.md (comprehensive artifact documentation)
3. ✅ SPEC_KIT_FORMULA_EXAMPLES.md (5 concrete examples)
4. ✅ SPEC_KIT_FORMULA_DESIGN.md (this document)

**Next Steps**:
1. Validate formula with actual spec-kit execution
2. Implement validation automation (Priority 1)
3. Extend beads schema for artifact tracking (Priority 2)
4. Collect feedback from real-world usage
5. Iterate based on learnings

This design serves as both a working implementation and a blueprint for formula-based workflow orchestration in the beads ecosystem.
