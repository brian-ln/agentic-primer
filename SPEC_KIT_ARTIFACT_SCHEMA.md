# Spec-Kit Artifact Schema

**Project**: GitHub spec-kit
**Analysis Date**: 2026-01-11
**Purpose**: Define artifact types, attachments, and information flow for spec-kit workflow

## Executive Summary

This document defines how artifacts attach to the spec-kit workflow, creating a complete information architecture from natural language feature descriptions to working code. Each workflow stage produces specific artifacts that serve as inputs to subsequent stages, with validation artifacts ensuring quality at each gate.

## Artifact Classification

### 1. Template Artifacts (Immutable Constraints)

**Purpose**: Constrain LLM behavior and enforce structural patterns

| Artifact | Type | Location | Purpose |
|----------|------|----------|---------|
| constitution.md (template) | governance-template | templates/commands/constitution.md | Defines Articles I-IX structure |
| spec-template.md | specification-template | templates/spec-template.md | Enforces WHAT vs HOW separation |
| plan-template.md | planning-template | templates/plan-template.md | Enforces constitutional gates |
| tasks-template.md | task-template | templates/tasks-template.md | Enforces user story organization |
| checklist-template.md | validation-template | templates/checklist-template.md | Quality self-review framework |

**Characteristics**:
- Immutable during workflow execution
- Contain explicit constraints ("NO implementation details")
- Include checklists for self-validation
- Force specific markers ([NEEDS CLARIFICATION], [P], [US#])
- Provide structural scaffolding for LLM outputs

### 2. Generated Artifacts (Mutable State)

**Purpose**: Information products created during workflow execution

#### Stage 1: Constitution
| Artifact | Type | Path | Content |
|----------|------|------|---------|
| constitution.md | governance | .specify/memory/constitution.md | 9 Articles, validation gates, governance rules |

#### Stage 2: Specification
| Artifact | Type | Path | Content |
|----------|------|------|---------|
| spec.md | specification | specs/###-feature-name/spec.md | User stories (P1-P3), requirements (FR-001+), success criteria, entities |
| requirements-checklist.md | validation-checklist | specs/###-feature-name/checklists/requirements.md | Quality validation items |

#### Stage 3: Clarification (Optional)
| Artifact | Type | Path | Content |
|----------|------|------|---------|
| spec.md (updated) | specification | specs/###-feature-name/spec.md | Resolved [NEEDS CLARIFICATION] markers, clarifications documented |

#### Stage 4: Planning
| Artifact | Type | Path | Content |
|----------|------|------|---------|
| plan.md | technical-plan | specs/###-feature-name/plan.md | Technical context, constitutional gates, structure, complexity tracking |
| research.md | research-documentation | specs/###-feature-name/research.md | Technology decisions, rationale, alternatives |
| data-model.md | data-model | specs/###-feature-name/data-model.md | Entities, relationships, validation rules, state transitions |
| contracts/ | api-contracts | specs/###-feature-name/contracts/ | OpenAPI, GraphQL, WebSocket, Event schemas |
| quickstart.md | validation-scenarios | specs/###-feature-name/quickstart.md | Validation scenarios, integration tests, manual testing |

#### Stage 5: Tasking
| Artifact | Type | Path | Content |
|----------|------|------|---------|
| tasks.md | task-breakdown | specs/###-feature-name/tasks.md | Phases, task IDs, dependencies, parallel markers [P] |

#### Stage 6: Implementation
| Artifact | Type | Path | Content |
|----------|------|------|---------|
| src/ | source-code | specs/###-feature-name/src/ | Implementation files per plan.md structure |
| tests/ | test-suite | specs/###-feature-name/tests/ | Test files (if TDD) |
| tasks.md (updated) | task-tracking | specs/###-feature-name/tasks.md | Completed tasks marked [X] |

### 3. Validation Artifacts (Quality Gates)

**Purpose**: Enable quality validation at each stage

| Artifact | Stage | Type | Content |
|----------|-------|------|---------|
| requirements-checklist.md | Specification | validation-checklist | Quality checks for spec.md |
| constitutional-compliance-report | Planning | validation-report | Gate compliance (Simplicity, Anti-Abstraction, Integration-First) |
| task-validation-report | Tasking | validation-report | Task format, completeness, dependencies |
| test-results | Implementation | test-execution-report | Test pass/fail, coverage |

## Artifact Dependency Graph

### Information Flow (DAG)

```
Templates (immutable)
    |
    v
constitution.md
    |
    +----------------------+
    |                      |
    v                      v
spec.md <-- spec-template.md
    |
    v
spec.md (clarified) [optional]
    |
    +-------------------+-------------------+-------------------+
    |                   |                   |                   |
    v                   v                   v                   v
plan.md <--     research.md         data-model.md         contracts/
plan-template.md        ^                   ^                   ^
    |                   |                   |                   |
    +-------------------+-------------------+-------------------+
    |                                       |
    v                                       v
quickstart.md                       tasks.md <-- tasks-template.md
                                        |
                                        v
                                    src/ + tests/
                                        |
                                        v
                                    Working Code
```

### Dependency Matrix

| Output Artifact | Depends On (Inputs) | Template Used |
|----------------|---------------------|---------------|
| constitution.md | User input (project values) | constitution.md (template) |
| spec.md | Feature description, constitution.md | spec-template.md |
| spec.md (clarified) | spec.md, user answers | N/A (interactive) |
| plan.md | spec.md, constitution.md, tech stack | plan-template.md |
| research.md | spec.md [NEEDS CLARIFICATION], tech choices | N/A |
| data-model.md | spec.md entities | N/A |
| contracts/ | spec.md requirements | N/A (OpenAPI, GraphQL templates) |
| quickstart.md | spec.md, plan.md | N/A |
| tasks.md | plan.md, spec.md, data-model.md, contracts/ | tasks-template.md |
| src/ | tasks.md, plan.md, spec.md, all design docs | N/A |
| tests/ | tasks.md, spec.md (if TDD) | N/A |

## Artifact Attachment Schema

### Step-Level Artifact Declarations

Each workflow step declares artifacts using three attachment types:

#### 1. Input Artifacts (steps.inputs)

**Purpose**: Declare required/optional artifacts needed by this step

**Schema**:
```toml
[[steps.inputs]]
artifact = "artifact-name"
path = "relative/or/variable/path"
type = "artifact-type"
required = true|false
validation = ["check1", "check2", ...]
```

**Example** (from Planning stage):
```toml
[[steps.inputs]]
artifact = "spec.md"
path = "{{spec_dir}}/specs/###-feature-name/spec.md"
type = "specification"
required = true

[[steps.inputs]]
artifact = "constitution.md"
path = "{{spec_dir}}/memory/constitution.md"
type = "governance"
required = true
```

#### 2. Output Artifacts (steps.outputs)

**Purpose**: Declare artifacts produced by this step

**Schema**:
```toml
[[steps.outputs]]
artifact = "artifact-name"
path = "relative/or/variable/path"
type = "artifact-type"
required = true|false
description = "Human-readable purpose"
validation = ["check1", "check2", ...]
```

**Example** (from Planning stage):
```toml
[[steps.outputs]]
artifact = "plan.md"
path = "{{spec_dir}}/specs/###-feature-name/plan.md"
type = "technical-plan"
required = true
description = "Technical implementation plan with context, gates, structure"
validation = [
  "File exists",
  "Technical context complete",
  "Constitutional gates checked",
  "Project structure defined"
]

[[steps.outputs]]
artifact = "data-model.md"
path = "{{spec_dir}}/specs/###-feature-name/data-model.md"
type = "data-model"
required = false  # Only if feature involves data
description = "Entity definitions, relationships, validation rules"
validation = [
  "File exists (if feature involves data)",
  "All entities from spec.md defined",
  "Relationships documented"
]
```

#### 3. Quality Gate Artifacts (steps.quality_gates)

**Purpose**: Declare validation requirements before proceeding

**Schema**:
```toml
[[steps.quality_gates]]
gate = "gate-name"
description = "Human-readable gate purpose"
blocking = true|false
thresholds = "quality_gates.validation_level"  # Optional
checks = ["check1", "check2", ...]  # Optional
```

**Example** (from Planning stage):
```toml
[[steps.quality_gates]]
gate = "constitutional_compliance"
description = "Plan must comply with constitutional gates or justify violations"
blocking = true
thresholds = "quality_gates.{{validation_level}}"
checks = [
  "Simplicity Gate: ≤{{max_projects}} projects OR justified",
  "Anti-Abstraction Gate: Framework used directly",
  "Integration-First Gate: Contracts defined"
]

[[steps.quality_gates]]
gate = "user_approval"
description = "User must approve plan before proceeding"
blocking = true
```

## Artifact Types (Taxonomy)

### Primary Types

| Type | Purpose | Examples |
|------|---------|----------|
| governance | Constitutional principles and rules | constitution.md |
| governance-template | Template for governance documents | constitution.md (template) |
| specification | Tech-agnostic feature descriptions | spec.md |
| specification-template | Template for specifications | spec-template.md |
| technical-plan | Implementation plan with tech stack | plan.md |
| planning-template | Template for plans | plan-template.md |
| research-documentation | Technology decisions and rationale | research.md |
| data-model | Entity definitions and relationships | data-model.md |
| api-contracts | API specifications in standard formats | contracts/ (OpenAPI, GraphQL) |
| validation-scenarios | Test scenarios and quickstart guides | quickstart.md |
| task-breakdown | Executable task list with dependencies | tasks.md |
| task-template | Template for task lists | tasks-template.md |
| task-tracking | Updated task list with completion status | tasks.md (updated) |
| source-code | Implementation files | src/ |
| test-suite | Test files | tests/ |
| validation-checklist | Quality validation items | checklists/ |
| validation-report | Quality gate results | constitutional-compliance-report |
| test-execution-report | Test pass/fail results | test-results |

### Meta Types

| Meta Type | Purpose | Examples |
|-----------|---------|----------|
| template | Immutable constraints | *-template.md |
| generated | Mutable workflow outputs | spec.md, plan.md, tasks.md |
| validation | Quality gate artifacts | checklists/, reports |
| executable | Can be run/executed | tests/, tasks.md |

## Artifact Validation Schema

### Validation Rule Types

Each artifact declares validation rules that can be checked programmatically:

#### 1. Existence Checks
```toml
validation = [
  "File exists",
  "Directory exists (if feature has APIs)"
]
```

#### 2. Content Checks
```toml
validation = [
  "Contains all required sections: Overview, Features, Usage",
  "Contains Articles I-IX",
  "All user stories have priorities (P1, P2, P3)"
]
```

#### 3. Constraint Checks
```toml
validation = [
  "No implementation details (tech stack, frameworks, APIs)",
  "Clarifications limited (≤3)",
  "Maximum {{max_projects}} projects initially"
]
```

#### 4. Quality Checks
```toml
validation = [
  "Requirements are testable",
  "Success criteria are measurable",
  "All tasks follow format: - [ ] [ID] [P?] [Story?] Description"
]
```

#### 5. Cross-Artifact Checks
```toml
validation = [
  "All entities from spec.md defined",
  "All requirements from spec.md have contracts",
  "Each user story has complete task set"
]
```

#### 6. Conditional Checks
```toml
validation = [
  "File exists (if feature involves data)",
  "Tests exist (if TDD)",
  "Directory exists (if feature has APIs)"
]
```

### Validation Enforcement

**Two enforcement levels**:

1. **Standard** (quality_gates.standard):
   - specification_quality_min = 90%
   - constitutional_compliance_min = 100%
   - task_completeness_min = 95%
   - test_pass_rate_min = 95%
   - require_user_approval = true

2. **Strict** (quality_gates.strict):
   - specification_quality_min = 100%
   - constitutional_compliance_min = 100%
   - task_completeness_min = 100%
   - test_pass_rate_min = 100%
   - checklist_completion_required = true
   - require_user_approval = true

## Artifact Flow Patterns

### Pattern 1: Template Application

```
Template + User Input → Generated Artifact
```

**Example**:
```
spec-template.md + "Build chat app" → spec.md
```

**Encoding in Formula**:
```toml
[[steps]]
id = "specification"
# Inputs: feature_description (var), spec-template.md (implicit)
# Outputs: spec.md

[[steps.outputs]]
artifact = "spec.md"
path = "{{spec_dir}}/specs/###-feature-name/spec.md"
type = "specification"
```

### Pattern 2: Refinement

```
Artifact → Enhanced Artifact (in-place update)
```

**Example**:
```
spec.md → spec.md (clarified)
```

**Encoding in Formula**:
```toml
[[steps]]
id = "clarification"
# Inputs: spec.md
# Outputs: spec.md (updated in-place)

[[steps.outputs]]
artifact = "spec.md (clarified)"
path = "{{spec_dir}}/specs/###-feature-name/spec.md"
type = "specification"
description = "Updated specification with clarifications resolved"
```

### Pattern 3: Decomposition

```
High-Level Artifact → Multiple Low-Level Artifacts
```

**Example**:
```
spec.md → plan.md + research.md + data-model.md + contracts/
```

**Encoding in Formula**:
```toml
[[steps]]
id = "planning"
# Inputs: spec.md, constitution.md
# Outputs: plan.md, research.md, data-model.md, contracts/, quickstart.md

[[steps.outputs]]
artifact = "plan.md"
# ...

[[steps.outputs]]
artifact = "research.md"
# ...

[[steps.outputs]]
artifact = "data-model.md"
required = false  # Conditional on feature type
# ...
```

### Pattern 4: Aggregation

```
Multiple Artifacts → Composite Artifact
```

**Example**:
```
plan.md + spec.md + data-model.md + contracts/ → tasks.md
```

**Encoding in Formula**:
```toml
[[steps]]
id = "tasking"
# Inputs: plan.md (required), spec.md (required),
#         data-model.md (optional), contracts/ (optional)
# Outputs: tasks.md

# Not explicitly declaring inputs (implicit from description)
# Could be enhanced:
[[steps.inputs]]
artifact = "plan.md"
path = "{{spec_dir}}/specs/###-feature-name/plan.md"
type = "technical-plan"
required = true

[[steps.inputs]]
artifact = "spec.md"
path = "{{spec_dir}}/specs/###-feature-name/spec.md"
type = "specification"
required = true

[[steps.inputs]]
artifact = "data-model.md"
path = "{{spec_dir}}/specs/###-feature-name/data-model.md"
type = "data-model"
required = false

[[steps.outputs]]
artifact = "tasks.md"
# ...
```

### Pattern 5: Validation

```
Artifact → Validation Report → Pass/Fail
```

**Example**:
```
spec.md → Quality Checklist → Pass/Fail → Proceed/Refine
```

**Encoding in Formula**:
```toml
[[steps]]
id = "specification"

[[steps.outputs]]
artifact = "spec.md"
# ...
validation = [
  "No implementation details",
  "Requirements testable",
  "Success criteria measurable"
]

[[steps.quality_gates]]
gate = "specification_quality"
blocking = true
checks = [
  "No implementation details",
  "Requirements testable",
  "Success criteria measurable"
]
```

### Pattern 6: Parallel Fan-Out/Fan-In

```
Single Input → Split → [Process 1 ∥ Process 2 ∥ Process 3] → Merge → Single Output
```

**Example**:
```
plan.md → Research → [Tech 1 ∥ Tech 2 ∥ Tech 3] → Consolidate → research.md
```

**Encoding in Formula**:
```toml
[[steps]]
id = "planning"
description = """
Phase 0 - Research unknowns (parallel if multiple)
Phase 1 - Design (parallel: data-model.md, contracts/, quickstart.md)
"""

# Parallel outputs in Phase 1
[[steps.outputs]]
artifact = "data-model.md"
# Can be generated in parallel with contracts/

[[steps.outputs]]
artifact = "contracts"
# Can be generated in parallel with data-model.md

[[steps.outputs]]
artifact = "quickstart.md"
# Can be generated in parallel with data-model.md and contracts/
```

## Constitutional Governance Artifacts

### Constitution as Reference Artifact

The constitution.md artifact serves as a **persistent reference** throughout the workflow:

```toml
# Stage 1: Create constitution
[[steps]]
id = "constitution"
[[steps.outputs]]
artifact = "constitution.md"
path = "{{spec_dir}}/memory/constitution.md"
type = "governance"

# Stage 2: Reference constitution (implicit)
[[steps]]
id = "specification"
# Implicitly reads constitution.md for guidance

# Stage 4: Validate against constitution (explicit)
[[steps]]
id = "planning"
description = """
Check Constitution gates (Simplicity, Anti-Abstraction, Integration-First)
"""
# Explicitly validates plan.md against constitution.md

[[steps.quality_gates]]
gate = "constitutional_compliance"
checks = [
  "Simplicity Gate: ≤{{max_projects}} projects OR justified",
  "Anti-Abstraction Gate: Framework used directly",
  "Integration-First Gate: Contracts defined"
]
```

### Constitutional Articles as Validation Rules

Each article translates to validation rules:

**Article III: Test-First Imperative**
```toml
[[steps]]
id = "implementation"
description = """
Follow TDD: tests before implementation (if {{tdd_approach}} == true)
"""
validation = [
  "Tests before implementation (if TDD)"
]
```

**Article VII: Simplicity Gate**
```toml
[vars.max_projects]
default = "3"

[[steps.quality_gates]]
gate = "constitutional_compliance"
checks = [
  "Simplicity Gate: ≤{{max_projects}} projects OR justified"
]
```

**Article VIII: Anti-Abstraction Gate**
```toml
[[steps.quality_gates]]
gate = "constitutional_compliance"
checks = [
  "Anti-Abstraction Gate: Framework used directly, single model representation"
]
```

**Article IX: Integration-First Gate**
```toml
[[steps.quality_gates]]
gate = "constitutional_compliance"
checks = [
  "Integration-First Gate: Contracts defined, contract tests specified"
]
```

## Parallel Execution and Artifacts

### Marking Parallel Opportunities

**At Planning Stage** (Phase 1):
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

[[steps.outputs]]
artifact = "contracts"

[[steps.outputs]]
artifact = "quickstart.md"
```

**At Implementation Stage** (Tasks):
```toml
[[steps]]
id = "implementation"
description = """
6. Respect [P] markers for parallel execution
"""

# tasks.md contains:
# - [ ] T005 [P] Implement auth middleware in src/middleware/auth.py
# - [ ] T006 [P] Implement logger in src/utils/logger.py
# These can run in parallel (different files, no dependencies)
```

### File-Based Coordination

**Constraint**: Tasks modifying the same file must be sequential

```toml
# tasks.md contains:
# - [ ] T010 Create User model in src/models/user.py
# - [ ] T011 Add validation to User model in src/models/user.py
# These CANNOT be parallel (same file)

# But these CAN be parallel:
# - [ ] T010 [P] Create User model in src/models/user.py
# - [ ] T012 [P] Create Post model in src/models/post.py
```

## Example: Complete Artifact Flow for One Feature

### Scenario: "Build a real-time chat application"

#### Stage 1: Constitution
**Inputs**:
- User values: "Simplicity, test-first, integration-first"
- max_projects: 3

**Outputs**:
- `.specify/memory/constitution.md` (9 articles, gates defined)

#### Stage 2: Specification
**Inputs**:
- Feature description: "Build a real-time chat application"
- constitution.md
- spec-template.md

**Outputs**:
- `specs/001-real-time-chat/spec.md` (3 user stories P1-P3, 12 requirements, 5 success criteria)
- `specs/001-real-time-chat/checklists/requirements.md` (quality checklist)

#### Stage 3: Clarification
**Inputs**:
- `specs/001-real-time-chat/spec.md` (with 2 [NEEDS CLARIFICATION] markers)
- User answers to clarifications

**Outputs**:
- `specs/001-real-time-chat/spec.md` (updated, markers resolved)

#### Stage 4: Planning
**Inputs**:
- `specs/001-real-time-chat/spec.md` (clarified)
- constitution.md
- plan-template.md
- Tech stack: "Node.js, Socket.io, PostgreSQL, Jest"

**Outputs** (parallel in Phase 1):
- `specs/001-real-time-chat/plan.md`
- `specs/001-real-time-chat/research.md`
- `specs/001-real-time-chat/data-model.md` (User, Message, Room entities)
- `specs/001-real-time-chat/contracts/` (WebSocket events, REST API)
- `specs/001-real-time-chat/quickstart.md`

#### Stage 5: Tasking
**Inputs**:
- `specs/001-real-time-chat/plan.md`
- `specs/001-real-time-chat/spec.md`
- `specs/001-real-time-chat/data-model.md`
- `specs/001-real-time-chat/contracts/`
- tasks-template.md

**Outputs**:
- `specs/001-real-time-chat/tasks.md` (45 tasks across 5 phases, 18 marked [P])

#### Stage 6: Implementation
**Inputs**:
- `specs/001-real-time-chat/tasks.md`
- `specs/001-real-time-chat/plan.md`
- `specs/001-real-time-chat/spec.md`
- All design docs

**Outputs**:
- `specs/001-real-time-chat/src/` (12 implementation files)
- `specs/001-real-time-chat/tests/` (15 test files, TDD)
- `specs/001-real-time-chat/tasks.md` (updated, all tasks [X])

### Artifact Count Summary
- **Templates**: 4 (constitution, spec, plan, tasks)
- **Governance**: 1 (constitution.md)
- **Specifications**: 1 (spec.md)
- **Planning**: 5 (plan.md, research.md, data-model.md, contracts/, quickstart.md)
- **Tasks**: 1 (tasks.md)
- **Implementation**: 27 (12 src, 15 tests)
- **Validation**: 1 (requirements-checklist.md)

**Total**: 40 artifacts created for one feature

## Extension: Artifact Tracking in Beads

### Proposed Beads Schema Extension

To fully support artifact tracking, beads could extend issue metadata:

```yaml
# .beads/issues/ISSUE-001.yaml
id: ISSUE-001
title: "Create specification for real-time chat"
status: in_progress
formula: spec-kit-workflow
formula_step: specification

# NEW: Artifact tracking
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
      status: in_progress
      validation:
        - check: "No implementation details"
          status: pass
        - check: "Requirements testable"
          status: pass
        - check: "Clarifications limited (≤3)"
          status: fail
          details: "4 clarifications found"

  quality_gates:
    - gate: specification_quality
      status: pending
      checks:
        - "No implementation details": pass
        - "Requirements testable": pass
        - "Success criteria measurable": pass
        - "Clarifications limited": fail
```

### Artifact State Machine

Each artifact transitions through states:

```
planned → in_progress → draft → validated → approved → archived
```

**Transitions**:
- `planned`: Declared in formula, not yet created
- `in_progress`: Being worked on
- `draft`: Complete but not validated
- `validated`: Passed validation checks
- `approved`: User approved
- `archived`: Persisted, immutable

## Reusability Across Formulas

### Common Artifact Patterns

The artifact schema is reusable beyond spec-kit:

**1. Template Application Pattern**
```toml
[[steps.inputs]]
artifact = "template"
type = "template"

[[steps.outputs]]
artifact = "generated-artifact"
type = "generated"
```

**2. Validation Pattern**
```toml
[[steps.outputs]]
artifact = "artifact-name"
validation = [
  "Existence check",
  "Content check",
  "Quality check"
]

[[steps.quality_gates]]
gate = "validation-gate"
checks = [...]
```

**3. Multi-Output Pattern**
```toml
[[steps.outputs]]
artifact = "output-1"

[[steps.outputs]]
artifact = "output-2"

[[steps.outputs]]
artifact = "output-3"
```

**4. Conditional Output Pattern**
```toml
[[steps.outputs]]
artifact = "optional-artifact"
required = false
description = "Only if condition met"
```

### Integration with component-dev.formula.toml

The component-dev formula uses similar artifact patterns:

```toml
# component-dev: System Modeling Quad
[[steps.outputs]]
artifact = "{{component_name}}.spec.md"
type = "documentation"
validation = [
  "File exists",
  "Contains required sections",
  "Intent clearly stated"
]

# spec-kit: Specification
[[steps.outputs]]
artifact = "spec.md"
type = "specification"
validation = [
  "File exists",
  "No implementation details",
  "Requirements testable"
]
```

**Common patterns**:
- Both use `[[steps.outputs]]` arrays
- Both include `type` classification
- Both include `validation` arrays
- Both support `required` flag for conditional artifacts

## Conclusion

The spec-kit artifact schema provides a complete information architecture for specification-driven development:

1. **Template artifacts** constrain LLM behavior and enforce patterns
2. **Generated artifacts** flow through 6 workflow stages
3. **Validation artifacts** ensure quality at each gate
4. **Dependency graph** defines information flow
5. **Attachment schema** (`[[steps.inputs]]`, `[[steps.outputs]]`, `[[steps.quality_gates]]`) enables explicit declaration
6. **Validation rules** enable programmatic checking
7. **Parallel patterns** optimize execution
8. **Constitutional governance** provides persistent reference

This schema is **reusable** across formulas and can be extended to support artifact tracking in beads issue management.

**Key insights**:
- Artifacts are first-class workflow elements
- Explicit declaration beats implicit assumptions
- Validation is artifact-level, not just step-level
- Templates are artifacts that constrain other artifacts
- Quality gates attach to artifacts, not just steps
- Parallel execution respects artifact dependencies

This design enables **formula composition**, **artifact reuse**, and **quality automation** across the beads ecosystem.
