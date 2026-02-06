# Spec-Kit Workflow Graph Analysis

**Project**: GitHub spec-kit
**Analysis Date**: 2026-01-11
**Repository**: https://github.com/github/spec-kit

## Executive Summary

This document maps the spec-kit system as a graph of activities, information artifacts, and transformations. Spec-kit implements Specification-Driven Development (SDD), where specifications are executable artifacts that generate code through a structured workflow.

## Graph Overview

The spec-kit system can be modeled as a directed acyclic graph (DAG) with:
- **Activity Nodes**: Commands and processes that transform inputs to outputs
- **Artifact Nodes**: Information artifacts (specs, plans, tasks, code)
- **Transformation Edges**: Information flow and dependencies between nodes
- **Validation Gates**: Decision points that must pass before proceeding

## Core Workflow Graph

```
[User Description]
    |
    v
[/speckit.constitution] --> [constitution.md]
    |                              |
    v                              |
[/speckit.specify] ----------------+
    |                              |
    v                              v
[spec.md] <-- [spec-template.md]  |
    |                              |
    v                              |
[/speckit.clarify] (optional)      |
    |                              |
    v                              |
[spec.md (clarified)] -------------+
    |                              |
    v                              v
[/speckit.plan] -------------------+
    |                              |
    v                              v
[plan.md] <-- [plan-template.md]   |
    |                              |
    +---> [research.md]            |
    +---> [data-model.md]          |
    +---> [contracts/]             |
    +---> [quickstart.md]          |
    |                              |
    v                              |
[/speckit.tasks] ------------------+
    |                              |
    v                              v
[tasks.md] <-- [tasks-template.md] |
    |                              |
    v                              |
[/speckit.implement] --------------+
    |
    v
[Working Code]
    |
    v
[Tests Pass]
```

## Activity Nodes (Commands)

### 1. /speckit.constitution
**Purpose**: Establish project principles and architectural constraints

**Inputs**:
- User description of project values and constraints
- constitution-template.md

**Outputs**:
- .specify/memory/constitution.md

**Process**:
1. Parse user input for core principles
2. Generate Articles (I-IX) defining architectural rules
3. Create governance section
4. Establish validation gates for later phases

**Constraints**:
- Must define test-first imperative
- Must establish simplicity gates
- Must define integration testing requirements

### 2. /speckit.specify
**Purpose**: Transform natural language feature description into structured specification

**Inputs**:
- User feature description (natural language)
- spec-template.md
- Existing specs (for numbering)

**Script Integration**:
- create-new-feature.sh (generates branch, determines feature number)

**Outputs**:
- Feature branch (e.g., 001-feature-name)
- specs/###-feature-name/spec.md
- specs/###-feature-name/checklists/requirements.md

**Process**:
1. Generate short name from description (2-4 words)
2. Check existing branches/specs for highest number
3. Create new branch with next sequential number
4. Fill spec template with:
   - User stories (with priorities P1, P2, P3)
   - Functional requirements (FR-001, FR-002, ...)
   - Success criteria (measurable outcomes)
   - Key entities (if data involved)
   - Edge cases
5. Mark ambiguities with [NEEDS CLARIFICATION] (max 3)
6. Generate quality checklist
7. Validate spec against checklist

**Constraints**:
- NO implementation details (no tech stack, APIs, frameworks)
- Focus on WHAT and WHY, not HOW
- Requirements must be testable
- Success criteria must be measurable and technology-agnostic
- Maximum 3 [NEEDS CLARIFICATION] markers

**Validation Gates**:
- Content quality: No implementation details
- Requirement completeness: All acceptance scenarios defined
- Feature readiness: Measurable outcomes exist

### 3. /speckit.clarify (Optional)
**Purpose**: Structured questioning to resolve ambiguities in specification

**Inputs**:
- spec.md with [NEEDS CLARIFICATION] markers

**Outputs**:
- Updated spec.md with clarifications resolved
- Clarifications section in spec.md

**Process**:
1. Extract all [NEEDS CLARIFICATION] markers
2. For each marker, present options to user
3. Wait for user selections
4. Replace markers with chosen answers
5. Document clarifications in spec

**Constraints**:
- Sequential questioning (one by one)
- Maximum 3 questions total
- Must provide suggested answers with implications

### 4. /speckit.plan
**Purpose**: Convert specification into technical implementation plan

**Inputs**:
- spec.md
- constitution.md
- plan-template.md
- User tech stack preferences

**Script Integration**:
- setup-plan.sh (creates plan structure)
- update-agent-context.sh (updates AI agent context)

**Outputs**:
- specs/###-feature-name/plan.md
- specs/###-feature-name/research.md
- specs/###-feature-name/data-model.md
- specs/###-feature-name/contracts/ (API specs)
- specs/###-feature-name/quickstart.md
- Updated agent context file (.claude/CLAUDE.md, etc.)

**Process**:
1. Fill Technical Context (language, dependencies, storage, testing)
2. Check Constitution gates (Simplicity, Anti-Abstraction, Integration-First)
3. Phase 0: Research unknowns, resolve NEEDS CLARIFICATION
4. Phase 1: Design data models, generate contracts, create quickstart
5. Update agent context with new technology choices
6. Re-evaluate constitution compliance

**Validation Gates**:
- Simplicity Gate: ≤3 projects, no future-proofing
- Anti-Abstraction Gate: Use framework directly, single model representation
- Integration-First Gate: Contracts defined, contract tests specified

**Constraints**:
- Must justify any gate violations in Complexity Tracking table
- All NEEDS CLARIFICATION must be resolved in research.md
- Contracts must use standard formats (OpenAPI, GraphQL)

### 5. /speckit.tasks
**Purpose**: Generate actionable task breakdown from design documents

**Inputs**:
- plan.md (required)
- spec.md (required - for user stories)
- data-model.md (optional)
- contracts/ (optional)
- research.md (optional)
- tasks-template.md

**Script Integration**:
- check-prerequisites.sh (validates all required docs exist)

**Outputs**:
- specs/###-feature-name/tasks.md

**Process**:
1. Extract user stories with priorities from spec.md
2. Extract tech stack and structure from plan.md
3. Map entities from data-model.md to user stories
4. Map contracts to user stories
5. Generate task phases:
   - Phase 1: Setup (project initialization)
   - Phase 2: Foundational (blocking prerequisites)
   - Phase 3+: One phase per user story (P1, P2, P3)
   - Final Phase: Polish & cross-cutting concerns
6. Mark parallel tasks with [P]
7. Assign task IDs (T001, T002, T003)
8. Assign user story labels ([US1], [US2], [US3])
9. Create dependency graph
10. Generate parallel execution examples

**Task Format**:
```
- [ ] [TaskID] [P?] [Story?] Description with exact file path
```

**Constraints**:
- ALL tasks must follow checklist format
- Tests (if requested) must come before implementation
- Each user story must be independently testable
- Tasks within same file must be sequential
- Maximum clarity on file paths

**Validation**:
- Each user story has complete task set
- Dependencies are correct
- Parallel opportunities identified
- File paths are absolute and correct

### 6. /speckit.implement
**Purpose**: Execute implementation plan by running all tasks

**Inputs**:
- tasks.md (required)
- plan.md (required)
- All design documents
- Checklist validation results

**Script Integration**:
- check-prerequisites.sh --require-tasks --include-tasks

**Outputs**:
- Working code implementing all user stories
- Passing tests (if TDD approach)
- Updated tasks.md with completed tasks marked

**Process**:
1. Validate checklists (if exist) - halt if incomplete
2. Load all design documents
3. Verify/create ignore files (.gitignore, .dockerignore, etc.)
4. Parse tasks.md for phases and dependencies
5. Execute phases in order:
   - Setup → Foundational → User Story phases → Polish
6. Respect [P] markers for parallel execution
7. Follow TDD: tests before implementation
8. Mark completed tasks with [X]
9. Report progress after each task
10. Halt on failures, suggest fixes

**Validation Gates**:
- Pre-flight: All checklists complete (or user confirms proceed)
- Per-phase: Checkpoint validation before next phase
- Post-implementation: All tests pass, features match spec

**Constraints**:
- Phase-by-phase execution (no skipping)
- Sequential tasks run in order
- Parallel tasks [P] can run together
- File-based coordination (same file = sequential)
- Test-first if TDD approach specified

## Artifact Nodes (Information)

### Templates (Immutable Constraints)

1. **spec-template.md**
   - Constrains specification structure
   - Enforces separation of WHAT vs HOW
   - Mandates user stories, requirements, success criteria
   - Forces [NEEDS CLARIFICATION] markers

2. **plan-template.md**
   - Constrains technical plan structure
   - Enforces constitutional gates
   - Mandates research phase
   - Requires complexity justification

3. **tasks-template.md**
   - Constrains task structure
   - Enforces user story organization
   - Mandates checklist format
   - Requires dependency documentation

4. **constitution.md (template)**
   - Defines Articles I-IX
   - Establishes validation gates
   - Creates architectural constraints

### Generated Artifacts (Mutable State)

1. **constitution.md** (generated)
   - Project-specific principles
   - Architectural constraints
   - Governance rules

2. **spec.md**
   - User stories with priorities
   - Functional requirements
   - Success criteria
   - Edge cases
   - Assumptions

3. **plan.md**
   - Technical context
   - Constitution check results
   - Project structure
   - Complexity tracking

4. **research.md**
   - Technology decisions
   - Rationale for choices
   - Alternatives considered
   - Best practices found

5. **data-model.md**
   - Entity definitions
   - Relationships
   - Validation rules
   - State transitions

6. **contracts/**
   - OpenAPI specs
   - GraphQL schemas
   - WebSocket protocols
   - Event schemas

7. **quickstart.md**
   - Key validation scenarios
   - Integration test cases
   - Manual testing steps

8. **tasks.md**
   - Task breakdown by phase
   - Task IDs and descriptions
   - Dependency graph
   - Parallel execution plan

9. **Implementation Code**
   - Source files
   - Test files
   - Configuration
   - Documentation

## Transformation Edges (Information Flow)

### Edge Types

1. **Template Application**: Template + User Input → Structured Document
   - spec-template.md + Feature Description → spec.md
   - plan-template.md + Tech Stack → plan.md
   - tasks-template.md + Design Docs → tasks.md

2. **Refinement**: Document → Enhanced Document
   - spec.md → spec.md (via /speckit.clarify)
   - plan.md → plan.md + research.md + data-model.md + contracts/

3. **Decomposition**: High-level Artifact → Low-level Artifacts
   - spec.md → plan.md (requirements → technical design)
   - plan.md → tasks.md (design → actionable steps)
   - tasks.md → Code (tasks → implementation)

4. **Validation**: Artifact → Pass/Fail + Feedback
   - spec.md → Quality Checklist → Pass/Fail
   - plan.md → Constitution Gates → Pass/Fail
   - tasks.md → Task Validation → Pass/Fail

5. **Aggregation**: Multiple Artifacts → Composite View
   - spec.md + data-model.md → tasks.md
   - All design docs → Implementation context

## Dependency Graph

### Strict Dependencies (Must Complete Before Next)

```
constitution.md
    ↓
spec.md
    ↓
spec.md (clarified)
    ↓
plan.md + research.md + data-model.md + contracts/
    ↓
tasks.md
    ↓
Code
```

### Optional Enhancements (Can Skip)

```
spec.md → /speckit.clarify → spec.md (enhanced)
```

### Parallel Opportunities

Within /speckit.plan Phase 0:
```
Research Unknown 1 ∥ Research Unknown 2 ∥ Research Unknown 3
```

Within /speckit.plan Phase 1:
```
Generate data-model.md ∥ Generate contracts/ ∥ Generate quickstart.md
```

Within /speckit.implement:
```
Tasks marked [P] within same phase can run in parallel
```

## Validation Gates (Decision Points)

### Gate 1: Specification Quality (after /speckit.specify)
**Location**: spec.md validation

**Checks**:
- [ ] No implementation details
- [ ] Requirements are testable
- [ ] Success criteria are measurable
- [ ] All acceptance scenarios defined
- [ ] No more than 3 [NEEDS CLARIFICATION]

**Pass**: Proceed to /speckit.clarify or /speckit.plan
**Fail**: Update spec.md until all checks pass

### Gate 2: Clarification Completeness (after /speckit.clarify)
**Location**: spec.md [NEEDS CLARIFICATION] markers

**Checks**:
- [ ] All clarifications resolved
- [ ] User has provided answers
- [ ] Spec updated with answers

**Pass**: Proceed to /speckit.plan
**Fail**: Continue clarification process

### Gate 3: Constitutional Compliance (during /speckit.plan)
**Location**: plan.md Constitution Check

**Checks**:
- [ ] Simplicity Gate: ≤3 projects
- [ ] Anti-Abstraction Gate: Framework used directly
- [ ] Integration-First Gate: Contracts defined

**Pass**: Proceed to Phase 0
**Fail**: Justify violations in Complexity Tracking OR revise plan

### Gate 4: Research Completeness (after /speckit.plan Phase 0)
**Location**: research.md

**Checks**:
- [ ] All NEEDS CLARIFICATION resolved
- [ ] Technology choices documented
- [ ] Rationale provided for each decision

**Pass**: Proceed to Phase 1
**Fail**: Continue research

### Gate 5: Task Completeness (after /speckit.tasks)
**Location**: tasks.md validation

**Checks**:
- [ ] All tasks follow checklist format
- [ ] Each user story has complete task set
- [ ] Dependencies are correct
- [ ] File paths are specified

**Pass**: Proceed to /speckit.implement
**Fail**: Regenerate tasks.md

### Gate 6: Checklist Validation (before /speckit.implement)
**Location**: checklists/ directory

**Checks**:
- [ ] All checklists have 0 incomplete items

**Pass**: Proceed automatically
**Fail**: Ask user to confirm proceed anyway

### Gate 7: Implementation Success (after /speckit.implement)
**Location**: Code + Tests

**Checks**:
- [ ] All tasks marked complete
- [ ] Tests pass (if TDD)
- [ ] Features match spec.md

**Pass**: Feature complete
**Fail**: Fix issues and retry

## State Machine View

The workflow can be viewed as a state machine:

**States**:
- S0: Unspecified (initial state)
- S1: Constitution Defined
- S2: Specified (spec.md exists)
- S3: Clarified (no [NEEDS CLARIFICATION])
- S4: Planned (plan.md + design docs exist)
- S5: Tasked (tasks.md exists)
- S6: Implemented (code exists)
- S7: Validated (tests pass)

**Transitions**:
- S0 →[/speckit.constitution]→ S1
- S1 →[/speckit.specify]→ S2
- S2 →[/speckit.clarify]→ S3 (optional)
- S2 →[/speckit.plan]→ S4 (if no clarification needed)
- S3 →[/speckit.plan]→ S4
- S4 →[/speckit.tasks]→ S5
- S5 →[/speckit.implement]→ S6
- S6 →[tests]→ S7

**Loops** (Refinement):
- S2 ↔ S2 (clarify multiple times)
- S4 ↔ S4 (refine plan)
- S5 ↔ S5 (regenerate tasks)
- S6 ↔ S6 (fix implementation)

## Parallel Execution Graph

### Research Phase (within /speckit.plan)
```
        ┌──────────────────────┐
        │  Phase 0: Research   │
        └──────────┬───────────┘
                   │
        ┌──────────┼──────────┐
        ▼          ▼          ▼
  [Research 1] [Research 2] [Research 3]
        │          │          │
        └──────────┼──────────┘
                   │
                   ▼
            [research.md]
```

### Task Execution (within /speckit.implement)
```
        ┌────────────────────┐
        │  Setup Phase       │
        └─────────┬──────────┘
                  │
        ┌─────────┼─────────┐
        ▼         ▼         ▼
    [T001]   [T002 P]  [T003 P]
        │         │         │
        └─────────┼─────────┘
                  │
                  ▼
        ┌────────────────────┐
        │  Foundational      │
        └─────────┬──────────┘
                  │
        ┌─────────┼─────────┐
        ▼         ▼         ▼
   [T004]   [T005 P]  [T006 P]
                  │
                  ▼
        ┌────────────────────┐
        │  User Story 1      │
        └─────────┬──────────┘
                  │
        ┌─────────┼─────────┐
        ▼         ▼         ▼
  [T007 US1] [T008 P US1] [T009 P US1]
```

## Information Flow Patterns

### 1. Template Constraint Pattern
```
Template → Constrains → LLM Behavior → Generates → Structured Output
```

**Example**:
```
spec-template.md → "NO implementation details" → LLM avoids tech stack → spec.md (tech-agnostic)
```

### 2. Validation Feedback Loop Pattern
```
Artifact → Validation Gate → [Pass/Fail] → [Proceed/Refine] → Updated Artifact
```

**Example**:
```
spec.md → Quality Checklist → [Fail: unclear requirements] → Update spec → spec.md (improved)
```

### 3. Decomposition Cascade Pattern
```
High-Level Artifact → Extract Components → Generate Lower-Level Artifacts
```

**Example**:
```
spec.md (user stories) → Map to tasks → tasks.md (T001, T002, T003...)
```

### 4. Parallel Fan-Out/Fan-In Pattern
```
Single Input → Split → [Process 1 ∥ Process 2 ∥ Process 3] → Merge → Single Output
```

**Example**:
```
plan.md → Research → [Tech 1 ∥ Tech 2 ∥ Tech 3] → Consolidate → research.md
```

### 5. Context Accumulation Pattern
```
Artifact 1 + Artifact 2 + ... + Artifact N → Composite Context → Next Phase
```

**Example**:
```
spec.md + plan.md + data-model.md + contracts/ → Implementation Context → /speckit.implement
```

## Critical Path Analysis

**Shortest Path** (no clarification):
```
constitution.md → spec.md → plan.md → tasks.md → code
```
**Time Estimate**: 30-60 minutes with AI assistance

**Standard Path** (with clarification):
```
constitution.md → spec.md → clarify → spec.md → plan.md → tasks.md → code
```
**Time Estimate**: 45-90 minutes with AI assistance

**Longest Path** (with refinement):
```
constitution.md → spec.md → clarify (multiple rounds) → plan.md → refine → tasks.md → regenerate → code → fix → code
```
**Time Estimate**: 2-4 hours with AI assistance

## Bottlenecks and Optimization Opportunities

**Bottleneck 1**: Constitution Check validation
- **Impact**: Can block entire planning phase
- **Optimization**: Pre-validate against constitution during specification

**Bottleneck 2**: Research phase completeness
- **Impact**: All NEEDS CLARIFICATION must be resolved
- **Optimization**: Limit NEEDS CLARIFICATION markers to 3, make informed guesses

**Bottleneck 3**: Task format validation
- **Impact**: Malformed tasks break implementation
- **Optimization**: Strict template adherence, automated format checking

**Bottleneck 4**: Sequential task execution
- **Impact**: Can't parallelize tasks on same file
- **Optimization**: Design for modularity, separate files for separate concerns

## Graph Metrics

**Node Count**:
- Activity Nodes (Commands): 6
- Template Nodes: 4
- Artifact Nodes: 9+
- Validation Gate Nodes: 7

**Edge Count**:
- Template Application: 3
- Refinement: 4+
- Decomposition: 3
- Validation: 7
- Aggregation: 5+

**Critical Path Length**: 6 activities (constitution → specify → plan → tasks → implement → validate)

**Parallelization Factor**:
- Research phase: Up to N parallel (N = number of unknowns)
- Task execution: Up to M parallel (M = number of [P] tasks in phase)

**Branching Factor**:
- /speckit.clarify: 0 or 1 (optional)
- Research tasks: 1 to N (varies)
- User stories: 1 to M (varies)

## Conclusion

The spec-kit workflow is a carefully orchestrated DAG that transforms natural language descriptions into working code through structured activities, constrained by templates and validated by gates. The graph design enables:

1. **Systematic progression**: Clear dependencies prevent premature optimization
2. **Parallel opportunities**: Research and task execution can scale horizontally
3. **Quality gates**: Validation at every stage ensures consistency
4. **Template constraints**: LLM behavior is channeled toward high-quality outputs
5. **Traceability**: Every artifact traces back to requirements

This graph-based workflow embodies the principles of Specification-Driven Development, where specifications are executable and code is the last-mile expression of intent.
