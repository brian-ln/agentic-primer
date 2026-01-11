# BMAD-METHOD Artifact Schema

**Date**: 2026-01-11, 9:30 AM EST
**Purpose**: Define artifact inputs/outputs, frontmatter specifications, and attachment patterns

## Overview

This document specifies the schema for all artifacts in BMAD-METHOD, including frontmatter structure, required sections, validation criteria, and attachment patterns for beads integration.

## Artifact Categories

### Phase 1 Artifacts (Analysis)

#### brainstorming.md
- **Created by**: brainstorm-project workflow
- **Used by**: create-product-brief, prd
- **Frontmatter**:
  ```yaml
  ---
  type: brainstorming
  project: {project_name}
  date: {date}
  stepsCompleted: [step-01, step-02, ...]
  status: in_progress | complete
  ---
  ```
- **Required sections**: Problem Statement, Ideas, Solutions, Next Steps
- **Validation criteria**: ≥5 ideas explored, problem clear

#### research.md (market/domain/technical)
- **Created by**: research workflow
- **Used by**: create-product-brief, prd
- **Frontmatter**:
  ```yaml
  ---
  type: research
  research_type: market | domain | technical
  project: {project_name}
  date: {date}
  stepsCompleted: [...]
  ---
  ```
- **Required sections**: Questions, Findings, Decisions, References
- **Validation criteria**: Key questions answered, decisions documented

#### product-brief.md
- **Created by**: create-product-brief workflow
- **Used by**: prd
- **Frontmatter**:
  ```yaml
  ---
  type: product-brief
  project: {project_name}
  date: {date}
  stepsCompleted: [...]
  status: complete
  ---
  ```
- **Required sections**: Vision, Market, Success Metrics, Stakeholders
- **Validation criteria**: Vision clear, market validated

### Phase 2 Artifacts (Planning)

#### PRD.md (Product Requirements Document)
- **Created by**: prd workflow (Create mode)
- **Used by**: create-ux-design, create-architecture, create-epics
- **Frontmatter**:
  ```yaml
  ---
  type: prd
  project: {project_name}
  version: 1.0
  date: {date}
  level: 2 | 3 | 4
  stepsCompleted: [step-01-init, step-02-context, ...]
  status: draft | in_progress | complete | validated
  validation_score: 0.85
  ---
  ```
- **Required sections**: Vision, User Personas, Functional Requirements (≥3), Non-Functional Requirements, Dependencies, Success Criteria
- **Validation criteria**: FRs complete, NFRs specified, testable, no implementation leakage, density ≥200 words/section
- **Quality score**: prd_quality = 0.2*clarity + 0.2*completeness + 0.15*testability + 0.15*measurability + 0.1*density + 0.1*no_impl + 0.1*coverage

#### ux-design.md
- **Created by**: create-ux-design workflow
- **Used by**: create-architecture, dev-story
- **Frontmatter**:
  ```yaml
  ---
  type: ux-design
  project: {project_name}
  prd: path/to/PRD.md
  date: {date}
  stepsCompleted: [...]
  status: complete | validated
  ---
  ```
- **Required sections**: User Journeys, Wireframes, Design System, Interactions
- **Validation criteria**: Journeys mapped, wireframes complete

#### tech-spec.md (Quick Flow)
- **Created by**: quick-spec workflow
- **Used by**: quick-dev
- **Frontmatter**:
  ```yaml
  ---
  type: tech-spec
  project: {project_name}
  level: 0 | 1
  date: {date}
  ---
  ```
- **Required sections**: Goal, Approach, Acceptance Criteria
- **Validation criteria**: Goal clear, approach feasible

### Phase 3 Artifacts (Solutioning)

#### architecture.md
- **Created by**: create-architecture workflow
- **Used by**: create-epics, dev-story
- **Frontmatter**:
  ```yaml
  ---
  type: architecture
  project: {project_name}
  prd: path/to/PRD.md
  ux_design: path/to/ux-design.md
  version: 1.0
  date: {date}
  stepsCompleted: [...]
  status: complete | validated
  adrs: [adr-001, adr-002, ...]
  ---
  ```
- **Required sections**: System Overview, Components, ADRs (≥5), Data Flows, Integration Points, Technology Stack
- **Validation criteria**: System complete, ADRs documented, patterns consistent
- **Quality score**: arch_quality = 0.25*completeness + 0.25*adr_quality + 0.2*consistency + 0.15*prd_coverage + 0.15*guidance

#### ADRs/ (Architecture Decision Records)
- **Created by**: create-architecture workflow
- **Used by**: dev-story
- **Format**: adr-NNN-title.md
- **Required sections**: Context, Decision, Consequences, Alternatives Considered

#### epic-{name}.md
- **Created by**: create-epics-and-stories workflow
- **Used by**: sprint-planning, create-story
- **Frontmatter**:
  ```yaml
  ---
  type: epic
  epic_id: EPIC-001
  project: {project_name}
  prd: path/to/PRD.md
  architecture: path/to/architecture.md
  date: {date}
  status: planned | in_progress | complete
  stories: [STORY-001, STORY-002, ...]
  ---
  ```
- **Required sections**: Epic Goal, User Stories (≥3), Dependencies, Acceptance Criteria, Estimates
- **Validation criteria**: Stories well-defined, dependencies clear, estimates reasonable

### Phase 4 Artifacts (Implementation)

#### sprint-status.md
- **Created by**: sprint-planning workflow
- **Used by**: create-story, correct-course, retrospective
- **Frontmatter**:
  ```yaml
  ---
  type: sprint-status
  sprint_number: 1
  project: {project_name}
  start_date: {date}
  stories_planned: 5
  stories_completed: 2
  stories_in_progress: [STORY-003]
  blockers: []
  ---
  ```
- **Required sections**: Sprint Goal, Stories, Capacity, Velocity, Blockers

#### story-{id}.md
- **Created by**: create-story workflow
- **Used by**: dev-story
- **Frontmatter**:
  ```yaml
  ---
  type: story
  story_id: STORY-001
  epic: EPIC-001
  project: {project_name}
  architecture: path/to/architecture.md
  assignee: dev
  status: ready | in_progress | complete | reviewed
  estimate: 8h
  date: {date}
  ---
  ```
- **Required sections**: User Story, Acceptance Criteria, Tasks, Dependencies, Architecture Reference, Test Criteria
- **Validation criteria**: AC clear, tasks defined, dependencies resolved
- **Quality score**: story_quality = 0.3*ac_quality + 0.25*task_breakdown + 0.2*dependency_clarity + 0.15*arch_alignment + 0.1*estimate_reason

#### code/ (implementation)
- **Created by**: dev-story workflow
- **Used by**: code-review
- **Artifacts**: Source files, tests, configuration
- **Validation criteria**: Tests pass, follows architecture, story complete

#### review-{pr}.md
- **Created by**: code-review workflow
- **Used by**: dev-story (feedback)
- **Frontmatter**:
  ```yaml
  ---
  type: code-review
  pr: {pr_number}
  story: STORY-001
  reviewer: dev
  date: {date}
  status: approved | needs_changes | rejected
  ---
  ```
- **Required sections**: Summary, Code Quality, Architecture Alignment, Test Coverage, Issues, Recommendation

#### retrospective.md
- **Created by**: retrospective workflow
- **Used by**: Next sprint planning
- **Frontmatter**:
  ```yaml
  ---
  type: retrospective
  sprint_number: 1
  project: {project_name}
  date: {date}
  ---
  ```
- **Required sections**: What Went Well, What Didn't, Action Items, Metrics

## Frontmatter State Tracking

### stepsCompleted Array
Used in continuable workflows to track progress:

```yaml
stepsCompleted:
  - step-01-init
  - step-02-context
  - step-03-vision
  # ... more steps as workflow progresses
```

**Purpose**: Enable multi-session workflows, allow resume

### Status Field
Tracks artifact lifecycle:

- **draft**: Initial creation started
- **in_progress**: Partially complete (stepsCompleted < total)
- **complete**: All steps finished
- **validated**: Passed validation workflow
- **needs_revision**: Failed validation, requires editing
- **consumed**: Used by downstream workflows
- **archived**: Project complete

## Beads Integration Patterns

### Attachment Pattern 1: Artifact-per-Bead

```toml
[activity.create-prd]
artifacts = [
  { type = "prd", path = "planning-artifacts/PRD.md", attachment = "primary" }
]
```

### Attachment Pattern 2: Multi-Artifact Activity

```toml
[activity.create-architecture]
artifacts = [
  { type = "architecture", path = "planning-artifacts/architecture.md", attachment = "primary" },
  { type = "adr", path = "planning-artifacts/ADRs/*.md", attachment = "supporting" }
]
```

### Attachment Pattern 3: Artifact Chain

```toml
[activity.dev-story]
inputs = [
  { type = "story", required = true },
  { type = "architecture", required = false }
]
outputs = [
  { type = "code", path = "src/**/*" },
  { type = "tests", path = "tests/**/*" }
]
```

## Summary

BMAD-METHOD artifacts use:
- **Frontmatter** for metadata and state tracking
- **stepsCompleted arrays** for continuable workflows
- **Status fields** for lifecycle tracking
- **Validation scores** for quality measurement
- **Explicit dependencies** for workflow ordering

This schema enables beads integration by providing:
- Clear artifact I/O per workflow
- State tracking for in-progress work
- Quality metrics for validation
- Dependency relationships for ordering

---

**Document**: BMAD Artifact Schema
**Date**: 2026-01-11, 9:30 AM EST
