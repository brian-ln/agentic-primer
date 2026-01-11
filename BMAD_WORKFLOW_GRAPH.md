# BMAD-METHOD Workflow Graph

**Project**: BMAD-METHOD Analysis
**Date**: 2026-01-11 (Sunday, January 11, 2026, 8:45 AM EST)
**Repository**: https://github.com/bmad-code-org/BMAD-METHOD
**Analysis Focus**: Workflow structure, activity graph, information flows

## Overview

This document models BMAD-METHOD as a directed graph of workflows (activity nodes), agents (executor nodes), artifacts (data nodes), and information flows (transformation edges). The graph represents the complete lifecycle of agile AI-driven development from brainstorming to deployment.

## Graph Structure

### Node Types

1. **Workflow Nodes (W)**: Activities that transform inputs to outputs
2. **Agent Nodes (A)**: Specialized AI agents that execute workflows
3. **Artifact Nodes (D)**: Documents, files, and data structures
4. **Gate Nodes (G)**: Validation and quality checkpoints

### Edge Types

1. **Execution Edges**: Agent → Workflow (who executes what)
2. **Transformation Edges**: Workflow → Artifact (what produces what)
3. **Dependency Edges**: Artifact → Workflow (what requires what)
4. **Validation Edges**: Artifact → Gate (what gets validated)

## Core Architecture

### Four-Phase System

```
Phase 1: Analysis (Optional) → Phase 2: Planning (Required) → Phase 3: Solutioning (Track-dependent) → Phase 4: Implementation (Required)
```

**Track Variants**:
- **Quick Flow**: Phase 2 (tech-spec) → Phase 4 (implement) [~5 minutes]
- **BMad Method**: Phase 1 (optional) → Phase 2 (PRD) → Phase 3 (architecture) → Phase 4 (implement) [~15 minutes]
- **Enterprise**: Phase 1 → Phase 2 (PRD) → Phase 3 (extended) → Phase 4 (implement) [~30 minutes]

### Scale-Adaptive Planning (Level 0-4)

The system automatically adjusts planning depth based on project complexity:

| Level | Complexity | Planning Depth | Example |
|-------|-----------|----------------|---------|
| 0 | Minimal | Tech spec only | Bug fix, minor UI tweak |
| 1 | Simple | Tech spec + basic plan | Single feature, no dependencies |
| 2 | Moderate | PRD + architecture outline | Multi-component feature |
| 3 | Complex | Full PRD + detailed architecture | Multi-epic project |
| 4 | Enterprise | PRD + architecture + extended compliance | Regulated/compliance-heavy system |

## Agent Nodes (29 Total)

### Core BMM Agents (9)

| Agent | Icon | Role | Primary Phase | Persona Name |
|-------|------|------|---------------|--------------|
| **bmad-master** | 🧙 | Master orchestrator | All | BMad Master |
| **analyst** | 📊 | Business analysis | Phase 1 | Mary |
| **pm** | 📋 | Product planning | Phase 2 | John |
| **ux-designer** | 🎨 | UX design | Phase 2-3 | Sally |
| **architect** | 🏛️ | System architecture | Phase 3 | Winston |
| **sm** | 🗓️ | Sprint management | Phase 4 | Bob |
| **dev** | 💻 | Implementation | Phase 4 | Amelia |
| **tea** | ☕ | Test architecture | Phase 3-4 | Murat |
| **quick-flow-solo-dev** | ⚡ | Fast solo dev | All (Quick Flow) | Barry |

### CIS (Creative Intelligence Suite) Agents (5)

| Agent | Icon | Role |
|-------|------|------|
| **brainstorming-coach** | 💡 | Ideation facilitation |
| **creative-problem-solver** | 🎯 | Problem-solving |
| **design-thinking-coach** | 🧠 | Design thinking |
| **innovation-strategist** | 🚀 | Innovation strategy |
| **storyteller** | 📖 | Narrative creation |

### BMGD (Game Development) Agents (6)

| Agent | Icon | Role |
|-------|------|------|
| **game-designer** | 🎮 | Game design |
| **game-architect** | 🏗️ | Game architecture |
| **game-dev** | 🕹️ | Game implementation |
| **game-scrum-master** | 📅 | Game sprint management |
| **game-qa** | 🧪 | Game QA |
| **game-solo-dev** | ⚡ | Quick game dev |

### BMB (BMad Builder) Agents (3)

| Agent | Icon | Role |
|-------|------|------|
| **module-builder** | 📦 | Module creation |
| **agent-builder** | 🤖 | Agent creation |
| **workflow-builder** | 🔄 | Workflow creation |

### Support Agents (6)

| Agent | Icon | Role |
|-------|------|------|
| **tech-writer** | ✍️ | Documentation |
| **commit-poet** | 🎭 | Commit messages |
| **journal-keeper** | 📓 | Project journal |
| **presentation-master** | 📊 | Presentations |
| **trend-analyst** | 📈 | Trend analysis |
| **security-engineer** | 🔒 | Security |

## Workflow Nodes (34+ in BMM)

### Phase 1: Analysis (Optional)

| Workflow | Agent | Input | Output | Duration |
|----------|-------|-------|--------|----------|
| **workflow-init** | bmad-master | Project context | Workflow status | 1-2 min |
| **brainstorm-project** | analyst | Idea, problem statement | Brainstorming document | 10-20 min |
| **research** (market/domain/technical) | analyst | Research questions | Research document | 15-30 min |
| **create-product-brief** | analyst | Brainstorming, research | Product brief | 10-15 min |
| **document-project** (brownfield) | analyst | Existing codebase | Project documentation | 30-60 min |

### Phase 2: Planning (Required)

| Workflow | Agent | Input | Output | Duration | Modes |
|----------|-------|-------|--------|----------|-------|
| **prd** | pm | Product brief (optional) | PRD (Product Requirements Doc) | 10-20 min | C/V/E |
| **create-ux-design** | ux-designer | PRD | UX design specification | 15-25 min | C/V/E |
| **quick-spec** (Quick Flow) | quick-flow-solo-dev | Feature description | Technical specification | 3-5 min | Single |

### Phase 3: Solutioning (Track-Dependent)

| Workflow | Agent | Input | Output | Duration | Modes |
|----------|-------|-------|--------|----------|-------|
| **create-architecture** | architect | PRD, UX design (optional) | Architecture document + ADRs | 15-30 min | C/V/E |
| **create-epics-and-stories** | pm | Architecture, PRD | Epic files with stories | 10-20 min | Create |
| **check-implementation-readiness** | architect/pm | Architecture, epics | Readiness report | 5-10 min | Validate |

### Phase 4: Implementation (Required)

| Workflow | Agent | Input | Output | Duration |
|----------|-------|-------|--------|----------|
| **sprint-planning** | sm | Epic files | Sprint status document | 5-10 min |
| **create-story** | sm | Epic file | Story file (detailed) | 5-10 min |
| **dev-story** | dev | Story file, architecture | Implementation + tests | 20-60 min |
| **code-review** | dev | Pull request | Review comments | 10-20 min |
| **correct-course** | sm | Sprint status | Course correction plan | 10-15 min |
| **retrospective** | sm | Sprint status | Retrospective document | 10-15 min |
| **sprint-status** | sm | Current sprint | Updated sprint status | 2-5 min |

### Test Architecture Workflows (Cross-Phase)

| Workflow | Agent | Input | Output | Duration |
|----------|-------|-------|--------|----------|
| **framework** | tea | Project type | Test framework setup | 10-20 min |
| **test-design** | tea | PRD, architecture | Test design document | 15-25 min |
| **atdd** | tea | Requirements | ATDD specifications | 15-25 min |
| **automate** | tea | Test design | Automated tests | 20-40 min |
| **test-review** | tea | Test suite | Review findings | 10-15 min |
| **trace** | tea | Requirements, tests | Traceability matrix | 10-15 min |
| **nfr-assess** | tea | NFRs | NFR assessment | 15-20 min |
| **ci** | tea | Tests, framework | CI/CD configuration | 15-25 min |

### Support Workflows (10+)

| Workflow | Agent | Purpose |
|----------|-------|---------|
| **workflow-status** | bmad-master | Track progress |
| **generate-project-context** | bmad-master | Generate context for agents |
| **document-project** | tech-writer | Brownfield documentation |
| **create-diagram** (excalidraw) | architect/ux | Visual diagrams |
| **create-wireframe** (excalidraw) | ux-designer | Wireframes |
| **create-flowchart** (excalidraw) | architect | Flowcharts |
| **create-dataflow** (excalidraw) | architect | Data flow diagrams |

## Artifact Nodes (Key Documents)

### Phase 1 Artifacts

| Artifact | Created By | Used By | Lifecycle |
|----------|-----------|---------|-----------|
| **brainstorming.md** | brainstorm-project | create-product-brief, prd | Analysis phase |
| **research.md** (market/domain/technical) | research | create-product-brief, prd | Analysis phase |
| **product-brief.md** | create-product-brief | prd | Analysis to Planning |
| **project-docs/** (brownfield) | document-project | All workflows | Project lifetime |

### Phase 2 Artifacts

| Artifact | Created By | Used By | Lifecycle | Modes |
|----------|-----------|---------|-----------|-------|
| **PRD.md** | prd | create-ux-design, create-architecture, create-epics | Planning to Solutioning | C/V/E |
| **ux-design.md** | create-ux-design | create-architecture, dev-story | Planning to Implementation | C/V/E |
| **tech-spec.md** (Quick Flow) | quick-spec | quick-dev | Planning to Implementation | Single |

### Phase 3 Artifacts

| Artifact | Created By | Used By | Lifecycle | Modes |
|----------|-----------|---------|-----------|-------|
| **architecture.md** | create-architecture | create-epics, dev-story | Solutioning to Implementation | C/V/E |
| **ADRs/** (Architecture Decision Records) | create-architecture | dev-story | Solutioning to Implementation | Part of architecture |
| **epic-{name}.md** (multiple) | create-epics-and-stories | sprint-planning, create-story | Solutioning to Implementation | Create |
| **readiness-report.md** | check-implementation-readiness | sprint-planning | Gate check | Validate |

### Phase 4 Artifacts

| Artifact | Created By | Used By | Lifecycle |
|----------|-----------|---------|-----------|
| **sprint-status.md** | sprint-planning | create-story, correct-course, retrospective | Sprint lifetime |
| **story-{id}.md** | create-story | dev-story | Story lifetime |
| **code/** (implementation) | dev-story | code-review | Project lifetime |
| **tests/** (test suite) | dev-story, automate | test-review, ci | Project lifetime |
| **review-{pr}.md** | code-review | dev-story (feedback) | Review lifetime |
| **retrospective.md** | retrospective | Next sprint planning | Sprint lifetime |

### Test Architecture Artifacts

| Artifact | Created By | Used By | Lifecycle |
|----------|-----------|---------|-----------|
| **test-framework-setup.md** | framework | automate, ci | Project lifetime |
| **test-design.md** | test-design | automate | Testing phase |
| **atdd-specs.md** | atdd | automate | Testing phase |
| **traceability-matrix.md** | trace | test-review | Testing phase |
| **nfr-assessment.md** | nfr-assess | test-design | Testing phase |
| **ci-config/** (CI/CD) | ci | Automated execution | Project lifetime |

## Tri-Modal Pattern (C/V/E)

Critical workflows use a tri-modal structure for quality assurance:

### Create Mode (C)
- **Purpose**: Build new artifacts from scratch or convert non-compliant documents
- **Steps**: Located in `steps-c/` directory
- **Output**: New compliant artifact
- **Entry**: User invokes with "create" or "-c" flag

### Validate Mode (V)
- **Purpose**: Check existing artifacts against standards
- **Steps**: Located in `steps-v/` directory
- **Output**: Validation report with compliance scores
- **Entry**: User invokes with "validate" or "-v" flag

### Edit Mode (E)
- **Purpose**: Modify existing artifacts while maintaining compliance
- **Steps**: Located in `steps-e/` directory
- **Output**: Updated compliant artifact
- **Entry**: User invokes with "edit" or "-e" flag

**Workflows with Tri-Modal**:
- prd (PRD creation/validation/editing)
- create-ux-design (UX specification)
- create-architecture (Architecture document)

## Information Flow Graph

### Quick Flow Track

```
quick-spec (tech-spec.md) → quick-dev (code + tests) → code-review (review) → DONE
    ↓
  Level 0-1: 5-10 minutes total
```

### BMad Method Track (Simple)

```
[Phase 1: Optional]
brainstorm-project → research → create-product-brief
         ↓              ↓              ↓
   brainstorming.md  research.md  product-brief.md
                                      ↓
[Phase 2: Planning]
                               prd (PRD.md)
                                      ↓
[Phase 3: Solutioning]
                          create-architecture (architecture.md + ADRs/)
                                      ↓
                          create-epics-and-stories (epic-*.md files)
                                      ↓
                          check-implementation-readiness (readiness-report.md)
                                      ↓
[Phase 4: Implementation]
                             sprint-planning (sprint-status.md)
                                      ↓
                             create-story (story-*.md)
                                      ↓
                             dev-story (code + tests)
                                      ↓
                             code-review (review)
                                      ↓
                             retrospective (retrospective.md)
```

### BMad Method Track (Complex with UX)

```
[Phase 1] → product-brief.md
            ↓
[Phase 2] → prd (PRD.md) → create-ux-design (ux-design.md)
                                      ↓
[Phase 3] → create-architecture (architecture.md, takes PRD + UX)
            ↓
         create-epics-and-stories (epic-*.md)
            ↓
         check-implementation-readiness
            ↓
[Phase 4] → sprint-planning → create-story → dev-story (uses architecture + UX) → code-review → retrospective
```

### Test Architecture Integration (Cross-Phase)

```
[Phase 2: Planning] → PRD.md
                        ↓
[Phase 3: Solutioning] → architecture.md
                        ↓
[TEA Workflows] → framework (test-framework-setup.md)
                        ↓
                  test-design (test-design.md)
                        ↓
                  atdd (atdd-specs.md)
                        ↓
                  automate (tests/)
                        ↓
                  ci (ci-config/)
                        ↓
[Phase 4: Implementation] → dev-story (uses test standards)
                        ↓
                  test-review (review findings)
                        ↓
                  trace (traceability-matrix.md)
```

## Dependency Graph

### Artifact Dependencies (Critical Path)

```
brainstorming.md ────┐
research.md ─────────┼──→ product-brief.md ──→ PRD.md ──┬──→ architecture.md ──→ epic-*.md ──→ story-*.md ──→ code
                     │                            ↓      │
                     │                     ux-design.md ─┘
                     │
                     └──→ (optional inputs)
```

### Validation Dependencies

```
PRD.md ──→ [Validate PRD] ──→ ✓ PRD validated
                ↓
          create-architecture
                ↓
          architecture.md ──→ [Validate Architecture] ──→ ✓ Architecture validated
                ↓
          create-epics-and-stories
                ↓
          epic-*.md ──→ [Check Implementation Readiness] ──→ ✓ Ready to implement
                ↓
          sprint-planning
```

## State Machine View

### Workflow State Transitions

```
IDLE ──[invoke workflow]──→ INITIALIZING
                               ↓
                       [load config, determine mode]
                               ↓
                          MODE_SELECT (if unclear)
                               ↓
                       [user selects C/V/E]
                               ↓
                          MODE_CONFIRMED
                               ↓
                       [load step-01]
                               ↓
                          STEP_EXECUTING
                               ↓
                       [complete step, update frontmatter]
                               ↓
                          STEP_COMPLETE
                               ↓
                       [load next step OR present menu]
                               ↓
                    ┌──────────┴──────────┐
                    ↓                     ↓
              AWAITING_INPUT         STEP_EXECUTING
                    ↓                     ↓
              [user selects]        [continue steps]
                    ↓                     ↓
              STEP_EXECUTING         STEP_COMPLETE
                    ↓                     ↓
                    └──────────┬──────────┘
                               ↓
                       [all steps complete]
                               ↓
                          WORKFLOW_COMPLETE
                               ↓
                       [write final artifact]
                               ↓
                             DONE
```

### Artifact State Machine

```
NOT_EXISTS ──[create workflow]──→ DRAFT
                                    ↓
                            [stepsCompleted tracking]
                                    ↓
                            IN_PROGRESS (continuable)
                                    ↓
                            [complete all steps]
                                    ↓
                            COMPLETE
                                    ↓
                            [validate workflow]
                                    ↓
                    ┌───────────────┴───────────────┐
                    ↓                               ↓
              VALIDATED                      NEEDS_REVISION
                    ↓                               ↓
            [use in next phase]              [edit workflow]
                    ↓                               ↓
              CONSUMED                          DRAFT
                    ↓                               ↓
            [downstream workflows use it]     [revision cycle]
                    ↓
              ARCHIVED (project complete)
```

## Validation Gates (Quality Checkpoints)

### Phase 1 Gates

| Gate | Checkpoint | Artifact | Validation Criteria |
|------|-----------|----------|---------------------|
| G1.1 | Brainstorming Complete | brainstorming.md | Ideas explored, problem statement clear, solutions identified |
| G1.2 | Research Complete | research.md | Key questions answered, decisions documented, references cited |
| G1.3 | Brief Quality | product-brief.md | Vision clear, market validated, success metrics defined |

### Phase 2 Gates

| Gate | Checkpoint | Artifact | Validation Criteria |
|------|-----------|----------|---------------------|
| G2.1 | PRD Validation | PRD.md | FRs complete, NFRs specified, testable, no implementation leakage |
| G2.2 | UX Validation | ux-design.md | User journeys mapped, wireframes complete, design system documented |
| G2.3 | Planning Complete | PRD + UX | All requirements captured, stakeholder sign-off |

### Phase 3 Gates

| Gate | Checkpoint | Artifact | Validation Criteria |
|------|-----------|----------|---------------------|
| G3.1 | Architecture Validation | architecture.md | System design complete, ADRs documented, patterns consistent |
| G3.2 | Epic Breakdown | epic-*.md | Stories well-defined, dependencies clear, estimates reasonable |
| G3.3 | Implementation Readiness | readiness-report.md | All blockers resolved, team ready, architecture approved |

### Phase 4 Gates

| Gate | Checkpoint | Artifact | Validation Criteria |
|------|-----------|----------|---------------------|
| G4.1 | Story Prepared | story-*.md | Acceptance criteria clear, tasks defined, dependencies resolved |
| G4.2 | Implementation Complete | code + tests | Tests pass, code follows architecture, story complete |
| G4.3 | Code Review Pass | review-*.md | Quality criteria met, no blocking issues, approved |
| G4.4 | Sprint Retrospective | retrospective.md | Lessons captured, improvements identified, action items |

## Parallel Execution Opportunities

### Phase 1 (Analysis)

```
brainstorm-project ──────┐
research (market) ────────┼──→ (can run in parallel if resources available)
research (technical) ─────┘
```

### Phase 2 (Planning)

```
prd ──────────────┬──→ (PRD must complete first)
                  ├──→ create-ux-design ──┐
                  │                       ├──→ (can run in parallel after PRD)
                  └──→ test-design ───────┘
```

### Phase 3 (Solutioning)

```
create-architecture ──────→ (must complete first)
                      ↓
      ┌───────────────┴───────────────┐
      ↓                               ↓
create-epics-and-stories     test-architecture workflows
      ↓                               ↓
      └───────────────┬───────────────┘
                      ↓
      check-implementation-readiness
```

### Phase 4 (Implementation)

```
sprint-planning ──→ create-story-1 ──→ dev-story-1 ──→ code-review-1
                    create-story-2 ──→ dev-story-2 ──→ code-review-2 (if no dependencies)
                    create-story-3 ──→ dev-story-3 ──→ code-review-3 (if no dependencies)
```

**Note**: Stories can be implemented in parallel if they:
- Affect different files
- Have no dependencies
- Don't conflict with each other

## Critical Path Analysis

### Quick Flow Critical Path

```
quick-spec (3-5 min) → quick-dev (10-30 min) → code-review (5-10 min)
Total: 18-45 minutes (critical path)
```

### BMad Method Critical Path (No Phase 1)

```
prd (10-20 min) → create-architecture (15-30 min) → create-epics (10-20 min) → check-readiness (5-10 min) → sprint-planning (5-10 min) → create-story (5-10 min) → dev-story (20-60 min) → code-review (10-20 min)

Total: 80-180 minutes per story (critical path)
```

### BMad Method Critical Path (With Phase 1)

```
brainstorm (10-20 min) → research (15-30 min) → brief (10-15 min) → prd (10-20 min) → create-architecture (15-30 min) → create-epics (10-20 min) → check-readiness (5-10 min) → sprint-planning (5-10 min) → create-story (5-10 min) → dev-story (20-60 min) → code-review (10-20 min)

Total: 115-245 minutes per story (critical path)
```

## Bottlenecks and Optimizations

### Bottlenecks

1. **PRD Creation**: Blocks all downstream workflows (solutioning + implementation)
2. **Architecture Creation**: Blocks epic breakdown and implementation
3. **Story Preparation**: Blocks implementation (one story at a time)
4. **Code Review**: Sequential bottleneck if single reviewer

### Optimizations

1. **Parallel Research**: Run market/technical/domain research in parallel (Phase 1)
2. **Concurrent Planning**: UX design and test design can run after PRD (Phase 2)
3. **Story Parallelization**: Multiple stories can be implemented concurrently if no dependencies
4. **Pre-validation**: Run validation workflows incrementally during creation (not just at end)
5. **Incremental Architecture**: Start with architectural outline, detail incrementally
6. **Reusable ADRs**: Template common decisions for faster architecture

## Graph Metrics

### Complexity Metrics

| Track | Workflows | Agents | Artifacts | Gates | Critical Path |
|-------|-----------|--------|-----------|-------|---------------|
| Quick Flow | 3 | 1 | 3 | 3 | 18-45 min |
| BMad Method (Simple) | 10-12 | 5-7 | 10-12 | 12 | 80-180 min |
| BMad Method (Complex) | 15-18 | 7-9 | 15-20 | 15 | 115-245 min |
| Enterprise | 20-25 | 9-12 | 20-30 | 20 | 150-300 min |

### Degree Analysis (BMM Module)

| Node Type | Count | Average In-Degree | Average Out-Degree |
|-----------|-------|-------------------|-------------------|
| Workflows | 34+ | 1.5 (inputs) | 1.8 (outputs) |
| Agents | 9 (core) | 0 | 3.8 (workflows per agent) |
| Artifacts | 25+ | 1.2 (creators) | 2.5 (consumers) |
| Gates | 16 | 1 (artifact) | 1 (pass/fail) |

### Centrality Analysis

**High Centrality Nodes** (critical to many paths):
1. **PRD.md**: Used by architecture, UX, epics, test design (betweenness centrality: high)
2. **architecture.md**: Used by epics, stories, dev workflows (betweenness centrality: high)
3. **architect agent**: Creates architecture, validates readiness (degree centrality: high)
4. **pm agent**: Creates PRD, creates epics (degree centrality: high)
5. **dev agent**: Implements stories, reviews code (degree centrality: high)

**Low Centrality Nodes** (optional or terminal):
1. **brainstorming.md**: Optional input to PRD
2. **retrospective.md**: Terminal output, doesn't feed forward
3. **review-*.md**: Terminal output per story

## Progressive Disclosure Architecture

BMAD-METHOD uses a unique **step-file architecture** to enforce workflow discipline:

### Core Principles

1. **Micro-File Design**: Each step is a self-contained instruction file
2. **Just-In-Time Loading**: Only current step is in memory (never load future steps)
3. **Sequential Enforcement**: Steps must be completed in order, no skipping
4. **State Tracking**: Progress tracked in output file frontmatter (`stepsCompleted` array)
5. **Append-Only Building**: Build documents by appending content as directed

### Step Processing Rules

```
LOAD step-N.md
↓
READ COMPLETELY (never skim)
↓
EXECUTE all numbered sections in order
↓
IF menu present THEN
    HALT and WAIT for user input
    ↓
    [User selects option]
    ↓
    CONTINUE based on selection
END IF
↓
UPDATE frontmatter (stepsCompleted)
↓
IF next step exists THEN
    LOAD step-(N+1).md
    REPEAT
ELSE
    WORKFLOW COMPLETE
END IF
```

### Critical Rules (No Exceptions)

- 🛑 **NEVER** load multiple step files simultaneously
- 📖 **ALWAYS** read entire step file before execution
- 🚫 **NEVER** skip steps or optimize sequence
- 💾 **ALWAYS** update frontmatter when writing output
- 🎯 **ALWAYS** follow exact instructions in step file
- ⏸️ **ALWAYS** halt at menus and wait for user input
- 📋 **NEVER** create mental todo lists from future steps

### Example: PRD Workflow (Create Mode)

```
workflow.md (entry point)
    ↓
steps-c/step-01-init.md (initialize PRD structure)
    ↓
steps-c/step-02-context.md (gather project context)
    ↓
steps-c/step-03-vision.md (define vision and goals)
    ↓
steps-c/step-04-users.md (identify user personas)
    ↓
steps-c/step-05-functional.md (define functional requirements)
    ↓
steps-c/step-06-nonfunctional.md (define non-functional requirements)
    ↓
steps-c/step-07-dependencies.md (document dependencies)
    ↓
... (continue through ~13 steps)
    ↓
steps-c/step-13-finalize.md (complete PRD)
```

Each step file contains:
- Clear instructions for that step only
- Questions to ask user (facilitative, not directive)
- What to write to output file
- Menu options (Continue, Revise, Exit)
- Next step to load (only revealed when ready)

## Related Concepts

### Continuable Workflows

Some workflows support **multi-session execution**:
- User can stop mid-workflow and resume later
- Progress tracked in `stepsCompleted` frontmatter array
- State persisted in output document
- Used for complex workflows (PRD, architecture, epics)

### Workflow Chaining

Workflows chain together where outputs become inputs:
```
brainstorming → research → brief → PRD → UX → architecture → epics → sprint-planning → implement-story → review
```

### Agent Context Management

Agents receive contextualized information:
- Project configuration (output folders, names, languages)
- User skill level (beginner/intermediate/expert)
- Module variables (planning_artifacts path, etc.)
- Workflow status (current phase, completed workflows)

## Summary

The BMAD-METHOD workflow graph represents a comprehensive, scale-adaptive system for AI-driven agile development. Key characteristics:

1. **Four-Phase Structure**: Analysis → Planning → Solutioning → Implementation
2. **34+ Workflows**: Cover entire lifecycle from brainstorming to deployment
3. **29 Specialized Agents**: Experts for each domain and role
4. **25+ Artifacts**: Documents that flow between phases
5. **16+ Validation Gates**: Quality checkpoints throughout
6. **Tri-Modal Pattern**: Create/Validate/Edit for critical artifacts
7. **Progressive Disclosure**: Step-file architecture enforces discipline
8. **Scale-Adaptive**: Automatically adjusts depth to project complexity
9. **Three Tracks**: Quick Flow, BMad Method, Enterprise
10. **Multi-Session Support**: Continuable workflows with state tracking

The graph enables parallel execution where possible, enforces dependencies where necessary, and provides multiple validation checkpoints to ensure quality throughout the development lifecycle.

---

**Analysis**: BMAD-METHOD Workflow Graph
**Date**: 2026-01-11, 8:45 AM EST
**Source**: https://github.com/bmad-code-org/BMAD-METHOD
