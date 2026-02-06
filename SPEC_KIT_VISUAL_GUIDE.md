# Spec-Kit Visual Guide

**Project**: GitHub spec-kit Visual Reference
**Created**: 2026-01-11

## Quick Reference Diagrams

### 1. Complete Workflow Diagram

```mermaid
graph TD
    A[User Description] --> B[/speckit.constitution]
    B --> C[constitution.md]
    C --> D[/speckit.specify]
    A --> D
    D --> E[spec.md]
    E --> F{Needs Clarification?}
    F -->|Yes| G[/speckit.clarify]
    G --> E
    F -->|No| H[/speckit.plan]
    E --> H
    C --> H
    H --> I[plan.md]
    H --> J[research.md]
    H --> K[data-model.md]
    H --> L[contracts/]
    H --> M[quickstart.md]
    I --> N[/speckit.tasks]
    E --> N
    K --> N
    L --> N
    N --> O[tasks.md]
    O --> P[/speckit.implement]
    I --> P
    E --> P
    K --> P
    L --> P
    J --> P
    M --> P
    P --> Q[Code]
    Q --> R{Tests Pass?}
    R -->|No| S[Fix Issues]
    S --> Q
    R -->|Yes| T[Feature Complete]

    style B fill:#e1f5ff
    style D fill:#e1f5ff
    style G fill:#fff4e1
    style H fill:#e1f5ff
    style N fill:#e1f5ff
    style P fill:#e1f5ff
    style T fill:#e1ffe1
```

### 2. State Machine Diagram

```mermaid
stateDiagram-v2
    [*] --> Unspecified
    Unspecified --> Constituted: /speckit.constitution
    Constituted --> Specified: /speckit.specify
    Specified --> Clarified: /speckit.clarify (optional)
    Specified --> Planned: /speckit.plan (skip clarify)
    Clarified --> Planned: /speckit.plan
    Planned --> Tasked: /speckit.tasks
    Tasked --> Implemented: /speckit.implement
    Implemented --> Validated: tests pass
    Implemented --> Implemented: tests fail → fix
    Validated --> [*]

    note right of Specified
        Validation Gate 1:
        - No implementation details
        - Requirements testable
        - ≤3 clarifications
    end note

    note right of Planned
        Validation Gate 3:
        - Constitutional compliance
        - Research complete
        - Contracts defined
    end note

    note right of Tasked
        Validation Gate 5:
        - All tasks valid format
        - Dependencies correct
        - File paths specified
    end note
```

### 3. Information Flow Graph

```mermaid
flowchart LR
    subgraph Templates
        T1[spec-template.md]
        T2[plan-template.md]
        T3[tasks-template.md]
        T4[constitution-template.md]
    end

    subgraph "User Input"
        U1[Feature Description]
        U2[Tech Stack Preferences]
    end

    subgraph "Generated Artifacts"
        A1[constitution.md]
        A2[spec.md]
        A3[plan.md]
        A4[research.md]
        A5[data-model.md]
        A6[contracts/]
        A7[tasks.md]
        A8[Code]
    end

    U1 --> T1
    T1 --> A2
    U1 --> T2
    U2 --> T2
    T2 --> A3
    A2 --> A3
    A1 --> A3
    A3 --> A4
    A3 --> A5
    A3 --> A6
    T3 --> A7
    A2 --> A7
    A3 --> A7
    A5 --> A7
    A6 --> A7
    A7 --> A8
    A3 --> A8
    A5 --> A8
    A6 --> A8
    T4 --> A1

    style T1 fill:#ffe1e1
    style T2 fill:#ffe1e1
    style T3 fill:#ffe1e1
    style T4 fill:#ffe1e1
    style A8 fill:#e1ffe1
```

### 4. Validation Gates Flowchart

```mermaid
flowchart TD
    Start[Start Workflow] --> Gate1{Gate 1:<br/>Spec Quality}
    Gate1 -->|Pass| Gate2{Gate 2:<br/>Clarifications<br/>Resolved?}
    Gate1 -->|Fail| Fix1[Update Spec]
    Fix1 --> Gate1
    Gate2 -->|Yes| Gate3{Gate 3:<br/>Constitutional<br/>Compliance}
    Gate2 -->|No| Clarify[/speckit.clarify]
    Clarify --> Gate2
    Gate3 -->|Pass| Gate4{Gate 4:<br/>Research<br/>Complete}
    Gate3 -->|Fail| Justify[Justify Violations<br/>in Complexity Tracking]
    Justify --> Gate4
    Gate4 -->|Pass| Gate5{Gate 5:<br/>Tasks Valid<br/>Format}
    Gate4 -->|Fail| Research[Continue Research]
    Research --> Gate4
    Gate5 -->|Pass| Gate6{Gate 6:<br/>Checklists<br/>Complete}
    Gate5 -->|Fail| FixTasks[Regenerate tasks.md]
    FixTasks --> Gate5
    Gate6 -->|Pass| Gate7{Gate 7:<br/>Tests Pass}
    Gate6 -->|User Confirms| Gate7
    Gate7 -->|Pass| Complete[Feature Complete]
    Gate7 -->|Fail| FixCode[Fix Issues]
    FixCode --> Gate7

    style Gate1 fill:#fff4e1
    style Gate2 fill:#fff4e1
    style Gate3 fill:#fff4e1
    style Gate4 fill:#fff4e1
    style Gate5 fill:#fff4e1
    style Gate6 fill:#fff4e1
    style Gate7 fill:#fff4e1
    style Complete fill:#e1ffe1
```

### 5. Task Execution Graph

```mermaid
graph TD
    subgraph "Phase 1: Setup"
        T001[T001: Project Init]
        T002[T002: Dependencies]
        T003[T003 P: Linting]
    end

    subgraph "Phase 2: Foundational"
        T004[T004: Database]
        T005[T005 P: Auth]
        T006[T006 P: API Structure]
    end

    subgraph "Phase 3: User Story 1 P1"
        T007[T007 P US1: Test 1]
        T008[T008 P US1: Test 2]
        T009[T009 P US1: Model A]
        T010[T010 P US1: Model B]
        T011[T011 US1: Service]
        T012[T012 US1: Endpoint]
    end

    subgraph "Phase 4: User Story 2 P2"
        T013[T013 P US2: Test]
        T014[T014 P US2: Model]
        T015[T015 US2: Service]
        T016[T016 US2: Endpoint]
    end

    subgraph "Phase 5: Polish"
        T017[T017 P: Docs]
        T018[T018 P: Optimization]
    end

    T001 --> T002
    T002 --> T003
    T001 --> T004
    T004 --> T005
    T004 --> T006
    T005 --> T007
    T005 --> T008
    T006 --> T007
    T006 --> T008
    T007 --> T009
    T007 --> T010
    T008 --> T009
    T008 --> T010
    T009 --> T011
    T010 --> T011
    T011 --> T012

    T005 --> T013
    T006 --> T013
    T013 --> T014
    T014 --> T015
    T015 --> T016

    T012 --> T017
    T016 --> T017
    T012 --> T018
    T016 --> T018

    style T003 fill:#e1f5ff
    style T005 fill:#e1f5ff
    style T006 fill:#e1f5ff
    style T007 fill:#e1f5ff
    style T008 fill:#e1f5ff
    style T009 fill:#e1f5ff
    style T010 fill:#e1f5ff
    style T013 fill:#e1f5ff
    style T014 fill:#e1f5ff
    style T017 fill:#e1f5ff
    style T018 fill:#e1f5ff
```

### 6. Template Constraint System

```mermaid
graph LR
    subgraph "Spec Template Constraints"
        SC1[NO implementation details]
        SC2[Focus on WHAT/WHY]
        SC3[Max 3 clarifications]
        SC4[Requirements testable]
        SC5[Success criteria measurable]
    end

    subgraph "Plan Template Constraints"
        PC1[Simplicity Gate: ≤3 projects]
        PC2[Anti-Abstraction Gate]
        PC3[Integration-First Gate]
        PC4[All unknowns in research.md]
        PC5[Contracts must exist]
    end

    subgraph "Task Template Constraints"
        TC1[Strict checkbox format]
        TC2[User story labels required]
        TC3[File paths mandatory]
        TC4[Parallel marking P]
        TC5[Independent testing per story]
    end

    subgraph "LLM Behavior"
        LLM[Language Model]
    end

    SC1 --> LLM
    SC2 --> LLM
    SC3 --> LLM
    SC4 --> LLM
    SC5 --> LLM
    PC1 --> LLM
    PC2 --> LLM
    PC3 --> LLM
    PC4 --> LLM
    PC5 --> LLM
    TC1 --> LLM
    TC2 --> LLM
    TC3 --> LLM
    TC4 --> LLM
    TC5 --> LLM

    LLM --> Output[Constrained High-Quality Output]

    style SC1 fill:#ffe1e1
    style SC2 fill:#ffe1e1
    style SC3 fill:#ffe1e1
    style SC4 fill:#ffe1e1
    style SC5 fill:#ffe1e1
    style PC1 fill:#fff4e1
    style PC2 fill:#fff4e1
    style PC3 fill:#fff4e1
    style PC4 fill:#fff4e1
    style PC5 fill:#fff4e1
    style TC1 fill:#e1f5ff
    style TC2 fill:#e1f5ff
    style TC3 fill:#e1f5ff
    style TC4 fill:#e1f5ff
    style TC5 fill:#e1f5ff
    style Output fill:#e1ffe1
```

### 7. Constitutional Articles Structure

```mermaid
mindmap
    root((Constitution))
        Article I
            Library-First Principle
            Standalone libraries
            Clear purpose
        Article II
            CLI Interface Mandate
            Text in/out
            JSON support
        Article III
            Test-First Imperative
            NON-NEGOTIABLE
            Red-Green-Refactor
        Article VII
            Simplicity
            ≤3 projects
            No future-proofing
        Article VIII
            Anti-Abstraction
            Framework directly
            Single model
        Article IX
            Integration-First
            Real databases
            Contract tests
```

### 8. Parallel Execution Opportunities

```mermaid
gantt
    title Task Execution Timeline (with parallelization)
    dateFormat X
    axisFormat %s

    section Setup
    T001 Project Init       :0, 5
    T002 Dependencies       :5, 10
    T003 Linting [P]        :5, 8

    section Foundation
    T004 Database           :10, 20
    T005 Auth [P]           :20, 30
    T006 API Structure [P]  :20, 30

    section US1 Tests
    T007 Test 1 [P]         :30, 35
    T008 Test 2 [P]         :30, 35

    section US1 Models
    T009 Model A [P]        :35, 45
    T010 Model B [P]        :35, 45

    section US1 Services
    T011 Service            :45, 60
    T012 Endpoint           :60, 70

    section US2
    T013 Test [P]           :30, 35
    T014 Model [P]          :35, 45
    T015 Service            :45, 60
    T016 Endpoint           :60, 70

    section Polish
    T017 Docs [P]           :70, 75
    T018 Optimization [P]   :70, 75
```

## Quick Command Reference

### Command Flow

```
1. /speckit.constitution → Establish principles
   ↓
2. /speckit.specify → Create specification
   ↓
3. /speckit.clarify → Resolve ambiguities (optional)
   ↓
4. /speckit.plan → Generate technical plan
   ↓
5. /speckit.tasks → Break down into tasks
   ↓
6. /speckit.implement → Execute implementation
```

### Command I/O Summary

| Command | Input | Output | Script Called |
|---------|-------|--------|---------------|
| /speckit.constitution | User principles | constitution.md | None |
| /speckit.specify | Feature description | spec.md, checklists/requirements.md | create-new-feature.sh |
| /speckit.clarify | spec.md with [NEEDS CLARIFICATION] | Updated spec.md | None |
| /speckit.plan | spec.md + Tech stack | plan.md, research.md, data-model.md, contracts/, quickstart.md | setup-plan.sh, update-agent-context.sh |
| /speckit.tasks | plan.md + spec.md | tasks.md | check-prerequisites.sh |
| /speckit.implement | tasks.md + all design docs | Code | check-prerequisites.sh (with flags) |

## Artifact Dependency Graph

```mermaid
graph TD
    Constitution[constitution.md]
    Spec[spec.md]
    Plan[plan.md]
    Research[research.md]
    DataModel[data-model.md]
    Contracts[contracts/]
    Quickstart[quickstart.md]
    Tasks[tasks.md]
    Code[Implementation Code]

    Constitution --> Spec
    Spec --> Plan
    Plan --> Research
    Plan --> DataModel
    Plan --> Contracts
    Plan --> Quickstart
    Spec --> Tasks
    Plan --> Tasks
    DataModel --> Tasks
    Contracts --> Tasks
    Tasks --> Code
    Plan --> Code
    Spec --> Code
    DataModel --> Code
    Contracts --> Code
    Research --> Code
    Quickstart --> Code

    style Constitution fill:#ffe1e1
    style Spec fill:#fff4e1
    style Plan fill:#e1f5ff
    style Tasks fill:#e1ffe1
    style Code fill:#d4f1d4
```

## Template Structure Overview

### Spec Template
```
┌─────────────────────────────────────┐
│ Feature Specification               │
├─────────────────────────────────────┤
│ • User Scenarios (with priorities)  │
│ • Acceptance Scenarios (Given/When) │
│ • Requirements (FR-001, FR-002)     │
│ • Key Entities (data involved)      │
│ • Success Criteria (measurable)     │
│ • Edge Cases                        │
│ • Assumptions                       │
│                                     │
│ Constraints:                        │
│ ✗ NO tech stack                     │
│ ✗ NO implementation details         │
│ ✓ Focus on WHAT and WHY             │
│ ✓ Max 3 [NEEDS CLARIFICATION]       │
└─────────────────────────────────────┘
```

### Plan Template
```
┌─────────────────────────────────────┐
│ Implementation Plan                 │
├─────────────────────────────────────┤
│ • Technical Context                 │
│ • Constitution Check                │
│ • Project Structure                 │
│ • Complexity Tracking               │
│                                     │
│ Phase 0: Research                   │
│ • Resolve all unknowns              │
│                                     │
│ Phase 1: Design                     │
│ • data-model.md                     │
│ • contracts/                        │
│ • quickstart.md                     │
│                                     │
│ Validation Gates:                   │
│ ✓ Simplicity (≤3 projects)          │
│ ✓ Anti-Abstraction (framework)      │
│ ✓ Integration-First (contracts)     │
└─────────────────────────────────────┘
```

### Tasks Template
```
┌─────────────────────────────────────┐
│ Task Breakdown                      │
├─────────────────────────────────────┤
│ Phase 1: Setup                      │
│ - [ ] T001 ...                      │
│ - [ ] T002 [P] ...                  │
│                                     │
│ Phase 2: Foundational (BLOCKS ALL)  │
│ - [ ] T003 ...                      │
│                                     │
│ Phase 3: User Story 1 (P1) MVP      │
│ - [ ] T004 [P] [US1] Test ...       │
│ - [ ] T005 [P] [US1] Model ...      │
│ - [ ] T006 [US1] Service ...        │
│                                     │
│ Phase 4: User Story 2 (P2)          │
│ - [ ] T007 [P] [US2] ...            │
│                                     │
│ Final Phase: Polish                 │
│ - [ ] T008 [P] Docs ...             │
│                                     │
│ Format: - [ ] [ID] [P?] [Story?]    │
│         Description with file path  │
└─────────────────────────────────────┘
```

## File Organization

```
project-root/
├── .specify/
│   ├── memory/
│   │   └── constitution.md
│   ├── scripts/
│   │   ├── bash/
│   │   │   ├── common.sh
│   │   │   ├── create-new-feature.sh
│   │   │   ├── setup-plan.sh
│   │   │   ├── check-prerequisites.sh
│   │   │   └── update-agent-context.sh
│   │   └── powershell/
│   │       └── (equivalent .ps1 files)
│   └── templates/
│       ├── spec-template.md
│       ├── plan-template.md
│       ├── tasks-template.md
│       └── commands/
│           ├── specify.md
│           ├── plan.md
│           ├── tasks.md
│           ├── implement.md
│           └── ...
├── specs/
│   ├── 001-feature-name/
│   │   ├── spec.md
│   │   ├── plan.md
│   │   ├── research.md
│   │   ├── data-model.md
│   │   ├── quickstart.md
│   │   ├── tasks.md
│   │   ├── contracts/
│   │   │   └── api-spec.json
│   │   └── checklists/
│   │       └── requirements.md
│   └── 002-another-feature/
│       └── ...
├── src/
│   └── (implementation code)
└── tests/
    └── (test code)
```

## Success Criteria Checklist

### For Specifications
- [ ] No implementation details (languages, frameworks, APIs)
- [ ] Focused on user value and business needs
- [ ] Requirements are testable and unambiguous
- [ ] Success criteria are measurable
- [ ] All acceptance scenarios defined
- [ ] Edge cases identified
- [ ] Maximum 3 [NEEDS CLARIFICATION] markers

### For Plans
- [ ] Technical context complete
- [ ] Constitution gates passed or justified
- [ ] All unknowns resolved in research.md
- [ ] Data model covers all entities
- [ ] Contracts generated for all requirements
- [ ] Quickstart scenarios defined

### For Tasks
- [ ] All tasks follow format: - [ ] [ID] [P?] [Story?] Description with file path
- [ ] Each user story has complete task set
- [ ] Dependencies are correct
- [ ] File paths are absolute and specified
- [ ] Parallel opportunities marked with [P]

### For Implementation
- [ ] All tasks marked complete [X]
- [ ] Tests pass (if TDD approach)
- [ ] Features match spec.md requirements
- [ ] Quickstart scenarios validated

## Formula Quick Reference

### Quality Score
```
specQuality = 0.3*clarity + 0.3*completeness + 0.2*testability + 0.2*measurability
where clarity = 1 - (clarifications/3)
```

### Parallelization
```
speedup = sequential_time / parallel_time
parallel_time = Σ max(time(group)) for each parallel group
```

### Critical Path
```
CP = max{Σ time(task) for task in path}
```

### Constitutional Compliance
```
compliant = ∀ gate : passes(gate) ∨ justified(violation(gate))
```

## Color Legend

In the diagrams above:
- **Red** (#ffe1e1): Templates (immutable structure)
- **Orange** (#fff4e1): Validation gates
- **Blue** (#e1f5ff): Commands and activities
- **Light Green** (#e1ffe1): Generated artifacts
- **Green** (#e1ffe1): Final outputs and success states

## Best Practices Summary

1. **Always start with constitution** - Establish principles first
2. **Keep specs technology-agnostic** - Focus on WHAT, not HOW
3. **Limit clarifications to 3** - Make informed guesses for the rest
4. **Validate at each gate** - Don't proceed with failures
5. **Organize by user story** - Enable incremental delivery
6. **Mark parallel tasks** - Optimize execution time
7. **Test-first always** - Non-negotiable for quality
8. **Justify complexity** - Track violations in Complexity Tracking table
9. **Update agent context** - Keep AI tools informed of tech choices
10. **Validate quickstart** - Manual testing scenarios are crucial

## Common Pitfalls to Avoid

1. ❌ Including tech stack in specifications
2. ❌ Skipping constitutional compliance checks
3. ❌ More than 3 [NEEDS CLARIFICATION] markers
4. ❌ Organizing tasks by technical layer instead of user story
5. ❌ Forgetting to mark parallel tasks with [P]
6. ❌ Omitting file paths in task descriptions
7. ❌ Writing implementation before tests
8. ❌ Violating constitution without justification
9. ❌ Proceeding with incomplete checklists
10. ❌ Skipping validation gates

---

**Visual Guide Complete**

This visual guide complements the detailed analysis documents and provides quick-reference diagrams for understanding spec-kit's workflow, structure, and patterns.
