# Specification Creation Workflow

**A Reusable Process for Creating Comprehensive Software Specifications**

Created: 2026-01-11
Based on: Event System specification creation (6,157 lines across 19 files)
Agents: 3 parallel agents completing 3 specification types in 10 minutes

---

## Executive Summary

This document captures the workflow used to create comprehensive specifications for the Event System, producing BDD scenarios, state machine specifications, and FIT decision tables. The process demonstrated:

- **High parallelism**: 3 agents working simultaneously on different specification types
- **Type-based organization**: Separate by specification formalism (BDD, State Machine, FIT) rather than by component
- **Source-driven**: Each agent read actual source code to create accurate specifications
- **Example-driven**: All specifications are concrete, testable, and implementation-ready

**Key Result**: 6,157 lines of specifications created in approximately 10 minutes using 3 parallel background agents.

---

## What We Did: Actual Process from Logs

### 1. Planning Phase (Manual)

**Created parent epic** with clear scope:
```bash
bd create --type epic \
  --title "Add proper specifications to Event System actors" \
  --description "Comprehensive implementation plan to add BDD scenarios,
                 state machine specs, FIT fixtures, and natural language
                 runners to all 6 Event System actors"
```

**Result**: Epic `agentic-primer-wne` (parent for all spec work)

### 2. Work Breakdown (Manual)

Created **3 parallel P0 tasks**, one per specification type:

```bash
# Task 1: BDD Scenarios
bd create --priority P0 \
  --title "Create BDD scenarios for all 6 actors (Gherkin .feature files)" \
  --description "Create comprehensive Gherkin .feature files for each actor covering:
  - Lifecycle transitions (start/stop/status)
  - Happy paths (successful operations)
  - Error paths (failures, edge cases)
  - Concurrent operations
  - Resource management

  Actors to cover:
  1. EventLogActor (event-log.js) - JSONL storage, append, query, checkpoint, replay
  2. HTTPServerActor (http-server.js) - REST API endpoints, request handling
  3. PatternMatcherActor (pattern-matcher.js) - Pattern registration, matching
  4. FunctionRegistryActor (function-registry.js) - Function catalog, scanning
  5. FunctionExecutorActor (function-executor.js) - Code/agent execution
  6. DaemonActor (daemon.js) - Orchestration (if exists)

  File locations: /path/to/specs/features/
  Example files: event-log-actor.feature, http-server-actor.feature, etc.

  Success criteria:
  - All 6 actors have .feature files
  - Each file has minimum 5 scenarios covering lifecycle and core operations
  - Scenarios use proper Given/When/Then format
  - All scenarios are concrete and testable"

# Task 2: State Machine Specifications
bd create --priority P0 \
  --title "Create state machine specifications with complete transition tables" \
  --description "Create complete state machine specifications for each actor with:
  - Complete state transition tables (markdown format)
  - Transition guards/conditions
  - State invariants
  - Entry/exit actions
  - Error states

  Actors to specify:
  1. EventLogActor - States: STOPPED, RUNNING (with write stream)
  2. HTTPServerActor - States: STOPPED, STARTING, RUNNING, STOPPING, ERROR
  3. PatternMatcherActor - States: STOPPED, RUNNING
  4. FunctionRegistryActor - States: STOPPED, RUNNING
  5. FunctionExecutorActor - States: STOPPED, RUNNING
  6. DaemonActor - States: STOPPED, STARTING, RUNNING, STOPPING, ERROR

  File locations: /path/to/specs/state-machines/
  Example files: event-log-actor-state-machine.md

  Table format:
  | Current State | Event | Guards | Next State | Actions | Error Handling |

  Success criteria:
  - All 6 actors have complete state machine specifications
  - All transitions documented with guards and actions
  - Error states and recovery paths specified
  - Invariants documented for each state"

# Task 3: FIT Decision Tables
bd create --priority P0 \
  --title "Create FIT/SLIM decision tables for all actors" \
  --description "Create FIT-style decision tables for testing actor behavior declaratively:

  Decision tables needed per actor:
  - Lifecycle state transitions (start/stop/getStatus)
  - Core operation input/output tables
  - Error condition tables
  - Integration scenario tables

  Actors to cover:
  1. EventLogActor - append, query, checkpoint, replay tables
  2. HTTPServerActor - endpoint routing, request/response tables
  3. PatternMatcherActor - pattern matching decision tables
  4. FunctionRegistryActor - registration, listing tables
  5. FunctionExecutorActor - execution success/failure tables
  6. DaemonActor - orchestration tables

  File locations: /path/to/specs/fit-fixtures/
  Example files: event-log-actor.fit.md

  Table format:
  | Input State | Action | Input Data | Expected State | Expected Output | Success |

  Success criteria:
  - All 6 actors have FIT decision tables
  - Minimum 3 tables per actor (lifecycle, happy path, error path)
  - All tables are executable format
  - Tables cover edge cases and boundaries"
```

**Result**: 3 beads (gsq, lcc, 7pb) blocked on parent epic

### 3. Parallel Execution Phase (Automated)

Launched **3 background agents simultaneously**:

```bash
# Start all 3 agents in parallel
bg "Complete the work for bead agentic-primer-gsq: Create BDD scenarios for all 6 actors (Gherkin .feature files).

Review the bead details with \`bd show agentic-primer-gsq\` and implement the complete specification.

When complete:
1. Mark the bead as completed: \`bd close agentic-primer-gsq\`
2. Stage and commit your changes
3. Provide a summary of what was created"

bg "Complete the work for bead agentic-primer-lcc: Create state machine specifications with complete transition tables.

Review the bead details with \`bd show agentic-primer-lcc\` and implement the complete specification.

When complete:
1. Mark the bead as completed: \`bd close agentic-primer-lcc\`
2. Stage and commit your changes
3. Provide a summary of what was created"

bg "Complete the work for bead agentic-primer-7pb: Create FIT/SLIM decision tables for all actors.

Review the bead details with \`bd show agentic-primer-7pb\` and implement the complete specification.

When complete:
1. Mark the bead as completed: \`bd close agentic-primer-7pb\`
2. Stage and commit your changes
3. Provide a summary of what was created"
```

**Agent Workflow** (identical for all 3):

1. **Read bead details**: `bd show <bead-id>`
2. **Identify actors**: From bead description (6 actors listed)
3. **Read source code**: Read each actor's implementation file
4. **Create specifications**: Generate spec files based on source code analysis
5. **Validate completeness**: Check all 6 actors covered
6. **Close bead**: Mark work complete
7. **Commit changes**: Stage and commit with descriptive message

**Timeline**:
- 07:01-07:05: BDD agent creates 6 .feature files
- 07:02-07:09: State Machine agent creates 7 .md files (6 actors + README)
- 07:02-07:07: FIT agent creates 6 .fit.md files

**Result**: 19 specification files created in parallel

### 4. Integration Phase (Manual Review)

After agents completed:
1. Reviewed agent outputs
2. Verified file structure and organization
3. Confirmed specifications matched source code
4. Checked cross-references between spec types

---

## Why It Worked: Analysis

### 1. Organization by Specification Type (Not Component)

**Decision**: Create one task per specification type (BDD, State Machine, FIT) covering ALL actors, rather than one task per actor covering all spec types.

**Rationale**:
- **Cognitive consistency**: Each agent maintains one mental model (e.g., "I write Gherkin scenarios")
- **Format expertise**: Agent develops expertise in one specification formalism
- **Parallel efficiency**: No dependencies between spec types, maximum parallelism
- **Quality consistency**: Single agent ensures consistent style within each spec type
- **Reusable patterns**: Agent can reuse patterns across similar actors

**Alternative (Not Used)**: One task per actor (6 tasks)
- **Problems**: Each agent must switch between 3 different formalisms, reducing quality and speed
- **Dependencies**: Harder to parallelize if actors have dependencies
- **Inconsistency**: 6 different agents writing BDD scenarios = 6 different styles

**Verdict**: **Type-based organization is superior for specification work**

### 2. Detailed Task Descriptions

Each bead included:
- **Explicit actor list**: Names and file paths for all 6 actors
- **Success criteria**: Measurable completion criteria
- **File locations**: Exact directory paths for outputs
- **Format examples**: Table structures and naming conventions
- **Minimum requirements**: "Minimum 5 scenarios", "Minimum 3 tables per actor"

**Impact**: Agents had zero ambiguity, no need for clarification questions

### 3. Source Code as Ground Truth

Agents read actual implementation files:
```javascript
// Agents read these files:
src/actors/event-log.js
src/actors/http-server.js
src/actors/pattern-matcher.js
src/actors/function-registry.js
src/actors/function-executor.js
src/actors/daemon.js
```

**Impact**: Specifications accurately reflected actual implementation, not idealized designs

### 4. Atomic Commits per Spec Type

Each agent made one commit:
- `254cb32 07:06:15 feat: Add comprehensive BDD scenarios for all 6 actors`
- `aaf2626 07:09:44 feat: Add FIT decision tables for FunctionExecutorActor and DaemonActor`
- `ecefaff 07:10:31 feat: Add complete state machine specifications for all 6 actors`

**Impact**: Clean git history, easy to review and rollback if needed

### 5. Self-Contained Execution

Each agent:
- Read its own bead for instructions
- Located and read source files independently
- Created output files in designated directories
- Closed its own bead
- Made its own commit

**Impact**: No coordination overhead, true parallel execution

---

## How to Reuse It: Generic Workflow

### Prerequisites

1. **Beads or equivalent issue tracker** for task management
2. **Background agent capability** (`bg` command or equivalent)
3. **Source code** to be specified
4. **Directory structure** for specifications

### Work Breakdown Structure (WBS)

```
Project: Create Specifications for System X
│
├── Epic: Add comprehensive specifications
│   │
│   ├── Task 1 (P0): Create BDD scenarios for all components
│   │   - Input: List of N components, source code locations
│   │   - Output: N .feature files in specs/features/
│   │   - Agent: BDD specialist
│   │   - Time: ~10min for 6 components
│   │
│   ├── Task 2 (P0): Create state machine specifications
│   │   - Input: List of N components, source code locations
│   │   - Output: N state-machine.md files in specs/state-machines/
│   │   - Agent: State machine specialist
│   │   - Time: ~10min for 6 components
│   │
│   ├── Task 3 (P0): Create FIT decision tables
│   │   - Input: List of N components, source code locations
│   │   - Output: N .fit.md files in specs/fit-fixtures/
│   │   - Agent: FIT specialist
│   │   - Time: ~10min for 6 components
│   │
│   ├── Task 4 (P1): Implement test runners (serial, after specs complete)
│   │   - Input: All specification files from Tasks 1-3
│   │   - Output: Test harness for BDD and FIT
│   │
│   ├── Task 5 (P1): Integrate and validate (serial)
│   │   - Input: Specifications + test runners
│   │   - Output: Passing test suite
│   │
│   └── Task 6 (P2): Documentation (serial)
│       - Input: All specifications and tests
│       - Output: Contribution guidelines, maintenance docs
```

### Directory Structure Template

```
project-root/
├── src/                          # Source code
│   └── components/               # Components to be specified
│       ├── component-a.js
│       ├── component-b.js
│       └── component-c.js
│
├── specs/                        # All specifications
│   ├── features/                 # BDD scenarios (Gherkin)
│   │   ├── component-a.feature
│   │   ├── component-b.feature
│   │   └── component-c.feature
│   │
│   ├── state-machines/           # State machine specs
│   │   ├── README.md             # Overview of all state machines
│   │   ├── component-a-state-machine.md
│   │   ├── component-b-state-machine.md
│   │   └── component-c-state-machine.md
│   │
│   └── fit-fixtures/             # FIT decision tables
│       ├── component-a.fit.md
│       ├── component-b.fit.md
│       └── component-c.fit.md
│
└── tests/                        # Test implementations
    ├── bdd/                      # BDD test runners
    ├── fit/                      # FIT test runners
    └── integration/              # Integration tests
```

### Task Template: BDD Scenarios

```markdown
Title: Create BDD scenarios for all [N] components (Gherkin .feature files)
Priority: P0
Type: task
Depends on: [parent epic]
Blocks: [test runner task]

Description:
Create comprehensive Gherkin .feature files for each component covering:
- Lifecycle transitions (initialize/start/stop/status)
- Happy paths (successful operations)
- Error paths (failures, edge cases)
- Concurrent operations (if applicable)
- Resource management (if applicable)

Components to cover:
1. ComponentA ([file-path]) - [brief description of responsibilities]
2. ComponentB ([file-path]) - [brief description of responsibilities]
3. ComponentC ([file-path]) - [brief description of responsibilities]
...

File locations: [absolute-path]/specs/features/
File naming: component-name.feature

Success criteria:
- All [N] components have .feature files
- Each file has minimum 5 scenarios covering lifecycle and core operations
- Scenarios use proper Given/When/Then format
- All scenarios are concrete and testable (no vague "system works")
- Background sections establish common preconditions
- Scenarios cover both happy and error paths

Template scenario structure:
```gherkin
Feature: ComponentName - Brief description
  As a [role]
  I want to [capability]
  So that [benefit]

  Background:
    Given [common precondition]
    And [another precondition]

  # Lifecycle Transitions
  Scenario: Start ComponentName successfully
    Given [precondition]
    When [action]
    Then [expected result]
    And [additional assertion]

  # Happy Paths
  Scenario: Perform core operation successfully
    ...

  # Error Paths
  Scenario: Handle error condition gracefully
    ...
```
```

### Task Template: State Machine Specifications

```markdown
Title: Create state machine specifications with complete transition tables
Priority: P0
Type: task
Depends on: [parent epic]
Blocks: [test runner task]

Description:
Create complete state machine specifications for each component with:
- Complete state transition tables (markdown format)
- Transition guards/conditions
- State invariants
- Entry/exit actions
- Error states and recovery

Components to specify:
1. ComponentA - States: [list expected states]
2. ComponentB - States: [list expected states]
3. ComponentC - States: [list expected states]
...

File locations: [absolute-path]/specs/state-machines/
File naming: component-name-state-machine.md

Table format:
| Current State | Event | Guards | Next State | Actions | Error Handling |

Success criteria:
- All [N] components have complete state machine specifications
- All transitions documented with guards and actions
- Error states and recovery paths specified
- Invariants documented for each state
- State diagrams included (ASCII art or mermaid)
- Entry/exit actions specified

Template structure:
```markdown
# ComponentName State Machine Specification

## Overview
Brief description, states, resources managed

## State Definitions

### STATE_NAME
**Description**: What this state represents
**Invariants**: Conditions that must always be true
**Entry Actions**: What happens when entering this state
**Exit Actions**: What happens when leaving this state
**Valid Operations**: What can be called in this state

## State Transition Table
[Complete transition table]

## State Diagram
[ASCII or mermaid diagram]

## Lifecycle Examples
[Code examples showing transitions]

## Testing Checklist
[Verification requirements]
```
```

### Task Template: FIT Decision Tables

```markdown
Title: Create FIT/SLIM decision tables for all components
Priority: P0
Type: task
Depends on: [parent epic]
Blocks: [test runner task]

Description:
Create FIT-style decision tables for testing component behavior declaratively:

Decision tables needed per component:
- Lifecycle state transitions (start/stop/getStatus)
- Core operation input/output tables
- Error condition tables
- Edge case tables
- Integration scenario tables (if applicable)

Components to cover:
1. ComponentA - [list key operations to table]
2. ComponentB - [list key operations to table]
3. ComponentC - [list key operations to table]
...

File locations: [absolute-path]/specs/fit-fixtures/
File naming: component-name.fit.md

Table format:
| Input State | Action | Input Data | Expected State | Expected Output | Success |

Success criteria:
- All [N] components have FIT decision tables
- Minimum 3 tables per component (lifecycle, happy path, error path)
- All tables are executable format (clear input/output mappings)
- Tables cover edge cases and boundaries
- Tables include both success and failure cases

Template structure:
```markdown
# ComponentName - FIT Decision Tables

## Table 1: Lifecycle State Transitions
Tests the core lifecycle methods

| Initial State | Action | Expected State | Expected Success | Expected Message/Error |
|--------------|--------|----------------|------------------|------------------------|
| ... | ... | ... | ... | ... |

## Table 2: Core Operation - Happy Path
Tests successful operations with various inputs

| Input A | Input B | Expected Output | Expected Success |
|---------|---------|-----------------|------------------|
| ... | ... | ... | ... |

## Table 3: Core Operation - Error Cases
Tests error handling for invalid inputs

| Input A | Input B | Expected Error Contains |
|---------|---------|------------------------|
| ... | ... | ... |

## Table 4: Edge Cases
Tests boundary conditions

| Scenario | Input | Expected Behavior |
|----------|-------|-------------------|
| ... | ... | ... |
```
```

### Execution Script Template

```bash
#!/bin/bash
# execute-spec-creation.sh

# Configuration
EPIC_ID="epic-spec-creation"
PROJECT_ROOT="/path/to/project"
COMPONENT_LIST=(
  "component-a:src/component-a.js:Core data storage"
  "component-b:src/component-b.js:API interface"
  "component-c:src/component-c.js:Business logic"
)

# Step 1: Create epic
echo "Creating specification epic..."
bd create --type epic \
  --title "Add comprehensive specifications to System X" \
  --description "Create BDD scenarios, state machine specs, and FIT fixtures for all components"

# Step 2: Create parallel tasks
echo "Creating parallel specification tasks..."

# BDD task
BDD_TASK=$(bd create --priority P0 \
  --depends-on "$EPIC_ID" \
  --title "Create BDD scenarios for all ${#COMPONENT_LIST[@]} components" \
  --description "$(cat <<EOF
Create comprehensive Gherkin .feature files for each component covering:
- Lifecycle transitions
- Happy paths
- Error paths
- Concurrent operations
- Resource management

Components to cover:
$(for component in "${COMPONENT_LIST[@]}"; do
  IFS=':' read -r name path desc <<< "$component"
  echo "- $name ($path) - $desc"
done)

File locations: $PROJECT_ROOT/specs/features/
Success criteria:
- All ${#COMPONENT_LIST[@]} components have .feature files
- Minimum 5 scenarios per file
- Proper Given/When/Then format
EOF
)" | tail -1)

# State Machine task
SM_TASK=$(bd create --priority P0 \
  --depends-on "$EPIC_ID" \
  --title "Create state machine specifications with complete transition tables" \
  --description "$(cat <<EOF
Create complete state machine specifications for each component with:
- Complete state transition tables
- Transition guards/conditions
- State invariants
- Entry/exit actions
- Error states

Components to specify:
$(for component in "${COMPONENT_LIST[@]}"; do
  IFS=':' read -r name path desc <<< "$component"
  echo "- $name - $desc"
done)

File locations: $PROJECT_ROOT/specs/state-machines/
Table format: | Current State | Event | Guards | Next State | Actions | Error Handling |
EOF
)" | tail -1)

# FIT task
FIT_TASK=$(bd create --priority P0 \
  --depends-on "$EPIC_ID" \
  --title "Create FIT/SLIM decision tables for all components" \
  --description "$(cat <<EOF
Create FIT-style decision tables for testing component behavior declaratively:

Decision tables needed per component:
- Lifecycle state transitions
- Core operation input/output tables
- Error condition tables
- Integration scenario tables

Components to cover:
$(for component in "${COMPONENT_LIST[@]}"; do
  IFS=':' read -r name path desc <<< "$component"
  echo "- $name - $desc"
done)

File locations: $PROJECT_ROOT/specs/fit-fixtures/
Table format: | Input State | Action | Input Data | Expected State | Expected Output | Success |
EOF
)" | tail -1)

# Step 3: Launch parallel agents
echo "Launching 3 parallel agents..."

bg "Complete the work for bead $BDD_TASK: Create BDD scenarios.

Review the bead details with \`bd show $BDD_TASK\` and implement the complete specification.

When complete:
1. Mark the bead as completed: \`bd close $BDD_TASK\`
2. Stage and commit your changes
3. Provide a summary of what was created"

bg "Complete the work for bead $SM_TASK: Create state machine specifications.

Review the bead details with \`bd show $SM_TASK\` and implement the complete specification.

When complete:
1. Mark the bead as completed: \`bd close $SM_TASK\`
2. Stage and commit your changes
3. Provide a summary of what was created"

bg "Complete the work for bead $FIT_TASK: Create FIT decision tables.

Review the bead details with \`bd show $FIT_TASK\` and implement the complete specification.

When complete:
1. Mark the bead as completed: \`bd close $FIT_TASK\`
2. Stage and commit your changes
3. Provide a summary of what was created"

echo "All agents launched. Monitor progress with: bd list --status open"
```

---

## Tool and Agent Coordination Patterns

### Pattern 1: Read-Then-Write

**Agent workflow**:
1. Read bead details (`bd show <bead-id>`)
2. Extract list of components from bead description
3. For each component:
   - Read source code file
   - Analyze structure, states, operations
   - Generate specification file
4. Validate completeness
5. Close bead and commit

**Key insight**: Agents never coordinate with each other, only with their bead

### Pattern 2: Specification Type Expertise

**Specialization strategy**:
- BDD agent: Expert in Gherkin syntax, scenario structure, Given/When/Then
- State Machine agent: Expert in FSM theory, transition tables, invariants
- FIT agent: Expert in decision tables, input/output mappings, test matrices

**Key insight**: Each agent develops and maintains expertise in ONE formalism

### Pattern 3: Atomic Commits

**Commit strategy**:
- One commit per agent
- Commit message format: `feat: Add <spec-type> for all <N> components`
- All files for one spec type in one commit

**Key insight**: Clean git history, easy to review each spec type independently

### Pattern 4: Parallel Execution with No Dependencies

**Dependency management**:
- All 3 tasks depend on parent epic (sequential planning → parallel execution)
- No dependencies between the 3 tasks (true parallelism)
- Downstream tasks (test runners) depend on all 3 completing

**Key insight**: Maximize parallelism by eliminating inter-task dependencies

### Pattern 5: Self-Contained Task Descriptions

**Information density**:
- Each task description contains ALL information needed
- No "see other bead" references
- No ambiguous requirements
- Explicit file paths, formats, success criteria

**Key insight**: Zero agent-to-agent or agent-to-human communication needed

---

## Decision Matrix: Spec Type vs Component Organization

| Criterion | **Type-Based** (1 task per spec type) | **Component-Based** (1 task per component) |
|-----------|--------------------------------|----------------------------------------|
| **Parallelism** | High (3 agents max) | Higher (N agents max) |
| **Quality Consistency** | ✅ High (single agent per type) | ⚠️ Variable (N agents writing same type) |
| **Agent Expertise** | ✅ Deep (maintains single mental model) | ⚠️ Shallow (must switch between models) |
| **Reusable Patterns** | ✅ Easy (agent reuses patterns across components) | ⚠️ Harder (each agent starts fresh) |
| **Code Review** | ✅ Easy (review one spec type at a time) | ⚠️ Harder (must review mixed types) |
| **Incremental Delivery** | ⚠️ Harder (all components at once) | ✅ Easy (deliver one component at once) |
| **Component Dependencies** | ✅ No issues (all specs in one pass) | ⚠️ Complex (if components depend on each other) |
| **Total Time** | Lower (expertise + patterns) | Higher (context switching penalty) |

**Recommendation**:
- **Type-based organization** for systems with 3-10 components where quality and consistency matter
- **Component-based organization** for systems with 10+ components where incremental delivery is critical

**Hybrid approach**: For very large systems (100+ components), create type-based tasks for batches of related components:
- Task 1: BDD scenarios for data layer (10 components)
- Task 2: BDD scenarios for API layer (15 components)
- Task 3: State machines for data layer (10 components)
- etc.

---

## Success Metrics

### Quantitative Results

| Metric | Value |
|--------|-------|
| **Total Specifications** | 6,157 lines across 19 files |
| **Components Specified** | 6 actors |
| **Specification Types** | 3 (BDD, State Machine, FIT) |
| **Agents Used** | 3 (parallel) |
| **Total Time** | ~10 minutes (07:01 - 07:10) |
| **Average per Component** | ~1,026 lines per component |
| **Lines per Minute** | ~615 lines/min across all agents |
| **Commits** | 3 (one per agent) |

### Qualitative Outcomes

✅ **Completeness**: All 6 actors covered in all 3 specification types
✅ **Accuracy**: Specifications matched actual source code implementation
✅ **Testability**: All specifications are concrete and executable
✅ **Consistency**: Single agent per spec type ensured consistent style
✅ **Maintainability**: Clear directory structure and naming conventions
✅ **Traceability**: Each spec file maps to one source file

### Process Efficiency

✅ **Zero clarifications**: No agent asked questions or needed human input
✅ **Zero conflicts**: No merge conflicts between agents
✅ **Zero rework**: No specifications needed to be rewritten
✅ **Clean commits**: Each agent made exactly one atomic commit
✅ **Parallel execution**: All agents ran simultaneously with no blocking

---

## Adaptation Guide

### For Different Specification Types

**If using different spec types (e.g., TLA+, Alloy, Property-Based Tests)**:
1. Replace BDD/FIT task descriptions with your spec type
2. Maintain type-based organization (one task per spec type)
3. Update directory structure (`specs/tla/`, `specs/alloy/`, etc.)
4. Ensure agents have access to documentation for the spec formalism

### For Different Component Counts

**Small systems (1-3 components)**:
- Consider manual creation or single agent
- May not need all 3 spec types

**Medium systems (4-10 components)**:
- Use process as-is with 3 parallel agents
- Expect ~10-20 minutes total time

**Large systems (10-50 components)**:
- Batch components into logical groups
- Create multiple rounds of 3-agent execution
- Consider component-based organization if incremental delivery needed

**Very large systems (50+ components)**:
- Use hybrid approach (type-based per component group)
- Create master epic with sub-epics per component group
- Stagger agent launches to avoid resource contention

### For Different Technologies

**Web APIs (REST/GraphQL)**:
- Add OpenAPI/GraphQL schema generation task
- Create endpoint-level BDD scenarios
- Use FIT tables for request/response validation

**Database Schemas**:
- Add ER diagram generation task
- Create migration testing scenarios
- Use FIT tables for constraint validation

**Distributed Systems**:
- Add sequence diagram generation task
- Create interaction scenarios between services
- Use state machines for each service

**UI Components**:
- Add Storybook story generation task
- Create interaction scenarios (click, type, etc.)
- Use visual regression testing tables

---

## Anti-Patterns to Avoid

### ❌ Anti-Pattern 1: Component-Based Organization for Small Teams

**Problem**: Creating 6 tasks (one per component) for a 3-person team
**Impact**: Only 50% parallelism, inconsistent quality across spec types
**Solution**: Use type-based organization (3 tasks for 3 agents)

### ❌ Anti-Pattern 2: Vague Task Descriptions

**Problem**: "Create specifications for the system"
**Impact**: Agents ask clarifying questions, waste time, produce inconsistent results
**Solution**: Explicit component lists, file paths, formats, success criteria

### ❌ Anti-Pattern 3: Sequential Execution

**Problem**: "First create BDD, then wait for it to complete before starting State Machine"
**Impact**: 3x longer total time (30 minutes instead of 10)
**Solution**: Launch all 3 agents simultaneously since they have no dependencies

### ❌ Anti-Pattern 4: Mixed Commits

**Problem**: Agent commits partial work across multiple spec types
**Impact**: Hard to review, hard to rollback, messy git history
**Solution**: One commit per agent covering all components for one spec type

### ❌ Anti-Pattern 5: Idealized Specifications

**Problem**: Writing specs based on how system "should" work without reading code
**Impact**: Specifications don't match reality, tests fail, wasted effort
**Solution**: Agents must read actual source code before creating specs

### ❌ Anti-Pattern 6: Missing Success Criteria

**Problem**: "Create BDD scenarios" without minimum scenario counts
**Impact**: Agent creates 1-2 trivial scenarios and calls it done
**Solution**: "Minimum 5 scenarios covering lifecycle, happy paths, and error paths"

---

## Conclusion

This workflow demonstrates that comprehensive specification creation can be:
- **Fast**: 6,157 lines in 10 minutes
- **Parallel**: 3 agents working simultaneously
- **High-quality**: Consistent, accurate, testable specifications
- **Reusable**: Template-based approach works for any system

**Key Success Factors**:
1. **Type-based organization** (not component-based) for quality and consistency
2. **Detailed task descriptions** with explicit success criteria
3. **Source code as ground truth** (not idealized designs)
4. **Atomic commits** per specification type
5. **True parallelism** with no inter-agent dependencies

**Reusability**: This process can be applied to any software system requiring comprehensive specifications. Adapt the specification types, directory structure, and task descriptions to your project's needs while maintaining the core workflow principles.

---

## Appendix: File Structure Example

### Event System Specifications (Actual Output)

```
specs/
├── features/                          # BDD Scenarios (1,970 lines)
│   ├── daemon-actor.feature           #   343 lines
│   ├── event-log-actor.feature        #   227 lines
│   ├── function-executor-actor.feature #   417 lines
│   ├── function-registry-actor.feature #   386 lines
│   ├── http-server-actor.feature      #   281 lines
│   └── pattern-matcher-actor.feature  #   316 lines
│
├── state-machines/                    # State Machine Specs (2,655 lines)
│   ├── README.md                      #   340 lines
│   ├── daemon-actor-state-machine.md  #   538 lines
│   ├── event-log-actor-state-machine.md #   263 lines
│   ├── function-executor-actor-state-machine.md # 365 lines
│   ├── function-registry-actor-state-machine.md # 379 lines
│   ├── http-server-actor-state-machine.md # 414 lines
│   └── pattern-matcher-actor-state-machine.md # 356 lines
│
└── fit-fixtures/                      # FIT Decision Tables (1,532 lines)
    ├── daemon-actor.fit.md            #   324 lines
    ├── event-log-actor.fit.md         #   169 lines
    ├── function-executor-actor.fit.md #   294 lines
    ├── function-registry-actor.fit.md #   273 lines
    ├── http-server-actor.fit.md       #   226 lines
    └── pattern-matcher-actor.fit.md   #   246 lines

Total: 6,157 lines across 19 files
```

---

**Document Version**: 1.0
**Last Updated**: 2026-01-11
**Author**: Extracted from Event System specification creation process
**License**: MIT
