# Spec-Kit Analysis Summary

**Project**: GitHub spec-kit - Specification-Driven Development Toolkit
**Repository**: https://github.com/github/spec-kit
**Analysis Date**: 2026-01-11 (as of Sunday, January 11, 2026, 8:15 AM EST)
**Analyst**: Claude Code (Sonnet 4.5)

## Executive Summary

GitHub spec-kit is a comprehensive toolkit that implements Specification-Driven Development (SDD), a methodology that inverts the traditional relationship between specifications and code. Instead of specifications serving as documentation that guides coding, SDD treats specifications as executable artifacts that directly generate implementations.

This analysis has extracted, documented, and formalized the workflows, activities, templates, and scripts that constitute spec-kit, representing them as:

1. **Activity Graph**: Nodes (activities/artifacts) and edges (transformations/dependencies)
2. **Process Algebra**: Formal workflow expressions using CSP notation
3. **Mathematical Formulas**: Quality metrics, cost functions, and optimization criteria
4. **State Machines**: Workflow states and transitions
5. **Constraint Logic**: Template rules and validation gates

## Key Findings

### 1. Core Architecture

Spec-kit is fundamentally a **constraint-based template system** that channels Large Language Model (LLM) behavior toward producing high-quality, structured specifications that can generate code.

**Three Pillars**:
1. **Templates**: Constrain LLM output structure and enforce separation of concerns
2. **Scripts**: Automate repository management, branch creation, and file organization
3. **Commands**: Orchestrate multi-step workflows with validation gates

### 2. Information Flow Model

The system implements a **six-stage pipeline**:

```
Constitution → Specification → Clarification → Planning → Tasking → Implementation
```

Each stage transforms inputs into outputs through:
- **Template application**: Structure + User Input → Structured Document
- **Validation gates**: Document → Pass/Fail → Feedback/Proceed
- **Decomposition**: High-level Artifact → Low-level Artifacts
- **Parallel fan-out**: Single Input → Multiple Parallel Processes → Consolidated Output

### 3. Workflow as Directed Acyclic Graph (DAG)

The workflow forms a DAG with:
- **6 primary activity nodes** (/speckit.* commands)
- **9+ artifact nodes** (constitution.md, spec.md, plan.md, tasks.md, etc.)
- **7 validation gates** (quality, clarification, constitution, research, tasks, checklists, tests)
- **Critical path length**: 6 activities (constitution → specify → plan → tasks → implement → validate)

**Parallelization opportunities**:
- Research phase: Up to N parallel agents (N = number of unknowns)
- Task execution: Up to M parallel tasks (M = tasks marked [P] in same phase)
- User story implementation: K parallel stories (K = number of independent stories)

### 4. Template Constraint Mechanism

Templates are not just structural scaffolding; they are **sophisticated prompt engineering** that:

**Constrains LLM behavior through**:
1. **Explicit prohibitions**: "NO implementation details", "NO tech stack"
2. **Forced markers**: "[NEEDS CLARIFICATION]" for ambiguities (max 3)
3. **Checklists**: Self-review frameworks embedded in templates
4. **Hierarchical structure**: Prevents information overload by separating detail levels
5. **Format requirements**: Strict task format "- [ ] [ID] [P?] [Story?] Description with path"

**Example**: The spec template's "NO implementation details" constraint prevents premature optimization by keeping specifications technology-agnostic.

### 5. Constitutional Governance

The constitution.md file implements a **rule-based system** that establishes architectural principles as enforceable constraints:

**Nine Articles**:
- **I. Library-First**: Every feature must be a standalone library
- **II. CLI Interface**: Every library must expose text-based CLI
- **III. Test-First**: NON-NEGOTIABLE - tests before implementation
- **VII. Simplicity**: Maximum 3 projects initially
- **VIII. Anti-Abstraction**: Use frameworks directly, avoid unnecessary layers
- **IX. Integration-First**: Real databases over mocks, contract tests mandatory

**Enforcement mechanism**: Plan template includes "Pre-Implementation Gates" that check each article and require justification for violations in a "Complexity Tracking" table.

### 6. Validation Gate System

Seven critical validation gates ensure quality at each stage:

| Gate # | Location | Purpose | Pass Criteria |
|--------|----------|---------|---------------|
| 1 | After /speckit.specify | Specification quality | No implementation details, testable requirements, ≤3 clarifications |
| 2 | After /speckit.clarify | Clarification completeness | All [NEEDS CLARIFICATION] resolved |
| 3 | During /speckit.plan | Constitutional compliance | Simplicity, Anti-Abstraction, Integration-First gates pass |
| 4 | After research phase | Research completeness | All unknowns resolved, decisions documented |
| 5 | After /speckit.tasks | Task completeness | All tasks have correct format, dependencies valid |
| 6 | Before /speckit.implement | Checklist validation | All checklists complete (or user confirms) |
| 7 | After /speckit.implement | Implementation success | All tests pass, features match spec |

### 7. Automation Scripts

**Five core bash/PowerShell scripts**:

1. **create-new-feature.sh**:
   - Generates feature branch name from description
   - Determines next sequential feature number
   - Creates branch and spec directory
   - Initializes spec file from template

2. **setup-plan.sh**:
   - Validates feature branch
   - Copies plan template
   - Sets up plan directory structure

3. **check-prerequisites.sh**:
   - Validates required documents exist
   - Returns JSON with file paths
   - Supports --require-tasks, --include-tasks flags

4. **update-agent-context.sh**:
   - Detects AI agent in use (Claude, Gemini, etc.)
   - Updates agent-specific context file
   - Preserves manual additions between markers

5. **common.sh**:
   - Shared functions for path resolution
   - Feature branch detection
   - Git vs non-git repo handling

### 8. Task Organization Pattern

Tasks are organized by **user story priority**, not by technical layer:

**Phases**:
1. **Setup**: Project initialization (no user story label)
2. **Foundational**: Blocking prerequisites for ALL stories (no user story label)
3. **User Story Phases**: One phase per story (labeled [US1], [US2], [US3], etc.)
4. **Polish**: Cross-cutting concerns (no user story label)

**Key insight**: Each user story phase should be independently implementable and testable, enabling incremental delivery and parallel team work.

**Task format** (strict):
```
- [ ] [TaskID] [P?] [Story?] Description with exact file path

Examples:
- [ ] T001 Create project structure per implementation plan
- [ ] T005 [P] Implement authentication middleware in src/middleware/auth.py
- [ ] T012 [P] [US1] Create User model in src/models/user.py
```

### 9. Quality Metrics

**Specification Quality Score**:
```
specQuality(s) = 0.3 * clarity(s) +
                 0.3 * completeness(s) +
                 0.2 * testability(s) +
                 0.2 * measurability(s)

where:
  clarity = 1 - (clarification_count / 3)
  completeness = completed_sections / required_sections
  testability = testable_requirements / total_requirements
  measurability = measurable_criteria / total_criteria
```

**Plan Quality Score**:
```
planQuality(p, c) = 0.4 * gateCompliance(p, c) +
                    0.3 * researchCompleteness(p) +
                    0.3 * designCoverage(p)
```

### 10. Reusability Patterns

The following patterns from spec-kit are reusable in other contexts:

**Pattern 1: Template-Constrained Generation**
- Use templates with explicit constraints to guide LLM behavior
- Embed validation checklists within templates
- Force ambiguity markers (e.g., [NEEDS CLARIFICATION]) to surface unknowns

**Pattern 2: Phase-Gate Workflow**
- Break complex processes into phases with validation gates
- Each gate checks completion criteria before allowing progression
- Gates can be automated (e.g., checklist validation) or manual (user approval)

**Pattern 3: Constitutional Governance**
- Establish immutable principles early (constitution)
- Enforce principles through validation gates
- Require justification for violations (complexity tracking)

**Pattern 4: User Story Organization**
- Organize work by user-facing value (stories), not technical layers
- Enable independent implementation and testing of each story
- Support incremental delivery and parallel team work

**Pattern 5: Parallel Task Marking**
- Mark tasks that can run in parallel with [P]
- Use file-based coordination (same file = sequential)
- Generate parallel execution examples for clarity

**Pattern 6: Script-Command Integration**
- Commands invoke scripts for automation
- Scripts return JSON for structured data
- Separation of concerns: commands orchestrate, scripts automate

**Pattern 7: Agent Context Management**
- Detect which AI agent is in use
- Update agent-specific context files
- Preserve manual additions between markers

**Pattern 8: Branch-Based Feature Isolation**
- One branch per feature (numbered sequentially)
- Spec directory matches branch name
- Easy to work on multiple features in parallel

**Pattern 9: Quality Checklist Generation**
- Auto-generate quality checklists from templates
- Use checklists as "unit tests for specifications"
- Validate before proceeding to next phase

**Pattern 10: Dependency-Aware Execution**
- Model workflow as DAG
- Topological sort for execution order
- Identify critical path and optimize

## Mathematical Formalization

### Workflow as Process Algebra

```
SDD = constitution → specify → clarify? → plan → tasks → implement → STOP

where:
  constitution : UserInput → Constitution
  specify      : UserInput → Spec
  clarify      : Spec → Spec (optional, iterative)
  plan         : Spec × Constitution → Plan × Research × DataModel × Contracts
  tasks        : Plan × Spec → Tasks
  implement    : Tasks × Plan × Spec → Code
```

### Key Formulas

**Template Application**:
```
apply_template(T, I) = fill(parse(T), generate(I, extract_constraints(T)))
```

**Specification Function**:
```
specify(description) = Spec {
  userStories, requirements, successCriteria, entities, clarifications
} where |clarifications| ≤ 3
```

**Validation Gates**:
```
validateSpec(s) = noImplementationDetails(s) ∧
                  requirementsTestable(s) ∧
                  successCriteriaMeasurable(s) ∧
                  clarificationsLimited(s, 3)
```

**Parallelization Factor**:
```
parallelism(tasks) = sequential_time / parallel_time
                   = (Σ time(t)) / (Σ max(time(group)))
```

**Constitutional Compliance**:
```
compliant(p, c) ↔ ∀ gate ∈ gates(c) : passes(p, gate) ∨ justified(violation(p, gate))
```

### State Machine

**States**: {Unspecified, Constituted, Specified, Clarified, Planned, Tasked, Implemented, Validated}

**Transitions**:
```
δ(Unspecified, constitution) = Constituted
δ(Constituted, specify) = Specified
δ(Specified, clarify) = Clarified
δ(Specified, plan) = Planned (if no clarification)
δ(Clarified, plan) = Planned
δ(Planned, tasks) = Tasked
δ(Tasked, implement) = Implemented
δ(Implemented, validate) = Validated (if pass)
```

## Component Inventory

### Templates (4)

1. **constitution.md** (template) - Project governance template
2. **spec-template.md** - Feature specification structure
3. **plan-template.md** - Technical implementation plan structure
4. **tasks-template.md** - Task breakdown structure

### Commands (6 core + 3 optional)

**Core workflow**:
1. **/speckit.constitution** - Establish project principles
2. **/speckit.specify** - Create feature specification
3. **/speckit.plan** - Generate technical plan
4. **/speckit.tasks** - Break down into tasks
5. **/speckit.implement** - Execute implementation
6. **/speckit.checklist** - Generate quality checklists

**Optional enhancements**:
7. **/speckit.clarify** - Clarify specification ambiguities
8. **/speckit.analyze** - Cross-artifact consistency analysis
9. **/speckit.taskstoissues** - Convert tasks to GitHub issues

### Scripts (5 bash + 5 PowerShell)

**Bash**:
1. common.sh - Shared utility functions
2. create-new-feature.sh - Branch and spec initialization
3. setup-plan.sh - Plan directory setup
4. check-prerequisites.sh - Document validation
5. update-agent-context.sh - AI agent context updates

**PowerShell** (equivalent):
1. common.ps1
2. create-new-feature.ps1
3. setup-plan.ps1
4. check-prerequisites.ps1
5. update-agent-context.ps1

### Generated Artifacts (9+)

1. **constitution.md** - Project-specific governance
2. **spec.md** - Feature specification
3. **plan.md** - Technical implementation plan
4. **research.md** - Technology decisions and rationale
5. **data-model.md** - Entity definitions
6. **contracts/** - API specifications (OpenAPI, GraphQL, etc.)
7. **quickstart.md** - Validation scenarios
8. **tasks.md** - Task breakdown
9. **checklists/** - Quality validation checklists

## Workflow Metrics

**Time estimates** (with AI assistance):
- Constitution: 5-10 minutes
- Specification: 10-15 minutes
- Clarification: 5-10 minutes (if needed)
- Planning: 15-30 minutes
- Tasking: 5-10 minutes
- Implementation: Varies by complexity (30 minutes to hours)

**Total for simple feature**: 30-60 minutes (specification to working code)
**Total for complex feature**: 2-4 hours (with multiple rounds of refinement)

**Compared to traditional development**:
- Specification-driven: 30-60 minutes → working code
- Traditional: Hours for specs → Hours for design → Hours for coding → Hours for debugging
- **Speed-up factor**: Estimated 2-5x faster (hypothesis, needs validation)

## Critical Insights

### 1. Specifications as Executable Code

The fundamental insight of SDD is treating specifications not as documentation but as **source code in a high-level language** (natural language constrained by templates). The LLM acts as a "compiler" that translates this high-level code into implementation code.

### 2. Template Engineering as Prompt Engineering

Templates are sophisticated prompts that:
- **Constrain search space**: Limit what LLM can generate
- **Enforce invariants**: Ensure critical properties always hold
- **Guide reasoning**: Structure LLM's thought process
- **Enable validation**: Create checkable properties

This is **meta-prompt engineering**: engineering prompts that guide how other prompts (user inputs) are processed.

### 3. Quality Through Constraints, Not Freedom

Counter-intuitively, spec-kit achieves high quality by **restricting** what can be done:
- Specification CANNOT include implementation details
- Clarifications LIMITED to 3
- Projects LIMITED to 3 (unless justified)
- Tasks MUST follow strict format
- Tests MUST come before implementation

These constraints channel creativity into the right areas (problem definition, user value) and prevent common pitfalls (premature optimization, over-engineering).

### 4. Incremental Delivery Through User Story Organization

By organizing tasks by user story rather than technical layer, spec-kit enables:
- **True MVP**: Implement just User Story 1 → working, valuable feature
- **Incremental value**: Each story adds value independently
- **Parallel teams**: Different developers work on different stories
- **Flexible prioritization**: Implement P1, skip P2, add P3 later

This is fundamentally different from layer-based organization (models → services → endpoints), which requires completing all layers before any user-facing value exists.

### 5. Constitutional Enforcement at Design Time

The constitution's validation gates catch architectural violations at **planning time**, not implementation time:
- Traditional: Write code → Code review catches violation → Rework
- SDD: Plan phase → Gate catches violation → Revise plan (no code written yet)

This shifts quality left, reducing rework cost significantly.

### 6. Test-First as Non-Negotiable

Article III (Test-First Imperative) makes TDD non-negotiable:
- Tests must be written first
- Tests must be approved by user
- Tests must initially fail (Red phase)
- Only then can implementation begin

This isn't optional or advisory; it's **enforced by the workflow**. The /speckit.implement command validates this ordering.

### 7. Parallel Execution Through File-Based Coordination

The [P] marker system is simple but powerful:
- **Rule**: Tasks can run in parallel if (1) different files, (2) no dependencies
- **Encoding**: Mark with [P] in task description
- **Execution**: LLM or automation can run [P] tasks concurrently

This avoids complex dependency management while enabling parallelization.

### 8. Context Accumulation Pattern

Each phase accumulates more context:
- Specify: spec.md
- Plan: spec.md + plan.md + research.md + data-model.md + contracts/
- Implement: All of the above + tasks.md

Later phases have **richer context**, enabling more informed decisions. This is why the workflow is sequential at the phase level: each phase needs output from previous phases.

### 9. Git-Optional Design

The system works with or without git:
- **With git**: Uses branches, remote tracking, git operations
- **Without git**: Uses directory structure, feature numbering still works
- **Detection**: Scripts check for git and adapt behavior

This makes the system more portable and reduces dependencies.

### 10. Agent-Agnostic Commands

Commands work across multiple AI agents (Claude Code, Gemini CLI, GitHub Copilot, Cursor, etc.):
- **Common interface**: /speckit.* slash commands
- **Agent detection**: Scripts detect which agent is running
- **Context updates**: Each agent gets its own context file updated

This creates a consistent development experience regardless of AI tool choice.

## Opportunities for Improvement

### 1. Formula-Based Task Generation

Currently, task generation is LLM-driven. Could be formalized as:
```
tasks = decompose(plan, spec)
      = setup_tasks(plan) ++
        foundation_tasks(plan) ++
        flatMap story_tasks(plan) (user_stories(spec)) ++
        polish_tasks(plan)
```

This could be **partially automated** using rule-based systems, with LLM filling gaps.

### 2. Automated Parallel Execution

The [P] markers are currently interpreted by humans or LLMs. Could be **automatically executed**:
```
parallel_group = {t | t in phase and marked_parallel(t) and prerequisites_met(t)}
execute_parallel(parallel_group)
```

This would require a task execution engine.

### 3. Dependency Graph Visualization

The dependency graph is implicit in task ordering. Could be **visualized**:
- Generate DOT file from tasks.md
- Render with Graphviz
- Show critical path, parallel opportunities
- Enable drag-and-drop re-ordering

### 4. Checklist Automation

Quality checklists are currently manual. Could be **partially automated**:
```
validate_spec(spec.md) →
  check_no_tech_stack(spec.md) → pass/fail
  check_requirements_testable(spec.md) → pass/fail
  check_success_criteria_measurable(spec.md) → pass/fail
```

This would provide instant feedback on specification quality.

### 5. Constitutional Compliance Checker

The constitution gates are manually checked. Could be **automated**:
```
check_simplicity_gate(plan.md) →
  count_projects(plan.md) ≤ 3 → pass
  count_projects(plan.md) > 3 and justified(plan.md) → pass
  otherwise → fail
```

This would enforce constitutional compliance automatically.

### 6. Progress Dashboard

Implementation progress is tracked in tasks.md checkboxes. Could be **visualized**:
- Burndown chart: tasks remaining over time
- Velocity: tasks completed per day
- Parallel utilization: percentage of parallelizable tasks executed concurrently
- Phase progress: percentage complete per phase

### 7. Template Versioning

Templates currently have no version control. Could add:
```
---
version: 2.1.0
last_updated: 2026-01-11
changelog:
  - 2.1.0: Added checklist generation
  - 2.0.0: Reorganized by user story
---
```

This would enable template evolution tracking.

### 8. Cross-Feature Analysis

Currently, each feature is isolated. Could analyze across features:
- Common entities across features (potential shared libraries)
- Repeated patterns (potential reusable components)
- Dependency relationships (feature A depends on feature B)

This would enable architectural insights.

### 9. Cost Estimation

No built-in cost estimation. Could add:
```
estimate_cost(tasks) = Σ t in tasks : time_estimate(t) * rate(role(t))
```

With historical data, could predict:
- How long implementation will take
- How much it will cost
- When it will be complete

### 10. Automated Testing Integration

The quickstart.md contains manual test scenarios. Could integrate with test frameworks:
```
quickstart.md → generate_test_skeleton → tests/integration/test_quickstart.py
```

This would make quickstart scenarios executable, not just documentation.

## Applicability to Other Domains

The spec-kit patterns can be applied beyond software development:

### 1. Infrastructure as Code
- **Specification**: Desired infrastructure state
- **Plan**: Terraform/CloudFormation templates
- **Tasks**: Resource creation steps
- **Implementation**: Infrastructure deployment

### 2. Data Pipelines
- **Specification**: Data transformations and outcomes
- **Plan**: Pipeline architecture (DAG)
- **Tasks**: Transformation steps
- **Implementation**: Airflow/dbt code

### 3. Machine Learning Workflows
- **Specification**: Model requirements and metrics
- **Plan**: Experiment design
- **Tasks**: Data prep, training, evaluation steps
- **Implementation**: MLflow/Kubeflow pipelines

### 4. Business Process Automation
- **Specification**: Business requirements
- **Plan**: Process flows and integrations
- **Tasks**: Automation steps
- **Implementation**: RPA workflows

### 5. Hardware Design
- **Specification**: Functional requirements
- **Plan**: Architecture and components
- **Tasks**: Design steps
- **Implementation**: Verilog/VHDL code

### 6. Documentation Systems
- **Specification**: Documentation requirements
- **Plan**: Structure and templates
- **Tasks**: Content creation steps
- **Implementation**: Markdown/DocBook files

### 7. Test Automation
- **Specification**: Test scenarios and coverage
- **Plan**: Test architecture
- **Tasks**: Test case breakdown
- **Implementation**: Test code

### 8. Configuration Management
- **Specification**: Desired system state
- **Plan**: Configuration structure
- **Tasks**: Configuration steps
- **Implementation**: Ansible/Chef code

### 9. API Development
- **Specification**: API requirements
- **Plan**: OpenAPI schema
- **Tasks**: Endpoint implementation steps
- **Implementation**: API code

### 10. System Administration
- **Specification**: System requirements
- **Plan**: Architecture and components
- **Tasks**: Setup and configuration steps
- **Implementation**: Scripts and configs

**Common theme**: Any domain where **specifications → implementation** can benefit from this structured, template-driven, validation-gated approach.

## Recommendations for Adoption

### For Teams

1. **Start small**: Apply to one feature first, learn the workflow
2. **Customize constitution**: Adapt articles to your architectural principles
3. **Evolve templates**: Start with defaults, refine based on experience
4. **Measure results**: Track time-to-value, rework cost, quality metrics
5. **Train on patterns**: Ensure team understands user story organization, validation gates
6. **Automate where possible**: Use scripts, integrate with CI/CD
7. **Iterate on process**: Retrospectives to improve workflow

### For Tool Builders

1. **Formalize task execution**: Build automated task runner respecting [P] markers
2. **Visualize dependencies**: Generate graphs from tasks.md
3. **Automate validation**: Build checkers for spec quality, constitutional compliance
4. **Integrate with IDEs**: Bring /speckit.* commands into VS Code, JetBrains
5. **Add progress tracking**: Dashboards showing phase completion, velocity
6. **Enable collaboration**: Multi-user editing of specs with conflict resolution
7. **Support versioning**: Track template evolution, spec history

### For Researchers

1. **Empirical validation**: Measure SDD vs traditional development (time, cost, quality)
2. **Optimization studies**: Find optimal parallelization, critical path reduction
3. **Template effectiveness**: Which constraints most improve specification quality?
4. **LLM comparison**: How do different models perform with same templates?
5. **Domain applicability**: Which domains benefit most from SDD?
6. **Scaling studies**: How does SDD scale to large teams, complex projects?
7. **Cost modeling**: Develop predictive models for SDD project costs

## Conclusion

GitHub spec-kit represents a **paradigm shift** in software development, treating specifications as executable artifacts rather than documentation. The toolkit's power comes from:

1. **Template-constrained generation**: Guiding LLM behavior through structured prompts
2. **Validation gates**: Ensuring quality at each stage before proceeding
3. **Constitutional governance**: Enforcing architectural principles from the start
4. **User story organization**: Enabling incremental delivery and parallel work
5. **Script automation**: Reducing manual toil in repository management
6. **Agent-agnostic design**: Working across multiple AI tools

The analysis has shown that spec-kit can be modeled as:
- **Directed acyclic graph**: Activities, artifacts, transformations
- **Process algebra**: Formal workflow expressions
- **State machine**: Workflow states and transitions
- **Mathematical formulas**: Quality metrics, cost functions, optimization criteria

These formalizations enable deeper understanding, optimization opportunities, and potential automation.

The patterns extracted from spec-kit are **highly reusable** in domains beyond software development, anywhere specifications drive implementations.

As AI capabilities continue to advance, specification-driven approaches like spec-kit will become increasingly important for maintaining alignment between human intent and machine-generated artifacts. The future of development may indeed be **code serving specifications**, not the other way around.

## Related Documents

This analysis consists of multiple detailed documents:

1. **SPEC_KIT_ANALYSIS_PLAN.md** - Analysis objectives and approach
2. **SPEC_KIT_WORKFLOW_GRAPH.md** - Detailed graph representation of workflows
3. **SPEC_KIT_FORMULA_EXPRESSIONS.md** - Mathematical formalization of patterns
4. **SPEC_KIT_ANALYSIS_SUMMARY.md** - This document (comprehensive findings)

All documents are located at: `/Users/bln/play/agentic-primer/`

## References

- GitHub spec-kit repository: https://github.com/github/spec-kit
- Spec-Driven Development blog post: https://github.blog/ai-and-ml/generative-ai/spec-driven-development-with-ai-get-started-with-a-new-open-source-toolkit/
- Microsoft Developer blog: https://developer.microsoft.com/blog/spec-driven-development-spec-kit

---

**Analysis complete**: 2026-01-11, 8:15 AM EST
**Tools used**: Claude Code (Sonnet 4.5), Read, Grep, Write
**Repository analyzed**: /Users/bln/play/agentic-primer/spec-kit (cloned 2026-01-11)
