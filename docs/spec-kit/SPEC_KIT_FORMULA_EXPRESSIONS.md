# Spec-Kit Formula Expressions

**Project**: GitHub spec-kit
**Analysis Date**: 2026-01-11

## Overview

This document expresses spec-kit workflows, patterns, and constraints as formal expressions, formulas, and process algebra. The goal is to capture the system's essence in mathematical notation that can be used for analysis, verification, and potentially automation.

## 1. Workflow as Process Algebra

Using CSP (Communicating Sequential Processes) notation to model the workflow:

### Complete SDD Workflow

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

### Subprocess Definitions

```
constitution(input) =
  parseInput(input) →
  generateArticles() →
  establishGates() →
  write(constitution.md)

specify(input) =
  generateShortName(input) →
  determineNumber() →
  createBranch() →
  fillTemplate(spec-template.md, input) →
  validateQuality() →
  (resolveAmbiguities() → specify(resolved) ∥ write(spec.md))

clarify(spec) =
  extractClarifications(spec) →
  askUser() →
  updateSpec() →
  (clarify(spec') ∥ spec')

plan(spec, const) =
  checkConstitution(const) →
  research() ∥ designDataModel() ∥ generateContracts() →
  consolidate() →
  updateAgentContext() →
  write(plan.md, research.md, data-model.md, contracts/)

tasks(plan, spec) =
  extractUserStories(spec) →
  mapEntitiesToStories(plan) →
  generatePhases() →
  assignIDs() →
  markParallel() →
  write(tasks.md)

implement(tasks, plan, spec) =
  validateChecklists() →
  parseTaskPhases() →
  executePhase(Setup) →
  executePhase(Foundation) →
  (executeUserStory(US1) ∥ executeUserStory(US2) ∥ ... ∥ executeUserStory(USn)) →
  executePhase(Polish) →
  validateTests()

executePhase(phase) =
  let tasks = taskList(phase)
  in (∥ t ∈ {t | t ∈ tasks ∧ parallel(t)} : executeTask(t)) →
     (· t ∈ {t | t ∈ tasks ∧ ¬parallel(t)} : executeTask(t))
```

## 2. Information Transformation Functions

### Template Application

```
apply_template :: Template → UserInput → Artifact

apply_template(T, I) =
  let structure = parse(T)
      constraints = extract_constraints(T)
      content = generate(I, constraints)
  in fill(structure, content)

Examples:
  apply_template(spec-template.md, "build chat app") = spec.md
  apply_template(plan-template.md, "use Python FastAPI") = plan.md
  apply_template(tasks-template.md, {plan.md, spec.md}) = tasks.md
```

### Specification Function

```
specify :: String → Spec

specify(description) =
  Spec {
    userStories    = extractUserStories(description),
    requirements   = deriveRequirements(description),
    successCriteria = defineSuccessCriteria(description),
    entities       = identifyEntities(description),
    clarifications = markAmbiguities(description)
  }

where:
  |clarifications| ≤ 3  (constraint: maximum 3 clarifications)
```

### Planning Function

```
plan :: Spec → Constitution → Plan

plan(s, c) =
  let tech_context = resolveTechContext(user_input)
      gates = checkGates(tech_context, c)
      research = resolveUnknowns(tech_context)
      data_model = extractEntities(s) |> defineModels
      contracts = extractRequirements(s) |> generateAPIs
  in Plan {
    context = tech_context,
    gates = gates,
    research = research,
    dataModel = data_model,
    contracts = contracts
  }
```

### Task Decomposition Function

```
decompose :: Plan → Spec → [Task]

decompose(plan, spec) =
  let stories = userStories(spec)
      phases = [Setup] ++ [Foundation] ++ map toPhase stories ++ [Polish]
  in flatMap (generateTasks plan) phases

generateTasks :: Plan → Phase → [Task]
generateTasks(plan, phase) =
  case phase of
    Setup → setupTasks(plan)
    Foundation → foundationTasks(plan)
    UserStory(us) → storyTasks(plan, us)
    Polish → polishTasks(plan)

storyTasks :: Plan → UserStory → [Task]
storyTasks(plan, us) =
  let entities = filterEntities(dataModel(plan), us)
      endpoints = filterEndpoints(contracts(plan), us)
      tests = if testDriven(plan) then generateTests(entities, endpoints) else []
  in tests ++ map modelTask entities ++ map serviceTask entities ++ map endpointTask endpoints
```

## 3. Validation Gates as Predicates

### Quality Gate Functions

```
validateSpec :: Spec → Bool
validateSpec(s) =
  noImplementationDetails(s) ∧
  requirementsTestable(s) ∧
  successCriteriaMeasurable(s) ∧
  acceptanceScenariosComplete(s) ∧
  clarificationsLimited(s, 3)

validatePlan :: Plan → Constitution → Bool
validatePlan(p, c) =
  simplicityGate(p, c) ∧
  antiAbstractionGate(p, c) ∧
  integrationFirstGate(p, c)

simplicityGate :: Plan → Constitution → Bool
simplicityGate(p, c) =
  projectCount(p) ≤ maxProjects(c) ∧
  ¬hasFutureProofing(p)

antiAbstractionGate :: Plan → Constitution → Bool
antiAbstractionGate(p, c) =
  usesFrameworkDirectly(p) ∧
  singleModelRepresentation(p)

integrationFirstGate :: Plan → Constitution → Bool
integrationFirstGate(p, c) =
  contractsDefined(p) ∧
  contractTestsSpecified(p)

validateTasks :: [Task] → Bool
validateTasks(tasks) =
  ∀ t ∈ tasks : validFormat(t) ∧
  ∀ us ∈ userStories : hasCompleteTasks(tasks, us) ∧
  dependenciesCorrect(tasks) ∧
  ∀ t ∈ tasks : hasFilePath(t)

validFormat :: Task → Bool
validFormat(t) =
  hasCheckbox(t) ∧
  hasTaskID(t) ∧
  hasDescription(t) ∧
  hasFilePath(t) ∧
  (inUserStoryPhase(t) → hasStoryLabel(t))
```

## 4. Constraint Expressions

### Template Constraints as Logic

```
-- Spec template constraints
SpecConstraints = {
  ∀ s : Spec . ¬contains(s, TechStack) ∧ ¬contains(s, Implementation),
  ∀ s : Spec . focusOn(s, What) ∧ focusOn(s, Why),
  ∀ r ∈ requirements(s) . testable(r),
  ∀ sc ∈ successCriteria(s) . measurable(sc) ∧ technologyAgnostic(sc),
  |{c | c ∈ clarifications(s)}| ≤ 3
}

-- Plan template constraints
PlanConstraints = {
  ∀ p : Plan . passes(p, simplicityGate) ∨ justified(violations(p)),
  ∀ u ∈ unknowns(p) . resolved(u) ∈ research(p),
  ∀ e ∈ entities(spec(p)) . defined(e) ∈ dataModel(p),
  ∀ r ∈ requirements(spec(p)) . ∃ c ∈ contracts(p) . implements(c, r)
}

-- Task template constraints
TaskConstraints = {
  ∀ t : Task . format(t) = "- [ ] [ID] [P?] [Story?] Description with file path",
  ∀ phase ∈ userStoryPhases . ∀ t ∈ tasks(phase) . hasLabel(t, storyID(phase)),
  ∀ us : UserStory . independently_testable(tasks(us)),
  ∀ t1, t2 : Task . sameFile(t1, t2) → sequential(t1, t2)
}
```

### Constitution Articles as Rules

```
-- Article I: Library-First Principle
∀ feature : Feature . isLibrary(feature) ∧ standalone(feature)

-- Article III: Test-First Imperative (NON-NEGOTIABLE)
∀ implementation : Code .
  ∃ tests : TestSuite .
    written_before(tests, implementation) ∧
    approved(tests) ∧
    initially_fails(tests)

-- Article VII: Simplicity
∀ project : Project .
  projectCount(project) ≤ 3 ∨
  (projectCount(project) > 3 → documented(justification(project)))

-- Article VIII: Anti-Abstraction
∀ feature : Feature .
  usesFrameworkDirectly(feature) ∧
  ¬hasUnnecessaryAbstraction(feature)

-- Article IX: Integration-First Testing
∀ test : Test .
  usesRealDatabase(test) ∨ usesMock(test) →
  preference(realDatabase, mock)
```

## 5. Dependency Relations

### Partial Order on Artifacts

```
Let ≺ be the "depends on" relation:

constitution.md ≺ spec.md
spec.md ≺ plan.md
plan.md ≺ research.md
plan.md ≺ data-model.md
plan.md ≺ contracts/
spec.md ≺ tasks.md
plan.md ≺ tasks.md
tasks.md ≺ code

Transitivity:
constitution.md ≺ spec.md ≺ plan.md ≺ tasks.md ≺ code

Therefore: constitution.md ≺ code
```

### Task Dependencies as DAG

```
Let G = (V, E) be a directed acyclic graph where:
  V = {t1, t2, ..., tn} (all tasks)
  E = {(ti, tj) | tj depends on ti}

Properties:
  1. Acyclic: ∀ path p in G : ¬cycle(p)
  2. Partial order: ≺ is transitive and asymmetric
  3. Topological sort: ∃ ordering [t1, t2, ..., tn] : ∀ (ti, tj) ∈ E : i < j

Parallel tasks:
  parallel(ti, tj) ↔ ¬(ti ≺ tj) ∧ ¬(tj ≺ ti) ∧ ¬sameFile(ti, tj)
```

## 6. State Transitions

### Workflow State Machine

```
States S = {Unspecified, Constituted, Specified, Clarified, Planned, Tasked, Implemented, Validated}

Transition function δ: S × Action → S

δ(Unspecified, constitution) = Constituted
δ(Constituted, specify) = Specified
δ(Specified, clarify) = Clarified
δ(Specified, plan) = Planned (if no clarification needed)
δ(Clarified, plan) = Planned
δ(Planned, tasks) = Tasked
δ(Tasked, implement) = Implemented
δ(Implemented, validate) = Validated (if tests pass)
δ(Implemented, validate) = Implemented (if tests fail)

Initial state: s0 = Unspecified
Final state: sf = Validated

Language accepted: L(SDD) = {constitution · specify · clarify* · plan · tasks · implement · validate+}
```

### Task State Transitions

```
TaskStates = {Pending, InProgress, Completed, Failed}

For each task t:
  state(t) ∈ TaskStates
  initial_state(t) = Pending

Transition rules:
  state(t) = Pending ∧ prerequisites_met(t) ∧ start(t) → state'(t) = InProgress
  state(t) = InProgress ∧ execute_success(t) → state'(t) = Completed
  state(t) = InProgress ∧ execute_fail(t) → state'(t) = Failed
  state(t) = Failed ∧ fix(t) → state'(t) = InProgress

Prerequisites:
  prerequisites_met(t) ↔ ∀ t' : t' ≺ t → state(t') = Completed
```

## 7. Quality Metrics

### Specification Quality Score

```
specQuality :: Spec → [0, 1]
specQuality(s) =
  let w1 = 0.3, w2 = 0.3, w3 = 0.2, w4 = 0.2  -- weights
  in w1 * clarity(s) +
     w2 * completeness(s) +
     w3 * testability(s) +
     w4 * measurability(s)

where:
  clarity(s) = 1 - (|clarifications(s)| / 3)  -- fewer clarifications = clearer
  completeness(s) = |completed_sections(s)| / |required_sections(s)|
  testability(s) = |testable_requirements(s)| / |requirements(s)|
  measurability(s) = |measurable_criteria(s)| / |success_criteria(s)|
```

### Plan Quality Score

```
planQuality :: Plan → Constitution → [0, 1]
planQuality(p, c) =
  let w1 = 0.4, w2 = 0.3, w3 = 0.3
  in w1 * gateCompliance(p, c) +
     w2 * researchCompleteness(p) +
     w3 * designCoverage(p)

where:
  gateCompliance(p, c) = |passed_gates(p, c)| / |total_gates(c)|
  researchCompleteness(p) = 1 - (|unresolved_unknowns(p)| / |total_unknowns(p)|)
  designCoverage(p) = |designed_requirements(p)| / |total_requirements(spec(p))|
```

### Implementation Progress

```
progress :: [Task] → [0, 1]
progress(tasks) =
  |{t | t ∈ tasks ∧ state(t) = Completed}| / |tasks|

velocityPerPhase :: Phase → [Task] → Time → Real
velocityPerPhase(phase, tasks, time) =
  let completed = {t | t ∈ tasks ∧ phase(t) = phase ∧ state(t) = Completed}
  in |completed| / time
```

## 8. Optimization Functions

### Parallel Execution Factor

```
parallelism :: [Task] → Real
parallelism(tasks) =
  let sequential_time = Σ t ∈ tasks : time(t)
      parallel_groups = partition(tasks, canRunParallel)
      parallel_time = Σ g ∈ parallel_groups : max(time(t) | t ∈ g)
  in sequential_time / parallel_time

Ideally: parallelism(tasks) > 1
```

### Critical Path Length

```
criticalPath :: [Task] → Real
criticalPath(tasks) =
  let G = dependencyGraph(tasks)
      paths = allPathsFromRootToLeaf(G)
  in max{Σ t ∈ p : time(t) | p ∈ paths}

Optimization goal: minimize(criticalPath(tasks))
```

### Resource Utilization

```
utilization :: [Task] → [Agent] → Real
utilization(tasks, agents) =
  let work = Σ t ∈ tasks : time(t)
      capacity = |agents| * max_time
  in work / capacity

Optimal utilization: 0.7 ≤ utilization ≤ 0.9
```

## 9. Temporal Logic Properties

### Workflow Invariants (LTL - Linear Temporal Logic)

```
-- Always: Constitution before Specification
□(constitute → ○specify)

-- Eventually: Specification leads to Code
□(specify → ◊code)

-- Until: Plan must exist until Implementation
□(tasked → (¬implement 𝒰 planned))

-- Global: No code without tests (if TDD)
□(testDriven → (implement → tests_written ∧ tests_fail))

-- Guarantee: Tasks eventually complete or fail
□(start_task(t) → ◊(complete(t) ∨ fail(t)))

-- Safety: No implementation without passing gates
□(gate_check → (¬implement 𝒰 gates_pass))
```

### Phase Ordering

```
-- Setup before Foundation
setup ≺ foundation

-- Foundation before User Stories
foundation ≺ userStory(us) for all us

-- User Stories before Polish
∀ us : userStory(us) ≺ polish

-- Tests before Implementation (if TDD)
testDriven → ∀ component : tests(component) ≺ implement(component)
```

## 10. Information Theory Metrics

### Specification Entropy

```
H(Spec) = -Σ p(clarification) * log2(p(clarification))

Lower entropy = clearer specification
Optimal: H(Spec) → 0 as clarifications → 0
```

### Information Gain from Clarification

```
IG(Spec, Clarify) = H(Spec) - H(Spec | Clarify)

Measures how much uncertainty is reduced by clarification process
```

### Task Independence Measure

```
independence :: [Task] → [0, 1]
independence(tasks) =
  let dependencies = {(ti, tj) | ti ≺ tj}
      max_dependencies = |tasks| * (|tasks| - 1) / 2
  in 1 - (|dependencies| / max_dependencies)

Higher independence = more parallelization potential
```

## 11. Category Theory View

### Functors Between Categories

```
Category Spec: Objects = Specifications, Morphisms = Refinements
Category Plan: Objects = Plans, Morphisms = Design Changes
Category Code: Objects = Implementations, Morphisms = Code Changes

Functor F: Spec → Plan
F(spec) = plan(spec, constitution)
F(refine :: spec1 → spec2) = update :: plan1 → plan2

Functor G: Plan → Code
G(plan) = implement(tasks(plan))
G(update :: plan1 → plan2) = modify :: code1 → code2

Composition: G ∘ F : Spec → Code
This is the essence of SDD: Specifications map directly to Code
```

### Natural Transformation

```
Template application is a natural transformation:

η: Id ⇒ Apply[Template]

For each artifact type A:
  ηA : A → Apply[Template](A)

Example:
  ηSpec : UserInput → apply_template(spec-template.md, UserInput)
  ηPlan : TechContext → apply_template(plan-template.md, TechContext)
```

## 12. Rewrite Rules

### Specification Rewriting

```
-- Clarification elimination
spec[NEEDS CLARIFICATION: q] →clarify(answer)→ spec[answer]

-- Requirement decomposition
requirement(r) →decompose→ {requirement(r1), requirement(r2), ...}

-- Entity extraction
spec[mentions entity E] →extract→ spec + entity(E)
```

### Plan Refinement

```
-- Research resolution
plan[NEEDS CLARIFICATION: tech] →research→ plan[decision(tech, rationale)]

-- Gate violation justification
plan[violates(gate)] →justify→ plan[justified_violation(gate, reason)]

-- Contract generation
requirement(r) →generate_contract→ contract(c) where implements(c, r)
```

### Task Transformation

```
-- Phase decomposition
phase(p) →decompose→ {task(t1), task(t2), ...}

-- Parallel marking
{task(t1), task(t2)} where ¬depends(t1, t2) ∧ ¬sameFile(t1, t2) →mark→ {task(t1)[P], task(t2)[P]}

-- Sequential ordering
{task(t1), task(t2)} where depends(t1, t2) →order→ sequence(t1, t2)
```

## 13. Cost Functions

### Development Cost Model

```
cost :: Workflow → Real
cost(workflow) =
  Σ activity ∈ workflow : time(activity) * rate(actor(activity))

Breakdown:
  cost_specify = time_specify * rate_product_manager
  cost_plan = time_research + time_design
  cost_implement = time_coding + time_testing
  cost_validate = time_validation + time_fixes

Total: cost_SDD = cost_specify + cost_plan + cost_implement + cost_validate
```

### Rework Cost

```
rework_cost :: Spec → Plan → Code → Real
rework_cost(s, p, c) =
  let spec_changes = changes(s)
      plan_impacts = propagate(spec_changes, p)
      code_impacts = propagate(plan_impacts, c)
  in |spec_changes| * cost_spec_change +
     |plan_impacts| * cost_plan_change +
     |code_impacts| * cost_code_change

SDD advantage: rework_cost_SDD < rework_cost_traditional
Reason: Specification changes regenerate plan and code automatically
```

## 14. Comparison Functions

### SDD vs Traditional Development

```
efficiency_ratio :: Workflow → Workflow → Real
efficiency_ratio(SDD, Traditional) =
  (time_to_value(Traditional) + rework_cost(Traditional)) /
  (time_to_value(SDD) + rework_cost(SDD))

Hypothesis: efficiency_ratio(SDD, Traditional) > 1.5
```

## 15. Summary of Key Formulas

**Workflow Execution**:
```
SDD = constitution → specify → clarify* → plan → tasks → implement
```

**Quality Metric**:
```
quality(s) = Σ wi * metri(s) where Σ wi = 1
```

**Parallel Speed-up**:
```
speedup = sequential_time / parallel_time
```

**Critical Path**:
```
CP = max{Σ t ∈ path : time(t)}
```

**Constitution Compliance**:
```
compliant(p, c) ↔ ∀ gate ∈ gates(c) : passes(p, gate) ∨ justified(violation(p, gate))
```

**Task Format Validity**:
```
valid(t) ↔ hasCheckbox(t) ∧ hasID(t) ∧ hasDescription(t) ∧ hasFilePath(t)
```

## Conclusion

These formal expressions capture the essence of spec-kit's workflow, constraints, and transformations. They can be used for:

1. **Verification**: Prove properties about the workflow
2. **Optimization**: Find bottlenecks and parallelize
3. **Automation**: Generate code from specifications
4. **Analysis**: Measure quality and progress
5. **Extension**: Adapt patterns to new domains

The formulas demonstrate that SDD is not just a methodology but a mathematically grounded approach to software development that can be analyzed, optimized, and proven correct.
