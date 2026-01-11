# BMAD-METHOD Formula Expressions

**Project**: BMAD-METHOD Analysis
**Date**: 2026-01-11 (Sunday, January 11, 2026, 9:00 AM EST)
**Repository**: https://github.com/bmad-code-org/BMAD-METHOD

## Overview

This document expresses BMAD-METHOD workflows, patterns, and constraints as formal expressions, formulas, and process algebra. The formalization captures the four-phase system, tri-modal pattern, progressive disclosure, and scale-adaptive behavior in mathematical notation for analysis, verification, and automation.

## 1. Workflow as Process Algebra

Using CSP (Communicating Sequential Processes) notation to model BMAD workflows:

### Complete BMad Method Workflow

```
BMAD = workflow_init → phase1? → phase2 → phase3? → phase4 → STOP

where:
  workflow_init : ProjectContext → WorkflowStatus
  phase1        : (optional) Analysis workflows
  phase2        : (required) Planning workflows
  phase3        : (track-dependent) Solutioning workflows
  phase4        : (required) Implementation workflows
```

### Track-Specific Workflows

```
QuickFlow = workflow_init → quick_spec → quick_dev → code_review → STOP

BMadMethod = workflow_init → phase1? → prd → architecture → epics → sprint_cycle → STOP

Enterprise = workflow_init → phase1 → prd → architecture_extended → epics → compliance → sprint_cycle → STOP

where:
  sprint_cycle = sprint_planning → (story_cycle)* → retrospective
  story_cycle  = create_story → dev_story → code_review
```

### Phase 1: Analysis (Optional)

```
Phase1 = (brainstorm ∥ research_market ∥ research_technical) → product_brief

where:
  brainstorm       : Idea → BrainstormingDoc
  research_market  : Questions → ResearchDoc
  research_technical : Questions → ResearchDoc
  product_brief    : {BrainstormingDoc, ResearchDoc*} → ProductBrief

Optionality:
  Phase1? = (Phase1 ∥ SKIP)
```

### Phase 2: Planning (Required)

```
Phase2_BMad = prd → (ux_design)?

Phase2_QuickFlow = quick_spec

where:
  prd         : {ProductBrief?, UserInput} → PRD
  ux_design   : PRD → UXDesign
  quick_spec  : UserInput → TechSpec

Tri-Modal Pattern (PRD):
  prd = (create_prd ∥ validate_prd ∥ edit_prd)

  create_prd   : UserInput → PRD
  validate_prd : PRD → ValidationReport
  edit_prd     : PRD × Changes → PRD'
```

### Phase 3: Solutioning (Track-Dependent)

```
Phase3 = architecture → epics → implementation_readiness

where:
  architecture    : {PRD, UXDesign?} → ArchitectureDoc × ADRs
  epics           : {ArchitectureDoc, PRD} → Epic*
  impl_readiness  : {ArchitectureDoc, Epic*} → ReadinessReport

Tri-Modal Pattern (Architecture):
  architecture = (create_arch ∥ validate_arch ∥ edit_arch)

Track-Dependent:
  Phase3? = (Phase3 ∥ SKIP) where SKIP if QuickFlow
```

### Phase 4: Implementation (Required)

```
Phase4 = sprint_planning → (story_implementation)* → retrospective

where:
  sprint_planning     : Epic* → SprintStatus
  story_implementation = create_story → dev_story → code_review
  retrospective       : SprintStatus → Retrospective

story_implementation details:
  create_story  : Epic × SprintStatus → StoryFile
  dev_story     : StoryFile × ArchitectureDoc? → Code × Tests
  code_review   : Code → ReviewDoc
```

### Progressive Disclosure Pattern

```
Workflow_PD = init → step_sequence

where:
  step_sequence = load_step(n) → execute_step(n) →
                  (menu_interaction? → step_sequence(n+1) ∥ STOP)

load_step(n) : StepID → StepInstructions
execute_step(n) : StepInstructions → Output × StateUpdate
menu_interaction : Menu → UserChoice

Invariants:
  ∀n : loaded_steps = {step_n} ∧ |loaded_steps| = 1
  ∀n : execute_step(n) BEFORE load_step(n+1)
  ∀n : state_updated(n) BEFORE load_step(n+1)
```

### Tri-Modal Workflow Pattern

```
TriModal_Workflow = mode_select → (create_mode ∥ validate_mode ∥ edit_mode)

mode_select : UserInput → Mode ∈ {Create, Validate, Edit}

create_mode : UserInput → Artifact
create_mode = init → (step_c_i)* → finalize
  where step_c_i ∈ steps-c/ directory

validate_mode : Artifact → ValidationReport
validate_mode = discover → (validation_check_i)* → report
  where validation_check_i ∈ steps-v/ directory

edit_mode : Artifact × Changes → Artifact'
edit_mode = discover → select_section → (step_e_i)* → finalize
  where step_e_i ∈ steps-e/ directory

Workflows with Tri-Modal:
  {prd, create-ux-design, create-architecture}
```

## 2. Information Transformation Functions

### Workflow Transformation

```
workflow :: Input → Output

workflow(I) =
  let config = load_config()
      mode = determine_mode(I)
      steps = select_steps(mode)
      state = initialize_state(config, I)
  in execute_steps(steps, state)

execute_steps :: [Step] → State → Output
execute_steps([], s) = finalize(s)
execute_steps(step:rest, s) =
  let s' = execute_step(step, s)
      s'' = update_frontmatter(s')
  in execute_steps(rest, s'')
```

### Artifact Generation

```
generate_artifact :: Template × Context × Steps → Artifact

generate_artifact(T, C, S) =
  let structure = parse_template(T)
      state = {completed_steps: []}
  in foldl(apply_step, structure, S)

apply_step :: Structure → Step → Structure
apply_step(struct, step) =
  let content = execute_step(step, context)
      section = map_to_section(step, struct)
  in append(struct, section, content)
     ∧ update_frontmatter(struct, step)
```

### Scale-Adaptive Function

```
scale_adapt :: ProjectComplexity → Level ∈ {0, 1, 2, 3, 4}

scale_adapt(complexity) =
  case complexity of
    Minimal    → Level 0  (tech-spec only)
    Simple     → Level 1  (tech-spec + basic plan)
    Moderate   → Level 2  (PRD + architecture outline)
    Complex    → Level 3  (full PRD + detailed architecture)
    Enterprise → Level 4  (PRD + architecture + compliance)

complexity_score :: ProjectMetrics → ℝ
complexity_score(m) =
  0.3 * num_epics(m) +
  0.25 * num_components(m) +
  0.2 * num_dependencies(m) +
  0.15 * num_integrations(m) +
  0.1 * regulatory_requirements(m)

level_threshold :: ℝ → Level
level_threshold(score) =
  if score < 2.0  then 0
  elif score < 5.0  then 1
  elif score < 10.0 then 2
  elif score < 20.0 then 3
  else 4
```

### Agent Selection Function

```
select_agent :: Phase × Track → Agent

select_agent(phase, track) =
  case (phase, track) of
    (Phase1, _)           → analyst
    (Phase2, QuickFlow)   → quick_flow_solo_dev
    (Phase2, BMad|Ent)    → pm ∥ ux_designer?
    (Phase3, BMad|Ent)    → architect ∥ tea?
    (Phase4, QuickFlow)   → quick_flow_solo_dev
    (Phase4, BMad|Ent)    → sm → dev

agent_capabilities :: Agent → PowerSet(Workflow)
agent_capabilities(analyst) = {brainstorm, research, product_brief, document_project}
agent_capabilities(pm) = {prd, create_epics}
agent_capabilities(architect) = {create_architecture, impl_readiness}
agent_capabilities(sm) = {sprint_planning, create_story, retrospective}
agent_capabilities(dev) = {dev_story, code_review}
```

## 3. Validation Gates as Predicates

### Phase 1 Validation Gates

```
G1_brainstorming_complete :: BrainstormingDoc → Bool
G1_brainstorming_complete(doc) =
  ideas_explored(doc) ∧
  problem_clear(doc) ∧
  solutions_identified(doc) ∧
  |sections(doc)| ≥ 5

G1_research_complete :: ResearchDoc → Bool
G1_research_complete(doc) =
  questions_answered(doc) ∧
  decisions_documented(doc) ∧
  references_cited(doc) ∧
  depth_adequate(doc)

G1_brief_quality :: ProductBrief → Bool
G1_brief_quality(brief) =
  vision_clear(brief) ∧
  market_validated(brief) ∧
  success_metrics_defined(brief) ∧
  stakeholders_identified(brief)
```

### Phase 2 Validation Gates

```
G2_prd_validation :: PRD → ValidationReport
G2_prd_validation(prd) =
  let checks = [
    ("functional_complete", frs_complete(prd)),
    ("nonfunctional_specified", nfrs_specified(prd)),
    ("testable", testable_requirements(prd)),
    ("no_implementation", ¬implementation_leakage(prd)),
    ("measurable", measurable_success(prd)),
    ("density_adequate", density_check(prd)),
    ("brief_coverage", covers_brief(prd)),
    ("completeness", completeness_check(prd))
  ]
  in generate_report(checks)

frs_complete :: PRD → Bool
frs_complete(prd) =
  |functional_requirements(prd)| ≥ 3 ∧
  ∀fr ∈ functional_requirements(prd) :
    has_acceptance_criteria(fr) ∧
    has_user_story_format(fr)

implementation_leakage :: PRD → Bool
implementation_leakage(prd) =
  ∃phrase ∈ technical_phrases : phrase ∈ content(prd)
  where technical_phrases = {"use library", "implement with", "deploy to", ...}

density_check :: PRD → Bool
density_check(prd) =
  let word_count = |words(prd)|
      section_count = |sections(prd)|
  in (word_count / section_count) ≥ 200
```

### Phase 3 Validation Gates

```
G3_architecture_validation :: ArchitectureDoc → Bool
G3_architecture_validation(arch) =
  system_design_complete(arch) ∧
  adrs_documented(arch) ∧
  patterns_consistent(arch) ∧
  |adrs(arch)| ≥ 5 ∧
  covers_prd(arch)

G3_epics_breakdown :: Epic* → Bool
G3_epics_breakdown(epics) =
  ∀epic ∈ epics :
    stories_defined(epic) ∧
    dependencies_clear(epic) ∧
    estimates_reasonable(epic) ∧
    acceptance_criteria(epic)

G3_implementation_readiness :: ReadinessReport → Bool
G3_implementation_readiness(report) =
  blockers_resolved(report) ∧
  team_ready(report) ∧
  architecture_approved(report) ∧
  dependencies_available(report)
```

### Phase 4 Validation Gates

```
G4_story_prepared :: StoryFile → Bool
G4_story_prepared(story) =
  acceptance_criteria_clear(story) ∧
  tasks_defined(story) ∧
  dependencies_resolved(story) ∧
  estimates_present(story)

G4_implementation_complete :: Code × Tests → Bool
G4_implementation_complete(code, tests) =
  tests_pass(tests) ∧
  follows_architecture(code) ∧
  story_complete(code) ∧
  coverage_adequate(tests)

G4_code_review_pass :: ReviewDoc → Bool
G4_code_review_pass(review) =
  quality_criteria_met(review) ∧
  ¬blocking_issues(review) ∧
  approved(review)
```

## 4. Quality Metrics Formulas

### PRD Quality Score

```
prd_quality :: PRD → ℝ ∈ [0, 1]
prd_quality(prd) =
  0.2 * clarity_score(prd) +
  0.2 * completeness_score(prd) +
  0.15 * testability_score(prd) +
  0.15 * measurability_score(prd) +
  0.1 * density_score(prd) +
  0.1 * no_implementation_score(prd) +
  0.1 * brief_coverage_score(prd)

clarity_score :: PRD → ℝ ∈ [0, 1]
clarity_score(prd) =
  (1 - ambiguous_phrases(prd) / total_phrases(prd))

completeness_score :: PRD → ℝ ∈ [0, 1]
completeness_score(prd) =
  sections_complete(prd) / required_sections(prd)

density_score :: PRD → ℝ ∈ [0, 1]
density_score(prd) =
  min(1.0, (word_count(prd) / section_count(prd)) / 200)
```

### Architecture Quality Score

```
architecture_quality :: ArchitectureDoc → ℝ ∈ [0, 1]
architecture_quality(arch) =
  0.25 * completeness_score(arch) +
  0.25 * adr_quality_score(arch) +
  0.2 * consistency_score(arch) +
  0.15 * prd_coverage_score(arch) +
  0.15 * implementation_guidance_score(arch)

adr_quality_score :: ArchitectureDoc → ℝ ∈ [0, 1]
adr_quality_score(arch) =
  let adrs = extract_adrs(arch)
  in (∑ (adr_complete(adr) | adr ∈ adrs)) / |adrs|

adr_complete :: ADR → Bool
adr_complete(adr) =
  has_context(adr) ∧
  has_decision(adr) ∧
  has_consequences(adr) ∧
  has_alternatives(adr)
```

### Story Quality Score

```
story_quality :: StoryFile → ℝ ∈ [0, 1]
story_quality(story) =
  0.3 * acceptance_criteria_quality(story) +
  0.25 * task_breakdown_quality(story) +
  0.2 * dependency_clarity(story) +
  0.15 * architecture_alignment(story) +
  0.1 * estimate_reasonableness(story)
```

### Workflow Completion Score

```
workflow_completion :: Artifact → ℝ ∈ [0, 1]
workflow_completion(artifact) =
  let completed = stepsCompleted(artifact)
      total = totalSteps(workflow_type(artifact))
  in |completed| / total

continuable :: Artifact → Bool
continuable(artifact) =
  0 < workflow_completion(artifact) < 1 ∧
  has_frontmatter(artifact) ∧
  has_stepsCompleted_array(artifact)
```

## 5. Constraint Expressions

### Progressive Disclosure Constraints

```
PD_single_step_load :: Workflow → Constraint
PD_single_step_load(w) =
  ∀t ∈ execution_time(w) : |loaded_steps(t)| = 1

PD_sequential_execution :: Workflow → Constraint
PD_sequential_execution(w) =
  ∀i ∈ [1..n-1] :
    complete_time(step_i) < start_time(step_i+1)

PD_no_future_knowledge :: Workflow → Constraint
PD_no_future_knowledge(w) =
  ∀i ∈ [1..n] :
    knowledge(step_i) ⊆ {step_1, ..., step_i} ∧
    knowledge(step_i) ∩ {step_i+1, ..., step_n} = ∅
```

### Tri-Modal Constraints

```
TM_mode_exclusive :: Workflow → Constraint
TM_mode_exclusive(w) =
  mode(w) ∈ {Create, Validate, Edit} ∧
  |active_modes(w)| = 1

TM_validate_requires_artifact :: Validation → Constraint
TM_validate_requires_artifact(v) =
  mode(v) = Validate ⟹ ∃artifact : input(v) = artifact

TM_edit_preserves_compliance :: Edit → Constraint
TM_edit_preserves_compliance(e) =
  compliant(input(e)) ⟹ compliant(output(e))
```

### Scale-Adaptive Constraints

```
SA_level_monotonic :: Project → Constraint
SA_level_monotonic(p) =
  complexity_increases(p) ⟹ level_increases(p)

SA_required_phases :: Level → PowerSet(Phase)
SA_required_phases(level) =
  case level of
    0 → {Phase2(quick), Phase4}
    1 → {Phase2(quick), Phase4}
    2 → {Phase2(prd), Phase3(outline), Phase4}
    3 → {Phase2(prd), Phase3(full), Phase4}
    4 → {Phase1, Phase2(prd), Phase3(extended), Phase4}
```

### Dependency Constraints

```
artifact_dependency :: Workflow → PowerSet(Artifact)
artifact_dependency(w) =
  {a | a must exist before w can execute}

Examples:
  artifact_dependency(create_architecture) = {PRD}
  artifact_dependency(create_epics) = {ArchitectureDoc, PRD}
  artifact_dependency(dev_story) = {StoryFile, ArchitectureDoc?}

workflow_ordering :: (Workflow, Workflow) → Bool
workflow_ordering(w1, w2) =
  outputs(w1) ∩ artifact_dependency(w2) ≠ ∅ ⟹
  complete_time(w1) < start_time(w2)
```

## 6. State Transition Functions

### Workflow State Machine

```
State = Idle | Initializing | ModeSelect | ModeConfirmed |
        StepExecuting | StepComplete | AwaitingInput |
        WorkflowComplete | Done

transition :: State × Event → State

transition(Idle, Invoke) = Initializing
transition(Initializing, ConfigLoaded) = ModeSelect
transition(ModeSelect, ModeChosen) = ModeConfirmed
transition(ModeConfirmed, StepLoaded) = StepExecuting
transition(StepExecuting, StepDone) = StepComplete
transition(StepComplete, MenuPresented) = AwaitingInput
transition(StepComplete, NoMenu) = StepExecuting (load next)
transition(AwaitingInput, UserChoice) = StepExecuting
transition(StepExecuting, AllStepsDone) = WorkflowComplete
transition(WorkflowComplete, ArtifactWritten) = Done
```

### Artifact Lifecycle State Machine

```
ArtifactState = NotExists | Draft | InProgress | Complete |
                Validated | NeedsRevision | Consumed | Archived

artifact_transition :: ArtifactState × Event → ArtifactState

artifact_transition(NotExists, CreateWorkflow) = Draft
artifact_transition(Draft, StepsProgress) = InProgress
artifact_transition(InProgress, AllStepsComplete) = Complete
artifact_transition(Complete, ValidateWorkflow) = Validated | NeedsRevision
artifact_transition(NeedsRevision, EditWorkflow) = Draft
artifact_transition(Validated, UsedByWorkflow) = Consumed
artifact_transition(Consumed, ProjectComplete) = Archived
```

## 7. Optimization Functions

### Critical Path Calculation

```
critical_path :: Track → [Workflow]
critical_path(track) =
  let workflows = workflows_for_track(track)
      dependencies = dependency_graph(workflows)
      durations = workflow_durations(workflows)
  in longest_path(dependencies, durations)

critical_path_duration :: Track → Time
critical_path_duration(track) =
  ∑ (duration(w) | w ∈ critical_path(track))

Examples:
  critical_path_duration(QuickFlow) = 18-45 minutes
  critical_path_duration(BMadMethod) = 80-180 minutes per story
  critical_path_duration(Enterprise) = 150-300 minutes
```

### Parallel Execution Potential

```
parallelizable :: [Workflow] → [[Workflow]]
parallelizable(workflows) =
  let deps = dependency_graph(workflows)
  in partition_by_independence(workflows, deps)

partition_by_independence :: [Workflow] → Graph → [[Workflow]]
partition_by_independence(ws, g) =
  group_by(ws, λw1 w2 → ¬depends_on(w1, w2, g) ∧ ¬depends_on(w2, w1, g))

speedup :: [[Workflow]] → ℝ
speedup(parallel_groups) =
  sequential_time(flatten(parallel_groups)) /
  max(group_time(g) | g ∈ parallel_groups)
```

### Agent Utilization

```
agent_utilization :: Agent → Track → ℝ ∈ [0, 1]
agent_utilization(agent, track) =
  time_executing(agent, track) / total_track_time(track)

agent_contention :: [Workflow] → Agent → Int
agent_contention(workflows, agent) =
  |{w | w ∈ workflows ∧ requires_agent(w) = agent}|

load_balance_score :: Track → ℝ ∈ [0, 1]
load_balance_score(track) =
  let utils = [agent_utilization(a, track) | a ∈ agents(track)]
      avg = mean(utils)
      variance = var(utils)
  in 1 - (variance / avg²)
```

## 8. Temporal Logic Properties

### Workflow Liveness

```
◇ (workflow_started ⟹ ◇ workflow_completed)
  "Every started workflow eventually completes"

◇ (step_executing ⟹ ◇ step_complete)
  "Every executing step eventually completes"

□ (awaiting_input ⟹ ◇ user_responds)
  "User eventually responds to input requests"
```

### Workflow Safety

```
□ (|loaded_steps| ≤ 1)
  "At most one step is loaded at any time (progressive disclosure)"

□ (step_complete(i) ⟹ ¬step_executing(j) where j < i)
  "Steps execute in order (no going backward)"

□ (artifact_created ⟹ eventually_validated)
  "All artifacts are eventually validated"

□ (validation_fails ⟹ revision_cycle)
  "Failed validation triggers revision"
```

### Dependency Safety

```
□ (workflow_starts(w) ⟹ ∀a ∈ dependencies(w) : artifact_exists(a))
  "Workflows only start when all dependencies exist"

□ (phase_complete(n) ⟹ phase_started(n+1) ∨ track_allows_skip(n+1))
  "Phases execute in order unless track allows skipping"
```

## 9. Cost Functions

### Time Cost

```
time_cost :: Workflow → Time
time_cost(w) =
  setup_time(w) +
  (∑ (step_time(s) | s ∈ steps(w))) +
  (∑ (interaction_time(i) | i ∈ interactions(w)))

track_time_cost :: Track → Time
track_time_cost(track) =
  ∑ (time_cost(w) | w ∈ workflows(track))
```

### Cognitive Cost

```
cognitive_cost :: Workflow → ℝ
cognitive_cost(w) =
  0.4 * decision_points(w) +
  0.3 * context_switches(w) +
  0.2 * interruptions(w) +
  0.1 * cognitive_load(w)

Progressive disclosure reduces cognitive cost:
  cognitive_cost_PD(w) < cognitive_cost_monolithic(w)
```

### Quality Cost

```
quality_cost :: Track → ℝ
quality_cost(track) =
  defect_rate(track) * rework_time(track)

Validation reduces quality cost:
  quality_cost(with_validation) < quality_cost(without_validation)
```

## 10. Comparison Functions

### Track Comparison

```
compare_tracks :: (Track, Track) → Ordering × Rationale

compare_tracks(QuickFlow, BMadMethod) =
  if project_complexity < 5.0
  then (QuickFlow_Better, "Simple project, lower overhead")
  else (BMadMethod_Better, "Complex project, needs architecture")

suitability :: Track → Project → ℝ ∈ [0, 1]
suitability(track, project) =
  case (track, complexity(project)) of
    (QuickFlow, c) where c < 2.0 → 0.9
    (QuickFlow, c) where c ≥ 2.0 → 0.3
    (BMadMethod, c) where 2.0 ≤ c < 20.0 → 0.9
    (BMadMethod, c) where c < 2.0 → 0.4
    (Enterprise, c) where c ≥ 20.0 → 0.9
    (Enterprise, c) where c < 20.0 → 0.5
```

### Agent Comparison

```
agent_effectiveness :: Agent → Workflow → ℝ ∈ [0, 1]
agent_effectiveness(agent, workflow) =
  if workflow ∈ agent_capabilities(agent)
  then 1.0
  else 0.2  (agent can attempt but not optimized)

multi_agent_efficiency :: [Agent] → [Workflow] → ℝ
multi_agent_efficiency(agents, workflows) =
  (∑ (agent_effectiveness(assign(w), w) | w ∈ workflows)) / |workflows|
  where assign :: Workflow → Agent
```

## 11. Rewrite Rules

### Workflow Simplification

```
w1 → w2 → w3  ⟹  w1 → skip(w2) → w3  (if w2 optional)

parallel(w1, w2) ∧ dependencies(w1) ∩ dependencies(w2) = ∅
  ⟹  w1 ∥ w2

sequential(w1, w2) ∧ outputs(w1) ∩ inputs(w2) ≠ ∅
  ⟹  w1 → w2  (must be sequential)
```

### Artifact Transformation

```
create(artifact) → validate(artifact) → edit(artifact)
  ⟹ create_validated(artifact)  (if validation integrated)

draft → validate → needs_revision → edit → validate
  ⟹ edit_until_valid(draft)  (revision cycle)
```

## Summary

BMAD-METHOD's mathematical formalization reveals:

1. **Four-Phase Structure**: Formally modeled as CSP processes with optional/required phases
2. **Tri-Modal Pattern**: Create/Validate/Edit as exclusive workflow modes
3. **Progressive Disclosure**: Strict constraints ensuring single-step loading and sequential execution
4. **Scale-Adaptive Behavior**: Formal mapping from complexity metrics to planning levels
5. **Agent Specialization**: Functions mapping phases/tracks to agent capabilities
6. **Validation Gates**: Predicates ensuring quality at each phase transition
7. **Quality Metrics**: Formal scoring functions for PRDs, architecture, stories
8. **Optimization Functions**: Critical path, parallelization, agent utilization
9. **Temporal Properties**: Liveness and safety properties in temporal logic
10. **Cost Models**: Time, cognitive, and quality cost functions

These formulas enable:
- Automated workflow verification
- Quality prediction and optimization
- Agent assignment optimization
- Track recommendation systems
- Workflow tooling and automation
- Formal reasoning about correctness

---

**Document**: BMAD-METHOD Formula Expressions
**Date**: 2026-01-11, 9:00 AM EST
**Repository**: https://github.com/bmad-code-org/BMAD-METHOD
