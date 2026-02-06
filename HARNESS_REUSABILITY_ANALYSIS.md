# Harness Reusability Analysis: What Should #2 Look Like?

**Date:** 2026-01-06
**Context:** Analyzing how to make SIMULATION_HARNESS.md reusable across projects

---

## Executive Summary

**Recommendation:** **Hybrid approach (Option D)** - Skill + Scripts + Documentation

- **Skill** (`/create-harness`) for scaffolding and guidance
- **Scripts** for execution automation and version control
- **Documentation** (HARNESS_GUIDE.md) for reference and customization

This approach balances:
- **Low barrier to entry** (skill guides you through setup)
- **Repeatability** (scripts are version-controlled and testable)
- **Flexibility** (docs enable deep customization)

---

## 1. What Makes a Harness "Reusable"?

### Analysis of Current SIMULATION_HARNESS.md

**Project-specific elements (VARIES between projects):**
- Domain-specific prompts (P1/P2/P3 for bootstrap project)
- Success criteria (S1/S2/S3 for copilot automation)
- Evaluation rubric dimensions (tailored to project goals)
- Output structure expectations
- File naming conventions
- Domain context ("@copilot", "GitHub", "issue-driven development")

**Universal elements (CONSTANT across projects):**
- 3-phase methodology (Simulation → Self-Reflection → Evaluation)
- Task tool usage pattern
- Parallel execution strategy (batching)
- Agent ID tracking patterns
- Resume-based self-reflection approach
- Independent evaluation principle
- Score aggregation logic
- Directory scaffolding patterns

**Configurable elements (SHOULD be parameterized):**
- Number of prompt variants (currently 3)
- Number of criteria variants (currently 3)
- Models to test (currently opus/sonnet/haiku)
- Batch size (currently 9 parallel agents)
- Rubric scoring dimensions and weights
- Time estimates per model
- Research policy (web search allowed/denied)

---

## 2. Design Options: Trade-off Analysis

### Option A: Documentation-Only

**What it includes:**
```
HARNESS_GUIDE.md
├── "How to adapt this harness for your project"
├── Step-by-step customization instructions
└── Examples of different harness adaptations

HARNESS_TEMPLATE.md
├── Template with [PLACEHOLDERS]
├── [DOMAIN], [PROMPT_1], [SUCCESS_CRITERIA_1]
└── Fill-in-the-blanks approach

examples/
├── bootstrap-harness/
├── ml-pipeline-harness/
└── api-design-harness/
```

**Pros:**
- Simplest to create and maintain
- No code dependencies
- Maximum flexibility (users edit freely)
- Easy to understand and version

**Cons:**
- Manual, error-prone process
- No validation of user inputs
- Users must understand structure deeply
- High cognitive load for first-time users
- No execution automation

**Best for:** Experienced users who understand simulation methodology deeply

---

### Option B: Skill-Based

**What it includes:**
```
/create-harness skill that:
1. Asks questions about your project:
   - What are you simulating? (domain context)
   - How many prompt variants? (default: 3)
   - How many criteria levels? (default: 3)
   - What models? (default: opus/sonnet/haiku)
   - What should rubric measure? (custom dimensions)

2. Generates customized files:
   - experiments/{project-name}/prompts/P1.txt, P2.txt, P3.txt
   - experiments/{project-name}/criteria/S1.txt, S2.txt, S3.txt
   - SIMULATION_HARNESS_{project-name}.md
   - RUN_SIMULATION_{project-name}.md

3. Provides next-step guidance
```

**Pros:**
- Guided, low cognitive load
- Interactive (asks clarifying questions)
- Validates inputs (e.g., "need at least 2 prompt variants")
- Generates working configuration immediately
- Can include best practices in prompts

**Cons:**
- Harder to version control (skill logic vs outputs)
- Limited to what skill designer anticipated
- Difficult to customize after generation
- Skill maintenance overhead
- May feel like "magic" (less transparency)

**Best for:** First-time users, rapid prototyping, standardized use cases

---

### Option C: Script-Based

**What it includes:**
```bash
scripts/scaffold-harness.sh
# Usage: ./scaffold-harness.sh --project ml-pipeline --variants 3x3x3

Creates:
experiments/{project}/
├── prompts/
│   ├── P1.txt (template with TODOs)
│   ├── P2.txt
│   └── P3.txt
├── criteria/
│   ├── S1.txt
│   ├── S2.txt
│   └── S3.txt
├── config.json
└── README.md

scripts/run-simulation-batch.sh
# Usage: ./run-simulation-batch.sh --config experiments/ml-pipeline/config.json --batch 1

Executes Phase 1 (simulation) for 9 tests in parallel
Tracks agent IDs
Returns when batch completes

scripts/evaluate-batch.sh
# Similar for Phase 2 evaluation

scripts/aggregate-analysis.sh
# Runs Phase 3 aggregate analysis
```

**config.template.json:**
```json
{
  "project": "ml-pipeline",
  "domain_context": "Machine learning pipeline optimization",
  "prompts": {
    "P1": {"file": "prompts/P1.txt", "description": "minimal"},
    "P2": {"file": "prompts/P2.txt", "description": "moderate"},
    "P3": {"file": "prompts/P3.txt", "description": "detailed"}
  },
  "criteria": {
    "S1": {"file": "criteria/S1.txt", "description": "minimal"},
    "S2": {"file": "criteria/S2.txt", "description": "moderate"},
    "S3": {"file": "criteria/S3.txt", "description": "comprehensive"}
  },
  "models": ["opus", "sonnet", "haiku"],
  "rubric": {
    "completeness": {"weight": 30, "description": "File coverage"},
    "correctness": {"weight": 25, "description": "Syntax validity"},
    "actionability": {"weight": 20, "description": "Ready to use"},
    "specificity": {"weight": 15, "description": "No placeholders"},
    "insight_quality": {"weight": 10, "description": "Novel approaches"}
  },
  "batch_size": 9,
  "research_policy": "web"
}
```

**Pros:**
- Version-controlled and testable
- Scriptable (can integrate with CI/CD)
- Transparent (users read the script)
- Composable (scripts call each other)
- Repeatable (deterministic execution)

**Cons:**
- Requires shell scripting knowledge
- Less guidance than skill
- Users must understand config structure
- Harder to validate inputs upfront
- May require debugging scripts

**Best for:** Teams with DevOps culture, CI/CD integration, repeated runs

---

### Option D: Hybrid (Skill + Scripts + Docs)

**What it includes:**

**1. Skill (`/create-harness`):**
```
Responsibilities:
- Guides user through initial setup (interactive Q&A)
- Validates inputs (checks for consistency)
- Generates config.json and directory structure
- Creates template files with helpful TODOs
- Explains what each file does
- Suggests next steps (fill in prompts, run scripts)

Does NOT:
- Execute simulations (delegates to scripts)
- Handle complex logic (delegates to scripts)
- Replace documentation (points to HARNESS_GUIDE.md)
```

**2. Scripts:**
```bash
scripts/harness/scaffold.sh
# Called by skill OR manually
# Creates directory structure based on config.json

scripts/harness/run-batch.sh
# Executes simulation batch (Phase 1)
# Manages parallelism and agent ID tracking

scripts/harness/evaluate-batch.sh
# Executes evaluation batch (Phase 2)

scripts/harness/aggregate-results.sh
# Aggregates all results (Phase 3)

scripts/harness/helpers.sh
# Shared utilities (parse config, validate files, etc.)
```

**3. Documentation:**
```
HARNESS_GUIDE.md
├── Concepts: What is a simulation harness?
├── Quick Start: "/create-harness to get started"
├── Customization: How to modify generated files
├── Advanced: Custom rubrics, non-standard matrices
├── Examples: Bootstrap, ML pipeline, API design
└── Troubleshooting: Common issues

HARNESS_TEMPLATE.md
├── Reference template showing all placeholders
├── Used by skill as generation source
└── Can be manually copied/edited for edge cases

examples/
├── bootstrap/
│   ├── Full working example (current project)
│   └── Demonstrates standard 3x3x3 matrix
├── ml-pipeline/
│   ├── Custom rubric example
│   └── Demonstrates 2x4x2 matrix (different dimensions)
└── api-design/
    ├── Domain-specific evaluation criteria
    └── Demonstrates specialized scoring
```

**Workflow:**
```
User: "I want to create a simulation harness for my project"

1. Run: /create-harness
   - Skill asks questions
   - Generates config.json + directory structure
   - Creates template files with TODOs
   - Outputs: "Fill in experiments/my-project/prompts/*.txt"

2. User fills in prompt/criteria files
   (Skill could optionally help with this too)

3. Run: scripts/harness/run-batch.sh --config experiments/my-project/config.json --batch 1
   - Script reads config
   - Launches 9 Task agents in parallel
   - Tracks agent IDs
   - Waits for completion

4. Run: scripts/harness/evaluate-batch.sh --config experiments/my-project/config.json --batch 1
   - Script launches 9 evaluator agents
   - Applies rubric from config.json

5. Run: scripts/harness/aggregate-results.sh --config experiments/my-project/config.json
   - Generates comparison report
   - Identifies optimal configurations

6. User reviews results in experiments/my-project/runs/
```

**Pros:**
- Low barrier to entry (skill guides setup)
- Repeatable execution (scripts)
- Deep customization (docs + manual editing)
- Version-controlled (scripts + config files)
- Testable (scripts have clear inputs/outputs)
- Transparent (users can read scripts and docs)
- Composable (skill calls scripts, scripts are independent)

**Cons:**
- More complex to build (3 components)
- Requires maintaining skill + scripts + docs
- Learning curve for advanced customization

**Best for:** Wide range of users (beginners use skill, experts use scripts)

---

## 3. Detailed Design: Hybrid Approach

### 3.1 Config Schema

**experiments/{project}/config.json:**
```json
{
  "schema_version": "1.0",
  "project": {
    "name": "bootstrap",
    "description": "Test bootstrap prompts for @copilot automation",
    "domain_context": "GitHub Copilot issue-driven development"
  },
  "test_matrix": {
    "prompts": [
      {
        "id": "P1",
        "file": "prompts/P1-minimal.txt",
        "description": "10 words, minimal context",
        "word_count": 10
      },
      {
        "id": "P2",
        "file": "prompts/P2-moderate.txt",
        "description": "14 words, moderate detail",
        "word_count": 14
      },
      {
        "id": "P3",
        "file": "prompts/P3-detailed.txt",
        "description": "35 words, full specification",
        "word_count": 35
      }
    ],
    "criteria": [
      {
        "id": "S1",
        "file": "criteria/S1-minimal.txt",
        "description": "Single requirement",
        "requirements_count": 1
      },
      {
        "id": "S2",
        "file": "criteria/S2-moderate.txt",
        "description": "3 requirements",
        "requirements_count": 3
      },
      {
        "id": "S3",
        "file": "criteria/S3-comprehensive.txt",
        "description": "7 observable outcomes",
        "requirements_count": 7
      }
    ],
    "models": [
      {"id": "opus", "name": "claude-opus-4-5"},
      {"id": "sonnet", "name": "claude-sonnet-4-5"},
      {"id": "haiku", "name": "claude-haiku-4"}
    ],
    "total_tests": 27
  },
  "execution": {
    "batch_size": 9,
    "research_policy": "web",
    "timeout_per_simulation": 600,
    "timeout_per_evaluation": 300
  },
  "rubric": {
    "total_points": 100,
    "dimensions": [
      {
        "name": "completeness",
        "points": 30,
        "description": "File coverage: Expected vs actual files created"
      },
      {
        "name": "correctness",
        "points": 25,
        "description": "Syntax validity: yamllint, shellcheck, markdownlint pass"
      },
      {
        "name": "actionability",
        "points": 20,
        "description": "Ready to use: Can be deployed immediately"
      },
      {
        "name": "specificity",
        "points": 15,
        "description": "Placeholder density: Count of TODOs, FIXMEs, etc."
      },
      {
        "name": "insight_quality",
        "points": 10,
        "description": "Novel approaches and explicitly stated assumptions"
      }
    ]
  },
  "output": {
    "run_directory_pattern": "experiments/{project}/runs/run-{timestamp}-{config}",
    "log_format": "compact",
    "analysis_format": "markdown"
  }
}
```

### 3.2 Skill Design

**/create-harness skill:**

**Prompt structure:**
```
You are helping the user create a simulation harness for their project.

A simulation harness is a framework for testing different variations of prompts,
success criteria, and models to find optimal configurations.

Ask the following questions ONE AT A TIME:

1. Project name: (e.g., "bootstrap", "ml-pipeline")
2. Domain context: What are you simulating? (e.g., "@copilot GitHub automation")
3. How many prompt variants to test? (default: 3, range: 2-5)
4. How many success criteria levels? (default: 3, range: 2-5)
5. Which models to test? (default: opus/sonnet/haiku, options: any Claude model)
6. What should the rubric measure? (suggest standard dimensions, allow customization)

After gathering inputs:
1. Validate configuration (e.g., total tests = prompts × criteria × models)
2. Call scripts/harness/scaffold.sh with config
3. Generate config.json
4. Create template files with helpful TODOs
5. Explain next steps

Provide examples from existing harnesses (bootstrap, if others exist).
```

**Implementation approach:**
- Skill should be conversational (not overwhelming)
- Use defaults liberally (reduce decision fatigue)
- Show preview of generated structure before creating
- Offer to pre-fill templates with examples

### 3.3 Script Design

**scripts/harness/scaffold.sh:**
```bash
#!/usr/bin/env bash
# scaffold.sh - Create harness directory structure
# Usage: ./scaffold.sh --config config.json OR --project my-project --prompts 3 --criteria 3 --models "opus sonnet haiku"

set -euo pipefail

# Parse arguments (from config.json OR command-line flags)
# Create experiments/{project}/ directory structure
# Generate config.json if not provided
# Create template files with TODOs
# Output success message with next steps
```

**scripts/harness/run-batch.sh:**
```bash
#!/usr/bin/env bash
# run-batch.sh - Execute simulation batch
# Usage: ./run-batch.sh --config experiments/my-project/config.json --batch 1

set -euo pipefail

# Parse config.json
# Determine which tests are in this batch (batch 1 = P1 × all criteria × all models)
# Generate Task prompts for each test
# Launch all tests in parallel (single message with multiple Task calls)
# Track agent IDs
# Output agent ID mapping file (agent_ids_batch_1.txt)
```

**Key insight:** Script should OUTPUT the Task prompts but NOT directly invoke Task tool. Why?
- Task tool can only be called from within Claude conversation
- Script generates the PROMPTS for the user to copy/paste
- OR: User tells Claude "run scripts/harness/run-batch.sh" and Claude executes it, then uses output to make Task calls

**Better approach:** Script is a library that Claude reads and uses
```bash
# Script outputs structured JSON that Claude parses
{
  "batch": 1,
  "tests": [
    {
      "id": "P1-S1-opus",
      "prompt_file": "experiments/my-project/prompts/P1.txt",
      "criteria_file": "experiments/my-project/criteria/S1.txt",
      "model": "opus",
      "task_description": "Bootstrap simulation P1+S1+opus",
      "full_prompt": "You are simulating @copilot...[full interpolated prompt]"
    },
    ...9 tests...
  ]
}
```

Claude reads this JSON and makes 9 Task calls in parallel.

**scripts/harness/evaluate-batch.sh:**
Similar structure, but for Phase 2 evaluation.

**scripts/harness/aggregate-results.sh:**
Collects all evaluation results and generates comparison report.

### 3.4 Documentation Design

**HARNESS_GUIDE.md:**
```markdown
# Simulation Harness Guide

## What is a Simulation Harness?

A simulation harness is a framework for systematically testing variations
of prompts, success criteria, and models to identify optimal configurations.

It uses a 3-phase methodology:
1. **Simulation:** Agents execute tasks based on prompts
2. **Self-Reflection:** Agents analyze their own performance
3. **Evaluation:** Independent agents score performance using a rubric

## Quick Start

### Option 1: Use the Skill (Recommended for First-Time Users)
\`\`\`
/create-harness
\`\`\`

The skill will:
- Ask questions about your project
- Generate configuration and templates
- Create directory structure
- Guide you through next steps

### Option 2: Manual Setup (Advanced Users)
\`\`\`bash
# 1. Copy template
cp -r examples/template experiments/my-project

# 2. Edit config
vim experiments/my-project/config.json

# 3. Fill in prompts and criteria
vim experiments/my-project/prompts/P1.txt
vim experiments/my-project/criteria/S1.txt

# 4. Run simulation
./scripts/harness/run-batch.sh --config experiments/my-project/config.json --batch 1
\`\`\`

## Customization

### Custom Rubric Dimensions
Edit \`config.json\`:
\`\`\`json
"rubric": {
  "dimensions": [
    {"name": "custom_dimension", "points": 20, "description": "..."}
  ]
}
\`\`\`

### Non-Standard Test Matrices
- 2 prompts × 4 criteria × 2 models = 16 tests
- 5 prompts × 2 criteria × 3 models = 30 tests

Just update the arrays in config.json.

## Examples

### Bootstrap Project (Standard 3×3×3)
See \`examples/bootstrap/\`

### ML Pipeline (Custom Rubric)
See \`examples/ml-pipeline/\`

## Troubleshooting

**Issue:** "Agent timeout during simulation"
**Solution:** Increase \`timeout_per_simulation\` in config.json

**Issue:** "Rubric scores don't make sense"
**Solution:** Review rubric dimension descriptions, add examples
```

**HARNESS_TEMPLATE.md:**
```markdown
# {PROJECT_NAME} Simulation Harness

## Test Matrix

Test {PROMPTS_COUNT} prompts × {CRITERIA_COUNT} criteria × {MODELS_COUNT} models = **{TOTAL_TESTS} simulations**

### Prompt Variants
[TODO: Describe your prompt variants]
- **P1** - {P1_DESCRIPTION}
- **P2** - {P2_DESCRIPTION}
...

### Success Criteria Variants
[TODO: Describe your success criteria]
- **S1** - {S1_DESCRIPTION}
- **S2** - {S2_DESCRIPTION}
...

### Models
[TODO: List models to test]
- **{MODEL_1}** - {MODEL_1_DESCRIPTION}
...

## Three-Phase Methodology
[STANDARD CONTENT - same across all harnesses]
...
```

---

## 4. What's Most Valuable?

### Analysis Dimensions

**1. Will users run this once or many times?**
- **Bootstrap project:** Many times (iterative optimization)
- **General pattern:** Likely many times (experimentation is iterative)
- **Conclusion:** Repeatability and scriptability are valuable

**2. Are users technical?**
- **Target audience:** Likely technical (working with Claude Code, simulations)
- **But:** First-time users need guidance
- **Conclusion:** Support both guided (skill) and expert (scripts) workflows

**3. Do variations need version control?**
- **Yes:** Prompts, criteria, config.json, scripts should all be in git
- **Reason:** Track what worked, compare iterations, collaborate
- **Conclusion:** Prefer files over ephemeral skill state

**4. Is standardization valuable?**
- **Yes:** Common patterns make harnesses easier to understand
- **But:** Projects vary (different domains, rubrics, constraints)
- **Conclusion:** Standardize structure, parameterize content

**5. How complex is setup?**
- **Current state:** Manual, error-prone (copying SIMULATION_HARNESS.md)
- **Pain points:**
  - Forgetting to update placeholder values
  - Inconsistent file naming
  - No validation of test matrix math
- **Conclusion:** Automation reduces errors significantly

---

## 5. Recommendation

### Hybrid Approach (Option D) with Phased Implementation

**Phase 1: Scripts + Documentation (Week 1)**
- Immediate value, lower implementation cost
- Extract current harness into reusable scripts
- Write HARNESS_GUIDE.md with manual setup instructions
- Create examples/ directory with bootstrap as first example

**Phase 2: Skill (Week 2-3)**
- Build /create-harness skill once scripts are proven
- Skill leverages scripts (doesn't duplicate logic)
- Focus skill on guidance and validation

**Phase 3: Iteration (Ongoing)**
- Add more examples as new projects use harness
- Refine rubric dimensions based on learnings
- Enhance scripts with better error handling

### Why This Ordering?

1. **Scripts first:** Testable, version-controlled, immediate value
2. **Docs first:** Unblocks manual users, informs skill design
3. **Skill last:** Benefits from proven scripts, clear use cases

### Success Criteria for #2 (HARNESS_GUIDE.md)

**HARNESS_GUIDE.md should:**
1. Explain what a simulation harness is (concepts)
2. Provide manual setup instructions (for Phase 1)
3. Document config.json schema
4. Show examples of customization
5. Link to working examples (bootstrap, future projects)
6. Be clear enough that someone can adapt the harness WITHOUT a skill

**It should NOT:**
- Replace the skill (skill is optional, docs are core reference)
- Include execution details (that's in scripts)
- Be bootstrap-specific (generic, reusable)

### Directory Structure (Final State)

```
agentic-primer/
├── HARNESS_GUIDE.md              # Reusable guide (core deliverable for #2)
├── HARNESS_TEMPLATE.md           # Template with placeholders
├── SIMULATION_HARNESS.md         # Bootstrap-specific instance (keeps current)
├── scripts/
│   └── harness/
│       ├── scaffold.sh           # Create directory structure
│       ├── run-batch.sh          # Execute simulations
│       ├── evaluate-batch.sh     # Execute evaluations
│       ├── aggregate-results.sh  # Aggregate analysis
│       └── helpers.sh            # Shared utilities
├── examples/
│   ├── bootstrap/                # Working example (current project)
│   │   ├── config.json
│   │   ├── prompts/
│   │   ├── criteria/
│   │   └── README.md
│   └── template/                 # Empty template for copying
│       ├── config.template.json
│       ├── prompts/
│       │   └── README.md         # Instructions for filling in
│       └── criteria/
│           └── README.md
└── experiments/
    ├── iteration-2/              # Bootstrap project (existing)
    └── {other-projects}/         # Future harness instances
```

---

## 6. Implementation Checklist for #2

- [ ] Create HARNESS_GUIDE.md
  - [ ] Concepts section (what is a harness?)
  - [ ] Quick start (both skill and manual paths)
  - [ ] Config schema reference
  - [ ] Customization guide
  - [ ] Examples section
  - [ ] Troubleshooting

- [ ] Create HARNESS_TEMPLATE.md
  - [ ] All placeholders clearly marked
  - [ ] Comments explaining each section
  - [ ] Reference to HARNESS_GUIDE.md

- [ ] Extract scripts/harness/
  - [ ] scaffold.sh (from create-experiment-run.sh)
  - [ ] run-batch.sh (NEW - automate Task calls)
  - [ ] evaluate-batch.sh (NEW - automate evaluations)
  - [ ] aggregate-results.sh (from finalize-experiment-run.sh + analysis)
  - [ ] helpers.sh (shared functions)

- [ ] Create examples/
  - [ ] examples/bootstrap/ (copy iteration-2 as reference)
  - [ ] examples/template/ (empty template for copying)

- [ ] Update existing docs
  - [ ] SIMULATION_HARNESS.md (mark as bootstrap-specific instance)
  - [ ] RUN_SIMULATION.md (update to reference new scripts)
  - [ ] README.md (link to HARNESS_GUIDE.md)

- [ ] Optional: Create /create-harness skill (Phase 2)

---

## 7. Open Questions for User

1. **Scope of #2:** Should we do all of Phase 1 (scripts + docs) or just HARNESS_GUIDE.md?
   - **Option A:** Just HARNESS_GUIDE.md (documentation only, manual setup)
   - **Option B:** Full Phase 1 (scripts + guide + examples)

2. **Script complexity:** How automated should scripts be?
   - **Option A:** Scripts generate prompts that user copies into Claude
   - **Option B:** Scripts output JSON that Claude reads and acts on
   - **Option C:** Scripts are purely documentation (explain what to do, don't execute)

3. **Skill timing:** Should we build /create-harness now or later?
   - **Now:** More comprehensive, better user experience
   - **Later:** Iterate on scripts first, then automate

4. **Example diversity:** Should we create a second example (non-bootstrap) for HARNESS_GUIDE.md?
   - **Yes:** Shows reusability better, validates design
   - **No:** Focus on perfecting bootstrap example first

---

## Conclusion

**For #2, recommend creating HARNESS_GUIDE.md as:**

A comprehensive guide that:
1. Explains simulation harness concepts (universal)
2. Documents config.json schema (parameterization)
3. Provides manual setup instructions (immediate usability)
4. Shows customization examples (flexibility)
5. Links to bootstrap example (concrete reference)

**Structure HARNESS_GUIDE.md as the "source of truth" that:**
- Skills can reference when helping users
- Scripts can implement programmatically
- Users can read to understand and customize

This makes it valuable standalone (Option A) while enabling future enhancements (scripts, skills).

**Next step:** User decides scope (just docs, or docs + scripts + examples).
