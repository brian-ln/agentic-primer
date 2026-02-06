# Simulation Harness Guide

**Version:** 1.0
**Date:** 2026-01-11
**Purpose:** Comprehensive guide for creating and customizing simulation harnesses for AI agent evaluation

---

## Table of Contents

1. [Concepts](#1-concepts)
2. [Quick Start](#2-quick-start)
3. [Configuration Schema](#3-configuration-schema)
4. [Customization](#4-customization)
5. [Examples](#5-examples)
6. [Troubleshooting](#6-troubleshooting)

---

## 1. Concepts

### What is a Simulation Harness?

A **simulation harness** is a systematic framework for testing variations of prompts, success criteria, and AI models to identify optimal configurations for agent-based tasks.

Think of it as A/B testing for AI agents - you define multiple test conditions and measure performance across all combinations to discover what works best.

### Core Methodology: Three Phases

The harness uses a three-phase evaluation approach:

#### Phase 1: Simulation
- Launch AI agents with specific prompt + success criteria + model combinations
- Agents execute the task and produce artifacts (code, docs, designs)
- Track agent IDs for later phases
- Optional Phase 1b: Same agents self-reflect on their performance

#### Phase 2: Independent Evaluation
- Launch NEW evaluator agents (independent, unbiased)
- Evaluators score simulation outputs using a rubric
- Rubric measures dimensions like completeness, correctness, actionability
- Generate quantitative scores and qualitative feedback

#### Phase 3: Aggregate Analysis
- Collect all evaluation results across test matrix
- Compare performance by model, prompt, criteria
- Identify optimal configurations
- Generate statistical insights and recommendations

### Why Use a Harness?

**Without a harness:**
- Ad-hoc testing, inconsistent evaluation
- No systematic comparison across models or prompts
- Hard to reproduce results
- Unclear which configuration is best

**With a harness:**
- Systematic testing of all combinations (e.g., 3 prompts × 3 criteria × 3 models = 27 tests)
- Consistent evaluation using standardized rubric
- Reproducible results (version-controlled configs)
- Data-driven decisions on optimal configurations
- Parallel execution for speed (3-4x faster than sequential)

### Key Components

A simulation harness consists of:

1. **Test Matrix Configuration** - Defines what to test
   - Prompt variants (P1, P2, P3...) - Different instruction styles
   - Success criteria variants (S1, S2, S3...) - Different quality bars
   - Models (opus, sonnet, haiku...) - Different AI capabilities

2. **Evaluation Rubric** - Defines how to score
   - Dimensions (completeness, correctness, actionability...)
   - Point allocations (30 pts, 25 pts, 15 pts...)
   - Scoring guidelines (what scores 10/10 vs 0/10)

3. **Execution Framework** - Defines how to run
   - Batch strategy (parallel vs sequential)
   - Agent ID tracking (for Phase 1 → 1b → 2 flow)
   - Output organization (directory structure)

4. **Analysis Tools** - Defines how to interpret
   - Score aggregation scripts
   - Comparison matrices
   - Pattern analysis

### Universal vs Project-Specific Elements

**Universal (same across all harnesses):**
- Three-phase methodology (Simulation → Evaluation → Analysis)
- Task tool usage pattern for parallel execution
- Agent ID tracking approach
- Directory scaffolding patterns
- Score aggregation logic

**Project-specific (varies per project):**
- Domain context (GitHub automation, ML pipelines, API design...)
- Prompt content (what you're asking agents to do)
- Success criteria (what defines "done")
- Rubric dimensions (what quality means for your domain)
- Output structure expectations (files, formats, naming)

**Configurable (you decide):**
- Number of prompt variants (2-5 typical, 3 recommended)
- Number of criteria levels (2-5 typical, 3 recommended)
- Models to test (any Claude models)
- Batch size (9 parallel agents is safe, 27 max)
- Research policy (web search allowed/denied)
- Timeout values (simulation: 10 min, evaluation: 5 min typical)

---

## 2. Quick Start

### Prerequisites

- Claude Code CLI installed and configured
- Access to Claude models (Opus, Sonnet, Haiku)
- Basic understanding of Task tool for launching agents
- Git repository for version control (recommended)

### Option 1: Manual Setup (Best for Learning)

#### Step 1: Create Project Structure

```bash
# Create experiment directory
mkdir -p experiments/my-project/{prompts,criteria,runs}
cd experiments/my-project
```

#### Step 2: Create Configuration File

Create `config.json`:

```json
{
  "project": {
    "name": "my-project",
    "description": "Brief description of what you're testing",
    "domain_context": "Domain-specific context (e.g., 'API design', 'ML pipelines')"
  },
  "test_matrix": {
    "prompts": [
      {
        "id": "P1",
        "file": "prompts/P1-minimal.txt",
        "description": "Minimal prompt (10-15 words)",
        "word_count": 10
      },
      {
        "id": "P2",
        "file": "prompts/P2-moderate.txt",
        "description": "Moderate prompt (15-20 words)",
        "word_count": 18
      },
      {
        "id": "P3",
        "file": "prompts/P3-detailed.txt",
        "description": "Detailed prompt (30-40 words)",
        "word_count": 35
      }
    ],
    "criteria": [
      {
        "id": "S1",
        "file": "criteria/S1-minimal.txt",
        "description": "Minimal success criteria (1 requirement)",
        "requirements_count": 1
      },
      {
        "id": "S2",
        "file": "criteria/S2-moderate.txt",
        "description": "Moderate criteria (3 requirements)",
        "requirements_count": 3
      },
      {
        "id": "S3",
        "file": "criteria/S3-comprehensive.txt",
        "description": "Comprehensive criteria (7+ requirements)",
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
        "description": "Coverage of requirements"
      },
      {
        "name": "correctness",
        "points": 25,
        "description": "Logical and syntactic correctness"
      },
      {
        "name": "actionability",
        "points": 20,
        "description": "Ready to use without modification"
      },
      {
        "name": "specificity",
        "points": 15,
        "description": "Low placeholder density"
      },
      {
        "name": "insight_quality",
        "points": 10,
        "description": "Documented assumptions and edge cases"
      }
    ]
  },
  "output": {
    "run_directory_pattern": "experiments/{project}/runs/run-{timestamp}",
    "log_format": "compact",
    "analysis_format": "markdown"
  }
}
```

#### Step 3: Create Prompt Files

Create `prompts/P1-minimal.txt`:
```
[Your minimal prompt - 10-15 words describing the task]
```

Create `prompts/P2-moderate.txt`:
```
[Your moderate prompt - 15-20 words with more context]
```

Create `prompts/P3-detailed.txt`:
```
[Your detailed prompt - 30-40 words with full specification:
- Specific files/deliverables
- Requirements
- Context]
```

#### Step 4: Create Success Criteria Files

Create `criteria/S1-minimal.txt`:
```
[Single requirement defining success]
```

Create `criteria/S2-moderate.txt`:
```
[3 requirements:
1. Requirement one
2. Requirement two
3. Requirement three]
```

Create `criteria/S3-comprehensive.txt`:
```
[7+ observable outcomes:
1. Outcome one (measurable)
2. Outcome two (verifiable)
3. Outcome three (testable)
... and so on]
```

#### Step 5: Create Run Directory

```bash
# Create run directory with timestamp
RUN_NAME="run-$(date +%Y%m%d-%H%M%S)-full-matrix"
mkdir -p "runs/$RUN_NAME"
cp config.json "runs/$RUN_NAME/"
```

#### Step 6: Run Simulations (Phase 1)

Launch first batch (P1 × all criteria × all models = 9 tests):

```
Ask Claude to run Phase 1 simulations using the Task tool:

"Launch 9 simulation agents in parallel using the Task tool:
- Read prompts from experiments/my-project/prompts/P1-minimal.txt
- Read criteria from experiments/my-project/criteria/S1-minimal.txt, S2-moderate.txt, S3-comprehensive.txt
- Test models: opus, sonnet, haiku
- Combinations: P1+S1+opus, P1+S1+sonnet, P1+S1+haiku, P1+S2+opus, P1+S2+sonnet, P1+S2+haiku, P1+S3+opus, P1+S3+sonnet, P1+S3+haiku
- Output location: experiments/my-project/runs/[RUN_NAME]/[P#]-[S#]-[model]/
- Save all 9 agent IDs"
```

Track the 9 agent IDs returned (e.g., a1b2c3d, a2b3c4d...).

#### Step 7: Self-Reflection (Phase 1b, Optional)

Resume each of the 9 agents for self-reflection:

```
"Resume all 9 agents from Phase 1 (agent IDs: ...) and ask each to self-reflect:
- How well did you meet the success criteria?
- What information was missing from the prompt?
- What would you do differently?"
```

#### Step 8: Run Evaluations (Phase 2)

Launch 9 NEW evaluator agents (Sonnet recommended):

```
"Launch 9 NEW Sonnet evaluator agents in parallel:
- Read simulation output from experiments/my-project/runs/[RUN_NAME]/[P#]-[S#]-[model]/
- Apply rubric from config.json
- Score each dimension
- Provide total score and grade
- Save all 9 evaluator agent IDs"
```

#### Step 9: Aggregate Analysis (Phase 3)

After all 27 tests (3 batches) complete:

```
"Launch 1 analysis agent to aggregate results:
- Read all 27 evaluation results
- Compare scores by model, prompt, criteria
- Identify optimal configuration
- Generate comparison tables
- Provide recommendations"
```

### Option 2: Using Scripts (Coming Soon)

Future versions will include automation scripts:

```bash
# Scaffold new harness (Phase 1 of implementation plan)
./scripts/harness/scaffold.sh --project my-project --prompts 3 --criteria 3 --models "opus sonnet haiku"

# Run simulation batch (Phase 1)
./scripts/harness/run-batch.sh --config experiments/my-project/config.json --batch 1

# Run evaluation batch (Phase 2)
./scripts/harness/evaluate-batch.sh --config experiments/my-project/config.json --batch 1

# Aggregate results (Phase 3)
./scripts/harness/aggregate-results.sh --config experiments/my-project/config.json
```

### Option 3: Using Skill (Future Enhancement)

Future versions may include a guided skill:

```
/create-harness
```

The skill will:
- Ask questions about your project (one at a time)
- Generate config.json and directory structure
- Create template files with helpful TODOs
- Explain next steps

---

## 3. Configuration Schema

### Complete Schema Reference

```json
{
  "schema_version": "1.0",

  "project": {
    "name": "string",           // Project identifier (kebab-case recommended)
    "description": "string",    // Brief project description
    "domain_context": "string"  // Domain-specific context for agents
  },

  "test_matrix": {
    "prompts": [
      {
        "id": "string",              // Unique ID (P1, P2, P3...)
        "file": "string",            // Relative path to prompt file
        "description": "string",     // Human-readable description
        "word_count": number         // Approximate word count (optional)
      }
    ],
    "criteria": [
      {
        "id": "string",              // Unique ID (S1, S2, S3...)
        "file": "string",            // Relative path to criteria file
        "description": "string",     // Human-readable description
        "requirements_count": number // Number of requirements (optional)
      }
    ],
    "models": [
      {
        "id": "string",              // Short ID (opus, sonnet, haiku)
        "name": "string"             // Full model name (claude-opus-4-5)
      }
    ],
    "total_tests": number            // Calculated: prompts × criteria × models
  },

  "execution": {
    "batch_size": number,            // Agents to run in parallel (9 recommended)
    "research_policy": "web|none",   // Allow web research or not
    "timeout_per_simulation": number, // Seconds (600 = 10 min typical)
    "timeout_per_evaluation": number  // Seconds (300 = 5 min typical)
  },

  "rubric": {
    "total_points": number,          // Total points (100 or 120)
    "dimensions": [
      {
        "name": "string",            // Dimension name (snake_case)
        "points": number,            // Points allocated
        "description": "string"      // Scoring guidance
      }
    ]
  },

  "output": {
    "run_directory_pattern": "string", // Pattern with {placeholders}
    "log_format": "compact|verbose",   // Log format preference
    "analysis_format": "markdown|json" // Analysis output format
  }
}
```

### Field Descriptions

#### project

- **name**: Identifier for your project (e.g., "bootstrap", "ml-pipeline", "api-design")
- **description**: Brief summary of what you're testing (1-2 sentences)
- **domain_context**: Context that helps agents understand the domain (e.g., "GitHub Actions automation", "TensorFlow model optimization", "REST API design patterns")

#### test_matrix

**prompts**: Array of prompt variants to test
- **id**: Short identifier (P1, P2, P3...) used in directory names
- **file**: Relative path from project root to prompt file
- **description**: Human-readable explanation of this prompt variant
- **word_count**: Approximate length (helps with completeness calibration)

**criteria**: Array of success criteria variants
- **id**: Short identifier (S1, S2, S3...) used in directory names
- **file**: Relative path from project root to criteria file
- **description**: Human-readable explanation of this criteria level
- **requirements_count**: Number of requirements (helps evaluators)

**models**: Array of AI models to test
- **id**: Short identifier for directory names (opus, sonnet, haiku)
- **name**: Full model name for Task tool (claude-opus-4-5, claude-sonnet-4-5, claude-haiku-4)

**total_tests**: Calculated automatically (prompts.length × criteria.length × models.length)

#### execution

- **batch_size**: Number of agents to run in parallel
  - Recommended: 9 (3 prompts × 3 criteria for one model)
  - Maximum safe: 27 (all tests at once)
  - Conservative: 3 (if resource-constrained)

- **research_policy**: Whether agents can use WebSearch
  - "web": Agents can research current best practices (recommended)
  - "none": Agents use only training data (for controlled experiments)

- **timeout_per_simulation**: Max time in seconds for Phase 1 agents
  - Typical: 600 (10 minutes)
  - Complex tasks: 1200 (20 minutes)
  - Simple tasks: 300 (5 minutes)

- **timeout_per_evaluation**: Max time in seconds for Phase 2 evaluators
  - Typical: 300 (5 minutes)
  - Complex rubrics: 600 (10 minutes)

#### rubric

- **total_points**: Sum of all dimension points (100 or 120 typical)

- **dimensions**: Array of scoring dimensions
  - **name**: Dimension identifier (completeness, correctness, actionability...)
  - **points**: Points allocated (should sum to total_points)
  - **description**: Brief scoring guidance (what this dimension measures)

Standard rubric dimensions (customize as needed):
1. **completeness** (30 pts) - Coverage of requirements
2. **correctness** (25 pts) - Logical and syntactic correctness
3. **actionability** (20 pts) - Ready to use without modification
4. **specificity** (15 pts) - Low placeholder density
5. **insight_quality** (10 pts) - Documented assumptions and edge cases

#### output

- **run_directory_pattern**: Template for run directory names
  - Placeholders: {project}, {timestamp}, {config}
  - Example: "experiments/{project}/runs/run-{timestamp}"

- **log_format**: How agent logs are stored
  - "compact": Compressed JSONL format (space-efficient)
  - "verbose": Full conversation history (debugging-friendly)

- **analysis_format**: Format for Phase 3 analysis
  - "markdown": Human-readable tables and text (recommended)
  - "json": Machine-readable structured data (for automation)

### Validation Rules

**Required fields:**
- project.name, project.description
- At least 2 prompts (minimum for comparison)
- At least 2 criteria levels (minimum for differentiation)
- At least 2 models (minimum for model comparison)
- rubric.dimensions must sum to rubric.total_points

**Recommendations:**
- Use 3 variants for each dimension (prompts, criteria) - good balance
- Use 3 models (opus, sonnet, haiku) - covers capability range
- Keep total_tests under 30 (reasonable execution time)
- Allocate more points to objective dimensions (syntax) than subjective (insight)

---

## 4. Customization

### Customizing Prompts

#### Prompt Variation Strategies

**Length-based (simple):**
- P1: Minimal (10 words) - "Task description only"
- P2: Moderate (20 words) - "Task + context"
- P3: Detailed (40 words) - "Task + context + requirements"

**Specificity-based (intermediate):**
- P1: Generic - "Build an API"
- P2: Constrained - "Build a REST API with authentication"
- P3: Prescriptive - "Build a REST API with JWT auth, rate limiting, and OpenAPI spec"

**Style-based (advanced):**
- P1: Imperative - "Create X"
- P2: Declarative - "System must have X"
- P3: User-story - "As a user, I need X so that Y"

**Best practices:**
- Keep P1 intentionally vague to test agent inference
- Make P3 prescriptive to test adherence to specifications
- Test prompt clarity impact: Does more detail → better output?

#### Example Prompts for Different Domains

**GitHub Automation:**
```
P1: Bootstrap @copilot issue automation with auto-review.
P2: Setup issue-driven development with @copilot auto-assignment and knowledge base.
P3: Create issue template, CODEOWNERS for auto-assignment, knowledge base (docs/knowledge/), README with workflow.
```

**Machine Learning:**
```
P1: Create ML model training pipeline.
P2: Create ML pipeline with data validation, model training, and evaluation metrics.
P3: Create TensorFlow pipeline: data loader (CSV/JSON), model training (configurable architecture), evaluation (precision/recall/F1), hyperparameter tuning (grid search).
```

**API Design:**
```
P1: Design REST API for task management.
P2: Design REST API with CRUD operations, authentication, and rate limiting.
P3: Design REST API with: GET/POST/PUT/DELETE endpoints, JWT authentication, rate limiting (100 req/min), OpenAPI 3.0 spec, error handling (4xx/5xx).
```

### Customizing Success Criteria

#### Criteria Level Strategies

**Requirements count (simple):**
- S1: 1 requirement - "Must work"
- S2: 3 requirements - "Must work + quality gate + documentation"
- S3: 7+ requirements - "Observable outcomes checklist"

**Verification method (intermediate):**
- S1: Subjective - "System should work well"
- S2: Testable - "System must pass unit tests"
- S3: Observable - "System completes task in <5 seconds with 99% accuracy"

**Completeness expectation (advanced):**
- S1: Minimal viable - "Core functionality only"
- S2: Production-ready - "Core + error handling + docs"
- S3: Excellence - "Production + monitoring + security + scalability"

**Best practices:**
- Make S1 intentionally vague to test agent interpretation
- Make S3 explicit and measurable (avoid subjective criteria)
- Test criteria impact: Does higher bar → better quality or just more output?

#### Example Criteria for Different Domains

**GitHub Automation:**
```
S1: System must process a test issue without errors.
S2: System must trigger on issue creation, auto-assign PR to owner, include knowledge base structure.
S3: Observable outcomes: (1) Functional test passes, (2) Files pass validation (yamllint/shellcheck), (3) Workflow triggers correctly, (4) 90%+ reliability, (5) Works with 3+ AI models, (6) Single-command bootstrap, (7) Self-improvement capability.
```

**Machine Learning:**
```
S1: Model must train and make predictions.
S2: Model must achieve >80% accuracy, log metrics, save checkpoints.
S3: Observable outcomes: (1) Data validation catches bad inputs, (2) Training completes in <30 min, (3) Accuracy >85%, (4) Metrics logged (MLflow), (5) Hyperparameter search runs, (6) Model explainability included, (7) Deployment script works.
```

**API Design:**
```
S1: API must handle basic requests.
S2: API must support CRUD operations, return proper HTTP codes, include authentication.
S3: Observable outcomes: (1) OpenAPI spec validates, (2) All endpoints return correct codes, (3) Auth prevents unauthorized access, (4) Rate limiting works, (5) Error messages are user-friendly, (6) Response time <100ms, (7) Handles concurrent requests.
```

### Customizing Rubrics

#### Standard 100-Point Rubric (Recommended)

```json
{
  "rubric": {
    "total_points": 100,
    "dimensions": [
      {"name": "completeness", "points": 30, "description": "Coverage of requirements"},
      {"name": "correctness", "points": 25, "description": "Logical and syntactic correctness"},
      {"name": "actionability", "points": 20, "description": "Ready to use without modification"},
      {"name": "specificity", "points": 15, "description": "Low placeholder density"},
      {"name": "insight_quality", "points": 10, "description": "Documented assumptions"}
    ]
  }
}
```

#### Enhanced 120-Point Rubric (For Research)

Includes completeness calibration and research quality (see ENHANCED_RUBRIC.md in examples/bootstrap/).

```json
{
  "rubric": {
    "total_points": 120,
    "dimensions": [
      {"name": "functional_verification", "points": 30, "description": "Syntax valid, structure correct"},
      {"name": "completeness_calibration", "points": 25, "description": "Appropriate detail for prompt (P1: 65-70%, P3: 75-85%)"},
      {"name": "correctness", "points": 20, "description": "Logically sound, handles edge cases"},
      {"name": "actionability", "points": 15, "description": "Immediate use vs rework needed"},
      {"name": "research_quality", "points": 15, "description": "Used WebSearch, cites 2026 sources"},
      {"name": "specificity", "points": 10, "description": "Low placeholder density"},
      {"name": "insight_quality", "points": 5, "description": "Assumptions and edge cases documented"}
    ]
  }
}
```

#### Domain-Specific Rubric Examples

**Code Quality (for development tasks):**
```json
{
  "rubric": {
    "total_points": 100,
    "dimensions": [
      {"name": "test_coverage", "points": 25, "description": "Unit tests >80% coverage"},
      {"name": "code_quality", "points": 25, "description": "Linter passes, no code smells"},
      {"name": "documentation", "points": 20, "description": "Docstrings, README, examples"},
      {"name": "performance", "points": 15, "description": "Meets latency/throughput SLOs"},
      {"name": "security", "points": 15, "description": "No vulnerabilities, follows best practices"}
    ]
  }
}
```

**User Experience (for interface tasks):**
```json
{
  "rubric": {
    "total_points": 100,
    "dimensions": [
      {"name": "usability", "points": 30, "description": "Clear UI, intuitive navigation"},
      {"name": "accessibility", "points": 25, "description": "WCAG 2.1 AA compliant"},
      {"name": "responsiveness", "points": 20, "description": "Works on mobile/tablet/desktop"},
      {"name": "error_handling", "points": 15, "description": "Clear error messages, recovery paths"},
      {"name": "polish", "points": 10, "description": "Consistent styling, animations"}
    ]
  }
}
```

**Research Quality (for analysis tasks):**
```json
{
  "rubric": {
    "total_points": 100,
    "dimensions": [
      {"name": "source_quality", "points": 30, "description": "Cites peer-reviewed sources"},
      {"name": "methodology", "points": 25, "description": "Rigorous research method"},
      {"name": "comprehensiveness", "points": 20, "description": "Covers multiple perspectives"},
      {"name": "currency", "points": 15, "description": "Sources from last 2 years"},
      {"name": "synthesis", "points": 10, "description": "Novel insights, not just summary"}
    ]
  }
}
```

### Non-Standard Test Matrices

#### 2×2×2 Matrix (Quick Test)

```json
{
  "test_matrix": {
    "prompts": [
      {"id": "P1", "file": "prompts/P1-minimal.txt", "description": "Minimal"},
      {"id": "P2", "file": "prompts/P2-detailed.txt", "description": "Detailed"}
    ],
    "criteria": [
      {"id": "S1", "file": "criteria/S1-basic.txt", "description": "Basic"},
      {"id": "S2", "file": "criteria/S2-comprehensive.txt", "description": "Comprehensive"}
    ],
    "models": [
      {"id": "opus", "name": "claude-opus-4-5"},
      {"id": "sonnet", "name": "claude-sonnet-4-5"}
    ],
    "total_tests": 8
  }
}
```

**Use case:** Rapid prototyping, proof of concept
**Time estimate:** ~15-20 minutes

#### 5×3×3 Matrix (Comprehensive)

```json
{
  "test_matrix": {
    "prompts": [
      {"id": "P1", "file": "prompts/P1-minimal.txt", "description": "Minimal (10 words)"},
      {"id": "P2", "file": "prompts/P2-short.txt", "description": "Short (20 words)"},
      {"id": "P3", "file": "prompts/P3-moderate.txt", "description": "Moderate (30 words)"},
      {"id": "P4", "file": "prompts/P4-detailed.txt", "description": "Detailed (50 words)"},
      {"id": "P5", "file": "prompts/P5-exhaustive.txt", "description": "Exhaustive (100 words)"}
    ],
    "criteria": [
      {"id": "S1", "file": "criteria/S1-minimal.txt", "description": "1 requirement"},
      {"id": "S2", "file": "criteria/S2-moderate.txt", "description": "5 requirements"},
      {"id": "S3", "file": "criteria/S3-comprehensive.txt", "description": "10 requirements"}
    ],
    "models": [
      {"id": "opus", "name": "claude-opus-4-5"},
      {"id": "sonnet", "name": "claude-sonnet-4-5"},
      {"id": "haiku", "name": "claude-haiku-4"}
    ],
    "total_tests": 45
  }
}
```

**Use case:** Research study, publication-quality data
**Time estimate:** ~60-90 minutes

#### Single-Variable Matrix (Model Comparison Only)

```json
{
  "test_matrix": {
    "prompts": [
      {"id": "P1", "file": "prompts/P1-standard.txt", "description": "Standard prompt"}
    ],
    "criteria": [
      {"id": "S1", "file": "criteria/S1-standard.txt", "description": "Standard criteria"}
    ],
    "models": [
      {"id": "opus", "name": "claude-opus-4-5"},
      {"id": "sonnet", "name": "claude-sonnet-4-5"},
      {"id": "haiku", "name": "claude-haiku-4"}
    ],
    "total_tests": 3
  }
}
```

**Use case:** Model capability comparison, cost analysis
**Time estimate:** ~5-10 minutes

### Advanced Customizations

#### Custom Output Structure

By default, outputs go to: `experiments/{project}/runs/{run-name}/{P#}-{S#}-{model}/`

To customize:

```json
{
  "output": {
    "run_directory_pattern": "results/{project}/{date}/{config}",
    "subdirectory_pattern": "{model}/{prompt}/{criteria}",
    "artifact_naming": "{prompt}-{criteria}-{model}-{artifact-type}.md"
  }
}
```

#### Custom Batching Strategy

By default, batches group by prompt (P1 × all criteria × all models = 9 tests).

Alternative batching strategies:

**Batch by model:**
```json
{
  "execution": {
    "batch_strategy": "by_model",
    "batch_size": 9
  }
}
```
Groups: (All prompts × all criteria) per model

**Batch by criteria:**
```json
{
  "execution": {
    "batch_strategy": "by_criteria",
    "batch_size": 9
  }
}
```
Groups: (All prompts × all models) per criteria

**Custom batch definitions:**
```json
{
  "execution": {
    "batch_strategy": "custom",
    "batches": [
      {"name": "batch-1", "tests": ["P1-S1-opus", "P1-S1-sonnet", "P1-S1-haiku"]},
      {"name": "batch-2", "tests": ["P1-S2-opus", "P2-S1-opus", "P3-S1-opus"]},
      {"name": "batch-3", "tests": ["P2-S2-sonnet", "P2-S3-sonnet", "P3-S2-sonnet"]}
    ]
  }
}
```

#### Adding Validation Scripts

Include automated validation in your harness:

```json
{
  "validation": {
    "enabled": true,
    "scripts": [
      {
        "name": "syntax_validation",
        "command": "yamllint {output_dir}/**/*.yml",
        "pass_threshold": 0.8
      },
      {
        "name": "shell_validation",
        "command": "shellcheck {output_dir}/**/*.sh",
        "pass_threshold": 0.9
      }
    ]
  }
}
```

---

## 5. Examples

### Example 1: Bootstrap Automation (Standard 3×3×3)

**Domain:** GitHub Copilot issue-driven development automation

**Location:** `examples/bootstrap/` (in this repository)

**Test Matrix:**
- 3 prompts (10 words, 14 words, 35 words)
- 3 criteria (1 requirement, 3 requirements, 7 outcomes)
- 3 models (opus, sonnet, haiku)
- Total: 27 tests

**Key Files:**
- `config.json` - Full configuration
- `prompts/P1-minimal.txt` - "Bootstrap @copilot issue automation with auto-review and knowledge base."
- `criteria/S3-comprehensive.txt` - 7 observable outcomes
- `runs/run-20260106-003027-full-matrix/` - Complete run with all 27 scenarios

**Rubric:** Enhanced 120-point system with completeness calibration

**Results:**
- Syntax validation: 88.4% pass rate (425/481 files)
- Best performer: P2-S2-sonnet (balanced completeness)
- Pattern: Opus favors minimal outputs, Sonnet favors comprehensive, Haiku has structure issues

**Learn from this example:**
- How to structure prompts for varying specificity
- How completeness calibration penalizes over-engineering (P1-S3-sonnet: 36 files = too much)
- How to measure research quality (WebSearch usage, 2026 sources)

### Example 2: ML Pipeline Design (2×4×2)

**Domain:** Machine learning model training pipeline

**Location:** `examples/ml-pipeline/` (reference example, simplified)

**Test Matrix:**
- 2 prompts (simple, detailed)
- 4 criteria (basic, validated, metrics, production)
- 2 models (opus, sonnet)
- Total: 16 tests

**Prompts:**
- P1: "Create ML model training pipeline."
- P2: "Create TensorFlow pipeline: data loader (CSV/JSON), model training (configurable architecture), evaluation (precision/recall/F1), hyperparameter tuning."

**Criteria:**
- S1: "Pipeline must train model and make predictions."
- S2: "Pipeline must validate inputs, train model, log metrics."
- S3: "Pipeline must achieve >85% accuracy on test set."
- S4: "Production-ready: data validation, training, evaluation, hyperparameter tuning, deployment script, monitoring."

**Rubric:** Custom 100-point system focused on code quality
- Test coverage (25 pts)
- Code quality (25 pts)
- Documentation (20 pts)
- Performance (15 pts)
- Security (15 pts)

**Key Insights:**
- Testing 4 criteria levels helps identify "sweet spot" for completeness
- 2 models sufficient when focus is prompt engineering (not model comparison)
- Custom rubric prioritizes domain-specific concerns (test coverage, performance)

### Example 3: API Design (Single Variable)

**Domain:** REST API design patterns

**Location:** `examples/api-design/` (reference example, simplified)

**Test Matrix:**
- 1 prompt (standard specification)
- 1 criteria (OpenAPI 3.0 compliance)
- 3 models (opus, sonnet, haiku)
- Total: 3 tests

**Prompt:**
```
Design REST API for task management:
- CRUD operations (GET, POST, PUT, DELETE)
- JWT authentication
- Rate limiting (100 req/min)
- OpenAPI 3.0 specification
- Error handling (4xx/5xx codes)
```

**Criteria:**
```
API must:
1. Pass OpenAPI validator
2. Include all CRUD endpoints with proper HTTP methods
3. Implement JWT auth (Bearer token)
4. Include rate limiting headers
5. Return structured error responses
```

**Rubric:** Focused on API quality
- OpenAPI compliance (30 pts)
- Security (25 pts)
- REST conventions (20 pts)
- Documentation (15 pts)
- Error handling (10 pts)

**Use Case:** Model capability comparison for API design tasks

**Results Pattern:** Opus tends toward minimal OpenAPI specs, Sonnet includes examples, Haiku may skip security details.

### Comparing Examples

| Example | Matrix Size | Focus | Time | Use Case |
|---------|-------------|-------|------|----------|
| Bootstrap | 3×3×3 (27) | Comprehensive evaluation | ~30 min | Production research, optimal config discovery |
| ML Pipeline | 2×4×2 (16) | Criteria impact study | ~20 min | Understanding quality bar impact |
| API Design | 1×1×3 (3) | Model comparison | ~5 min | Quick model capability check |

### Adapting Examples to Your Domain

**Step 1:** Choose the closest example (Bootstrap for automation, ML Pipeline for data tasks, API Design for design tasks)

**Step 2:** Copy the configuration:
```bash
cp examples/bootstrap/config.json experiments/my-project/config.json
```

**Step 3:** Customize project details:
```json
{
  "project": {
    "name": "my-project",
    "description": "Your project description",
    "domain_context": "Your domain context"
  }
}
```

**Step 4:** Adapt prompts to your domain:
- Keep the length progression (minimal → moderate → detailed)
- Replace domain-specific terms
- Maintain same structure (task → task+context → task+context+requirements)

**Step 5:** Adapt criteria to your domain:
- Keep the requirement count progression (1 → 3 → 7)
- Replace domain-specific requirements
- Ensure criteria are measurable/observable

**Step 6:** Customize rubric (optional):
- Keep standard dimensions if they apply
- Add domain-specific dimensions if needed
- Maintain total points around 100-120

---

## 6. Troubleshooting

### Common Issues

#### Issue: Agent Timeouts

**Symptom:** Agents exceed timeout_per_simulation and are terminated.

**Causes:**
- Task is too complex for time limit
- Agent is stuck in research loop (too many WebSearch calls)
- Model is slow (Opus can be slower than Sonnet)

**Solutions:**
1. Increase timeout in config.json:
   ```json
   {
     "execution": {
       "timeout_per_simulation": 1200  // Increase to 20 minutes
     }
   }
   ```

2. Reduce research_policy if agents are over-researching:
   ```json
   {
     "execution": {
       "research_policy": "none"  // Disable web research
     }
   }
   ```

3. Simplify prompts (use P1/P2 instead of P3)

4. Break complex tasks into subtasks with separate harnesses

---

#### Issue: Too Many/Too Few Files Generated

**Symptom:** Agents create 30+ files for minimal prompt OR only 1-2 files for detailed prompt.

**Causes:**
- Prompt-criteria mismatch (P1+S3 encourages over-engineering)
- Agent interpretation of "complete solution"
- No explicit guidance on scope

**Solutions:**
1. Add scope guidance to prompts:
   ```
   P1 (minimal): "Create minimal bootstrap with core files only."
   P3 (detailed): "Create complete implementation with all specified files."
   ```

2. Add file count expectations to criteria:
   ```
   S1: "Must include 3-5 core files."
   S3: "Must include 10-15 files covering all requirements."
   ```

3. Use completeness calibration in rubric (Enhanced 120-point):
   - P1 optimal: 4-8 files (65-70% complete)
   - P2 optimal: 6-12 files (70-75% complete)
   - P3 optimal: 8-16 files (75-85% complete)

4. Review actual outputs and adjust prompts iteratively

---

#### Issue: Inconsistent Evaluation Scores

**Symptom:** Similar outputs receive very different scores from evaluators.

**Causes:**
- Rubric is too subjective (e.g., "good quality" vs "excellent quality")
- Evaluators interpret dimensions differently
- No objective measurement criteria

**Solutions:**
1. Make rubric dimensions objective:
   ```json
   {
     "name": "completeness",
     "points": 30,
     "description": "File coverage: (files_created / files_required) × 30"
   }
   ```

2. Provide scoring examples in rubric:
   ```json
   {
     "name": "actionability",
     "points": 20,
     "description": "0 TODOs = 20pts, 1-3 TODOs = 15pts, 4-8 TODOs = 10pts, 9+ TODOs = 0pts"
   }
   ```

3. Use multiple evaluators and average scores:
   ```
   Launch 3 evaluators per simulation, average the 3 scores
   ```

4. Use automated validation where possible:
   ```bash
   yamllint {output_dir}/**/*.yml  # Objective syntax check
   grep -r "TODO\|FIXME" {output_dir} | wc -l  # Objective placeholder count
   ```

---

#### Issue: Flat File Structure (Haiku Pattern)

**Symptom:** Agent creates `docs-knowledge-README.md` instead of `docs/knowledge/README.md`.

**Causes:**
- Model limitation (Haiku exhibits this pattern systematically)
- Unclear directory structure expectations

**Solutions:**
1. Make directory structure explicit in prompts:
   ```
   P3: "Create proper directory hierarchy: .github/workflows/, docs/knowledge/patterns/, NOT flat structure."
   ```

2. Add structure requirement to criteria:
   ```
   S3: "Files must be in proper directories (.github/, docs/knowledge/), not flat structure."
   ```

3. Penalize flat structure in rubric:
   ```json
   {
     "name": "structure_correctness",
     "points": 10,
     "description": "Proper directories = 10pts, flat structure = 0pts"
   }
   ```

4. Use Opus or Sonnet instead of Haiku for tasks requiring proper structure

5. Post-process Haiku outputs to restructure files (automation script)

---

#### Issue: Rubric Scores Don't Sum to Total

**Symptom:** Total score calculation is incorrect.

**Cause:** Dimension points don't sum to total_points in config.json.

**Solution:**
```bash
# Validate config.json before running
jq '.rubric.dimensions | map(.points) | add' config.json
# Should equal .rubric.total_points

# Example output should be:
# 100  (if total_points is 100)
```

Fix dimension allocations:
```json
{
  "rubric": {
    "total_points": 100,
    "dimensions": [
      {"name": "completeness", "points": 30},  // 30
      {"name": "correctness", "points": 25},   // 30+25 = 55
      {"name": "actionability", "points": 20}, // 55+20 = 75
      {"name": "specificity", "points": 15},   // 75+15 = 90
      {"name": "insight_quality", "points": 10} // 90+10 = 100 ✓
    ]
  }
}
```

---

#### Issue: Can't Track Agent IDs

**Symptom:** Lost track of which agent corresponds to which test (P#-S#-model).

**Cause:** Didn't save agent IDs in structured format.

**Solutions:**
1. Use spreadsheet or table to track:
   ```
   | Test | Agent ID | Evaluator ID |
   |------|----------|--------------|
   | P1-S1-opus | a1b2c3d | aa1bb2cc |
   | P1-S1-sonnet | a2b3c4d | aa2bb3cc |
   | P1-S1-haiku | a3b4c5d | aa3bb4cc |
   ```

2. Create agent_ids.json:
   ```json
   {
     "batch_1": {
       "P1-S1-opus": {"simulation": "a1b2c3d", "evaluation": "aa1bb2cc"},
       "P1-S1-sonnet": {"simulation": "a2b3c4d", "evaluation": "aa2bb3cc"},
       "P1-S1-haiku": {"simulation": "a3b4c5d", "evaluation": "aa3bb4cc"}
     }
   }
   ```

3. Ask Claude to track IDs in structured format:
   ```
   "Save all agent IDs in a table format:
   | Test | Simulation ID | Evaluator ID |"
   ```

4. Use scripts/harness/helpers.sh (future enhancement) to automate tracking

---

#### Issue: Phase 3 Analysis is Shallow

**Symptom:** Aggregate analysis doesn't provide actionable insights.

**Causes:**
- Too few test variations to identify patterns
- Rubric dimensions don't capture important quality aspects
- Analysis agent prompt is too generic

**Solutions:**
1. Ensure sufficient test matrix size (minimum 9 tests for pattern detection):
   - 3×3×1 (9 tests) - Minimum for prompt × criteria patterns
   - 3×3×3 (27 tests) - Recommended for model comparison
   - 5×5×3 (75 tests) - Research-grade dataset

2. Review rubric dimensions - do they measure what matters?
   ```json
   // Instead of generic "quality" (subjective)
   {"name": "quality", "points": 30, "description": "Overall quality"}

   // Use specific, measurable dimensions
   {"name": "syntax_errors", "points": 30, "description": "Error count: 0 errors = 30pts, 1-2 = 20pts, 3+ = 0pts"}
   ```

3. Provide structured analysis template to Phase 3 agent:
   ```
   "Analyze results and provide:
   1. Score distribution by model (table with mean, median, std dev)
   2. Score distribution by prompt (identify optimal prompt length)
   3. Score distribution by criteria (identify optimal criteria level)
   4. Best/worst performers (top 3, bottom 3)
   5. Pattern analysis:
      - Does more detail → better scores? (P1 vs P3)
      - Does higher criteria → better quality? (S1 vs S3)
      - Which model performs best for which prompt type?
   6. Statistical significance tests (if applicable)
   7. Recommendations for iteration N+1"
   ```

4. Include raw scores in analysis input (not just summaries)

---

#### Issue: Parallel Execution Fails

**Symptom:** Launching 9 agents in parallel causes errors or timeouts.

**Causes:**
- Resource limits (too many concurrent agents)
- Task tool limitations
- Claude Code CLI rate limits

**Solutions:**
1. Reduce batch_size:
   ```json
   {
     "execution": {
       "batch_size": 3  // Run 3 at a time instead of 9
     }
   }
   ```

2. Run batches sequentially:
   ```
   "Launch batch 1 (3 agents), wait for completion, then launch batch 2 (3 agents), etc."
   ```

3. Check Task tool documentation for concurrency limits

4. Monitor resource usage (CPU, memory) during parallel execution

5. Use single-agent sequential execution if parallelism is problematic:
   ```bash
   # Run tests one at a time
   for test in P1-S1-opus P1-S1-sonnet P1-S1-haiku; do
     echo "Running $test"
     # Launch single agent, wait for completion
   done
   ```

---

### Debugging Tips

#### Enable Verbose Logging

```json
{
  "output": {
    "log_format": "verbose"  // Instead of "compact"
  }
}
```

Verbose logs include full conversation history, making it easier to debug agent behavior.

#### Validate Configuration Before Running

```bash
# Check JSON syntax
jq empty config.json && echo "Valid JSON" || echo "Invalid JSON"

# Check rubric sum
jq '.rubric.dimensions | map(.points) | add' config.json

# Check total tests calculation
PROMPTS=$(jq '.test_matrix.prompts | length' config.json)
CRITERIA=$(jq '.test_matrix.criteria | length' config.json)
MODELS=$(jq '.test_matrix.models | length' config.json)
EXPECTED=$((PROMPTS * CRITERIA * MODELS))
ACTUAL=$(jq '.test_matrix.total_tests' config.json)
echo "Expected: $EXPECTED, Actual: $ACTUAL"
```

#### Dry Run with Single Test

Before running full matrix (27 tests), test with single configuration:

```
"Launch 1 simulation agent:
- Prompt: P2 (moderate)
- Criteria: S2 (moderate)
- Model: Sonnet
- Output: experiments/my-project/runs/dry-run/P2-S2-sonnet/

Verify output structure before running full matrix."
```

#### Review Reference Examples

If stuck, review working examples:
- `examples/bootstrap/` - Complete working harness
- `experiments/iteration-2/runs/run-20260106-003027-full-matrix/` - Full run with all outputs

#### Use Automated Validation

```bash
# From examples/bootstrap/runs/run-20260106-003027-full-matrix/
./validate-scenarios.sh

# Generates:
# - VALIDATION_REPORT.md (complete results)
# - VALIDATION_SUMMARY.md (executive summary)
```

Adapt validation script to your domain (replace yamllint/shellcheck with domain-specific validators).

---

### Getting Help

#### Documentation References

- **HARNESS_REUSABILITY_ANALYSIS.md** - Design rationale and trade-offs
- **SIMULATION_HARNESS.md** - Bootstrap-specific implementation
- **ENHANCED_RUBRIC.md** - 120-point evaluation system
- **examples/bootstrap/README.md** - Complete working example

#### Community Resources

- Post questions in [GitHub Discussions](https://github.com/your-repo/discussions) (if using this framework)
- Share your harness configurations for feedback
- Contribute examples for new domains

#### Iterative Improvement

Harness development is iterative:

1. **Iteration 1:** Start simple (2×2×2 matrix, basic rubric)
2. **Review results:** What patterns emerge? What's unclear?
3. **Iteration 2:** Refine prompts, criteria, rubric based on learnings
4. **Iteration 3:** Expand matrix size for production-quality data

Expect 2-3 iterations to converge on optimal configuration.

---

## Appendix

### Quick Reference Card

```
┌─────────────────────────────────────────────────────────────────┐
│ SIMULATION HARNESS QUICK REFERENCE                              │
├─────────────────────────────────────────────────────────────────┤
│ 1. Create config.json (3 prompts × 3 criteria × 3 models = 27) │
│ 2. Write prompts (P1-minimal.txt, P2-moderate.txt, P3-detailed)│
│ 3. Write criteria (S1-minimal.txt, S2-moderate, S3-comprehensive)│
│ 4. Run Phase 1 (simulation): Launch 9 agents in parallel        │
│ 5. Track agent IDs (a1b2c3d, a2b3c4d...)                        │
│ 6. Run Phase 1b (self-reflection): Resume same 9 agents         │
│ 7. Run Phase 2 (evaluation): Launch 9 NEW evaluator agents      │
│ 8. Track evaluator IDs (aa1bb2cc, aa2bb3cc...)                  │
│ 9. Repeat for batches 2 and 3                                   │
│ 10. Run Phase 3 (analysis): 1 agent aggregates all 27 results   │
│                                                                  │
│ Time estimate: ~20-30 minutes for full matrix (27 tests)        │
│ Parallel speedup: 3-4× faster than sequential execution         │
└─────────────────────────────────────────────────────────────────┘
```

### Configuration Templates

#### Minimal Template (2×2×2)

```json
{
  "project": {"name": "my-project", "description": "Quick test", "domain_context": "Your domain"},
  "test_matrix": {
    "prompts": [
      {"id": "P1", "file": "prompts/P1.txt", "description": "Minimal"},
      {"id": "P2", "file": "prompts/P2.txt", "description": "Detailed"}
    ],
    "criteria": [
      {"id": "S1", "file": "criteria/S1.txt", "description": "Basic"},
      {"id": "S2", "file": "criteria/S2.txt", "description": "Comprehensive"}
    ],
    "models": [
      {"id": "opus", "name": "claude-opus-4-5"},
      {"id": "sonnet", "name": "claude-sonnet-4-5"}
    ],
    "total_tests": 8
  },
  "execution": {"batch_size": 4, "research_policy": "web", "timeout_per_simulation": 600, "timeout_per_evaluation": 300},
  "rubric": {
    "total_points": 100,
    "dimensions": [
      {"name": "completeness", "points": 30, "description": "Coverage"},
      {"name": "correctness", "points": 25, "description": "Logic"},
      {"name": "actionability", "points": 20, "description": "Usability"},
      {"name": "specificity", "points": 15, "description": "Detail"},
      {"name": "insight_quality", "points": 10, "description": "Assumptions"}
    ]
  },
  "output": {"run_directory_pattern": "experiments/{project}/runs/run-{timestamp}", "log_format": "compact", "analysis_format": "markdown"}
}
```

#### Standard Template (3×3×3)

See full example in "Configuration Schema" section above.

### Glossary

- **Agent ID**: Unique identifier for Task tool agents (e.g., a1b2c3d)
- **Batch**: Group of tests run in parallel (typically 9 agents)
- **Criteria**: Success criteria defining what "done" means (S1, S2, S3)
- **Harness**: Framework for systematic testing and evaluation
- **Model**: AI model being tested (opus, sonnet, haiku)
- **Phase 1**: Simulation phase where agents execute tasks
- **Phase 1b**: Self-reflection phase where same agents analyze performance
- **Phase 2**: Evaluation phase where independent agents score outputs
- **Phase 3**: Aggregate analysis phase where all results are compared
- **Prompt**: Task instruction given to agents (P1, P2, P3)
- **Rubric**: Scoring system defining quality dimensions and point allocations
- **Test Matrix**: All combinations of prompts × criteria × models
- **Simulation**: Single test execution (one prompt + criteria + model)

---

## Version History

- **v1.0** (2026-01-11): Initial release
  - Core concepts, quick start, configuration schema
  - Customization guide with examples
  - Bootstrap, ML Pipeline, API Design examples
  - Troubleshooting section with common issues

---

## License

This guide is part of the agentic-primer project. Adapt freely for your own harness implementations.

---

## Acknowledgments

This guide was developed through iterative experimentation with the bootstrap automation project (experiments/iteration-2/). Key insights came from:

- 27 simulation runs testing prompt × criteria × model combinations
- Enhanced 120-point rubric with completeness calibration
- Automated validation showing 88.4% syntax pass rate
- Pattern analysis revealing model-specific behaviors (Opus minimal, Sonnet comprehensive, Haiku flat structure)

For detailed research findings, see:
- `HARNESS_REUSABILITY_ANALYSIS.md` - Design rationale
- `experiments/iteration-2/runs/run-20260106-003027-full-matrix/FINAL_ANALYSIS.md` - Comprehensive results

---

**Last Updated:** 2026-01-11
**Author:** Claude Sonnet 4.5
**Status:** Production Ready
