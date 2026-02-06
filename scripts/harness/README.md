# Harness Automation Scripts

Reusable automation scripts for simulation harness execution.

## Overview

These scripts provide a framework for running systematic experiments with Claude agents. They handle:

1. **Scaffolding** - Creating directory structures and configuration
2. **Simulation** - Generating test matrices for agent execution (Phase 1)
3. **Evaluation** - Generating evaluation tasks for scoring (Phase 2)
4. **Aggregation** - Collecting and analyzing results (Phase 3)

## Design Philosophy

**Scripts output JSON, Claude acts on it**

These scripts cannot directly invoke Claude's Task tool (only available in conversation context). Instead, they:

1. Read configuration files (config.json)
2. Generate structured JSON describing what needs to be done
3. Output JSON to stdout for Claude to parse
4. Claude reads the JSON and launches Task agents accordingly

This design provides:
- Version-controlled, testable automation
- Clear separation of concerns (config → scripts → Claude → agents)
- Repeatability (same config always produces same test matrix)

## Scripts

### 1. scaffold.sh

Creates experiment run directory structure with templates.

**Usage:**
```bash
# Create new run in iteration
./scaffold.sh --iteration iteration-2

# Create with custom suffix
./scaffold.sh --iteration iteration-2 --suffix full-matrix

# Use existing config
./scaffold.sh --config experiments/my-project/config.json
```

**Outputs:**
```json
{
  "success": true,
  "run_directory": "/path/to/run",
  "config_file": "/path/to/config.json",
  "structure": {
    "config": "/path/to/config.json",
    "readme": "/path/to/README.md",
    "logs_dir": "/path/to/logs",
    "results_dir": "/path/to/results"
  },
  "next_steps": [...],
  "total_tests": 27
}
```

**Creates:**
```
experiments/iteration-2/runs/run-20260111-123456/
├── config.json          # Test configuration
├── README.md            # Run documentation
├── logs/                # Agent logs (populated later)
└── results/             # Simulation outputs (populated later)
```

### 2. run-batch.sh

Generates test matrix for simulation execution (Phase 1).

**Usage:**
```bash
./run-batch.sh --config experiments/iteration-2/config.json
```

**Outputs:**
```json
{
  "success": true,
  "phase": "simulation",
  "total_tests": 27,
  "tests": [
    {
      "test_id": "P1-S1-opus",
      "test_number": 1,
      "prompt": {
        "id": "P1",
        "file": "/path/to/prompts/P1.txt",
        "description": "Minimal prompt"
      },
      "criteria": {
        "id": "S1",
        "file": "/path/to/criteria/S1.txt",
        "description": "Single requirement"
      },
      "model": {
        "id": "opus",
        "name": "claude-opus-4-5"
      },
      "research_policy": "web",
      "task_description": "Simulation P1+S1+opus for run-20260111-123456"
    },
    ...26 more tests...
  ]
}
```

**Claude's responsibility:**
1. Parse the JSON output
2. For each test in the array:
   - Read prompt file
   - Read criteria file
   - Launch Task agent with appropriate model
   - Track returned agent ID
3. Save agent IDs to file for next phase

### 3. evaluate-batch.sh

Generates evaluation tasks for completed simulations (Phase 2).

**Usage:**
```bash
./evaluate-batch.sh --config config.json --agent-ids agent_ids.txt
```

**Agent IDs file format:**
```
# Agent IDs from Phase 1 simulation
ad7d53c
a7c3dfb
a525bb6
...
```

**Outputs:**
```json
{
  "success": true,
  "phase": "evaluation",
  "total_evaluations": 27,
  "rubric": {
    "total_points": 100,
    "dimensions": [
      {
        "name": "completeness",
        "points": 30,
        "description": "File coverage and output quality"
      },
      ...
    ]
  },
  "evaluations": [
    {
      "evaluation_id": "eval-ad7d53c",
      "evaluation_number": 1,
      "agent_id": "ad7d53c",
      "rubric": {...},
      "task_description": "Evaluate simulation agent ad7d53c for run-20260111-123456"
    },
    ...26 more evaluations...
  ]
}
```

**Claude's responsibility:**
1. Parse the JSON output
2. For each evaluation in the array:
   - Retrieve agent resume/output from agent_id
   - Launch evaluator Task agent with rubric
   - Collect evaluation scores
3. Save evaluation results for aggregation

### 4. aggregate-results.sh

Aggregates simulation and evaluation results (Phase 3).

**Usage:**
```bash
./aggregate-results.sh --config config.json --run-dir experiments/iteration-2/runs/run-20260111-123456
```

**Outputs:**
```json
{
  "success": true,
  "phase": "aggregation",
  "test_matrix": {
    "prompts": 3,
    "criteria": 3,
    "models": 3,
    "total_expected": 27
  },
  "results_collected": {
    "agent_logs": 27,
    "result_files": 54,
    "logs_directory": "/path/to/logs",
    "results_directory": "/path/to/results"
  },
  "analysis_tasks": [
    {
      "task": "analyze_by_prompt",
      "description": "Compare performance across prompt variants",
      "dimensions": ["P1", "P2", "P3"]
    },
    {
      "task": "analyze_by_criteria",
      "description": "Compare performance across criteria variants",
      "dimensions": ["S1", "S2", "S3"]
    },
    {
      "task": "analyze_by_model",
      "description": "Compare performance across models",
      "dimensions": ["opus", "sonnet", "haiku"]
    },
    {
      "task": "identify_optimal",
      "description": "Identify best performing configuration",
      "criteria": "highest_score"
    }
  ],
  "next_steps": [...]
}
```

**Claude's responsibility:**
1. Parse the JSON output
2. For each analysis task:
   - Load relevant logs and evaluation scores
   - Compare performance across dimensions
   - Identify patterns and optimal configurations
3. Generate comparison report (COMPARISON.md)
4. Update run README.md with findings

### 5. helpers.sh

Shared utility functions for all scripts.

**Usage:**
```bash
# Source in other scripts
source "$(dirname "$0")/helpers.sh"

# Then use functions
validate_config config.json
log_info "Processing..."
total=$(count_total_tests config.json)
```

**Provides:**
- `parse_json_field()` - Extract JSON field from file
- `parse_json_array()` - Extract JSON array from file
- `require_file()` - Validate file exists
- `require_dir()` - Validate directory exists
- `log_info()`, `log_success()`, `log_warning()`, `log_error()` - Colored logging
- `validate_config()` - Validate config.json structure
- `count_total_tests()` - Calculate test matrix size
- `generate_timestamp()` - Generate timestamp string
- `generate_run_dir()` - Generate run directory path
- And more...

## Configuration Format

**config.json structure:**

```json
{
  "run_name": "run-20260111-123456",
  "created": "2026-01-11T12:34:56-05:00",
  "prompts": [
    {
      "id": "P1",
      "file": "prompts/P1-minimal.txt",
      "description": "Minimal prompt (10 words)"
    },
    {
      "id": "P2",
      "file": "prompts/P2-moderate.txt",
      "description": "Moderate prompt (14 words)"
    },
    {
      "id": "P3",
      "file": "prompts/P3-detailed.txt",
      "description": "Detailed prompt (35 words)"
    }
  ],
  "criteria": [
    {
      "id": "S1",
      "file": "criteria/S1-minimal.txt",
      "description": "Single requirement"
    },
    {
      "id": "S2",
      "file": "criteria/S2-moderate.txt",
      "description": "3 requirements"
    },
    {
      "id": "S3",
      "file": "criteria/S3-comprehensive.txt",
      "description": "7 observable outcomes"
    }
  ],
  "models": [
    "claude-opus-4-5",
    "claude-sonnet-4-5",
    "claude-haiku-4"
  ],
  "research_policy": "web",
  "rubric": {
    "total_points": 100,
    "dimensions": [
      {
        "name": "completeness",
        "points": 30,
        "description": "File coverage and output quality"
      },
      {
        "name": "correctness",
        "points": 25,
        "description": "Syntax validity and technical accuracy"
      },
      {
        "name": "actionability",
        "points": 20,
        "description": "Ready to use without modifications"
      },
      {
        "name": "specificity",
        "points": 15,
        "description": "Concrete details, no placeholders"
      },
      {
        "name": "insight_quality",
        "points": 10,
        "description": "Novel approaches and clear reasoning"
      }
    ]
  },
  "notes": ""
}
```

**Field descriptions:**

- `prompts[]` - Array of prompt variants to test
  - `id` - Short identifier (e.g., "P1", "P2")
  - `file` - Relative path to prompt file
  - `description` - Human-readable description

- `criteria[]` - Array of success criteria variants to test
  - `id` - Short identifier (e.g., "S1", "S2")
  - `file` - Relative path to criteria file
  - `description` - Human-readable description

- `models[]` - Array of Claude models to test (strings or objects)

- `research_policy` - "web" (allow web search) or "local" (no web search)

- `rubric` - Evaluation scoring rubric
  - `total_points` - Maximum score (usually 100)
  - `dimensions[]` - Individual scoring dimensions
    - `name` - Dimension identifier
    - `points` - Maximum points for this dimension
    - `description` - What to evaluate

## Complete Workflow Example

**1. Create experiment run:**
```bash
./scripts/harness/scaffold.sh --iteration iteration-2 --suffix test-run
```

Output tells Claude where config.json was created.

**2. Customize configuration:**
```bash
# Edit config.json to update prompt/criteria files
# Edit prompt files (e.g., experiments/.../prompts/P1.txt)
# Edit criteria files (e.g., experiments/.../criteria/S1.txt)
```

**3. Generate simulation test matrix:**
```bash
./scripts/harness/run-batch.sh --config experiments/iteration-2/runs/run-20260111-123456/config.json
```

Claude reads JSON, launches 27 Task agents in parallel:
```
Agent launched: ad7d53c (P1-S1-opus)
Agent launched: a7c3dfb (P1-S1-sonnet)
Agent launched: a525bb6 (P1-S1-haiku)
...
```

Save agent IDs to file:
```bash
echo "ad7d53c
a7c3dfb
a525bb6
..." > agent_ids.txt
```

**4. Wait for simulations to complete**

Monitor agents with:
```bash
./scripts/view-session.py ~/.claude/projects/-Users-bln-play-agentic-primer/agent-ad7d53c.jsonl
```

**5. Export logs:**
```bash
./scripts/export-agent-logs.sh ad7d53c a7c3dfb a525bb6 ...
mv logs/* experiments/iteration-2/runs/run-20260111-123456/logs/
```

**6. Generate evaluation tasks:**
```bash
./scripts/harness/evaluate-batch.sh \
  --config experiments/iteration-2/runs/run-20260111-123456/config.json \
  --agent-ids agent_ids.txt
```

Claude reads JSON, launches 27 evaluator agents:
```
Evaluator launched: eval-ad7d53c
Evaluator launched: eval-a7c3dfb
...
```

**7. Wait for evaluations to complete**

**8. Aggregate results:**
```bash
./scripts/harness/aggregate-results.sh \
  --config experiments/iteration-2/runs/run-20260111-123456/config.json \
  --run-dir experiments/iteration-2/runs/run-20260111-123456
```

Claude reads JSON, generates comparison report:
- Analyze by prompt variant (P1 vs P2 vs P3)
- Analyze by criteria variant (S1 vs S2 vs S3)
- Analyze by model (opus vs sonnet vs haiku)
- Identify optimal configuration (highest score)

**9. Review results:**
```bash
cat experiments/iteration-2/runs/run-20260111-123456/COMPARISON.md
cat experiments/iteration-2/runs/run-20260111-123456/README.md
```

## Dependencies

**Required:**
- bash 4.0+
- python 3.6+ (for JSON parsing)
- Standard Unix utilities (find, cat, date, etc.)

**Optional:**
- jq (for manual JSON inspection)

## Testing Scripts

Test individual scripts:

```bash
# Test scaffold
./scripts/harness/scaffold.sh --iteration test-iteration

# Test run-batch JSON generation
./scripts/harness/run-batch.sh --config experiments/iteration-2/runs/run-20260106-003027-full-matrix/config.json | python3 -m json.tool

# Test evaluate-batch JSON generation
echo -e "ad7d53c\na7c3dfb\na525bb6" > /tmp/test-agent-ids.txt
./scripts/harness/evaluate-batch.sh --config <config> --agent-ids /tmp/test-agent-ids.txt | python3 -m json.tool

# Test aggregate-results
./scripts/harness/aggregate-results.sh --config <config> --run-dir <run-dir> | python3 -m json.tool
```

## Version Controlled Automation

All scripts are version-controlled and testable:

```bash
# Add to git
git add scripts/harness/*.sh
git commit -m "Add harness automation scripts"

# Track changes over time
git log scripts/harness/

# Compare versions
git diff HEAD~1 scripts/harness/run-batch.sh
```

## Integration with Claude

**Typical Claude workflow:**

1. User: "Create a new experiment run for iteration-2"
2. Claude: Runs `scaffold.sh`, reads JSON output, reports success
3. User: "Run the simulation batch"
4. Claude: Runs `run-batch.sh`, reads JSON, launches Task agents in parallel
5. Claude: Tracks agent IDs, saves to file
6. User: "Evaluate the results"
7. Claude: Runs `evaluate-batch.sh`, reads JSON, launches evaluator agents
8. User: "Aggregate and analyze"
9. Claude: Runs `aggregate-results.sh`, reads JSON, generates comparison report

## Extending Scripts

**Adding a new dimension to test matrix:**

Edit config.json:
```json
{
  "new_dimension": [
    {"id": "D1", "file": "dimension/D1.txt"},
    {"id": "D2", "file": "dimension/D2.txt"}
  ]
}
```

Update `run-batch.sh` to include new dimension in test matrix generation.

**Adding a new rubric dimension:**

Edit config.json:
```json
{
  "rubric": {
    "dimensions": [
      ...existing dimensions...,
      {
        "name": "new_dimension",
        "points": 10,
        "description": "Evaluate X aspect"
      }
    ]
  }
}
```

No script changes needed - evaluators automatically use full rubric.

## Best Practices

1. **Always validate config** before running scripts
   ```bash
   python3 -m json.tool config.json > /dev/null || echo "Invalid JSON"
   ```

2. **Track agent IDs immediately** after launching simulations
   - Don't rely on memory
   - Save to version-controlled file

3. **Export logs early** - Don't wait until all agents complete
   ```bash
   ./scripts/export-agent-logs.sh <completed-agent-ids>
   ```

4. **Review evaluation rubric** before Phase 2
   - Ensure dimensions align with goals
   - Check point allocations sum to 100

5. **Version control everything**
   - Config files
   - Prompt files
   - Criteria files
   - Agent ID lists
   - Generated reports

## Troubleshooting

**"Config file not found"**
- Check path is absolute or relative to current directory
- Verify file exists: `ls -la <config-file>`

**"Invalid JSON in config"**
- Validate with: `python3 -m json.tool config.json`
- Check for trailing commas, missing quotes

**"No tests generated"**
- Check prompts/criteria/models arrays are not empty
- Verify file paths in config are correct

**"Agent IDs file empty"**
- Ensure you saved agent IDs after Phase 1
- Check file format (one ID per line, no extra spaces)

**"Python not found"**
- Scripts require Python 3.6+
- Install with package manager or from python.org

## Future Enhancements

Potential improvements:

- [ ] Add validation for prompt/criteria files exist before generating matrix
- [ ] Support batching large test matrices (9 agents at a time)
- [ ] Add resume support (skip already-completed tests)
- [ ] Generate visual comparison charts (bar graphs, scatter plots)
- [ ] Add `--dry-run` flag to preview without executing
- [ ] Support custom scoring formulas (weighted rubrics)
- [ ] Add parallel execution tracking (kill hung agents)

## Related Documentation

- **HARNESS_REUSABILITY_ANALYSIS.md** - Design decisions and trade-offs
- **SIMULATION_ALGORITHM.md** - 3-phase methodology details
- **experiments/iteration-2/** - Working example using these scripts

## License

Same as parent project.

## Authors

Created as part of agentic-primer project.
Extracted from create-experiment-run.sh and finalize-experiment-run.sh.
