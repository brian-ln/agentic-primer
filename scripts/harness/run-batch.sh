#!/usr/bin/env bash
# run-batch.sh - Execute simulation batch (Phase 1)
#
# Usage: ./run-batch.sh --config config.json [--batch <number>]
#
# Reads config.json and outputs JSON describing the test matrix.
# Claude reads this JSON and launches Task agents accordingly.
#
# The script does NOT directly invoke Task tool (can't from shell).
# Instead, it outputs structured data for Claude to act on.

set -euo pipefail

# Source helpers
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/helpers.sh"

# Parse arguments
CONFIG_FILE=""
BATCH_NUM=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --config)
            CONFIG_FILE="$2"
            shift 2
            ;;
        --batch)
            BATCH_NUM="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 --config <config.json> [--batch <number>]"
            echo ""
            echo "Generates test matrix for simulation execution (Phase 1)."
            echo "Outputs JSON for Claude to parse and launch Task agents."
            echo ""
            echo "Options:"
            echo "  --config FILE     Configuration file (required)"
            echo "  --batch NUM       Batch number (optional, for large test matrices)"
            echo "  -h, --help        Show this help"
            echo ""
            echo "Example:"
            echo "  $0 --config experiments/iteration-2/config.json"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Validate required arguments
if [[ -z "$CONFIG_FILE" ]]; then
    log_error "Missing required argument: --config"
    exit 1
fi

require_file "$CONFIG_FILE" "Config file"
validate_config "$CONFIG_FILE"

# Get experiment directory
EXPERIMENT_DIR="$(get_experiment_dir "$CONFIG_FILE")"
log_info "Experiment directory: $EXPERIMENT_DIR" >&2

# Parse config
log_info "Parsing config..." >&2

# Extract test matrix configuration
PROMPTS=$(python3 -c "
import json
config = json.load(open('$CONFIG_FILE'))
for p in config.get('prompts', []):
    print(json.dumps(p))
")

CRITERIA=$(python3 -c "
import json
config = json.load(open('$CONFIG_FILE'))
for c in config.get('criteria', []):
    print(json.dumps(c))
")

MODELS=$(python3 -c "
import json
config = json.load(open('$CONFIG_FILE'))
for m in config.get('models', []):
    if isinstance(m, dict):
        print(json.dumps(m))
    else:
        print(json.dumps({'id': m, 'name': m}))
")

RESEARCH_POLICY=$(python3 -c "import json; print(json.load(open('$CONFIG_FILE')).get('research_policy', 'web'))")
RUN_NAME=$(python3 -c "import json; print(json.load(open('$CONFIG_FILE')).get('run_name', 'experiment'))")

# Generate test matrix
log_info "Generating test matrix..." >&2

# Export variables for Python script
export CONFIG_FILE
export EXPERIMENT_DIR
export RESEARCH_POLICY
export RUN_NAME

# Build JSON array of tests
python3 <<'PYTHON_SCRIPT'
import json
import sys
import os

config_file = os.environ['CONFIG_FILE']
experiment_dir = os.environ['EXPERIMENT_DIR']
research_policy = os.environ['RESEARCH_POLICY']
run_name = os.environ['RUN_NAME']

config = json.load(open(config_file))
prompts = config.get('prompts', [])
criteria = config.get('criteria', [])
models = config.get('models', [])

# Normalize models to dict format
models_normalized = []
for m in models:
    if isinstance(m, dict):
        models_normalized.append(m)
    else:
        models_normalized.append({'id': m, 'name': m})

# Generate test matrix
tests = []
test_id = 1

for prompt in prompts:
    prompt_id = prompt.get('id', f'P{test_id}')
    prompt_file = prompt.get('file', '')
    prompt_desc = prompt.get('description', '')

    # Resolve prompt file path
    if prompt_file and not prompt_file.startswith('/'):
        prompt_file = os.path.join(experiment_dir, prompt_file)

    for criterion in criteria:
        criterion_id = criterion.get('id', f'S{test_id}')
        criterion_file = criterion.get('file', '')
        criterion_desc = criterion.get('description', '')

        # Resolve criterion file path
        if criterion_file and not criterion_file.startswith('/'):
            criterion_file = os.path.join(experiment_dir, criterion_file)

        for model in models_normalized:
            model_id = model.get('id', 'unknown')
            model_name = model.get('name', model_id)

            test = {
                'test_id': f'{prompt_id}-{criterion_id}-{model_id}',
                'test_number': test_id,
                'prompt': {
                    'id': prompt_id,
                    'file': prompt_file,
                    'description': prompt_desc
                },
                'criteria': {
                    'id': criterion_id,
                    'file': criterion_file,
                    'description': criterion_desc
                },
                'model': {
                    'id': model_id,
                    'name': model_name
                },
                'research_policy': research_policy,
                'task_description': f'Simulation {prompt_id}+{criterion_id}+{model_id} for {run_name}'
            }

            tests.append(test)
            test_id += 1

# Output result
result = {
    'success': True,
    'run_name': run_name,
    'experiment_dir': experiment_dir,
    'config_file': config_file,
    'phase': 'simulation',
    'research_policy': research_policy,
    'total_tests': len(tests),
    'tests': tests
}

print(json.dumps(result, indent=2))
PYTHON_SCRIPT

log_info "Test matrix generated successfully" >&2
