#!/usr/bin/env bash
# evaluate-batch.sh - Execute evaluation batch (Phase 2)
#
# Usage: ./evaluate-batch.sh --config config.json --agent-ids <file>
#
# Reads config.json and agent IDs, outputs JSON for evaluation tasks.
# Claude reads this JSON and launches evaluator Task agents.
#
# The script does NOT directly invoke Task tool (can't from shell).
# Instead, it outputs structured data for Claude to act on.

set -euo pipefail

# Source helpers
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/helpers.sh"

# Parse arguments
CONFIG_FILE=""
AGENT_IDS_FILE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --config)
            CONFIG_FILE="$2"
            shift 2
            ;;
        --agent-ids)
            AGENT_IDS_FILE="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 --config <config.json> --agent-ids <file>"
            echo ""
            echo "Generates evaluation tasks for completed simulations (Phase 2)."
            echo "Outputs JSON for Claude to parse and launch evaluator agents."
            echo ""
            echo "Options:"
            echo "  --config FILE        Configuration file (required)"
            echo "  --agent-ids FILE     File with agent IDs (one per line) (required)"
            echo "  -h, --help           Show this help"
            echo ""
            echo "Example:"
            echo "  $0 --config experiments/iteration-2/config.json --agent-ids agent_ids.txt"
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

if [[ -z "$AGENT_IDS_FILE" ]]; then
    log_error "Missing required argument: --agent-ids"
    exit 1
fi

require_file "$CONFIG_FILE" "Config file"
require_file "$AGENT_IDS_FILE" "Agent IDs file"
validate_config "$CONFIG_FILE"

# Get experiment directory
EXPERIMENT_DIR="$(get_experiment_dir "$CONFIG_FILE")"
log_info "Experiment directory: $EXPERIMENT_DIR" >&2

# Read agent IDs
log_info "Reading agent IDs..." >&2
AGENT_IDS=()
while IFS= read -r line; do
    # Skip empty lines and comments
    [[ -z "$line" || "$line" =~ ^# ]] && continue
    AGENT_IDS+=("$line")
done < "$AGENT_IDS_FILE"

log_info "Found ${#AGENT_IDS[@]} agent IDs" >&2

# Parse rubric from config
log_info "Parsing evaluation rubric..." >&2

RUN_NAME=$(python3 -c "import json; print(json.load(open('$CONFIG_FILE')).get('run_name', 'experiment'))")

# Pass agent IDs to Python script
export AGENT_IDS_STR="${AGENT_IDS[*]}"
export AGENT_IDS_STR="${AGENT_IDS_STR// /,}"

# Export variables for Python
export CONFIG_FILE
export EXPERIMENT_DIR
export RUN_NAME

# Generate evaluation tasks
python3 <<'PYTHON_SCRIPT'
import json
import sys
import os

config_file = os.environ['CONFIG_FILE']
experiment_dir = os.environ['EXPERIMENT_DIR']
run_name = os.environ['RUN_NAME']
agent_ids_str = os.environ.get('AGENT_IDS_STR', '')

# Parse agent IDs from environment
agent_ids = [aid.strip() for aid in agent_ids_str.split(',') if aid.strip()]

config = json.load(open(config_file))

# Extract rubric if present
rubric = config.get('rubric', {
    'total_points': 100,
    'dimensions': [
        {
            'name': 'completeness',
            'points': 30,
            'description': 'File coverage and output quality'
        },
        {
            'name': 'correctness',
            'points': 25,
            'description': 'Syntax validity and technical accuracy'
        },
        {
            'name': 'actionability',
            'points': 20,
            'description': 'Ready to use without modifications'
        },
        {
            'name': 'specificity',
            'points': 15,
            'description': 'Concrete details, no placeholders'
        },
        {
            'name': 'insight_quality',
            'points': 10,
            'description': 'Novel approaches and clear reasoning'
        }
    ]
})

# Generate evaluation tasks
evaluation_tasks = []
for idx, agent_id in enumerate(agent_ids, 1):
    task = {
        'evaluation_id': f'eval-{agent_id[:7]}',
        'evaluation_number': idx,
        'agent_id': agent_id,
        'rubric': rubric,
        'task_description': f'Evaluate simulation agent {agent_id[:7]} for {run_name}'
    }
    evaluation_tasks.append(task)

# Output result
result = {
    'success': True,
    'run_name': run_name,
    'experiment_dir': experiment_dir,
    'config_file': config_file,
    'phase': 'evaluation',
    'total_evaluations': len(evaluation_tasks),
    'rubric': rubric,
    'evaluations': evaluation_tasks
}

print(json.dumps(result, indent=2))
PYTHON_SCRIPT

log_info "Evaluation tasks generated successfully" >&2
