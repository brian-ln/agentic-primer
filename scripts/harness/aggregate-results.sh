#!/usr/bin/env bash
# aggregate-results.sh - Aggregate analysis (Phase 3)
#
# Usage: ./aggregate-results.sh --config config.json --run-dir <directory>
#
# Aggregates all simulation and evaluation results into a comparison report.
# Generates summary statistics and identifies optimal configurations.
#
# Outputs JSON with aggregated results for Claude to analyze.

set -euo pipefail

# Source helpers
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/helpers.sh"

# Parse arguments
CONFIG_FILE=""
RUN_DIR=""
OUTPUT_FILE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --config)
            CONFIG_FILE="$2"
            shift 2
            ;;
        --run-dir)
            RUN_DIR="$2"
            shift 2
            ;;
        --output)
            OUTPUT_FILE="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 --config <config.json> --run-dir <directory> [--output <file>]"
            echo ""
            echo "Aggregates simulation and evaluation results (Phase 3)."
            echo "Generates comparison report and identifies optimal configurations."
            echo ""
            echo "Options:"
            echo "  --config FILE        Configuration file (required)"
            echo "  --run-dir DIR        Run directory with logs/ and results/ (required)"
            echo "  --output FILE        Output file for comparison report (optional)"
            echo "  -h, --help           Show this help"
            echo ""
            echo "Example:"
            echo "  $0 --config config.json --run-dir experiments/iteration-2/runs/run-20260106"
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

if [[ -z "$RUN_DIR" ]]; then
    log_error "Missing required argument: --run-dir"
    exit 1
fi

require_file "$CONFIG_FILE" "Config file"
require_dir "$RUN_DIR" "Run directory"
validate_config "$CONFIG_FILE"

# Get experiment directory
EXPERIMENT_DIR="$(get_experiment_dir "$CONFIG_FILE")"
log_info "Experiment directory: $EXPERIMENT_DIR" >&2

# Set default output file
if [[ -z "$OUTPUT_FILE" ]]; then
    OUTPUT_FILE="$RUN_DIR/COMPARISON.md"
fi

log_info "Run directory: $RUN_DIR" >&2
log_info "Output file: $OUTPUT_FILE" >&2

# Collect logs and results
LOGS_DIR="$RUN_DIR/logs"
RESULTS_DIR="$RUN_DIR/results"

ensure_dir "$LOGS_DIR"
ensure_dir "$RESULTS_DIR"

log_info "Collecting logs and results..." >&2

# Find all agent logs
AGENT_LOGS=()
if [[ -d "$LOGS_DIR" ]]; then
    while IFS= read -r -d '' file; do
        AGENT_LOGS+=("$file")
    done < <(find "$LOGS_DIR" -name "agent-*.jsonl" -print0 2>/dev/null)
fi

log_info "Found ${#AGENT_LOGS[@]} agent logs" >&2

# Find all result files
RESULT_FILES=()
if [[ -d "$RESULTS_DIR" ]]; then
    while IFS= read -r -d '' file; do
        RESULT_FILES+=("$file")
    done < <(find "$RESULTS_DIR" -type f -print0 2>/dev/null)
fi

log_info "Found ${#RESULT_FILES[@]} result files" >&2

# Parse config
RUN_NAME=$(python3 -c "import json; print(json.load(open('$CONFIG_FILE')).get('run_name', 'experiment'))")

# Export counts for Python script
export AGENT_LOGS_COUNT="${#AGENT_LOGS[@]}"
export RESULT_FILES_COUNT="${#RESULT_FILES[@]}"

# Export variables for Python
export CONFIG_FILE
export EXPERIMENT_DIR
export RUN_DIR
export RUN_NAME
export LOGS_DIR
export RESULTS_DIR

# Generate aggregation report
python3 <<'PYTHON_SCRIPT'
import json
import sys
import os
from pathlib import Path

config_file = os.environ['CONFIG_FILE']
experiment_dir = os.environ['EXPERIMENT_DIR']
run_dir = os.environ['RUN_DIR']
run_name = os.environ['RUN_NAME']
logs_dir = os.environ['LOGS_DIR']
results_dir = os.environ['RESULTS_DIR']
agent_logs_count = int(os.environ.get('AGENT_LOGS_COUNT', '0'))
result_files_count = int(os.environ.get('RESULT_FILES_COUNT', '0'))

config = json.load(open(config_file))

# Extract test matrix dimensions
prompts = config.get('prompts', [])
criteria = config.get('criteria', [])
models = config.get('models', [])

# Calculate expected tests
total_expected = len(prompts) * len(criteria) * len(models)

# Build aggregation result
result = {
    'success': True,
    'run_name': run_name,
    'experiment_dir': experiment_dir,
    'run_dir': run_dir,
    'config_file': config_file,
    'phase': 'aggregation',
    'test_matrix': {
        'prompts': len(prompts),
        'criteria': len(criteria),
        'models': len(models),
        'total_expected': total_expected
    },
    'results_collected': {
        'agent_logs': agent_logs_count,
        'result_files': result_files_count,
        'logs_directory': logs_dir,
        'results_directory': results_dir
    },
    'analysis_tasks': [
        {
            'task': 'analyze_by_prompt',
            'description': 'Compare performance across prompt variants',
            'dimensions': [p.get('id', f'P{i+1}') for i, p in enumerate(prompts)]
        },
        {
            'task': 'analyze_by_criteria',
            'description': 'Compare performance across criteria variants',
            'dimensions': [c.get('id', f'S{i+1}') for i, c in enumerate(criteria)]
        },
        {
            'task': 'analyze_by_model',
            'description': 'Compare performance across models',
            'dimensions': [m.get('id', m) if isinstance(m, dict) else m for m in models]
        },
        {
            'task': 'identify_optimal',
            'description': 'Identify best performing configuration',
            'criteria': 'highest_score'
        }
    ],
    'next_steps': [
        f'Review logs in {logs_dir}',
        f'Analyze results in {results_dir}',
        'Generate comparison matrix',
        'Identify optimal configuration',
        'Document findings in README.md'
    ]
}

print(json.dumps(result, indent=2))
PYTHON_SCRIPT

log_info "Aggregation complete" >&2
