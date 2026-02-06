#!/usr/bin/env bash
# helpers.sh - Shared utilities for harness automation
#
# Usage: source scripts/harness/helpers.sh
#
# Provides:
# - JSON parsing functions
# - File validation
# - Common variables and patterns
# - Error handling utilities

set -euo pipefail

# Colors for output
export RED='\033[0;31m'
export GREEN='\033[0;32m'
export YELLOW='\033[1;33m'
export BLUE='\033[0;34m'
export CYAN='\033[0;36m'
export NC='\033[0m'

# Common paths
export SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export EXPERIMENTS_DIR="$PROJECT_ROOT/experiments"

# Parse JSON field from file
# Usage: parse_json_field config.json ".project.name"
parse_json_field() {
    local file="$1"
    local field="$2"

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file" >&2
        return 1
    fi

    # Use python's json module for reliable parsing
    python3 -c "import json; print(json.load(open('$file'))$field)" 2>/dev/null || echo ""
}

# Parse JSON array from file
# Usage: parse_json_array config.json ".prompts"
parse_json_array() {
    local file="$1"
    local field="$2"

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file" >&2
        return 1
    fi

    python3 -c "
import json
data = json.load(open('$file'))
items = data$field if '$field' else data
for item in items:
    if isinstance(item, dict):
        print(json.dumps(item))
    else:
        print(item)
" 2>/dev/null
}

# Validate required file exists
# Usage: require_file config.json "Configuration"
require_file() {
    local file="$1"
    local description="${2:-File}"

    if [[ ! -f "$file" ]]; then
        echo -e "${RED}Error:${NC} $description not found: $file" >&2
        return 1
    fi
}

# Validate directory exists
# Usage: require_dir experiments/iteration-2 "Experiment directory"
require_dir() {
    local dir="$1"
    local description="${2:-Directory}"

    if [[ ! -d "$dir" ]]; then
        echo -e "${RED}Error:${NC} $description not found: $dir" >&2
        return 1
    fi
}

# Log info message
log_info() {
    echo -e "${BLUE}INFO:${NC} $*"
}

# Log success message
log_success() {
    echo -e "${GREEN}✓${NC} $*"
}

# Log warning message
log_warning() {
    echo -e "${YELLOW}WARNING:${NC} $*"
}

# Log error message
log_error() {
    echo -e "${RED}ERROR:${NC} $*" >&2
}

# Get experiment directory from config
# Usage: get_experiment_dir config.json
get_experiment_dir() {
    local config="$1"
    local config_dir
    config_dir="$(cd "$(dirname "$config")" && pwd)"
    echo "$config_dir"
}

# Read file content
# Usage: read_file prompts/P1.txt
read_file() {
    local file="$1"

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file" >&2
        return 1
    fi

    cat "$file"
}

# Generate timestamp
# Usage: timestamp=$(generate_timestamp)
generate_timestamp() {
    date +%Y%m%d-%H%M%S
}

# Generate run directory name
# Usage: run_dir=$(generate_run_dir "iteration-2" "full-matrix")
generate_run_dir() {
    local iteration="$1"
    local suffix="${2:-}"
    local timestamp
    timestamp="$(generate_timestamp)"

    local run_name="run-${timestamp}"
    if [[ -n "$suffix" ]]; then
        run_name="${run_name}-${suffix}"
    fi

    echo "$EXPERIMENTS_DIR/$iteration/runs/$run_name"
}

# Create directory if it doesn't exist
# Usage: ensure_dir logs/
ensure_dir() {
    local dir="$1"

    if [[ ! -d "$dir" ]]; then
        mkdir -p "$dir"
    fi
}

# Count total tests from config
# Usage: total=$(count_total_tests config.json)
count_total_tests() {
    local config="$1"

    python3 -c "
import json
data = json.load(open('$config'))
prompts = len(data.get('prompts', []))
criteria = len(data.get('criteria', []))
models = len(data.get('models', []))
print(prompts * criteria * models)
" 2>/dev/null || echo "0"
}

# Validate config structure
# Usage: validate_config config.json
validate_config() {
    local config="$1"

    require_file "$config" "Config file" || return 1

    # Check if valid JSON
    python3 -c "import json; json.load(open('$config'))" 2>/dev/null
    if [[ $? -ne 0 ]]; then
        log_error "Invalid JSON in config file"
        return 1
    fi

    # Check required fields using python
    python3 <<EOF
import json
import sys

try:
    config = json.load(open('$config'))

    # Check required fields
    required = ['prompts', 'criteria', 'models']
    for field in required:
        if field not in config:
            print(f"ERROR: Missing required field: {field}", file=sys.stderr)
            sys.exit(1)
        if not config[field] or len(config[field]) == 0:
            print(f"ERROR: Field '{field}' must not be empty", file=sys.stderr)
            sys.exit(1)

    sys.exit(0)
except Exception as e:
    print(f"ERROR: {e}", file=sys.stderr)
    sys.exit(1)
EOF

    if [[ $? -ne 0 ]]; then
        return 1
    fi

    return 0
}

# Pretty print JSON
# Usage: echo "$json" | pretty_json
pretty_json() {
    python3 -m json.tool 2>/dev/null || cat
}

# Generate test ID from components
# Usage: test_id=$(generate_test_id "P1" "S2" "opus")
generate_test_id() {
    local prompt_id="$1"
    local criteria_id="$2"
    local model_id="$3"

    echo "${prompt_id}-${criteria_id}-${model_id}"
}

# Extract prompt ID from test ID
# Usage: prompt_id=$(extract_prompt_id "P1-S2-opus")
extract_prompt_id() {
    local test_id="$1"
    echo "$test_id" | cut -d'-' -f1
}

# Extract criteria ID from test ID
# Usage: criteria_id=$(extract_criteria_id "P1-S2-opus")
extract_criteria_id() {
    local test_id="$1"
    echo "$test_id" | cut -d'-' -f2
}

# Extract model ID from test ID
# Usage: model_id=$(extract_model_id "P1-S2-opus")
extract_model_id() {
    local test_id="$1"
    echo "$test_id" | cut -d'-' -f3
}

# Export functions for use in other scripts
export -f parse_json_field
export -f parse_json_array
export -f require_file
export -f require_dir
export -f log_info
export -f log_success
export -f log_warning
export -f log_error
export -f get_experiment_dir
export -f read_file
export -f generate_timestamp
export -f generate_run_dir
export -f ensure_dir
export -f count_total_tests
export -f validate_config
export -f pretty_json
export -f generate_test_id
export -f extract_prompt_id
export -f extract_criteria_id
export -f extract_model_id
