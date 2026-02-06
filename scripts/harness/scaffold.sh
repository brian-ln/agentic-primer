#!/usr/bin/env bash
# scaffold.sh - Create directory structure from config
#
# Usage: ./scaffold.sh --config config.json
#        ./scaffold.sh --iteration <name> [--suffix <suffix>]
#
# Examples:
#   ./scaffold.sh --iteration iteration-2
#   ./scaffold.sh --iteration iteration-2 --suffix P3-S2-sonnet
#   ./scaffold.sh --config experiments/iteration-2/config.json
#
# Creates experiment run directory structure with templates
# Outputs JSON with created directory information for Claude to parse

set -euo pipefail

# Source helpers
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/helpers.sh"

# Parse arguments
CONFIG_FILE=""
ITERATION=""
SUFFIX=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --config)
            CONFIG_FILE="$2"
            shift 2
            ;;
        --iteration)
            ITERATION="$2"
            shift 2
            ;;
        --suffix)
            SUFFIX="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 --config <config.json>"
            echo "   OR: $0 --iteration <name> [--suffix <suffix>]"
            echo ""
            echo "Creates experiment run directory structure with templates."
            echo ""
            echo "Options:"
            echo "  --config FILE     Use existing config file"
            echo "  --iteration NAME  Create new run in iteration (e.g., iteration-2)"
            echo "  --suffix TEXT     Optional suffix for run name"
            echo "  -h, --help        Show this help"
            echo ""
            echo "Examples:"
            echo "  $0 --iteration iteration-2"
            echo "  $0 --iteration iteration-2 --suffix full-matrix"
            echo "  $0 --config experiments/my-project/config.json"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Determine run directory
if [[ -n "$CONFIG_FILE" ]]; then
    # Using existing config
    require_file "$CONFIG_FILE" "Config file"
    RUN_DIR="$(get_experiment_dir "$CONFIG_FILE")"
    log_info "Using existing config: $CONFIG_FILE"
elif [[ -n "$ITERATION" ]]; then
    # Creating new run
    RUN_DIR="$(generate_run_dir "$ITERATION" "$SUFFIX")"
    log_info "Creating new run: $RUN_DIR"
else
    log_error "Must specify either --config or --iteration"
    exit 1
fi

# Check if already exists
if [[ -d "$RUN_DIR" && -n "$ITERATION" ]]; then
    log_error "Run directory already exists: $RUN_DIR"
    exit 1
fi

# Create structure
if [[ -n "$ITERATION" ]]; then
    ensure_dir "$RUN_DIR"
    ensure_dir "$RUN_DIR/logs"
    ensure_dir "$RUN_DIR/results"

    # Generate config.json
    CONFIG_FILE="$RUN_DIR/config.json"
    RUN_NAME="$(basename "$RUN_DIR")"
    TIMESTAMP="$(date -Iseconds)"

    cat > "$CONFIG_FILE" <<EOF
{
  "run_name": "$RUN_NAME",
  "created": "$TIMESTAMP",
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
  "notes": "TODO: Update prompts and criteria for your experiment"
}
EOF

    # Generate README.md
    README_FILE="$RUN_DIR/README.md"
    TOTAL_TESTS="$(count_total_tests "$CONFIG_FILE")"

    cat > "$README_FILE" <<EOF
# Experiment Run: $RUN_NAME

**Created:** $(date)

## Configuration

- **Prompts Tested:** P1 (minimal), P2 (moderate), P3 (detailed)
- **Success Criteria:** S1 (minimal), S2 (moderate), S3 (comprehensive)
- **Models:** opus, sonnet, haiku
- **Research Policy:** web search allowed

## Test Matrix

Total simulations: $TOTAL_TESTS (3 prompts × 3 criteria × 3 models)

## Results Summary

[Fill in after runs complete]

### Key Findings

1. [TODO]
2. [TODO]
3. [TODO]

### Optimal Configuration

- Best prompt length: [TODO]
- Best criteria level: [TODO]
- Best model: [TODO]
- Score: [TODO]/100

## Files

- \`config.json\` - Test configuration
- \`logs/\` - Compact agent logs and analysis
- \`results/\` - Simulation outputs and evaluations
- \`comparison.md\` - Cross-run analysis

## Next Steps

1. Update prompts in referenced files (see config.json)
2. Update criteria in referenced files (see config.json)
3. Run simulations: \`scripts/harness/run-batch.sh --config config.json\`
4. Evaluate results: \`scripts/harness/evaluate-batch.sh --config config.json\`
5. Aggregate analysis: \`scripts/harness/aggregate-results.sh --config config.json\`
EOF

    log_success "Created run directory: $RUN_DIR"
    log_success "Created config.json"
    log_success "Created README.md"
else
    log_info "Using existing directory: $RUN_DIR"
fi

# Output JSON for Claude to parse
cat <<EOF
{
  "success": true,
  "run_directory": "$RUN_DIR",
  "config_file": "$CONFIG_FILE",
  "structure": {
    "config": "$CONFIG_FILE",
    "readme": "$RUN_DIR/README.md",
    "logs_dir": "$RUN_DIR/logs",
    "results_dir": "$RUN_DIR/results"
  },
  "next_steps": [
    "Update prompt files referenced in config.json",
    "Update criteria files referenced in config.json",
    "Run simulations with scripts/harness/run-batch.sh",
    "Evaluate results with scripts/harness/evaluate-batch.sh",
    "Aggregate analysis with scripts/harness/aggregate-results.sh"
  ],
  "total_tests": $(count_total_tests "$CONFIG_FILE")
}
EOF
