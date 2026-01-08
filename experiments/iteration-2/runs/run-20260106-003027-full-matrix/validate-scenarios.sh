#!/usr/bin/env bash
set -euo pipefail

# Validation script for all scenarios in the experiment run
# Validates YAML, shell scripts, and markdown files

BASE_DIR="/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix"
OUTPUT_FILE="$BASE_DIR/VALIDATION_REPORT.md"
TEMP_DIR=$(mktemp -d)

echo "Starting validation at $(date)"
echo "Base directory: $BASE_DIR"
echo "Output file: $OUTPUT_FILE"
echo ""

# Initialize report header
cat > "$OUTPUT_FILE" << 'HEADER_EOF'
# Syntax Validation Report

HEADER_EOF

# Add dynamic header content
echo "**Generated:** $(date)" >> "$OUTPUT_FILE"
echo "**Run Directory:** experiments/iteration-2/runs/run-20260106-003027-full-matrix" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "## Executive Summary" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Function to validate YAML files
validate_yaml() {
    local file="$1"

    # Try python3 yaml validation
    if command -v python3 &> /dev/null; then
        python3 -c "import yaml, sys; yaml.safe_load(open('$file'))" 2>&1
        return $?
    # Try ruby yaml validation
    elif command -v ruby &> /dev/null; then
        ruby -ryaml -e "YAML.load_file('$file')" 2>&1
        return $?
    # Basic syntax check
    else
        # Check for basic YAML issues
        if grep -q $'\t' "$file"; then
            echo "ERROR: File contains tabs (YAML requires spaces)"
            return 1
        fi
        return 0
    fi
}

# Function to validate shell scripts
validate_shell() {
    local file="$1"

    # Check if shellcheck is available
    if command -v shellcheck &> /dev/null; then
        shellcheck -S warning "$file" 2>&1
        return $?
    else
        # Basic syntax check with bash -n
        bash -n "$file" 2>&1
        return $?
    fi
}

# Function to validate markdown files
validate_markdown() {
    local file="$1"

    # Basic markdown validation checks
    local errors=0
    local output=""

    # Check for unclosed code blocks
    local backticks=$(grep -o '```' "$file" | wc -l || echo 0)
    if [ $((backticks % 2)) -ne 0 ]; then
        output+="ERROR: Unclosed code block (odd number of backtick markers)\n"
        ((errors++))
    fi

    # Check for malformed links
    if grep -qE '\[.*\]\(\s*\)' "$file"; then
        output+="WARNING: Empty link target found\n"
    fi

    # Check for tab characters
    if grep -q $'\t' "$file"; then
        output+="WARNING: File contains tab characters\n"
    fi

    if [ $errors -eq 0 ]; then
        return 0
    else
        echo -e "$output"
        return 1
    fi
}

# Counters
total_yaml_pass=0
total_yaml_fail=0
total_shell_pass=0
total_shell_fail=0
total_md_pass=0
total_md_fail=0
total_scenarios=0

# Get list of all scenario directories (excluding template directories)
scenarios=$(find "$BASE_DIR" -maxdepth 1 -type d -name "P*-S*-*" | grep -v '{' | sort)

echo "Found scenarios:"
echo "$scenarios"
echo ""

# Create detailed results file
details_file="$TEMP_DIR/details.md"
: > "$details_file"

# Process each scenario
for scenario_dir in $scenarios; do
    scenario=$(basename "$scenario_dir")
    total_scenarios=$((total_scenarios + 1))

    echo "Validating: $scenario"

    # Counters for this scenario
    s_yaml_pass=0
    s_yaml_fail=0
    s_shell_pass=0
    s_shell_fail=0
    s_md_pass=0
    s_md_fail=0

    # Error files
    yaml_errors="$TEMP_DIR/${scenario}_yaml.txt"
    shell_errors="$TEMP_DIR/${scenario}_shell.txt"
    md_errors="$TEMP_DIR/${scenario}_md.txt"

    : > "$yaml_errors"
    : > "$shell_errors"
    : > "$md_errors"

    # Validate YAML files
    while IFS= read -r -d '' yaml_file; do
        rel_path=${yaml_file#$scenario_dir/}
        echo "  YAML: $rel_path"

        if error_output=$(validate_yaml "$yaml_file" 2>&1); then
            s_yaml_pass=$((s_yaml_pass + 1))
            echo "    PASS"
        else
            s_yaml_fail=$((s_yaml_fail + 1))
            echo "    FAIL"
            echo "File: $rel_path" >> "$yaml_errors"
            echo "$error_output" >> "$yaml_errors"
            echo "" >> "$yaml_errors"
        fi
    done < <(find "$scenario_dir" -type f \( -name "*.yml" -o -name "*.yaml" \) -print0 2>/dev/null || true)

    # Validate shell scripts
    while IFS= read -r -d '' shell_file; do
        rel_path=${shell_file#$scenario_dir/}
        echo "  Shell: $rel_path"

        if error_output=$(validate_shell "$shell_file" 2>&1); then
            s_shell_pass=$((s_shell_pass + 1))
            echo "    PASS"
        else
            s_shell_fail=$((s_shell_fail + 1))
            echo "    FAIL"
            echo "File: $rel_path" >> "$shell_errors"
            echo "$error_output" >> "$shell_errors"
            echo "" >> "$shell_errors"
        fi
    done < <(find "$scenario_dir" -type f \( -name "*.sh" -o -name "*.bash" \) -print0 2>/dev/null || true)

    # Validate markdown files
    while IFS= read -r -d '' md_file; do
        rel_path=${md_file#$scenario_dir/}
        echo "  Markdown: $rel_path"

        if error_output=$(validate_markdown "$md_file" 2>&1); then
            s_md_pass=$((s_md_pass + 1))
            echo "    PASS"
        else
            s_md_fail=$((s_md_fail + 1))
            echo "    FAIL"
            echo "File: $rel_path" >> "$md_errors"
            echo "$error_output" >> "$md_errors"
            echo "" >> "$md_errors"
        fi
    done < <(find "$scenario_dir" -type f -name "*.md" -print0 2>/dev/null || true)

    # Update totals
    total_yaml_pass=$((total_yaml_pass + s_yaml_pass))
    total_yaml_fail=$((total_yaml_fail + s_yaml_fail))
    total_shell_pass=$((total_shell_pass + s_shell_pass))
    total_shell_fail=$((total_shell_fail + s_shell_fail))
    total_md_pass=$((total_md_pass + s_md_pass))
    total_md_fail=$((total_md_fail + s_md_fail))

    # Write scenario details
    {
        echo "### $scenario"
        echo ""

        # Extract model and prompt
        model=$(echo "$scenario" | grep -oE '(opus|sonnet|haiku|CONTROL|TEST)$' || echo "unknown")
        prompt=$(echo "$scenario" | grep -oE '^P[1-3]' || echo "unknown")

        echo "- **Model:** $model"
        echo "- **Prompt:** $prompt"
        echo ""

        scenario_total=$((s_yaml_pass + s_yaml_fail + s_shell_pass + s_shell_fail + s_md_pass + s_md_fail))

        if [ $scenario_total -gt 0 ]; then
            echo "| File Type | Pass | Fail | Total |"
            echo "|-----------|------|------|-------|"
            [ $((s_yaml_pass + s_yaml_fail)) -gt 0 ] && echo "| YAML      | $s_yaml_pass | $s_yaml_fail | $((s_yaml_pass + s_yaml_fail)) |"
            [ $((s_shell_pass + s_shell_fail)) -gt 0 ] && echo "| Shell     | $s_shell_pass | $s_shell_fail | $((s_shell_pass + s_shell_fail)) |"
            [ $((s_md_pass + s_md_fail)) -gt 0 ] && echo "| Markdown  | $s_md_pass | $s_md_fail | $((s_md_pass + s_md_fail)) |"
            echo ""

            # Show errors if any
            if [ $s_yaml_fail -gt 0 ] && [ -s "$yaml_errors" ]; then
                echo "#### YAML Errors"
                echo ""
                echo '```'
                cat "$yaml_errors"
                echo '```'
                echo ""
            fi

            if [ $s_shell_fail -gt 0 ] && [ -s "$shell_errors" ]; then
                echo "#### Shell Script Errors"
                echo ""
                echo '```'
                cat "$shell_errors"
                echo '```'
                echo ""
            fi

            if [ $s_md_fail -gt 0 ] && [ -s "$md_errors" ]; then
                echo "#### Markdown Errors"
                echo ""
                echo '```'
                cat "$md_errors"
                echo '```'
                echo ""
            fi
        else
            echo "*No files found for validation*"
            echo ""
        fi
    } >> "$details_file"

    echo ""
done

# Generate summary statistics
{
    echo "- **Total Scenarios Validated:** $total_scenarios"
    echo "- **Validation Date:** $(date '+%Y-%m-%d %H:%M:%S %Z')"
    echo ""

    # Calculate pass rates
    total_yaml=$((total_yaml_pass + total_yaml_fail))
    total_shell=$((total_shell_pass + total_shell_fail))
    total_md=$((total_md_pass + total_md_fail))
    total_all=$((total_yaml + total_shell + total_md))
    total_pass=$((total_yaml_pass + total_shell_pass + total_md_pass))
    total_fail=$((total_yaml_fail + total_shell_fail + total_md_fail))

    yaml_rate=$(awk "BEGIN {if ($total_yaml > 0) printf \"%.1f\", ($total_yaml_pass/$total_yaml)*100; else print \"N/A\"}")
    shell_rate=$(awk "BEGIN {if ($total_shell > 0) printf \"%.1f\", ($total_shell_pass/$total_shell)*100; else print \"N/A\"}")
    md_rate=$(awk "BEGIN {if ($total_md > 0) printf \"%.1f\", ($total_md_pass/$total_md)*100; else print \"N/A\"}")
    total_rate=$(awk "BEGIN {if ($total_all > 0) printf \"%.1f\", ($total_pass/$total_all)*100; else print \"N/A\"}")

    echo "### Overall Results"
    echo ""
    echo "| File Type | Pass | Fail | Total | Pass Rate |"
    echo "|-----------|------|------|-------|-----------|"
    echo "| YAML      | $total_yaml_pass | $total_yaml_fail | $total_yaml | ${yaml_rate}% |"
    echo "| Shell     | $total_shell_pass | $total_shell_fail | $total_shell | ${shell_rate}% |"
    echo "| Markdown  | $total_md_pass | $total_md_fail | $total_md | ${md_rate}% |"
    echo "| **Total** | **$total_pass** | **$total_fail** | **$total_all** | **${total_rate}%** |"
    echo ""

    echo "## Detailed Results by Scenario"
    echo ""
    cat "$details_file"

    echo "## Validation Tools Used"
    echo ""
    echo "- **YAML Validation:**"
    if command -v python3 &> /dev/null; then
        echo "  - Python3 yaml module ($(python3 --version))"
    elif command -v ruby &> /dev/null; then
        echo "  - Ruby YAML parser ($(ruby --version | head -1))"
    else
        echo "  - Basic syntax checks"
    fi
    echo "- **Shell Script Validation:**"
    if command -v shellcheck &> /dev/null; then
        echo "  - shellcheck ($(shellcheck --version | head -n2 | tail -n1))"
    else
        echo "  - bash -n (syntax check only)"
    fi
    echo "- **Markdown Validation:**"
    echo "  - Custom validation (code blocks, links, tabs)"
    echo ""

    echo "---"
    echo ""
    echo "*Report generated by validate-scenarios.sh on $(date)*"
} >> "$OUTPUT_FILE"

# Clean up
rm -rf "$TEMP_DIR"

echo "Validation complete!"
echo "Report saved to: $OUTPUT_FILE"
