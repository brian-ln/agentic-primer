#!/bin/bash

# Instruction Clarity Test Runner
# Tests 3 instruction formats for agent clarity

set -e

TEST_DIR="/tmp/instruction-clarity-tests"
mkdir -p "$TEST_DIR"

echo "=== Instruction Clarity Micro-Experiments ==="
echo "Date: $(date '+%Y-%m-%d %H:%M:%S')"
echo ""

# Test function that captures metrics
run_test() {
    local exp_num=$1
    local instruction=$2
    local exp_name=$3
    local test_dir="$TEST_DIR/exp$exp_num"

    mkdir -p "$test_dir"
    cd "$test_dir"

    echo "---"
    echo "Experiment $exp_num: $exp_name"
    echo "Instruction: $instruction"
    echo ""

    # Create a simple task file
    cat > task.txt <<EOF
Task: Create hello.py with documentation
- hello.py: Simple script (10-15 lines), prints "Hello, World!"
- README.md: Brief doc (5-10 lines), explains usage
EOF

    # Measure execution (simulated - would normally call agent)
    local start_time=$(date +%s%N)

    # Simulate agent output
    cat > hello.py <<'EOF'
#!/usr/bin/env python3
"""Simple hello world script."""

def greet(name: str = "World") -> str:
    """Greet someone."""
    return f"Hello, {name}!"

if __name__ == "__main__":
    print(greet())
EOF

    cat > README.md <<'EOF'
# Hello World Script

A simple Python script that greets the user.

## Usage

```bash
python hello.py
```

Output: `Hello, World!`
EOF

    local end_time=$(date +%s%N)
    local duration_ms=$(( (end_time - start_time) / 1000000 ))

    # Count lines
    local py_lines=$(wc -l < hello.py)
    local readme_lines=$(wc -l < README.md)
    local total_lines=$((py_lines + readme_lines))

    # Log results
    echo "Output files created:"
    echo "  - hello.py: $py_lines lines"
    echo "  - README.md: $readme_lines lines"
    echo "  - Total: $total_lines lines"
    echo "  - Time: ${duration_ms}ms"
    echo ""

    # Write metrics
    cat > metrics.json <<EOF
{
  "experiment": $exp_num,
  "name": "$exp_name",
  "py_lines": $py_lines,
  "readme_lines": $readme_lines,
  "total_lines": $total_lines,
  "duration_ms": $duration_ms,
  "files_created": 2,
  "quality_estimate": 4
}
EOF

    cat metrics.json
    echo ""
}

# Run experiments
run_test 1 "Use web search and any tools @copilot would use. DO NOT call GitHub APIs." "Baseline (Permissive)"
run_test 2 "✅ ALLOWED: WebSearch, WebFetch, Read, Write, Edit, Grep, Glob, AskUserQuestion, Bash. ❌ FORBIDDEN: GitHub APIs" "Whitelist (Explicit)"
run_test 3 "Research with WebSearch/WebFetch. Simulate GitHub deployment." "Ultra-minimal (Simulation)"

# Summary
echo "=== Summary ==="
echo "All tests completed"
echo "Test directory: $TEST_DIR"
echo ""

# Parse and display comparison
echo "Metrics Comparison:"
for i in 1 2 3; do
    if [ -f "$TEST_DIR/exp$i/metrics.json" ]; then
        echo ""
        echo "Experiment $i:"
        grep -o '"name":"[^"]*"' "$TEST_DIR/exp$i/metrics.json" | head -1
        grep -o '"total_lines":[0-9]*' "$TEST_DIR/exp$i/metrics.json"
        grep -o '"duration_ms":[0-9]*' "$TEST_DIR/exp$i/metrics.json"
    fi
done

echo ""
echo "=== Test Complete ==="
