#!/bin/bash
# agent-score-correctness.sh
# Uses LLM agent to score Correctness dimension (20 points)
# Part of 120-point Enhanced Rubric automation

set -e

SCENARIO_DIR="$1"
AGENT_ID="$2"

if [ -z "$SCENARIO_DIR" ] || [ -z "$AGENT_ID" ]; then
    cat <<EOF
Usage: $0 <scenario-dir> <agent-id>

Evaluates Correctness dimension (20 points) using LLM agent.

Example:
  $0 experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S1-opus a715b99

Scores:
  - Logical Correctness (15 points): Workflow logic trace
  - Edge Case Handling (5 points): Error handling, validation
EOF
    exit 1
fi

SCENARIO_NAME=$(basename "$SCENARIO_DIR")
OUTPUT_FILE="/tmp/correctness-${SCENARIO_NAME}.json"

echo "Evaluating Correctness for $SCENARIO_NAME..."

# Read workflow files (limit to first 3 to avoid token overflow)
WORKFLOWS=""
if [ -d "$SCENARIO_DIR/.github/workflows" ]; then
    for workflow in "$SCENARIO_DIR/.github/workflows"/*.yml; do
        if [ -f "$workflow" ]; then
            WORKFLOWS="$WORKFLOWS

### $(basename "$workflow"):
\`\`\`yaml
$(cat "$workflow")
\`\`\`
"
        fi
    done
fi

# Read CODEOWNERS
CODEOWNERS=""
if [ -f "$SCENARIO_DIR/.github/CODEOWNERS" ]; then
    CODEOWNERS="
### .github/CODEOWNERS:
\`\`\`
$(cat "$SCENARIO_DIR/.github/CODEOWNERS")
\`\`\`
"
fi

# Read issue template
ISSUE_TEMPLATE=""
if [ -d "$SCENARIO_DIR/.github/ISSUE_TEMPLATE" ]; then
    for template in "$SCENARIO_DIR/.github/ISSUE_TEMPLATE"/*.yml; do
        if [ -f "$template" ]; then
            ISSUE_TEMPLATE="$ISSUE_TEMPLATE

### $(basename "$template"):
\`\`\`yaml
$(cat "$template")
\`\`\`
"
            break  # Just take first template
        fi
    done
fi

# Create evaluation prompt
cat > /tmp/correctness-prompt.txt <<EOF
You are evaluating a GitHub-based automation system for **Correctness (20 points)**.

## Scenario: $SCENARIO_NAME

## Files to Evaluate:

$WORKFLOWS

$CODEOWNERS

$ISSUE_TEMPLATE

---

## Evaluation Criteria

### 5.1 Logical Correctness (15 points)

**Task:** Trace the execution path step-by-step.

**Questions to answer:**
1. If a user creates an issue, what happens?
2. Does each workflow step have required inputs?
3. Are GitHub API calls syntactically valid?
4. Would CODEOWNERS syntax be parsed correctly by GitHub?
5. Would issue templates render properly in GitHub UI?

**Scoring:**
- **15 points:** ALL FILES CORRECT - Logic traces successfully, would execute without errors, all API calls valid
- **12 points:** MINOR ISSUES - 1-2 fixable issues (e.g., outdated action versions @v2, overly broad CODEOWNERS patterns)
- **9 points:** MODERATE ISSUES - 3-4 issues (e.g., workflow missing steps, incomplete knowledge base structure)
- **6 points:** MAJOR ISSUES - 5+ issues (e.g., workflow would fail on execution, invalid CODEOWNERS syntax)
- **0 points:** FUNDAMENTALLY BROKEN - Unparseable YAML, incorrect GitHub API usage, system won't function

### 5.2 Edge Case Handling (5 points)

**Task:** Look for edge case handling in workflows, scripts, or documentation.

**Common edge cases:**
- Input validation (empty issue body, missing fields)
- Error handling (\`continue-on-error: true\`, try/catch blocks)
- Concurrency control (\`concurrency:\` in workflows)
- Missing files/directories (mkdir -p, test -f)
- Workflow failures or timeouts

**Scoring:**
- **5 points:** EXCELLENT - Handles 3+ edge cases explicitly (e.g., validates inputs, creates directories, concurrency control)
- **4 points:** GOOD - Handles 2 edge cases or has general error handling patterns
- **2 points:** MINIMAL - Assumes happy path only, no explicit edge case handling
- **0 points:** NONE - No error handling, would fail on any edge case

---

## Your Task

1. **Trace the logic step-by-step** - Write out execution flow when an issue is created
2. **Identify issues** - List any logical errors, syntax problems, or missing components
3. **Check edge cases** - Note error handling, validation, concurrency control
4. **Assign scores** - Give scores for 5.1 (0-15) and 5.2 (0-5)

## Output Format

Respond with ONLY valid JSON (no markdown, no explanation outside JSON):

{
  "scenario": "$SCENARIO_NAME",
  "correctness": {
    "total_score": <0-20>,
    "logical_correctness": {
      "score": <0-15>,
      "issues_found": [
        "Issue 1 description",
        "Issue 2 description"
      ],
      "execution_trace": "Step-by-step description: Issue created → ...",
      "assessment": "Brief justification for score"
    },
    "edge_case_handling": {
      "score": <0-5>,
      "edge_cases_found": [
        "Edge case 1",
        "Edge case 2"
      ],
      "missing_edge_cases": [
        "Missing edge case 1"
      ],
      "assessment": "Brief justification for score"
    }
  }
}

**Be thorough but strict.** Focus on actual correctness issues that would prevent the system from working, not style preferences.
EOF

# Use claude (assuming claude CLI is available, or use Task tool)
# For now, create a marker file that can be processed by Task agent
echo "Creating evaluation task..."

# Write task prompt
cat > /tmp/correctness-task-${SCENARIO_NAME}.txt <<EOF
$(cat /tmp/correctness-prompt.txt)

Write the JSON output to: $OUTPUT_FILE
EOF

echo ""
echo "Task prompt created at: /tmp/correctness-task-${SCENARIO_NAME}.txt"
echo "Output will be saved to: $OUTPUT_FILE"
echo ""
echo "To execute evaluation, run:"
echo "  claude-code task \"\$(cat /tmp/correctness-task-${SCENARIO_NAME}.txt)\""
echo ""
echo "Or use this script will be integrated with Task tool infrastructure."

# For now, return the task details
cat <<RESULT
{
  "status": "ready",
  "scenario": "$SCENARIO_NAME",
  "task_file": "/tmp/correctness-task-${SCENARIO_NAME}.txt",
  "output_file": "$OUTPUT_FILE",
  "evaluation": "correctness",
  "max_points": 20
}
RESULT
