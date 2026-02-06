#!/bin/bash
# agent-score-correctness.sh
# Use LLM agent to score Correctness dimension (20 points)

set -e

SCENARIO_DIR="$1"
AGENT_ID="$2"

if [ -z "$SCENARIO_DIR" ] || [ -z "$AGENT_ID" ]; then
    echo "Usage: $0 <scenario-dir> <agent-id>"
    exit 1
fi

# Extract scenario name
SCENARIO_NAME=$(basename "$SCENARIO_DIR")

# Read workflow files
WORKFLOWS=$(find "$SCENARIO_DIR/.github/workflows" -name "*.yml" -exec cat {} \;)
CODEOWNERS=$(cat "$SCENARIO_DIR/.github/CODEOWNERS" 2>/dev/null || echo "Not found")
ISSUE_TEMPLATE=$(find "$SCENARIO_DIR/.github/ISSUE_TEMPLATE" -name "*.yml" -exec cat {} \; 2>/dev/null || echo "Not found")

# Create evaluation prompt
PROMPT=$(cat <<EOF
You are evaluating a GitHub-based automation system for Correctness (20 points).

## Scenario: $SCENARIO_NAME

## Files to Evaluate:

### Workflow (.github/workflows/*.yml):
\`\`\`yaml
$WORKFLOWS
\`\`\`

### CODEOWNERS (.github/CODEOWNERS):
\`\`\`
$CODEOWNERS
\`\`\`

### Issue Template (.github/ISSUE_TEMPLATE/*.yml):
\`\`\`yaml
$ISSUE_TEMPLATE
\`\`\`

## Evaluation Criteria (20 points):

### 5.1 Logical Correctness (15 points)
Trace execution path step-by-step:
1. If a user creates an issue, what happens?
2. Does each workflow step have required inputs?
3. Are GitHub API calls syntactically valid?
4. Would CODEOWNERS syntax be parsed correctly?
5. Would issue templates render in GitHub UI?

**Scoring:**
- 15pts: ALL FILES CORRECT - Logic traces successfully
- 12pts: MINOR ISSUES - 1-2 fixable issues (outdated versions, generic patterns)
- 9pts: MODERATE ISSUES - 3-4 issues (missing steps, incomplete structure)
- 6pts: MAJOR ISSUES - 5+ issues (workflow would fail)
- 0pts: FUNDAMENTALLY BROKEN - Unparseable or system won't function

### 5.2 Edge Case Handling (5 points)
Look for edge case handling:
- Input validation (empty fields, missing data)
- Error handling (continue-on-error, try/catch)
- Concurrency control
- Missing files/directories handling

**Scoring:**
- 5pts: EXCELLENT - Handles 3+ edge cases explicitly
- 4pts: GOOD - Handles 2 edge cases
- 2pts: MINIMAL - Happy path only
- 0pts: NONE - No error handling

## Your Task:

1. **Trace the logic step-by-step** - Write out what happens when an issue is created
2. **Identify issues** - List any logical errors, syntax problems, or missing components
3. **Check edge cases** - Note any error handling or edge case coverage
4. **Assign scores** - Give scores for 5.1 (15 pts) and 5.2 (5 pts)

## Output Format (JSON):

\`\`\`json
{
  "scenario": "$SCENARIO_NAME",
  "correctness": {
    "total_score": <0-20>,
    "logical_correctness": {
      "score": <0-15>,
      "issues": ["issue 1", "issue 2"],
      "execution_trace": "Step-by-step description of what happens",
      "assessment": "Brief justification"
    },
    "edge_case_handling": {
      "score": <0-5>,
      "edge_cases_found": ["edge case 1", "edge case 2"],
      "assessment": "Brief justification"
    }
  }
}
\`\`\`

Be thorough but concise. Focus on actual correctness issues, not style preferences.
EOF
)

# Call Claude API (or use claude-code Task agent)
echo "$PROMPT" | claude-3.5-sonnet --json > "/tmp/correctness-$SCENARIO_NAME.json"

# Parse and display results
cat "/tmp/correctness-$SCENARIO_NAME.json" | jq .

echo ""
echo "Correctness evaluation saved to: /tmp/correctness-$SCENARIO_NAME.json"
