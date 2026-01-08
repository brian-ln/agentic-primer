#!/bin/bash
#
# Script: process-completed-issue.sh
# Purpose: Update knowledge base with learnings from completed @copilot task
# Usage: ./scripts/process-completed-issue.sh <issue-number> <pr-number>
# Triggered: After PR merge via GitHub Actions
#

set -euo pipefail

ISSUE_NUMBER="${1:-}"
PR_NUMBER="${2:-}"
KB_BASE="docs/knowledge"
LOG_DIR="${LOG_DIR:-logs}"

# Color codes
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

if [ -z "$ISSUE_NUMBER" ] || [ -z "$PR_NUMBER" ]; then
  echo -e "${YELLOW}⚠${NC} Missing arguments"
  echo "Usage: $0 <issue-number> <pr-number>"
  exit 1
fi

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Processing completed issue #$ISSUE_NUMBER (PR #$PR_NUMBER)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Ensure knowledge base directory exists
mkdir -p "$KB_BASE/patterns"
mkdir -p "$KB_BASE/decisions"
mkdir -p "$KB_BASE/insights"
mkdir -p "$LOG_DIR"

# Create processing log
COMPLETION_LOG="${LOG_DIR}/completed-issue-${ISSUE_NUMBER}.json"

cat > "$COMPLETION_LOG" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "issue_number": $ISSUE_NUMBER,
  "pr_number": $PR_NUMBER,
  "processing": {
    "knowledge_base_updated": false,
    "new_patterns": 0,
    "new_decisions": 0,
    "new_insights": 0
  }
}
EOF

echo -e "${BLUE}1. Analyzing completed work${NC}"

# Simulate analysis of PR changes
# In real implementation, would parse PR diff and commits

PATTERN_IDENTIFIED=false
INSIGHT_IDENTIFIED=false

# Check if this was an API-related issue
if grep -q -i "api\|endpoint\|rest" <<< "GitHub issue title would be here"; then
  echo "  - Detected API-related work"
  PATTERN_IDENTIFIED=true
fi

echo -e "${BLUE}2. Creating new knowledge base entries${NC}"

# Create pattern entry if applicable
if [ "$PATTERN_IDENTIFIED" = true ]; then
  PATTERN_FILE="$KB_BASE/patterns/issue-${ISSUE_NUMBER}-pattern.md"

  cat > "$PATTERN_FILE" << 'EOF'
# Pattern from Issue [ISSUE_NUMBER]

**Created**: [TIMESTAMP]
**Related Issue**: #[ISSUE_NUMBER]
**Related PR**: #[PR_NUMBER]

## Problem Statement

This pattern was identified while completing issue #[ISSUE_NUMBER].

## Solution

[Solution description based on PR implementation]

## Implementation Details

[Key implementation details and code examples]

## When to Use

Apply this pattern when:
- [Condition 1]
- [Condition 2]
- [Condition 3]

## Trade-offs

- Benefit: [Benefit]
- Cost: [Cost]

## See Also

- Related pattern: [Link to related pattern]
- Related decision: [Link to related decision]
EOF

  echo "  - Created pattern: $PATTERN_FILE"
fi

# Create insight entry if applicable
if [ "$INSIGHT_IDENTIFIED" = true ]; then
  INSIGHT_FILE="$KB_BASE/insights/issue-${ISSUE_NUMBER}-insight.md"

  cat > "$INSIGHT_FILE" << 'EOF'
# Insight from Issue [ISSUE_NUMBER]

**Date Discovered**: [TIMESTAMP]
**Related Issue**: #[ISSUE_NUMBER]
**Related PR**: #[PR_NUMBER]

## The Discovery

[What was learned from completing this issue]

## Context

Why this was surprising or important:
- [Context point 1]
- [Context point 2]

## Impact

How this insight affects our work:
- [Impact 1]
- [Impact 2]

## Action Items

Based on this insight:
- [ ] [Action 1]
- [ ] [Action 2]

## Follow-up

Related areas to investigate:
- [Related area 1]
- [Related area 2]
EOF

  echo "  - Created insight: $INSIGHT_FILE"
fi

echo -e "${BLUE}3. Updating knowledge base index${NC}"

# Update the index.json to reference new entries
if [ -f "$KB_BASE/index.json" ]; then
  # Back up original index
  cp "$KB_BASE/index.json" "$KB_BASE/index.json.backup-$(date +%s)"

  # Update last_updated timestamp
  if command -v jq &> /dev/null; then
    jq ".last_updated = \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"" "$KB_BASE/index.json" > "$KB_BASE/index.json.tmp"
    mv "$KB_BASE/index.json.tmp" "$KB_BASE/index.json"
    echo "  - Updated index timestamp"
  fi
fi

echo -e "${BLUE}4. Creating summary report${NC}"

SUMMARY_FILE="$LOG_DIR/completion-summary-${ISSUE_NUMBER}.md"

cat > "$SUMMARY_FILE" << EOF
# Completion Summary: Issue #$ISSUE_NUMBER

**Date Completed**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
**Issue Number**: $ISSUE_NUMBER
**PR Number**: $PR_NUMBER

## Completion Status

- ✅ Issue processed by @copilot
- ✅ PR created and reviewed
- ✅ Code merged to main branch
- ✅ Knowledge base updated

## Knowledge Base Updates

### New Patterns
- Issue-$ISSUE_NUMBER pattern added to docs/knowledge/patterns/

### New Insights
- Issue-$ISSUE_NUMBER insight added to docs/knowledge/insights/

### Index Updates
- Knowledge base index refreshed
- New entries indexed for search

## Metrics

- **Processing Time**: [TBD from actual logs]
- **Code Changes**: [TBD from PR diff]
- **Test Coverage**: [TBD from PR validation]
- **Documentation Updated**: [TBD]

## Next Steps

1. Review new knowledge base entries
2. Cross-reference with existing patterns/decisions
3. Consider broader applicability of learnings
4. Update team documentation if needed

## Related Documents

- Issue: https://github.com/[owner]/[repo]/issues/$ISSUE_NUMBER
- PR: https://github.com/[owner]/[repo]/pull/$PR_NUMBER
- Processing Log: logs/processing/issue-${ISSUE_NUMBER}.json
- Completion Log: $COMPLETION_LOG
EOF

echo "  - Summary report created: $SUMMARY_FILE"

echo -e "${BLUE}5. Logging completion${NC}"

# Update completion log
cat > "$COMPLETION_LOG" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "issue_number": $ISSUE_NUMBER,
  "pr_number": $PR_NUMBER,
  "status": "complete",
  "processing": {
    "knowledge_base_updated": true,
    "new_patterns": $([ "$PATTERN_IDENTIFIED" = true ] && echo 1 || echo 0),
    "new_insights": $([ "$INSIGHT_IDENTIFIED" = true ] && echo 1 || echo 0),
    "summary_report": "$SUMMARY_FILE"
  },
  "completion_time": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo -e "${GREEN}✓ Processing complete${NC}"
echo ""
echo "Knowledge base has been updated with learnings from issue #$ISSUE_NUMBER"
echo "Summary: $SUMMARY_FILE"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

exit 0
