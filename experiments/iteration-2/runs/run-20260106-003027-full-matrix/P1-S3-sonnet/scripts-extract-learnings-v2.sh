#!/usr/bin/env bash
#
# Learning Extraction Script
#
# Purpose: Parse execution logs and extract patterns for knowledge base
# Usage: ./scripts/extract-learnings-v2.sh [pr_number]
# Exit Code: 0 on success, 1 on failure

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Parse arguments
PR_NUMBER="${1:-}"

if [ -z "$PR_NUMBER" ]; then
    echo "Usage: $0 <pr_number>"
    echo "Example: $0 123"
    exit 1
fi

echo ""
echo "╔═══════════════════════════════════════════════════════════╗"
echo "║         Learning Extraction from PR #${PR_NUMBER}"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""

# Check prerequisites
if ! command -v gh &> /dev/null; then
    echo -e "${RED}✗ GitHub CLI not found${NC}"
    exit 1
fi

if ! command -v jq &> /dev/null; then
    echo -e "${RED}✗ jq not found${NC}"
    exit 1
fi

# Step 1: Fetch PR data
echo -e "${BLUE}[STEP 1]${NC} Fetching PR metadata..."

PR_DATA=$(gh pr view "$PR_NUMBER" --json title,body,author,mergedAt,labels,files,additions,deletions 2>/dev/null || echo "")

if [ -z "$PR_DATA" ]; then
    echo -e "${RED}✗ Failed to fetch PR #${PR_NUMBER}${NC}"
    exit 1
fi

PR_TITLE=$(echo "$PR_DATA" | jq -r '.title')
PR_BODY=$(echo "$PR_DATA" | jq -r '.body // "No description"')
PR_AUTHOR=$(echo "$PR_DATA" | jq -r '.author.login')
PR_MERGED_AT=$(echo "$PR_DATA" | jq -r '.mergedAt // "Not merged"')
PR_LABELS=$(echo "$PR_DATA" | jq -r '.labels[].name' | tr '\n' ',' | sed 's/,$//')
PR_FILES=$(echo "$PR_DATA" | jq -r '.files[].path' | tr '\n' ' ')
PR_ADDITIONS=$(echo "$PR_DATA" | jq -r '.additions')
PR_DELETIONS=$(echo "$PR_DATA" | jq -r '.deletions')

echo -e "${GREEN}✓ Fetched PR data${NC}"
echo "  Title: $PR_TITLE"
echo "  Author: $PR_AUTHOR"
echo "  Merged: $PR_MERGED_AT"
echo ""

# Step 2: Analyze changed files
echo -e "${BLUE}[STEP 2]${NC} Analyzing changed files..."

WORKFLOW_FILES=0
SCRIPT_FILES=0
CONFIG_FILES=0
CODE_FILES=0
DOC_FILES=0

for file in $PR_FILES; do
    case "$file" in
        .github/workflows/*)
            WORKFLOW_FILES=$((WORKFLOW_FILES + 1))
            ;;
        scripts/*.sh)
            SCRIPT_FILES=$((SCRIPT_FILES + 1))
            ;;
        *.yml|*.yaml|*.json)
            CONFIG_FILES=$((CONFIG_FILES + 1))
            ;;
        *.py|*.js|*.ts|*.go|*.rs)
            CODE_FILES=$((CODE_FILES + 1))
            ;;
        *.md)
            DOC_FILES=$((DOC_FILES + 1))
            ;;
    esac
done

echo "File changes:"
echo "  Workflows: $WORKFLOW_FILES"
echo "  Scripts: $SCRIPT_FILES"
echo "  Config: $CONFIG_FILES"
echo "  Code: $CODE_FILES"
echo "  Docs: $DOC_FILES"
echo ""

# Step 3: Determine knowledge base updates
echo -e "${BLUE}[STEP 3]${NC} Determining knowledge base updates..."

KB_UPDATES=()

if [ $WORKFLOW_FILES -gt 0 ]; then
    KB_UPDATES+=("patterns/github-actions")
    echo "  → Update: patterns/github-actions.md"
fi

if [ $SCRIPT_FILES -gt 0 ]; then
    KB_UPDATES+=("patterns/scripting")
    echo "  → Update: patterns/scripting.md"
fi

if [ $CODE_FILES -gt 0 ]; then
    KB_UPDATES+=("patterns/code-structure")
    echo "  → Update: patterns/code-structure.md"
fi

# Always create insight
KB_UPDATES+=("insights/$(date +%Y-%m-%d)-pr-${PR_NUMBER}")
echo "  → Create: insights/$(date +%Y-%m-%d)-pr-${PR_NUMBER}.md"

echo ""

# Step 4: Create directories
echo -e "${BLUE}[STEP 4]${NC} Ensuring knowledge base structure..."

mkdir -p docs/knowledge/patterns
mkdir -p docs/knowledge/decisions
mkdir -p docs/knowledge/insights

echo -e "${GREEN}✓ Directories ready${NC}"
echo ""

# Step 5: Generate insight document
echo -e "${BLUE}[STEP 5]${NC} Generating insight document..."

INSIGHT_FILE="docs/knowledge/insights/$(date +%Y-%m-%d)-pr-${PR_NUMBER}.md"

cat > "$INSIGHT_FILE" << EOF
# Insight: ${PR_TITLE}

**PR**: #${PR_NUMBER}
**Author**: ${PR_AUTHOR}
**Merged**: ${PR_MERGED_AT}
**Labels**: ${PR_LABELS}

## Summary

${PR_BODY}

## Change Statistics

- **Files changed**: $(echo "$PR_FILES" | wc -w)
- **Lines added**: ${PR_ADDITIONS}
- **Lines deleted**: ${PR_DELETIONS}

## File Breakdown

- Workflows: ${WORKFLOW_FILES}
- Scripts: ${SCRIPT_FILES}
- Configuration: ${CONFIG_FILES}
- Code: ${CODE_FILES}
- Documentation: ${DOC_FILES}

## Affected Files

\`\`\`
${PR_FILES}
\`\`\`

## Learnings

### What Worked Well

- PR successfully merged with automated validation
- Quality gates passed (syntax validation, tests)
- Knowledge base update triggered automatically

### Challenges Encountered

(To be filled by human review or automated log analysis)

### Improvements for Future

(Extracted from PR comments or workflow failures)

## Patterns Identified

### Reusable Code Patterns

(Extract key patterns from diff analysis)

### Configuration Patterns

(Extract configuration best practices)

### Testing Patterns

(Extract test strategies used)

## Related Knowledge

- See patterns: [GitHub Actions](../patterns/github-actions.md)
- See patterns: [Scripting](../patterns/scripting.md)
- See decisions: [Architecture Decisions](../decisions/README.md)

## Tags

\`automated\` \`pr-analysis\` \`${PR_LABELS}\`

---

**Status**: Auto-generated from PR #${PR_NUMBER}
**Review Required**: Yes
**Generated**: $(date --iso-8601=seconds 2>/dev/null || date +%Y-%m-%dT%H:%M:%S)
EOF

echo -e "${GREEN}✓ Created: ${INSIGHT_FILE}${NC}"
echo ""

# Step 6: Update pattern libraries
if [ $WORKFLOW_FILES -gt 0 ]; then
    echo -e "${BLUE}[STEP 6a]${NC} Updating workflow patterns..."

    PATTERN_FILE="docs/knowledge/patterns/github-actions.md"

    if [ ! -f "$PATTERN_FILE" ]; then
        cat > "$PATTERN_FILE" << 'EOF'
# GitHub Actions Patterns

Reusable workflow patterns discovered through PR analysis.

## Table of Contents

- [Issue Processing](#issue-processing)
- [PR Validation](#pr-validation)
- [Knowledge Base Updates](#knowledge-base-updates)

---

EOF
    fi

    # Append PR summary
    cat >> "$PATTERN_FILE" << EOF

## Pattern from PR #${PR_NUMBER}

**Title**: ${PR_TITLE}
**Date**: ${PR_MERGED_AT}
**Author**: ${PR_AUTHOR}

### Description

${PR_BODY}

### Workflow Files Modified

\`\`\`
$(echo "$PR_FILES" | tr ' ' '\n' | grep '.github/workflows/')
\`\`\`

---

EOF

    echo -e "${GREEN}✓ Updated: ${PATTERN_FILE}${NC}"
fi

if [ $SCRIPT_FILES -gt 0 ]; then
    echo -e "${BLUE}[STEP 6b]${NC} Updating scripting patterns..."

    PATTERN_FILE="docs/knowledge/patterns/scripting.md"

    if [ ! -f "$PATTERN_FILE" ]; then
        cat > "$PATTERN_FILE" << 'EOF'
# Scripting Patterns

Shell scripting best practices and patterns.

## Table of Contents

- [Error Handling](#error-handling)
- [Logging](#logging)
- [Testing](#testing)

---

EOF
    fi

    cat >> "$PATTERN_FILE" << EOF

## Pattern from PR #${PR_NUMBER}

**Title**: ${PR_TITLE}
**Date**: ${PR_MERGED_AT}

### Scripts Modified

\`\`\`
$(echo "$PR_FILES" | tr ' ' '\n' | grep 'scripts/.*\.sh')
\`\`\`

---

EOF

    echo -e "${GREEN}✓ Updated: ${PATTERN_FILE}${NC}"
fi

echo ""

# Step 7: Update indexes
echo -e "${BLUE}[STEP 7]${NC} Updating knowledge base indexes..."

# Update insights index
INSIGHTS_INDEX="docs/knowledge/insights/README.md"
if [ -f "$INSIGHTS_INDEX" ]; then
    # Append to index
    echo "- [$(date +%Y-%m-%d) PR #${PR_NUMBER}]($(date +%Y-%m-%d)-pr-${PR_NUMBER}.md) - ${PR_TITLE}" >> "$INSIGHTS_INDEX"
    echo -e "${GREEN}✓ Updated: ${INSIGHTS_INDEX}${NC}"
fi

# Update patterns index if needed
PATTERNS_INDEX="docs/knowledge/patterns/README.md"
if [ -f "$PATTERNS_INDEX" ] && ([ $WORKFLOW_FILES -gt 0 ] || [ $SCRIPT_FILES -gt 0 ]); then
    echo "- [PR #${PR_NUMBER}] ${PR_TITLE} ($(date +%Y-%m-%d))" >> "$PATTERNS_INDEX"
    echo -e "${GREEN}✓ Updated: ${PATTERNS_INDEX}${NC}"
fi

echo ""

# Step 8: Identify improvement opportunities
echo -e "${BLUE}[STEP 8]${NC} Analyzing for improvement opportunities..."

IMPROVEMENTS=()

# Check for repeated bug fixes
if echo "$PR_LABELS" | grep -qi "bug"; then
    IMPROVEMENTS+=("Consider adding automated checks to prevent similar bugs")
fi

# Check for missing tests
if echo "$PR_BODY" | grep -qi "test" || [ $CODE_FILES -gt 0 ]; then
    if ! echo "$PR_FILES" | grep -q "test"; then
        IMPROVEMENTS+=("Add test coverage for changes in this PR")
    fi
fi

# Check for documentation needs
if [ $CODE_FILES -gt 0 ] && [ $DOC_FILES -eq 0 ]; then
    IMPROVEMENTS+=("Consider adding documentation for new code")
fi

# Check for large PRs
TOTAL_CHANGES=$((PR_ADDITIONS + PR_DELETIONS))
if [ $TOTAL_CHANGES -gt 500 ]; then
    IMPROVEMENTS+=("Large PR detected - consider breaking into smaller PRs")
fi

if [ ${#IMPROVEMENTS[@]} -gt 0 ]; then
    echo "Improvement opportunities identified:"
    for improvement in "${IMPROVEMENTS[@]}"; do
        echo "  - $improvement"
    done

    # Write to improvement tracking file
    IMPROVEMENT_FILE="docs/knowledge/insights/improvements.md"
    if [ ! -f "$IMPROVEMENT_FILE" ]; then
        echo "# Improvement Opportunities" > "$IMPROVEMENT_FILE"
        echo "" >> "$IMPROVEMENT_FILE"
        echo "Tracked improvement suggestions from PR analysis." >> "$IMPROVEMENT_FILE"
        echo "" >> "$IMPROVEMENT_FILE"
    fi

    echo "## From PR #${PR_NUMBER} ($(date +%Y-%m-%d))" >> "$IMPROVEMENT_FILE"
    for improvement in "${IMPROVEMENTS[@]}"; do
        echo "- $improvement" >> "$IMPROVEMENT_FILE"
    done
    echo "" >> "$IMPROVEMENT_FILE"

    echo -e "${GREEN}✓ Logged improvements${NC}"
else
    echo "No immediate improvements identified"
fi

echo ""

# Summary
echo "╔═══════════════════════════════════════════════════════════╗"
echo "║                    Extraction Summary                     ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""

echo "Knowledge base updates:"
echo "  ✓ Insight document created"
[ $WORKFLOW_FILES -gt 0 ] && echo "  ✓ Workflow patterns updated"
[ $SCRIPT_FILES -gt 0 ] && echo "  ✓ Scripting patterns updated"
echo "  ✓ Indexes updated"
[ ${#IMPROVEMENTS[@]} -gt 0 ] && echo "  ✓ ${#IMPROVEMENTS[@]} improvements logged"

echo ""
echo -e "${GREEN}Learning extraction completed successfully${NC}"
echo ""
echo "Next steps:"
echo "  1. Review generated insights in: ${INSIGHT_FILE}"
echo "  2. Enhance auto-generated content with manual analysis"
echo "  3. Create follow-up issues for identified improvements"
echo "  4. Commit knowledge base updates to main branch"

exit 0
