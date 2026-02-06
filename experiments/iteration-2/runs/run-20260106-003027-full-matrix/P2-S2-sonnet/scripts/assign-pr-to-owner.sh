#!/usr/bin/env bash
# assign-pr-to-owner.sh
# Assigns a pull request to the creator of the linked issue
#
# Usage: ./scripts/assign-pr-to-owner.sh <pr-number>
#
# Prerequisites:
#   - GitHub CLI (gh) must be installed and authenticated
#   - PR must reference an issue with "Closes #N" or "Fixes #N"
#
# Exit codes:
#   0: PR successfully assigned
#   1: Assignment failed
#   2: Missing prerequisites or invalid input

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if PR number provided
if [ $# -eq 0 ]; then
  echo -e "${RED}Error: PR number required${NC}"
  echo ""
  echo "Usage: $0 <pr-number>"
  echo ""
  echo "Example:"
  echo "  $0 42"
  exit 2
fi

PR_NUMBER="$1"

# Validate PR number is numeric
if ! [[ "$PR_NUMBER" =~ ^[0-9]+$ ]]; then
  echo -e "${RED}Error: PR number must be numeric${NC}"
  echo "Provided: $PR_NUMBER"
  exit 2
fi

# Check if gh CLI is available
if ! command -v gh >/dev/null 2>&1; then
  echo -e "${RED}Error: GitHub CLI (gh) not found${NC}"
  echo ""
  echo "Install from: https://cli.github.com/"
  echo ""
  echo "After installation, authenticate with:"
  echo "  gh auth login"
  exit 2
fi

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  PR Auto-Assignment${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "PR: #$PR_NUMBER"
echo ""

# Get PR details
echo -e "${BLUE}Fetching PR details...${NC}"

if ! PR_BODY=$(gh pr view "$PR_NUMBER" --json body --jq '.body' 2>&1); then
  echo -e "${RED}✗ Failed to fetch PR details${NC}"
  echo "$PR_BODY"
  exit 1
fi

echo -e "${GREEN}✓ PR details fetched${NC}"
echo ""

# Extract issue number from PR body
echo -e "${BLUE}Looking for linked issue...${NC}"

# Match patterns like: Closes #123, Fixes #456, Resolves #789
ISSUE_NUMBER=$(echo "$PR_BODY" | grep -oiE "(closes|fixes|resolves)\s+#[0-9]+" | head -n1 | grep -oE "[0-9]+" || true)

if [ -z "$ISSUE_NUMBER" ]; then
  echo -e "${YELLOW}⚠ No linked issue found${NC}"
  echo ""
  echo "PR body should contain one of:"
  echo "  - Closes #123"
  echo "  - Fixes #123"
  echo "  - Resolves #123"
  echo ""
  echo "Cannot auto-assign without linked issue."
  exit 1
fi

echo -e "${GREEN}✓ Found linked issue: #$ISSUE_NUMBER${NC}"
echo ""

# Get issue creator
echo -e "${BLUE}Fetching issue creator...${NC}"

if ! ISSUE_CREATOR=$(gh issue view "$ISSUE_NUMBER" --json author --jq '.author.login' 2>&1); then
  echo -e "${RED}✗ Failed to fetch issue details${NC}"
  echo "$ISSUE_CREATOR"
  exit 1
fi

echo -e "${GREEN}✓ Issue creator: @$ISSUE_CREATOR${NC}"
echo ""

# Check if already assigned
echo -e "${BLUE}Checking current assignees...${NC}"

CURRENT_ASSIGNEES=$(gh pr view "$PR_NUMBER" --json assignees --jq '.assignees[].login' || true)

if echo "$CURRENT_ASSIGNEES" | grep -q "^$ISSUE_CREATOR$"; then
  echo -e "${GREEN}✓ PR already assigned to @$ISSUE_CREATOR${NC}"
  echo ""
  echo "No action needed."
  exit 0
fi

if [ -n "$CURRENT_ASSIGNEES" ]; then
  echo -e "${YELLOW}⚠ PR currently assigned to:${NC}"
  echo "$CURRENT_ASSIGNEES" | sed 's/^/  @/'
  echo ""
  echo "Adding @$ISSUE_CREATOR as additional assignee..."
else
  echo -e "${BLUE}No current assignees${NC}"
  echo ""
  echo "Assigning to @$ISSUE_CREATOR..."
fi

echo ""

# Assign PR to issue creator
if gh pr edit "$PR_NUMBER" --add-assignee "$ISSUE_CREATOR" 2>&1; then
  echo ""
  echo -e "${GREEN}✓ Successfully assigned PR #$PR_NUMBER to @$ISSUE_CREATOR${NC}"
  echo ""

  # Add a comment to the PR
  COMMENT="🤖 **Auto-Assignment**

This PR has been automatically assigned to @$ISSUE_CREATOR (creator of issue #$ISSUE_NUMBER).

The person who creates an issue is typically the best reviewer for its implementation."

  if gh pr comment "$PR_NUMBER" --body "$COMMENT" 2>&1; then
    echo -e "${GREEN}✓ Added comment to PR${NC}"
  else
    echo -e "${YELLOW}⚠ Failed to add comment (non-critical)${NC}"
  fi

  exit 0
else
  echo ""
  echo -e "${RED}✗ Failed to assign PR${NC}"
  echo ""
  echo "Possible reasons:"
  echo "  - User @$ISSUE_CREATOR does not have access to this repository"
  echo "  - Insufficient permissions to assign PRs"
  echo "  - GitHub API error"
  exit 1
fi
