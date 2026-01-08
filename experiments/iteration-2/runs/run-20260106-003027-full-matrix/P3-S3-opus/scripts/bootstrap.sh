#!/usr/bin/env bash
#
# bootstrap.sh - Single-command setup for issue-driven development system
#
# Usage: ./scripts/bootstrap.sh [owner-username]
#
# Arguments:
#   owner-username  GitHub username for CODEOWNERS (default: @owner)
#
# This script:
# 1. Creates all required directories
# 2. Copies template files
# 3. Initializes knowledge base
# 4. Validates all syntax
# 5. Reports success/failure
#
# Exit codes:
#   0 - Bootstrap completed successfully
#   1 - Bootstrap failed
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
OWNER="${1:-@owner}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Print header
echo ""
echo -e "${BLUE}=============================================="
echo "  Issue-Driven Development Bootstrap"
echo "==============================================${NC}"
echo ""
echo "Project root: $PROJECT_ROOT"
echo "Owner: $OWNER"
echo ""

# Function to create directory with feedback
create_dir() {
    local dir="$1"
    if [ ! -d "$dir" ]; then
        mkdir -p "$dir"
        echo -e "  ${GREEN}Created${NC}: ${dir#$PROJECT_ROOT/}"
    else
        echo -e "  ${YELLOW}Exists${NC}:  ${dir#$PROJECT_ROOT/}"
    fi
}

# Function to check if file exists
file_exists() {
    [ -f "$1" ]
}

# Step 1: Create directory structure
echo -e "${BLUE}Step 1: Creating directory structure...${NC}"
echo ""

create_dir "$PROJECT_ROOT/.github/ISSUE_TEMPLATE"
create_dir "$PROJECT_ROOT/.github/workflows"
create_dir "$PROJECT_ROOT/docs/knowledge/patterns"
create_dir "$PROJECT_ROOT/docs/knowledge/decisions"
create_dir "$PROJECT_ROOT/docs/knowledge/insights"
create_dir "$PROJECT_ROOT/scripts"

echo ""

# Step 2: Verify required files exist
echo -e "${BLUE}Step 2: Verifying required files...${NC}"
echo ""

REQUIRED_FILES=(
    ".github/ISSUE_TEMPLATE/task.yml"
    ".github/CODEOWNERS"
    ".github/workflows/copilot-issue.yml"
    "docs/knowledge/README.md"
    "docs/knowledge/patterns/README.md"
    "docs/knowledge/decisions/README.md"
    "docs/knowledge/insights/README.md"
    "scripts/validate.sh"
    "README.md"
)

MISSING_FILES=()

for file in "${REQUIRED_FILES[@]}"; do
    if file_exists "$PROJECT_ROOT/$file"; then
        echo -e "  ${GREEN}Found${NC}: $file"
    else
        echo -e "  ${RED}Missing${NC}: $file"
        MISSING_FILES+=("$file")
    fi
done

echo ""

# Step 3: Update CODEOWNERS with actual owner
echo -e "${BLUE}Step 3: Updating CODEOWNERS...${NC}"
echo ""

if file_exists "$PROJECT_ROOT/.github/CODEOWNERS"; then
    if [ "$OWNER" != "@owner" ]; then
        sed -i.bak "s/@owner/$OWNER/g" "$PROJECT_ROOT/.github/CODEOWNERS"
        rm -f "$PROJECT_ROOT/.github/CODEOWNERS.bak"
        echo -e "  ${GREEN}Updated${NC}: CODEOWNERS with owner $OWNER"
    else
        echo -e "  ${YELLOW}Skipped${NC}: Using default @owner placeholder"
    fi
else
    echo -e "  ${RED}Error${NC}: CODEOWNERS not found"
fi

echo ""

# Step 4: Make scripts executable
echo -e "${BLUE}Step 4: Making scripts executable...${NC}"
echo ""

for script in "$PROJECT_ROOT"/scripts/*.sh; do
    if [ -f "$script" ]; then
        chmod +x "$script"
        echo -e "  ${GREEN}Executable${NC}: $(basename "$script")"
    fi
done

echo ""

# Step 5: Run validation
echo -e "${BLUE}Step 5: Running validation...${NC}"
echo ""

if [ -x "$PROJECT_ROOT/scripts/validate.sh" ]; then
    if "$PROJECT_ROOT/scripts/validate.sh"; then
        VALIDATION_PASSED=true
    else
        VALIDATION_PASSED=false
    fi
else
    echo -e "  ${YELLOW}Skipped${NC}: validate.sh not executable"
    VALIDATION_PASSED=true
fi

echo ""

# Step 6: Initialize git hooks (optional)
echo -e "${BLUE}Step 6: Checking Git configuration...${NC}"
echo ""

if [ -d "$PROJECT_ROOT/.git" ]; then
    echo -e "  ${GREEN}Found${NC}: Git repository"

    # Check if GitHub remote exists
    if git -C "$PROJECT_ROOT" remote get-url origin >/dev/null 2>&1; then
        REMOTE_URL=$(git -C "$PROJECT_ROOT" remote get-url origin)
        echo -e "  ${GREEN}Remote${NC}: $REMOTE_URL"
    else
        echo -e "  ${YELLOW}No remote${NC}: Add with 'git remote add origin <url>'"
    fi
else
    echo -e "  ${YELLOW}Not a git repo${NC}: Run 'git init' first"
fi

echo ""

# Print summary
echo -e "${BLUE}=============================================="
echo "  Bootstrap Summary"
echo "==============================================${NC}"
echo ""

if [ ${#MISSING_FILES[@]} -eq 0 ] && [ "$VALIDATION_PASSED" = true ]; then
    echo -e "${GREEN}Bootstrap completed successfully!${NC}"
    echo ""
    echo "Next steps:"
    echo "  1. Update CODEOWNERS with your GitHub username"
    echo "  2. Push to GitHub"
    echo "  3. Enable GitHub Actions in repository settings"
    echo "  4. Create your first issue using the task template"
    echo ""
    echo "Quick start:"
    echo "  git add ."
    echo "  git commit -m 'feat: Add issue-driven development system'"
    echo "  git push origin main"
    echo ""
    exit 0
else
    echo -e "${RED}Bootstrap completed with issues:${NC}"
    echo ""

    if [ ${#MISSING_FILES[@]} -gt 0 ]; then
        echo "Missing files:"
        for file in "${MISSING_FILES[@]}"; do
            echo "  - $file"
        done
        echo ""
    fi

    if [ "$VALIDATION_PASSED" = false ]; then
        echo "Validation failed - see errors above"
        echo ""
    fi

    exit 1
fi
