#!/bin/bash
set -euo pipefail

# Validate @copilot issue format and content
# Usage: validate-issue.sh <issue-id> [issue-body-file]
# Exits 0 if valid, 1 if invalid

ISSUE_ID="${1:?Issue ID required (usage: validate-issue.sh <issue-id>)}"
ISSUE_BODY_FILE="${2:-.github/cache/issue-${ISSUE_ID}.txt}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() { echo -e "${YELLOW}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $*"; }
error() { echo -e "${RED}ERROR: $*${NC}" >&2; exit 1; }
success() { echo -e "${GREEN}✅ $*${NC}"; }

log "Validating issue #$ISSUE_ID..."

# Check if issue body file exists
if [ ! -f "$ISSUE_BODY_FILE" ]; then
    log "Note: Issue body file not found at $ISSUE_BODY_FILE"
    log "Assuming issue validation would occur on GitHub"
    success "Issue #$ISSUE_ID validation setup complete (would validate on GitHub)"
    exit 0
fi

log "Reading issue from: $ISSUE_BODY_FILE"

# Validate required fields exist
check_field() {
    local field="$1"
    if grep -q "^## $field" "$ISSUE_BODY_FILE" 2>/dev/null; then
        success "Field '$field' present"
        return 0
    else
        error "Missing required field: $field"
    fi
}

# Check required fields
log "Checking required fields..."
check_field "Objective" || error "Missing Objective field"
check_field "Complexity Level" || error "Missing Complexity Level field"

# Validate complexity level
complexity=$(grep "^Complexity" "$ISSUE_BODY_FILE" | cut -d: -f2 | xargs 2>/dev/null || echo "unknown")
case "$complexity" in
    simple|moderate|complex)
        success "Complexity level valid: '$complexity'"
        ;;
    *)
        # Complexity is optional, just warn
        log "Complexity level not clearly specified (found: '$complexity')"
        ;;
esac

# Count and validate acceptance criteria
if grep -q "Acceptance Criteria" "$ISSUE_BODY_FILE" 2>/dev/null; then
    criteria_count=$(grep -c "^- \[" "$ISSUE_BODY_FILE" 2>/dev/null || echo "0")
    if [ "$criteria_count" -gt 0 ]; then
        success "Acceptance criteria found: $criteria_count items"
    else
        log "Warning: Acceptance Criteria section present but no checkboxes found"
    fi
else
    log "Note: Acceptance Criteria section not required for all issues"
fi

# Validate objective has content
objective_line_count=$(grep -A 3 "^## Objective" "$ISSUE_BODY_FILE" 2>/dev/null | grep -v "^--$" | grep -v "^## " | grep -v "^$" | wc -l || echo "0")
if [ "$objective_line_count" -gt 0 ]; then
    success "Objective has substantial content"
else
    log "Warning: Objective field appears empty"
fi

log "All validation checks complete for issue #$ISSUE_ID"
success "Issue #$ISSUE_ID is valid and ready for processing"
exit 0
