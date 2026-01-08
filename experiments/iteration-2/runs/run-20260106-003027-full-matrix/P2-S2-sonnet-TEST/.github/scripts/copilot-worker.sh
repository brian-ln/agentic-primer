#!/usr/bin/env bash
#
# Copilot Worker Script
#
# This script is the core implementation logic for @copilot issue processing.
# It handles: validation, KB querying, branch creation, implementation, and PR opening.
#
# Usage: copilot-worker.sh --issue-number <num> --repo <owner/repo> --kb-context-file <file>
#

set -euo pipefail

# --- Configuration ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
OUTPUT_DIR="${REPO_ROOT}/worker-output"
# KB_DIR exported for potential use by external scripts
export KB_DIR="${REPO_ROOT}/knowledge-base"

# --- Color codes for output ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# --- Logging functions ---
log() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

# --- Parse arguments ---
ISSUE_NUMBER=""
REPO=""
KB_CONTEXT_FILE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --issue-number)
            ISSUE_NUMBER="$2"
            shift 2
            ;;
        --repo)
            REPO="$2"
            shift 2
            ;;
        --kb-context-file)
            KB_CONTEXT_FILE="$2"
            shift 2
            ;;
        *)
            error "Unknown argument: $1"
            exit 1
            ;;
    esac
done

# --- Validate required arguments ---
if [ -z "$ISSUE_NUMBER" ] || [ -z "$REPO" ]; then
    error "Missing required arguments"
    echo "Usage: $0 --issue-number <num> --repo <owner/repo> --kb-context-file <file>"
    exit 1
fi

log "Starting copilot worker for issue #${ISSUE_NUMBER} in ${REPO}"

# --- Create output directory ---
mkdir -p "$OUTPUT_DIR"

# --- Fetch issue details ---
log "Fetching issue details..."

# SIMULATION: In production, this would use gh CLI to fetch real issue data
# For now, we create a simulated issue structure
ISSUE_TITLE="Sample Issue Title"
ISSUE_BODY="Sample issue body with description and acceptance criteria"
# Note: ISSUE_LABELS would be used in production for KB context selection
# shellcheck disable=SC2034
ISSUE_LABELS="copilot,enhancement"

# In production:
# ISSUE_DATA=$(gh issue view "$ISSUE_NUMBER" --repo "$REPO" --json title,body,labels)
# ISSUE_TITLE=$(echo "$ISSUE_DATA" | jq -r '.title')
# ISSUE_BODY=$(echo "$ISSUE_DATA" | jq -r '.body')

log "Issue title: $ISSUE_TITLE"

# --- Parse issue body ---
parse_issue_body() {
    local body="$1"

    # Extract acceptance criteria
    # In a real implementation, this would parse the structured YAML issue body
    log "Parsing issue body for structured data..."

    # SIMULATION: Simple parsing logic
    # Production would use more sophisticated parsing (jq, yq, regex)
    echo "$body" > "$OUTPUT_DIR/issue-body.txt"

    success "Issue body parsed"
}

parse_issue_body "$ISSUE_BODY"

# --- Load knowledge base context ---
load_kb_context() {
    log "Loading knowledge base context..."

    if [ -n "$KB_CONTEXT_FILE" ] && [ -f "$KB_CONTEXT_FILE" ]; then
        KB_CONTEXT=$(cat "$KB_CONTEXT_FILE")
        echo "$KB_CONTEXT" > "$OUTPUT_DIR/kb-context.txt"
        log "KB context loaded: $(wc -l < "$KB_CONTEXT_FILE") lines"
    else
        warn "No KB context file provided, proceeding without KB context"
        echo "" > "$OUTPUT_DIR/kb-context.txt"
    fi
}

load_kb_context

# --- Generate branch name ---
generate_branch_name() {
    local issue_num="$1"
    local title="$2"

    # Slugify title (declare and assign separately per shellcheck SC2155)
    local slug
    slug=$(echo "$title" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g' | sed 's/--*/-/g' | sed 's/^-//' | sed 's/-$//' | cut -c1-50)

    local branch_name="copilot/issue-${issue_num}-${slug}"
    echo "$branch_name"
}

BRANCH_NAME=$(generate_branch_name "$ISSUE_NUMBER" "$ISSUE_TITLE")
log "Branch name: $BRANCH_NAME"
echo "$BRANCH_NAME" > "$OUTPUT_DIR/branch-name.txt"

# --- Create feature branch ---
create_branch() {
    local branch="$1"

    log "Creating feature branch: $branch"

    # Ensure we're on main/master
    MAIN_BRANCH=$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@' || echo "main")
    log "Main branch: $MAIN_BRANCH"

    # SIMULATION: In production, this would create and checkout a real branch
    # git checkout "$MAIN_BRANCH"
    # git pull origin "$MAIN_BRANCH"
    # git checkout -b "$branch"

    log "Branch would be created from $MAIN_BRANCH"
    success "Branch created (simulated)"
}

create_branch "$BRANCH_NAME"

# --- Generate implementation plan ---
generate_plan() {
    log "Generating implementation plan..."

    # SIMULATION: In production, this would call an LLM API to generate a plan
    # based on the issue description and KB context

    # Example LLM prompt structure:
    cat > "$OUTPUT_DIR/implementation-plan.txt" <<EOF
Implementation Plan for Issue #${ISSUE_NUMBER}

Issue: ${ISSUE_TITLE}

Based on:
- Issue description
- Acceptance criteria
- Knowledge base context
- Codebase structure

Steps:
1. Identify files to create/modify
2. Generate code changes
3. Add/update tests
4. Update documentation
5. Run validation

SIMULATION NOTE: In production, an LLM (Claude, GPT-4, etc.) would generate
a detailed, context-aware implementation plan here.
EOF

    log "Implementation plan generated"
}

generate_plan

# --- Implement changes ---
implement_changes() {
    log "Implementing changes..."

    # SIMULATION: In production, this would:
    # 1. Call LLM API to generate code
    # 2. Create/modify files based on LLM output
    # 3. Validate syntax
    # 4. Run tests (if available)

    # For simulation, we create placeholder files demonstrating the structure

    # Example: Create a sample implementation file
    local impl_dir="${REPO_ROOT}/src"
    mkdir -p "$impl_dir"

    cat > "${impl_dir}/copilot-generated-example.txt" <<EOF
# This is a placeholder file created by copilot-worker simulation

# In production, this would contain:
# - Real code implementing the issue requirements
# - Generated by an LLM based on issue context
# - Following project conventions from KB
# - With appropriate tests and documentation

Issue #${ISSUE_NUMBER}: ${ISSUE_TITLE}

Implementation notes:
- This demonstrates the file structure
- Real implementation would be fully functional code
- Would respect coding standards from KB
- Would include appropriate error handling
EOF

    success "Changes implemented (simulated)"

    # Log what was changed
    echo "src/copilot-generated-example.txt" > "$OUTPUT_DIR/changed-files.txt"
}

implement_changes

# --- Validate changes ---
validate_changes() {
    log "Validating changes..."

    # Check syntax (example for different languages)
    # This would be customized based on project type

    local validation_passed=true

    # SIMULATION: In production, this would run actual validation
    # Examples:
    # - Python: python -m py_compile file.py
    # - JavaScript: eslint file.js
    # - TypeScript: tsc --noEmit
    # - Shell: shellcheck script.sh

    if [ "$validation_passed" = true ]; then
        success "Validation passed"
        echo "passed" > "$OUTPUT_DIR/validation-status.txt"
    else
        error "Validation failed"
        echo "failed" > "$OUTPUT_DIR/validation-status.txt"
        return 1
    fi
}

validate_changes

# --- Commit changes ---
commit_changes() {
    log "Committing changes..."

    # Generate commit message
    local commit_msg
    commit_msg=$(cat <<EOF
Implement: ${ISSUE_TITLE}

This commit implements the requirements from issue #${ISSUE_NUMBER}.

Acceptance criteria addressed:
- All criteria from issue description

Changes:
$(cat "$OUTPUT_DIR/changed-files.txt")

Co-authored-by: GitHub Copilot <noreply@github.com>
EOF
)

    # SIMULATION: In production, this would commit to the actual branch
    # git add -A
    # git commit -m "$commit_msg"

    echo "$commit_msg" > "$OUTPUT_DIR/commit-message.txt"
    success "Changes committed (simulated)"
}

commit_changes

# --- Push branch ---
push_branch() {
    local branch="$1"

    log "Pushing branch to remote..."

    # SIMULATION: In production, this would push to GitHub
    # git push -u origin "$branch"

    success "Branch pushed (simulated): $branch"
}

push_branch "$BRANCH_NAME"

# --- Open draft PR ---
open_pr() {
    local branch="$1"
    local issue_num="$2"

    log "Opening draft PR..."

    # Generate PR title and body
    local pr_title="[Copilot] ${ISSUE_TITLE}"
    local pr_body
    pr_body=$(cat <<EOF
## Implementation for Issue #${issue_num}

This PR was automatically generated by @copilot to implement the requirements from issue #${issue_num}.

### Changes

$(cat "$OUTPUT_DIR/changed-files.txt")

### Testing

- [ ] Manual testing completed
- [ ] All acceptance criteria verified
- [ ] Code review completed

### Knowledge Base Context Used

$(cat "$OUTPUT_DIR/kb-context.txt" | head -20)

---

**Ready for review:** Please review the implementation and convert this from draft when satisfied.

Closes #${issue_num}
EOF
)

    # SIMULATION: In production, this would use gh CLI to create a real PR
    # PR_URL=$(gh pr create \
    #     --repo "$REPO" \
    #     --head "$branch" \
    #     --title "$pr_title" \
    #     --body "$pr_body" \
    #     --draft)

    # For simulation, generate a fake PR number
    local pr_number=$((ISSUE_NUMBER + 1))

    echo "$pr_number" > "$OUTPUT_DIR/pr-number.txt"
    echo "$pr_title" > "$OUTPUT_DIR/pr-title.txt"
    echo "$pr_body" > "$OUTPUT_DIR/pr-body.txt"

    success "Draft PR created (simulated): #${pr_number}"
    log "PR would link to issue #${issue_num}"
}

open_pr "$BRANCH_NAME" "$ISSUE_NUMBER"

# --- Write status ---
echo "success" > "$OUTPUT_DIR/status.txt"

# --- Summary ---
success "Copilot worker completed successfully!"
log "Summary:"
log "  Issue: #${ISSUE_NUMBER}"
log "  Branch: ${BRANCH_NAME}"
log "  PR: #$(cat "$OUTPUT_DIR/pr-number.txt")"
log "  Changed files: $(wc -l < "$OUTPUT_DIR/changed-files.txt")"

exit 0
