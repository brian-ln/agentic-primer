#!/bin/bash
set -euo pipefail

# Process completed issue: extract learnings and update knowledge base
# Usage: process-completed-issue.sh <issue-id> <pr-number> [learnings-file]

ISSUE_ID="${1:?Issue ID required (usage: process-completed-issue.sh <issue-id> <pr-number>)}"
PR_NUMBER="${2:?PR number required}"
LEARNINGS_FILE="${3:-}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
KB_PATH="${REPO_ROOT}/docs/knowledge"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log() { echo -e "${YELLOW}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $*"; }
error() { echo -e "${RED}ERROR: $*${NC}" >&2; return 1; }
success() { echo -e "${GREEN}✅ $*${NC}"; }
info() { echo -e "${BLUE}ℹ️ $*${NC}"; }

log "Processing completion of issue #$ISSUE_ID (PR #$PR_NUMBER)..."

# Create logs directory if needed
mkdir -p "${REPO_ROOT}/logs/completed-issues"
mkdir -p "${REPO_ROOT}/logs/metrics"

# Create completion log entry
COMPLETION_LOG="${REPO_ROOT}/logs/completed-issues/completions.jsonl"
cat >> "$COMPLETION_LOG" <<EOF
{"issue_id": $ISSUE_ID, "pr_number": $PR_NUMBER, "completed_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)", "status": "processed", "learnings_extracted": $([[ -n "$LEARNINGS_FILE" ]] && echo "true" || echo "false")}
EOF

success "Logged completion for issue #$ISSUE_ID"

# Create metrics entry
METRICS_LOG="${REPO_ROOT}/logs/metrics/issue-completion.jsonl"
cat >> "$METRICS_LOG" <<EOF
{"event": "issue_completed", "issue_id": $ISSUE_ID, "pr_number": $PR_NUMBER, "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"}
EOF

success "Recorded completion metrics"

# Update knowledge base index if it exists
if [ -f "${KB_PATH}/index.json" ]; then
    info "Updating knowledge base index timestamp..."

    # Backup original
    cp "${KB_PATH}/index.json" "${KB_PATH}/index.json.bak"

    # Update timestamp
    if command -v jq &> /dev/null; then
        jq '.last_updated = "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'"' "${KB_PATH}/index.json.bak" > "${KB_PATH}/index.json"
        success "Updated knowledge base index timestamp"
    else
        # Simple sed-based update if jq not available
        sed -i.bak2 's/"last_updated": "[^"]*"/"last_updated": "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'"/g' "${KB_PATH}/index.json"
        success "Updated knowledge base timestamp (sed mode)"
    fi
else
    info "Knowledge base index not found (OK if first issue)"
fi

# Extract learnings if provided
if [ -n "$LEARNINGS_FILE" ] && [ -f "$LEARNINGS_FILE" ]; then
    info "Processing extracted learnings..."

    # Create insight entry from learnings
    INSIGHT_ID="insight-$(date +%s)"
    INSIGHT_FILE="${KB_PATH}/insights/${INSIGHT_ID}.md"

    # Simple copy with metadata prepended
    cat > "$INSIGHT_FILE" <<EOF
# Insight from Issue #$ISSUE_ID

**Issue:** #$ISSUE_ID
**PR:** #$PR_NUMBER
**Extracted:** $(date -u +%Y-%m-%dT%H:%M:%SZ)

---

$(cat "$LEARNINGS_FILE")

---

**Status:** Ready for review and integration into knowledge base
EOF

    success "Created insight entry: $INSIGHT_ID"
    log "Insight saved to: $INSIGHT_FILE"
fi

# Create summary log
SUMMARY_FILE="${REPO_ROOT}/logs/completed-issues/issue-${ISSUE_ID}-summary.json"
cat > "$SUMMARY_FILE" <<EOF
{
  "issue_id": $ISSUE_ID,
  "pr_number": $PR_NUMBER,
  "status": "completed",
  "completed_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "logs": {
    "completion_logged": true,
    "metrics_recorded": true,
    "knowledge_base_updated": $([ -f "${KB_PATH}/index.json" ] && echo "true" || echo "false"),
    "insights_extracted": $([ -n "$LEARNINGS_FILE" ] && [ -f "$LEARNINGS_FILE" ] && echo "true" || echo "false")
  },
  "processing_duration_seconds": $SECONDS
}
EOF

success "Completion summary saved"

# Print summary
log ""
log "╔════════════════════════════════════════════════════════════════╗"
log "║         Issue Completion Processing Summary                    ║"
log "╠════════════════════════════════════════════════════════════════╣"
log "║ Issue #$ISSUE_ID → PR #$PR_NUMBER"
log "║"
log "║ ✅ Completion logged"
log "║ ✅ Metrics recorded"
log "║ ✅ Knowledge base updated"
log "║"
log "║ Files:"
log "║   - $COMPLETION_LOG"
log "║   - $SUMMARY_FILE"
log "║"
log "║ Processing Time: ${SECONDS}s"
log "╚════════════════════════════════════════════════════════════════╝"
log ""

success "Issue #$ISSUE_ID processing complete"
exit 0
