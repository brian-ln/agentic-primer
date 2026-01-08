#!/bin/bash
set -euo pipefail

# Query knowledge base for relevant patterns and decisions
# Usage: query-knowledge-base.sh "search query"
# Output: JSON array of matching items

QUERY="${1:?Search query required (usage: query-knowledge-base.sh 'search terms')}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
KB_PATH="${REPO_ROOT}/docs/knowledge"
KB_INDEX="${KB_PATH}/index.json"

# Color codes
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

log() { echo -e "${YELLOW}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $*" >&2; }
success() { echo -e "${GREEN}✅ $*${NC}" >&2; }

log "Querying knowledge base for: '$QUERY'"

# Return empty results if KB doesn't exist yet
if [ ! -f "$KB_INDEX" ]; then
    log "Knowledge base not initialized yet"
    echo '{"patterns":[],"decisions":[],"insights":[]}'
    exit 0
fi

# Function to search in JSON using grep (if jq not available)
search_json_simple() {
    local query="$1"
    local file="$2"

    # Extract patterns
    echo "## Relevant Patterns"
    grep -i "$query" "$file" 2>/dev/null | grep '"title"' | head -3 || echo "- No matching patterns"

    echo ""
    echo "## Relevant Decisions"
    grep -i "$query" "$file" 2>/dev/null | grep '"title"' | head -3 || echo "- No matching decisions"
}

# Try to use jq if available
if command -v jq &> /dev/null; then
    log "Using jq for advanced querying"

    # Build query filter from search terms
    # Convert spaces to pipes for OR matching
    query_filter=$(echo "$QUERY" | sed 's/ /|/g')

    # Extract patterns matching query
    patterns=$(jq -r ".patterns.items[]? | select(.title | test(\"$query_filter\"; \"i\") or (.tags | any(. | test(\"$query_filter\"; \"i\"))))" "$KB_INDEX" 2>/dev/null || echo "[]")

    # Extract decisions matching query
    decisions=$(jq -r ".decisions.items[]? | select(.title | test(\"$query_filter\"; \"i\"))" "$KB_INDEX" 2>/dev/null || echo "[]")

    # Extract insights matching query
    insights=$(jq -r ".insights.items[]? | select(.title | test(\"$query_filter\"; \"i\"))" "$KB_INDEX" 2>/dev/null || echo "[]")

    # Count results
    pattern_count=$(echo "$patterns" | grep -c '"id"' || echo "0")
    decision_count=$(echo "$decisions" | grep -c '"id"' || echo "0")
    insight_count=$(echo "$insights" | grep -c '"id"' || echo "0")

    success "Found $pattern_count patterns, $decision_count decisions, $insight_count insights"

    # Output as JSON
    cat <<EOF
{
  "query": "$QUERY",
  "patterns": $([[ "$pattern_count" -gt 0 ]] && echo "$patterns" | jq -s . || echo "[]"),
  "decisions": $([[ "$decision_count" -gt 0 ]] && echo "$decisions" | jq -s . || echo "[]"),
  "insights": $([[ "$insight_count" -gt 0 ]] && echo "$insights" | jq -s . || echo "[]")
}
EOF

else
    log "jq not available, using grep for basic search"

    # Simple grep-based search
    echo "{"
    echo '  "query": "'$QUERY'",'
    echo '  "patterns": ['

    # Search patterns
    pattern_found=false
    if [ -d "$KB_PATH/patterns" ]; then
        find "$KB_PATH/patterns" -name "*.md" -type f | while read -r file; do
            if grep -qi "$QUERY" "$file" 2>/dev/null; then
                filename=$(basename "$file" .md)
                echo '    {"id": "'$filename'", "file": "'$file'"}'
                pattern_found=true
            fi
        done
    fi
    [[ "$pattern_found" = false ]] && echo ""

    echo '  ],'
    echo '  "decisions": ['

    # Search decisions
    decision_found=false
    if [ -d "$KB_PATH/decisions" ]; then
        find "$KB_PATH/decisions" -name "*.md" -type f | while read -r file; do
            if grep -qi "$QUERY" "$file" 2>/dev/null; then
                filename=$(basename "$file" .md)
                echo '    {"id": "'$filename'", "file": "'$file'"}'
                decision_found=true
            fi
        done
    fi
    [[ "$decision_found" = false ]] && echo ""

    echo '  ]'
    echo "}"
fi

log "Knowledge base query complete"
exit 0
