#!/bin/bash
#
# Script: query-knowledge-base.sh
# Purpose: Search knowledge base for patterns/decisions/insights matching issue topic
# Usage: ./scripts/query-knowledge-base.sh "<search-query>"
# Output: JSON array of matching knowledge base entries
#

set -euo pipefail

SEARCH_QUERY="${1:-}"
KB_INDEX="docs/knowledge/index.json"
LOG_DIR="${LOG_DIR:-logs}"

# Color codes
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

if [ -z "$SEARCH_QUERY" ]; then
  echo -e "${YELLOW}⚠${NC} No search query provided"
  echo "Usage: $0 '<search-query>'"
  echo "Example: $0 'API authentication endpoints'"
  exit 1
fi

# Check if knowledge base index exists
if [ ! -f "$KB_INDEX" ]; then
  echo -e "${YELLOW}⚠${NC} Knowledge base index not found at $KB_INDEX"
  echo "[]"
  exit 0
fi

# Initialize search results
SEARCH_RESULTS="[]"
mkdir -p "$LOG_DIR"

echo -e "${GREEN}🔍${NC} Searching knowledge base for: '$SEARCH_QUERY'" >&2

# Extract keywords from search query
KEYWORDS=$(echo "$SEARCH_QUERY" | tr '[:upper:]' '[:lower:]' | tr ' ' '\n')

# Search through index using jq
if command -v jq &> /dev/null; then
  # Extract all entries and filter by relevance
  SEARCH_RESULTS=$(jq --arg query "$SEARCH_QUERY" '
    def keyword_match(kw):
      [.patterns, .decisions, .insights]
      | map(select(
          (.title | ascii_downcase | contains(kw)) or
          (.topics[]? | ascii_downcase | contains(kw)) or
          (.keywords[]? | ascii_downcase | contains(kw))
        ))
      | .[];

    def score_match(entry):
      def count_matches:
        0
        | if ($query | ascii_downcase) == (entry.title | ascii_downcase) then . + 5 else . end
        | if ($query | ascii_downcase) | test(entry.title | ascii_downcase) then . + 3 else . end
        | if (entry.topics[]? | ascii_downcase | test($query | ascii_downcase)) then . + 2 else . end
        | if (entry.keywords[]? | ascii_downcase | test($query | ascii_downcase)) then . + 2 else . end;
      count_matches;

    [.patterns, .decisions, .insights]
    | flatten
    | map(. + {match_score: (score_match(.))})
    | select(.[].match_score > 0)
    | sort_by(.match_score) | reverse
    | .[0:5]
  ' "$KB_INDEX" 2>/dev/null || echo "[]")
else
  # Fallback: basic grep-based search
  echo "⚠️ jq not available, using basic search" >&2

  # Extract patterns matching query
  SEARCH_RESULTS=$(
    jq '[.patterns[]?, .decisions[]?, .insights[]?] | map(select(
      (.title | contains("'"$SEARCH_QUERY"'")) or
      (.file | contains("'"$SEARCH_QUERY"'"))
    ))' "$KB_INDEX"
  )
fi

# Log search results
SEARCH_LOG="${LOG_DIR}/knowledge-search-$(date +%s).json"
cat > "$SEARCH_LOG" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "search_query": "$SEARCH_QUERY",
  "results_count": $(echo "$SEARCH_RESULTS" | jq 'length'),
  "results": $SEARCH_RESULTS
}
EOF

# Output results for downstream processing
echo "$SEARCH_RESULTS"

# Print human-readable summary to stderr
RESULT_COUNT=$(echo "$SEARCH_RESULTS" | jq 'length')
if [ "$RESULT_COUNT" -gt 0 ]; then
  echo -e "${GREEN}✓${NC} Found $RESULT_COUNT matching knowledge base entries:" >&2
  echo "$SEARCH_RESULTS" | jq -r '.[] | "  - \(.title) (\(.file))"' >&2
else
  echo -e "${YELLOW}⚠${NC} No matching entries found in knowledge base" >&2
fi

exit 0
