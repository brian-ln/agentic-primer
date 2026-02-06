#!/usr/bin/env bash
#
# Knowledge Base Query Script
#
# Queries the knowledge base for relevant context based on input text.
# Returns matching KB entries that can inform implementation.
#
# Usage: knowledge-query.sh <query-text>
#

set -euo pipefail

QUERY_TEXT="${1:-}"

if [ -z "$QUERY_TEXT" ]; then
    echo "Usage: $0 <query-text>"
    exit 1
fi

# --- Configuration ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
KB_DIR="$(cd "$SCRIPT_DIR/../../knowledge-base" && pwd 2>/dev/null || echo "")"

# --- Logging ---
log_debug() {
    # Uncomment for debugging
    # echo "[DEBUG] $*" >&2
    :
}

# --- Query functions ---

extract_keywords() {
    local text="$1"

    # Extract meaningful keywords (3+ characters, lowercase)
    echo "$text" | \
        tr '[:upper:]' '[:lower:]' | \
        grep -oE '\w{3,}' | \
        sort -u
}

search_kb_files() {
    local keywords="$1"

    if [ ! -d "$KB_DIR" ]; then
        log_debug "Knowledge base directory not found: $KB_DIR"
        echo "# Knowledge base not configured"
        return 0
    fi

    log_debug "Searching KB directory: $KB_DIR"

    # Find all YAML files in KB
    local kb_files
    kb_files=$(find "$KB_DIR" -name "*.yml" -o -name "*.yaml" 2>/dev/null || echo "")

    if [ -z "$kb_files" ]; then
        log_debug "No KB files found"
        echo "# Knowledge base is empty"
        return 0
    fi

    # Search for keyword matches
    local results=""

    while IFS= read -r keyword; do
        log_debug "Searching for keyword: $keyword"

        # Search in file contents
        # Note: kb_files is intentionally unquoted to allow word splitting for grep
        local matches
        # shellcheck disable=SC2086
        matches=$(grep -il "$keyword" $kb_files 2>/dev/null || echo "")

        if [ -n "$matches" ]; then
            results="$results"$'\n'"$matches"
        fi
    done <<< "$keywords"

    # Deduplicate and return file paths
    echo "$results" | sort -u | grep -v '^$' || echo ""
}

extract_relevant_sections() {
    local file="$1"
    local keywords="$2"

    if [ ! -f "$file" ]; then
        return
    fi

    # Extract relevant sections from YAML file
    # For simplicity, return the entire file if it matches
    # Production would use yq to extract specific sections

    echo "## From: $(basename "$file")"
    echo ""

    # If yq is available, use it to extract specific fields
    if command -v yq &> /dev/null; then
        # Try to extract description and patterns
        yq eval '.description // ""' "$file" 2>/dev/null || cat "$file"
    else
        # Fallback: return first 20 lines
        head -20 "$file"
    fi

    echo ""
    echo "---"
    echo ""
}

# --- Main query logic ---

log_debug "Querying KB with: $QUERY_TEXT"

# Extract keywords from query
KEYWORDS=$(extract_keywords "$QUERY_TEXT")
log_debug "Extracted keywords: $(echo "$KEYWORDS" | tr '\n' ' ')"

# Search KB files
MATCHING_FILES=$(search_kb_files "$KEYWORDS")

if [ -z "$MATCHING_FILES" ]; then
    echo "# No relevant knowledge base entries found"
    echo ""
    echo "Query: $QUERY_TEXT"
    echo ""
    echo "Suggestion: Proceed with implementation based on issue description."
    exit 0
fi

log_debug "Found matching files: $(echo "$MATCHING_FILES" | wc -l)"

# Extract and format relevant sections
echo "# Knowledge Base Context"
echo ""
echo "Relevant entries for query: $QUERY_TEXT"
echo ""

while IFS= read -r file; do
    if [ -n "$file" ]; then
        extract_relevant_sections "$file" "$KEYWORDS"
    fi
done <<< "$MATCHING_FILES"

exit 0
