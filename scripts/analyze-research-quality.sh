#!/bin/bash
# analyze-research-quality.sh
# Deterministic script to score Research Quality (15 points) from agent logs and generated docs
# Part of the Enhanced 120-Point Evaluation Rubric (Dimension 6)

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Usage function
usage() {
    cat <<EOF
Usage: $0 [OPTIONS] <scenario-dir> <agent-id>

Analyzes agent logs and generated documentation to score Research Quality (15 points).

Arguments:
  scenario-dir    Path to scenario directory (e.g., P1-S1-opus)
  agent-id        Agent task ID (e.g., a715b99)

Options:
  -h, --help      Show this help message
  -v, --verbose   Show detailed analysis
  -j, --json      Output results in JSON format

Examples:
  $0 experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S1-opus a715b99
  $0 -v P2-S2-sonnet a826764
  $0 --json P3-S3-haiku aa98b45

Research Quality Scoring (15 points total):
  6.1 WebSearch Tool Usage (8 points)
      - 8pts: Used WebSearch 3+ times for critical components
      - 6pts: Used WebSearch 1-2 times
      - 3pts: No WebSearch but implementation is current (2026 standards)
      - 0pts: No WebSearch AND outdated implementation

  6.2 Source Citation & Currency (7 points)
      - 7pts: Cites 2+ sources from 2025-2026 with URLs
      - 5pts: Cites 1-2 sources from 2024-2026, no URLs
      - 3pts: No citations but implementation is current
      - 0pts: No citations AND outdated practices

Agent logs expected at: /tmp/claude/-Users-bln-play-agentic-primer/tasks/<agent-id>.output
EOF
}

# Parse arguments
VERBOSE=false
JSON_OUTPUT=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -j|--json)
            JSON_OUTPUT=true
            shift
            ;;
        -*)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
        *)
            break
            ;;
    esac
done

if [ $# -lt 2 ]; then
    echo -e "${RED}Error: Missing required arguments${NC}"
    usage
    exit 1
fi

SCENARIO_DIR="$1"
AGENT_ID="$2"
AGENT_LOG="/tmp/claude/-Users-bln-play-agentic-primer/tasks/${AGENT_ID}.output"

# Validate inputs
if [ ! -d "$SCENARIO_DIR" ]; then
    echo -e "${RED}Error: Scenario directory not found: $SCENARIO_DIR${NC}"
    exit 1
fi

if [ ! -f "$AGENT_LOG" ]; then
    echo -e "${RED}Error: Agent log not found: $AGENT_LOG${NC}"
    exit 1
fi

# Extract scenario name from path
SCENARIO_NAME=$(basename "$SCENARIO_DIR")

# Initialize scoring variables
WEBSEARCH_SCORE=0
CITATION_SCORE=0
TOTAL_SCORE=0

# Analysis data
WEBSEARCH_COUNT=0
WEBSEARCH_QUERIES=()
CITATION_COUNT=0
CITATION_URLS=()
YEAR_REFERENCES=()
ACTION_VERSIONS=()
CURRENT_PRACTICES=()
OUTDATED_PATTERNS=()

# ============================================================================
# 6.1 WEBSEARCH TOOL USAGE (8 points)
# ============================================================================

if [ "$VERBOSE" = true ]; then
    echo -e "${BLUE}=== Analyzing WebSearch Tool Usage ===${NC}"
fi

# Count WebSearch and WebFetch calls in agent log
WEBSEARCH_COUNT=$(grep "WebSearch\|WebFetch" "$AGENT_LOG" 2>/dev/null | wc -l | tr -d ' \n')
if [ -z "$WEBSEARCH_COUNT" ] || [ "$WEBSEARCH_COUNT" = "" ]; then
    WEBSEARCH_COUNT=0
fi

# Extract WebSearch queries (look for query parameters in tool calls)
WEBSEARCH_QUERIES=()
while IFS= read -r line; do
    WEBSEARCH_QUERIES+=("$line")
done < <(grep -o '"query":"[^"]*"' "$AGENT_LOG" 2>/dev/null | cut -d'"' -f4 || true)

if [ "$VERBOSE" = true ]; then
    echo "  WebSearch/WebFetch calls found: $WEBSEARCH_COUNT"
    if [ ${#WEBSEARCH_QUERIES[@]} -gt 0 ]; then
        echo "  Sample queries:"
        for i in "${!WEBSEARCH_QUERIES[@]}"; do
            if [ $i -lt 3 ]; then
                echo "    - ${WEBSEARCH_QUERIES[$i]}"
            fi
        done
    fi
fi

# ============================================================================
# Check implementation currency (2026 standards)
# ============================================================================

if [ "$VERBOSE" = true ]; then
    echo -e "${BLUE}=== Checking Implementation Currency ===${NC}"
fi

# Check for current GitHub Actions versions
if [ -d "$SCENARIO_DIR/.github/workflows" ]; then
    ACTION_VERSIONS=()
    while IFS= read -r line; do
        ACTION_VERSIONS+=("$line")
    done < <(grep -h "uses:.*@v" "$SCENARIO_DIR/.github/workflows/"*.yml 2>/dev/null | sed 's/.*uses: //' | sort -u || true)
    if [ "$VERBOSE" = true ] && [ ${#ACTION_VERSIONS[@]} -gt 0 ]; then
        echo "  Action versions found:"
        for version in "${ACTION_VERSIONS[@]}"; do
            echo "    - $version"
        done
    fi
fi

# Check for outdated patterns
# Pattern 1: Old action versions (v1, v2)
if grep -qE "actions/checkout@v[12]|actions/setup-node@v[12]" "$SCENARIO_DIR/.github/workflows/"*.yml 2>/dev/null; then
    OUTDATED_PATTERNS+=("outdated_action_versions")
    if [ "$VERBOSE" = true ]; then
        echo -e "  ${YELLOW}⚠ Found outdated action versions (v1 or v2)${NC}"
    fi
fi

# Pattern 2: Markdown templates instead of issue forms
if [ -f "$SCENARIO_DIR/.github/ISSUE_TEMPLATE/bug_report.md" ] || [ -f "$SCENARIO_DIR/.github/ISSUE_TEMPLATE/feature_request.md" ]; then
    OUTDATED_PATTERNS+=("markdown_templates")
    if [ "$VERBOSE" = true ]; then
        echo -e "  ${YELLOW}⚠ Uses markdown templates (outdated, should use .yml issue forms)${NC}"
    fi
fi

# Pattern 3: Old Node.js versions
if grep -qE "node-version.*['\"]12|node-version.*['\"]14|node-version.*['\"]16" "$SCENARIO_DIR/.github/workflows/"*.yml 2>/dev/null; then
    OUTDATED_PATTERNS+=("old_nodejs_version")
    if [ "$VERBOSE" = true ]; then
        echo -e "  ${YELLOW}⚠ Uses outdated Node.js version (12/14/16)${NC}"
    fi
fi

# Check for current practices
# Practice 1: Uses actions v3+ (preferably v4)
if grep -qE "actions/checkout@v[34]|actions/setup-node@v[34]" "$SCENARIO_DIR/.github/workflows/"*.yml 2>/dev/null; then
    CURRENT_PRACTICES+=("current_action_versions")
    if [ "$VERBOSE" = true ]; then
        echo -e "  ${GREEN}✅ Uses current action versions (v3 or v4)${NC}"
    fi
fi

# Practice 2: Uses .yml issue forms
if [ -d "$SCENARIO_DIR/.github/ISSUE_TEMPLATE" ] && ls "$SCENARIO_DIR/.github/ISSUE_TEMPLATE/"*.yml &>/dev/null; then
    CURRENT_PRACTICES+=("issue_forms")
    if [ "$VERBOSE" = true ]; then
        echo -e "  ${GREEN}✅ Uses .yml issue forms (current standard)${NC}"
    fi
fi

# Practice 3: Uses current Node.js (v18, v20)
if grep -qE "node-version.*['\"]18|node-version.*['\"]20" "$SCENARIO_DIR/.github/workflows/"*.yml 2>/dev/null; then
    CURRENT_PRACTICES+=("current_nodejs")
    if [ "$VERBOSE" = true ]; then
        echo -e "  ${GREEN}✅ Uses current Node.js version (18 or 20 LTS)${NC}"
    fi
fi

# Score WebSearch usage (8 points)
if [ "$WEBSEARCH_COUNT" -ge 3 ]; then
    WEBSEARCH_SCORE=8
    WEBSEARCH_LABEL="EXCELLENT RESEARCH"
elif [ "$WEBSEARCH_COUNT" -ge 1 ]; then
    WEBSEARCH_SCORE=6
    WEBSEARCH_LABEL="GOOD RESEARCH"
elif [ ${#OUTDATED_PATTERNS[@]} -eq 0 ] && [ ${#CURRENT_PRACTICES[@]} -gt 0 ]; then
    WEBSEARCH_SCORE=3
    WEBSEARCH_LABEL="NO RESEARCH BUT CURRENT"
else
    WEBSEARCH_SCORE=0
    WEBSEARCH_LABEL="NO RESEARCH AND OUTDATED"
fi

# ============================================================================
# 6.2 SOURCE CITATION & CURRENCY (7 points)
# ============================================================================

if [ "$VERBOSE" = true ]; then
    echo -e "${BLUE}=== Analyzing Source Citations & Currency ===${NC}"
fi

# Count source citations in documentation
CITATION_COUNT=0
CITATION_URLS=()
while IFS= read -r line; do
    CITATION_URLS+=("$line")
done < <(grep -rhE "https?://[^\s)]+" "$SCENARIO_DIR"/*.md "$SCENARIO_DIR"/docs/**/*.md 2>/dev/null | grep -v "example.com" | head -20 || true)

if [ ${#CITATION_URLS[@]} -gt 0 ]; then
    CITATION_COUNT=${#CITATION_URLS[@]}
    if [ "$VERBOSE" = true ]; then
        echo "  URLs found in documentation: $CITATION_COUNT"
        echo "  Sample URLs:"
        for i in "${!CITATION_URLS[@]}"; do
            if [ $i -lt 3 ]; then
                echo "    - ${CITATION_URLS[$i]}"
            fi
        done
    fi
fi

# Check for explicit source attributions
SOURCE_ATTR_COUNT=$(grep -rhE "source:|per:|according to:|based on:" "$SCENARIO_DIR"/*.md "$SCENARIO_DIR"/docs/**/*.md 2>/dev/null | wc -l | tr -d ' \n' || echo "0")
if [ -z "$SOURCE_ATTR_COUNT" ]; then
    SOURCE_ATTR_COUNT=0
fi

if [ "$VERBOSE" = true ] && [ "$SOURCE_ATTR_COUNT" -gt 0 ]; then
    echo "  Source attributions found: $SOURCE_ATTR_COUNT"
fi

# Check for year references (2024-2026 = current)
YEAR_REFERENCES=()
while IFS= read -r line; do
    YEAR_REFERENCES+=("$line")
done < <(grep -rhE "202[456]" "$SCENARIO_DIR"/*.md "$SCENARIO_DIR"/docs/**/*.md 2>/dev/null | grep -oE "202[456]" | sort -u || true)

if [ "$VERBOSE" = true ] && [ ${#YEAR_REFERENCES[@]} -gt 0 ]; then
    echo "  Year references found: ${YEAR_REFERENCES[*]}"
fi

# Check for official documentation URLs
OFFICIAL_DOCS_COUNT=0
if [ ${#CITATION_URLS[@]} -gt 0 ]; then
    OFFICIAL_DOCS_COUNT=$(printf '%s\n' "${CITATION_URLS[@]}" | grep "docs.github.com\|github.blog\|actions/\|github.com/.*/docs" | wc -l | tr -d ' \n' || echo "0")
    if [ -z "$OFFICIAL_DOCS_COUNT" ]; then
        OFFICIAL_DOCS_COUNT=0
    fi
fi

if [ "$VERBOSE" = true ] && [ "$OFFICIAL_DOCS_COUNT" -gt 0 ]; then
    echo "  Official GitHub documentation URLs: $OFFICIAL_DOCS_COUNT"
fi

# Score citations (7 points)
if [ "$CITATION_COUNT" -ge 2 ] && [ "${#YEAR_REFERENCES[@]}" -gt 0 ] && [ "$OFFICIAL_DOCS_COUNT" -gt 0 ]; then
    CITATION_SCORE=7
    CITATION_LABEL="EXCELLENT CITATIONS"
elif [ "$CITATION_COUNT" -ge 1 ] || [ "$SOURCE_ATTR_COUNT" -gt 0 ]; then
    CITATION_SCORE=5
    CITATION_LABEL="GOOD CITATIONS"
elif [ ${#OUTDATED_PATTERNS[@]} -eq 0 ] && [ ${#CURRENT_PRACTICES[@]} -gt 0 ]; then
    CITATION_SCORE=3
    CITATION_LABEL="IMPLICIT CURRENCY"
else
    CITATION_SCORE=0
    CITATION_LABEL="NO CITATIONS AND STALE"
fi

# ============================================================================
# CALCULATE TOTAL SCORE
# ============================================================================

TOTAL_SCORE=$((WEBSEARCH_SCORE + CITATION_SCORE))

# ============================================================================
# OUTPUT RESULTS
# ============================================================================

if [ "$JSON_OUTPUT" = true ]; then
    # JSON output for automation
    PERCENTAGE=$(awk "BEGIN {printf \"%.1f\", ($TOTAL_SCORE * 100) / 15}")

    # Build JSON arrays safely
    QUERIES_JSON="[]"
    if [ ${#WEBSEARCH_QUERIES[@]} -gt 0 ]; then
        QUERIES_JSON=$(printf '%s\n' "${WEBSEARCH_QUERIES[@]}" | jq -R . | jq -s .)
    fi

    YEARS_JSON="[]"
    if [ ${#YEAR_REFERENCES[@]} -gt 0 ]; then
        YEARS_JSON=$(printf '%s\n' "${YEAR_REFERENCES[@]}" | jq -R . | jq -s .)
    fi

    PRACTICES_JSON="[]"
    if [ ${#CURRENT_PRACTICES[@]} -gt 0 ]; then
        PRACTICES_JSON=$(printf '%s\n' "${CURRENT_PRACTICES[@]}" | jq -R . | jq -s .)
    fi

    OUTDATED_JSON="[]"
    if [ ${#OUTDATED_PATTERNS[@]} -gt 0 ]; then
        OUTDATED_JSON=$(printf '%s\n' "${OUTDATED_PATTERNS[@]}" | jq -R . | jq -s .)
    fi

    cat <<EOF
{
  "scenario": "$SCENARIO_NAME",
  "agent_id": "$AGENT_ID",
  "research_quality": {
    "total_score": $TOTAL_SCORE,
    "max_score": 15,
    "percentage": $PERCENTAGE,
    "websearch": {
      "score": $WEBSEARCH_SCORE,
      "max_score": 8,
      "count": $WEBSEARCH_COUNT,
      "label": "$WEBSEARCH_LABEL",
      "queries": $QUERIES_JSON
    },
    "citations": {
      "score": $CITATION_SCORE,
      "max_score": 7,
      "count": $CITATION_COUNT,
      "label": "$CITATION_LABEL",
      "urls_found": $CITATION_COUNT,
      "source_attributions": $SOURCE_ATTR_COUNT,
      "official_docs_count": $OFFICIAL_DOCS_COUNT,
      "year_references": $YEARS_JSON
    },
    "currency": {
      "current_practices": $PRACTICES_JSON,
      "outdated_patterns": $OUTDATED_JSON
    }
  }
}
EOF
else
    # Human-readable output
    echo ""
    echo "=========================================="
    echo "  RESEARCH QUALITY ANALYSIS"
    echo "=========================================="
    echo ""
    echo "Scenario: $SCENARIO_NAME"
    echo "Agent ID: $AGENT_ID"
    echo ""
    echo "----------------------------------------"
    echo "6.1 WebSearch Tool Usage (8 points)"
    echo "----------------------------------------"
    echo -e "Score: ${GREEN}$WEBSEARCH_SCORE/8${NC} - $WEBSEARCH_LABEL"
    echo "  WebSearch calls: $WEBSEARCH_COUNT"
    if [ ${#WEBSEARCH_QUERIES[@]} -gt 0 ]; then
        echo "  Sample queries:"
        for i in "${!WEBSEARCH_QUERIES[@]}"; do
            if [ $i -lt 3 ]; then
                echo "    $((i+1)). ${WEBSEARCH_QUERIES[$i]}"
            fi
        done
    else
        echo "  No WebSearch queries found in agent log"
    fi
    echo ""
    echo "Implementation Currency:"
    if [ ${#CURRENT_PRACTICES[@]} -gt 0 ]; then
        for practice in "${CURRENT_PRACTICES[@]}"; do
            echo -e "  ${GREEN}✅${NC} $(echo "$practice" | sed 's/_/ /g')"
        done
    fi
    if [ ${#OUTDATED_PATTERNS[@]} -gt 0 ]; then
        for pattern in "${OUTDATED_PATTERNS[@]}"; do
            echo -e "  ${RED}❌${NC} $(echo "$pattern" | sed 's/_/ /g')"
        done
    fi
    if [ ${#CURRENT_PRACTICES[@]} -eq 0 ] && [ ${#OUTDATED_PATTERNS[@]} -eq 0 ]; then
        echo "  No specific patterns detected"
    fi
    echo ""
    echo "----------------------------------------"
    echo "6.2 Source Citation & Currency (7 points)"
    echo "----------------------------------------"
    echo -e "Score: ${GREEN}$CITATION_SCORE/7${NC} - $CITATION_LABEL"
    echo "  URLs in documentation: $CITATION_COUNT"
    echo "  Source attributions: $SOURCE_ATTR_COUNT"
    echo "  Official GitHub docs: $OFFICIAL_DOCS_COUNT"
    if [ ${#YEAR_REFERENCES[@]} -gt 0 ]; then
        echo "  Year references: ${YEAR_REFERENCES[*]}"
    else
        echo "  Year references: None found"
    fi
    if [ $CITATION_COUNT -gt 0 ] && [ "$VERBOSE" = true ]; then
        echo ""
        echo "  Sample citations:"
        for i in "${!CITATION_URLS[@]}"; do
            if [ $i -lt 3 ]; then
                echo "    $((i+1)). ${CITATION_URLS[$i]}"
            fi
        done
    fi
    PERCENTAGE=$(awk "BEGIN {printf \"%.1f\", ($TOTAL_SCORE * 100) / 15}")
    echo ""
    echo "=========================================="
    echo -e "  TOTAL SCORE: ${GREEN}$TOTAL_SCORE/15${NC} ($PERCENTAGE%)"
    echo "=========================================="
    echo ""

    # Interpretation
    if [ "$TOTAL_SCORE" -ge 13 ]; then
        echo -e "${GREEN}✅ EXCELLENT${NC} - Strong research quality, current implementation"
    elif [ "$TOTAL_SCORE" -ge 10 ]; then
        echo -e "${GREEN}✅ GOOD${NC} - Adequate research or current practices"
    elif [ "$TOTAL_SCORE" -ge 6 ]; then
        echo -e "${YELLOW}⚠ ACCEPTABLE${NC} - Some currency but limited research evidence"
    else
        echo -e "${RED}❌ POOR${NC} - Lacks research evidence and/or uses outdated practices"
    fi
    echo ""
fi

# Exit with score as exit code (for automation)
exit 0
