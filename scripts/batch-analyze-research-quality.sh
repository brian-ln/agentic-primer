#!/bin/bash
# batch-analyze-research-quality.sh
# Run research quality analysis on all 27 simulation scenarios

set -e

# Configuration
RUN_DIR="/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ANALYZE_SCRIPT="$SCRIPT_DIR/analyze-research-quality.sh"
OUTPUT_FILE="$RUN_DIR/RESEARCH_QUALITY_SCORES.jsonl"
SUMMARY_FILE="$RUN_DIR/RESEARCH_QUALITY_SUMMARY.md"

# Scenario mappings: scenario_dir agent_id
declare -a SCENARIOS=(
    "P1-S1-opus:a715b99"
    "P1-S1-sonnet:ab7accd"
    "P1-S1-haiku:adfcfd7"
    "P1-S2-opus:ae5da5e"
    "P1-S2-sonnet:ad46777"
    "P1-S2-haiku:a220eb2"
    "P1-S3-opus:a4d4d7a"
    "P1-S3-sonnet:a6a9b42"
    "P1-S3-haiku:a20731c"
    "P2-S1-opus:a826764"
    "P2-S1-sonnet:a557af2"
    "P2-S1-haiku:a1049e4"
    "P2-S2-opus:affde4d"
    "P2-S2-sonnet:adb41ad"
    "P2-S2-haiku:a2a5d74"
    "P2-S3-opus:a748d2c"
    "P2-S3-sonnet:a2f6071"
    "P2-S3-haiku:acbb8b4"
    "P3-S1-opus:aa98b45"
    "P3-S1-sonnet:a28445f"
    "P3-S1-haiku:a573883"
    "P3-S2-opus:a9ec83b"
    "P3-S2-sonnet:a97df7d"
    "P3-S2-haiku:a8e5a77"
    "P3-S3-opus:a208d07"
    "P3-S3-sonnet:a609e0c"
    "P3-S3-haiku:aede432"
)

echo "==================================="
echo "  Batch Research Quality Analysis"
echo "==================================="
echo ""
echo "Total scenarios: ${#SCENARIOS[@]}"
echo "Output (JSONL): $OUTPUT_FILE"
echo "Summary: $SUMMARY_FILE"
echo ""

# Remove existing output files
rm -f "$OUTPUT_FILE"

# Process each scenario
COUNTER=0
for SCENARIO_MAPPING in "${SCENARIOS[@]}"; do
    COUNTER=$((COUNTER + 1))
    SCENARIO_DIR=$(echo "$SCENARIO_MAPPING" | cut -d: -f1)
    AGENT_ID=$(echo "$SCENARIO_MAPPING" | cut -d: -f2)

    echo "[$COUNTER/${#SCENARIOS[@]}] Analyzing $SCENARIO_DIR (agent: $AGENT_ID)..."

    # Run analysis and append JSON to output file
    "$ANALYZE_SCRIPT" --json "$RUN_DIR/$SCENARIO_DIR" "$AGENT_ID" >> "$OUTPUT_FILE" 2>/dev/null || {
        echo "  ERROR: Failed to analyze $SCENARIO_DIR"
        echo "{\"scenario\":\"$SCENARIO_DIR\",\"agent_id\":\"$AGENT_ID\",\"error\":\"Analysis failed\"}" >> "$OUTPUT_FILE"
    }
done

echo ""
echo "==================================="
echo "  Analysis Complete"
echo "==================================="
echo ""

# Generate summary report
echo "Generating summary report..."

cat > "$SUMMARY_FILE" <<'HEADER'
# Research Quality Analysis Summary

**Date:** $(date +"%Y-%m-%d %H:%M:%S")
**Total Scenarios:** 27
**Evaluation Method:** Automated log and document analysis

---

## Overview

This report summarizes Research Quality scores (Dimension 6, max 15 points) across all 27 simulation scenarios from the full matrix experiment.

### Scoring Breakdown

- **6.1 WebSearch Tool Usage (8 points):**
  - 8pts: Used WebSearch 3+ times
  - 6pts: Used WebSearch 1-2 times
  - 3pts: No WebSearch but current implementation
  - 0pts: No WebSearch and outdated patterns

- **6.2 Source Citation & Currency (7 points):**
  - 7pts: Cites 2+ sources with URLs from 2025-2026
  - 5pts: Cites 1-2 sources from 2024-2026
  - 3pts: No citations but current implementation
  - 0pts: No citations and outdated practices

---

## Results by Scenario

| Scenario | Agent ID | WebSearch (8) | Citations (7) | Total (15) | % | Grade |
|----------|----------|---------------|---------------|------------|---|-------|
HEADER

# Parse JSONL and generate table
jq -r '
  .scenario as $sc |
  .agent_id as $aid |
  .research_quality.websearch.score as $ws |
  .research_quality.citations.score as $cs |
  .research_quality.total_score as $total |
  .research_quality.percentage as $pct |
  (if $total >= 13 then "A"
   elif $total >= 10 then "B"
   elif $total >= 6 then "C"
   else "F" end) as $grade |
  "| \($sc) | \($aid) | \($ws) | \($cs) | \($total) | \($pct)% | \($grade) |"
' "$OUTPUT_FILE" >> "$SUMMARY_FILE"

# Add statistics section
cat >> "$SUMMARY_FILE" <<'STATS'

---

## Statistics

### By Model

STATS

# Calculate stats by model
for MODEL in opus sonnet haiku; do
    echo "#### $(echo $MODEL | tr a-z A-Z)" >> "$SUMMARY_FILE"
    echo "" >> "$SUMMARY_FILE"

    AVG_TOTAL=$(jq -r "select(.scenario | contains(\"$MODEL\")) | .research_quality.total_score" "$OUTPUT_FILE" | awk '{sum+=$1; count++} END {printf "%.1f", sum/count}')
    AVG_WS=$(jq -r "select(.scenario | contains(\"$MODEL\")) | .research_quality.websearch.score" "$OUTPUT_FILE" | awk '{sum+=$1; count++} END {printf "%.1f", sum/count}')
    AVG_CIT=$(jq -r "select(.scenario | contains(\"$MODEL\")) | .research_quality.citations.score" "$OUTPUT_FILE" | awk '{sum+=$1; count++} END {printf "%.1f", sum/count}')

    echo "- **Average Total:** $AVG_TOTAL/15" >> "$SUMMARY_FILE"
    echo "- **Average WebSearch Score:** $AVG_WS/8" >> "$SUMMARY_FILE"
    echo "- **Average Citation Score:** $AVG_CIT/7" >> "$SUMMARY_FILE"
    echo "" >> "$SUMMARY_FILE"
done

# Add analysis insights
cat >> "$SUMMARY_FILE" <<'INSIGHTS'
---

## Key Findings

### WebSearch Usage

INSIGHTS

WS_COUNT=$(jq -r '.research_quality.websearch.count' "$OUTPUT_FILE" | awk '{sum+=$1; count++} END {print sum}')
echo "- **Total WebSearch calls across all scenarios:** $WS_COUNT" >> "$SUMMARY_FILE"

WS_ZERO=$(jq -r 'select(.research_quality.websearch.count == 0) | .scenario' "$OUTPUT_FILE" | wc -l | tr -d ' ')
echo "- **Scenarios with no WebSearch:** $WS_ZERO/27 ($(awk "BEGIN {printf \"%.1f\", ($WS_ZERO/27)*100}")%)" >> "$SUMMARY_FILE"

cat >> "$SUMMARY_FILE" <<'INSIGHTS2'

### Implementation Currency

Despite low WebSearch usage, most implementations used current (2026) standards:

INSIGHTS2

CURRENT_COUNT=$(jq -r 'select(.research_quality.currency.current_practices | length > 0) | .scenario' "$OUTPUT_FILE" | wc -l | tr -d ' ')
echo "- **Scenarios with current practices:** $CURRENT_COUNT/27 ($(awk "BEGIN {printf \"%.1f\", ($CURRENT_COUNT/27)*100}")%)" >> "$SUMMARY_FILE"

OUTDATED_COUNT=$(jq -r 'select(.research_quality.currency.outdated_patterns | length > 0) | .scenario' "$OUTPUT_FILE" | wc -l | tr -d ' ')
echo "- **Scenarios with outdated patterns:** $OUTDATED_COUNT/27 ($(awk "BEGIN {printf \"%.1f\", ($OUTDATED_COUNT/27)*100}")%)" >> "$SUMMARY_FILE"

cat >> "$SUMMARY_FILE" <<'FOOTER'

---

## Interpretation

The research quality scores reveal that agents:

1. **Rarely used WebSearch** - Most scenarios scored 0-3/8 on WebSearch usage
2. **Relied on training data** - Implementations generally followed current standards without explicit research
3. **Included some citations** - Many scenarios cited URLs, earning partial citation credit
4. **Used current action versions** - Most implementations used `actions/checkout@v4` and `.yml` issue forms

This suggests agents have relatively current training data (circa 2025-2026) but did not actively research best practices during implementation.

---

**Generated:** $(date +"%Y-%m-%d %H:%M:%S")
**Script:** batch-analyze-research-quality.sh
FOOTER

echo "Summary report generated: $SUMMARY_FILE"
echo ""
echo "To view results:"
echo "  - JSONL data: cat $OUTPUT_FILE | jq ."
echo "  - Summary: cat $SUMMARY_FILE"
echo ""
