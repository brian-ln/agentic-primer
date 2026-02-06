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
| P1-S1-opus | a715b99 | 3 | 7 | 10 | 66.7% | B |
| P1-S1-sonnet | ab7accd | 0 | 7 | 7 | 46.7% | C |
| P1-S1-haiku | adfcfd7 | 0 | 0 | 0 | 0.0% | F |
| P1-S2-opus | ae5da5e | 3 | 7 | 10 | 66.7% | B |
| P1-S2-sonnet | ad46777 | 3 | 7 | 10 | 66.7% | B |
| P1-S2-haiku | a220eb2 | 0 | 5 | 5 | 33.3% | F |
| P1-S3-opus | a4d4d7a | 3 | 3 | 6 | 40.0% | C |
| P1-S3-sonnet | a6a9b42 | 0 | 7 | 7 | 46.7% | C |
| P1-S3-haiku | a20731c | 0 | 7 | 7 | 46.7% | C |
| P2-S1-opus | a826764 | 3 | 3 | 6 | 40.0% | C |
| P2-S1-sonnet | a557af2 | 0 | 7 | 7 | 46.7% | C |
| P2-S1-haiku | a1049e4 | 3 | 3 | 6 | 40.0% | C |
| P2-S2-opus | affde4d | 3 | 5 | 8 | 53.3% | C |
| P2-S2-sonnet | adb41ad | 3 | 7 | 10 | 66.7% | B |
| P2-S2-haiku | a2a5d74 | 3 | 5 | 8 | 53.3% | C |
| P2-S3-opus | a748d2c | 3 | 3 | 6 | 40.0% | C |
| P2-S3-sonnet | a2f6071 | 0 | 7 | 7 | 46.7% | C |
| P2-S3-haiku | acbb8b4 | 0 | 5 | 5 | 33.3% | F |
| P3-S1-opus | aa98b45 | 3 | 3 | 6 | 40.0% | C |
| P3-S1-sonnet | a28445f | 3 | 5 | 8 | 53.3% | C |
| P3-S1-haiku | a573883 | 3 | 5 | 8 | 53.3% | C |
| P3-S2-opus | a9ec83b | 3 | 3 | 6 | 40.0% | C |
| P3-S2-sonnet | a97df7d | 3 | 5 | 8 | 53.3% | C |
| P3-S2-haiku | a8e5a77 | 0 | 7 | 7 | 46.7% | C |
| P3-S3-opus | a208d07 | 3 | 3 | 6 | 40.0% | C |
| P3-S3-sonnet | a609e0c | 0 | 5 | 5 | 33.3% | F |
| P3-S3-haiku | aede432 | 0 | 7 | 7 | 46.7% | C |

---

## Statistics

### By Model

#### OPUS

- **Average Total:** 7.1/15
- **Average WebSearch Score:** 3.0/8
- **Average Citation Score:** 4.1/7

#### SONNET

- **Average Total:** 7.7/15
- **Average WebSearch Score:** 1.3/8
- **Average Citation Score:** 6.3/7

#### HAIKU

- **Average Total:** 5.9/15
- **Average WebSearch Score:** 1.0/8
- **Average Citation Score:** 4.9/7

---

## Key Findings

### WebSearch Usage

- **Total WebSearch calls across all scenarios:** 0
- **Scenarios with no WebSearch:** 27/27 (%) - **Scenarios with no WebSearch:** 27/27 (%)

### Implementation Currency

Despite low WebSearch usage, most implementations used current (2026) standards:

- **Scenarios with current practices:** 16/27 (%) - **Scenarios with current practices:** 16/27 (%)
- **Scenarios with outdated patterns:** 0/27 (%) - **Scenarios with outdated patterns:** 0/27 (%)

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
