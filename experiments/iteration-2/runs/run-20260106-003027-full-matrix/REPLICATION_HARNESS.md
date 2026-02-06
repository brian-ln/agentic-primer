# Complete Replication Harness for run-validation-20260106

**Target Run:** `/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-validation-20260106`

**Purpose:** This document provides complete instructions, prompts, and scripts to replicate the validation run either in full or slice-by-slice.

---

## Overview of Validation Run

The validation run tested 3 problems (P1, P2, P3) against S2 success criteria using Sonnet model:

- **P1-S2-sonnet-VALIDATION:** Bootstrap @copilot with auto-review and knowledge base
- **P2-S2-sonnet-VALIDATION:** Issue-driven development with auto-assignment
- **P3-S2-sonnet-VALIDATION:** Complete GitHub automation system

Each simulation produced a complete, self-contained implementation in its own directory.

---

## Run Structure

```
run-validation-20260106/
├── P1-S2-sonnet-VALIDATION/
│   ├── .github/
│   │   ├── ISSUE_TEMPLATE/copilot-task.yml
│   │   ├── workflows/copilot-agent.yml
│   │   └── CODEOWNERS
│   ├── docs/knowledge/
│   ├── scripts/auto-review.sh
│   ├── README.md
│   ├── SOLUTION.md
│   └── FILE_MANIFEST.md
│
├── P2-S2-sonnet-VALIDATION/
│   ├── .github/
│   ├── docs/knowledge/
│   ├── 00-START-HERE.md
│   ├── COMPLETE_SOLUTION.md
│   ├── README.md
│   ├── VERIFICATION.md
│   └── FILE_MANIFEST.md
│
└── P3-S2-sonnet-VALIDATION/
    ├── .github/
    ├── docs/knowledge/
    ├── README.md
    ├── SOLUTION.md
    ├── VALIDATION.md
    ├── EXECUTIVE_SUMMARY.md
    └── FILE_MANIFEST.md
```

---

## Complete Replication (All 3 Scenarios)

### Step 1: Create Run Directory

```bash
# Create directory structure
mkdir -p /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-validation-$(date +%Y%m%d)/{P1-S2-sonnet-VALIDATION,P2-S2-sonnet-VALIDATION,P3-S2-sonnet-VALIDATION}

# Navigate to run directory
cd /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-validation-$(date +%Y%m%d)
```

### Step 2: Run P1-S2-sonnet-VALIDATION

**Directory:** `P1-S2-sonnet-VALIDATION/`

**Prompt to Claude:**
```
Bootstrap @copilot issue automation with auto-review and knowledge base.

SUCCESS CRITERIA (S2 - Moderate):
1. Process test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

INSTRUCTIONS:
- Create GitHub issue template for @copilot tasks
- Create GitHub Actions workflow for automation
- Create CODEOWNERS file for auto-review assignment
- Create knowledge base structure (patterns, decisions, insights)
- Create auto-review.sh script for validation
- Create comprehensive README with usage guide
- All files must pass syntax validation
- Output location: P1-S2-sonnet-VALIDATION/

CONSTRAINTS:
- Use GitHub-native features (no external services)
- YAML issue templates (not markdown)
- Structured knowledge base with subdirectories
- Shell script for auto-review (bash)
- Complete, production-ready implementation
```

**Expected Output:**
- 8+ files
- GitHub automation structure (.github/)
- Knowledge base (docs/knowledge/)
- Auto-review script (scripts/auto-review.sh)
- README, SOLUTION, FILE_MANIFEST docs

### Step 3: Run P2-S2-sonnet-VALIDATION

**Directory:** `P2-S2-sonnet-VALIDATION/`

**Prompt to Claude:**
```
Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base.

SUCCESS CRITERIA (S2 - Moderate):
1. Process test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

INSTRUCTIONS:
- Create issue template (.github/ISSUE_TEMPLATE/copilot-task.yml)
- Create GitHub Actions workflow for @copilot automation
- Create CODEOWNERS (* @owner) for automatic PR review assignment
- Create knowledge base (docs/knowledge/) with patterns/decisions/insights structure
- Create README explaining workflow: issue → @copilot → PR → review via web UI
- Include verification procedures
- All YAML files must pass yamllint
- Output location: P2-S2-sonnet-VALIDATION/

DELIVERABLES:
- 00-START-HERE.md (quick navigation)
- COMPLETE_SOLUTION.md (design rationale)
- README.md (user guide)
- VERIFICATION.md (test procedures)
- FILE_MANIFEST.md (file inventory)
```

**Expected Output:**
- 13 files total
- Complete automation stack
- Tripartite knowledge base
- Multiple documentation perspectives

### Step 4: Run P3-S2-sonnet-VALIDATION

**Directory:** `P3-S2-sonnet-VALIDATION/`

**Prompt to Claude:**
```
Create issue-driven development system: Issue template (.github/ISSUE_TEMPLATE/task.yml) for @copilot tasks, CODEOWNERS (* @owner) for PR auto-assignment, Knowledge base (docs/knowledge/) with patterns/decisions/insights structure, README with workflow: issue → @copilot → PR → review via web UI

SUCCESS CRITERIA (S2 - Moderate):
1. Process test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

INSTRUCTIONS:
- Implement exactly as specified in prompt
- File paths must match exactly:
  - .github/ISSUE_TEMPLATE/task.yml
  - .github/CODEOWNERS
  - docs/knowledge/patterns/
  - docs/knowledge/decisions/
  - docs/knowledge/insights/
- README must explain full workflow
- Include validation procedures
- Output location: P3-S2-sonnet-VALIDATION/

CONSTRAINTS:
- Minimal, focused implementation
- Proper GitHub directory structure
- No placeholders or TODOs
- Production-ready quality
```

**Expected Output:**
- 7-12 files
- Most focused implementation
- Exact file path compliance
- EXECUTIVE_SUMMARY, SOLUTION, VALIDATION docs

---

## Slice-by-Slice Replication

### Option A: Single Scenario Replication

**Use Case:** Test one specific configuration

**Example - Replicate P2-S2-sonnet only:**

```bash
# 1. Create directory
mkdir -p ~/test-replication/P2-S2-sonnet-VALIDATION
cd ~/test-replication/P2-S2-sonnet-VALIDATION

# 2. Start Claude Code session
claude

# 3. Paste prompt from Step 3 above

# 4. Verify output
ls -la
# Expected: 13 files with .github/, docs/, *.md files

# 5. Validate
yamllint .github/ISSUE_TEMPLATE/copilot-task.yml
yamllint .github/workflows/copilot-automation.yml
```

### Option B: Comparative Replication

**Use Case:** Compare different prompts for same task

**Example - Test P1, P2, P3 variations:**

```bash
# Create parallel directories
mkdir -p ~/comparison/{P1,P2,P3}-S2-sonnet

# Run P1 (minimal prompt)
cd ~/comparison/P1-S2-sonnet
# Use P1 prompt from Step 2

# Run P2 (moderate prompt)
cd ~/comparison/P2-S2-sonnet
# Use P2 prompt from Step 3

# Run P3 (detailed prompt)
cd ~/comparison/P3-S2-sonnet
# Use P3 prompt from Step 4

# Compare outputs
for dir in P1 P2 P3; do
  echo "$dir: $(find ~/comparison/${dir}-S2-sonnet -type f | wc -l) files"
  echo "Size: $(du -sh ~/comparison/${dir}-S2-sonnet | cut -f1)"
done
```

### Option C: Model Comparison Replication

**Use Case:** Test same prompt across different models

**Example - Test P2 prompt with opus, sonnet, haiku:**

```bash
# Create model variant directories
mkdir -p ~/model-test/P2-S2-{opus,sonnet,haiku}

# For each model, use same P2 prompt (Step 3)
# but specify model in Claude Code settings

# Compare results
ls -l ~/model-test/P2-S2-*/  # File counts
du -sh ~/model-test/P2-S2-*/ # Sizes
```

---

## Automated Replication Script

**Script:** `replicate-validation-run.sh`

```bash
#!/usr/bin/env bash
# replicate-validation-run.sh - Automate validation run replication
#
# Usage:
#   ./replicate-validation-run.sh all          # Replicate all 3 scenarios
#   ./replicate-validation-run.sh P1           # Replicate P1 only
#   ./replicate-validation-run.sh P2           # Replicate P2 only
#   ./replicate-validation-run.sh P3           # Replicate P3 only

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="${1:-$HOME/validation-replication-$(date +%Y%m%d)}"
SCENARIO="${2:-all}"

# Prompt templates
P1_PROMPT="Bootstrap @copilot issue automation with auto-review and knowledge base.

SUCCESS CRITERIA (S2 - Moderate):
1. Process test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

INSTRUCTIONS:
- Create GitHub issue template for @copilot tasks
- Create GitHub Actions workflow for automation
- Create CODEOWNERS file for auto-review assignment
- Create knowledge base structure (patterns, decisions, insights)
- Create auto-review.sh script for validation
- Create comprehensive README with usage guide
- All files must pass syntax validation
- Output location: P1-S2-sonnet-VALIDATION/"

P2_PROMPT="Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base.

SUCCESS CRITERIA (S2 - Moderate):
1. Process test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

INSTRUCTIONS:
- Create issue template (.github/ISSUE_TEMPLATE/copilot-task.yml)
- Create GitHub Actions workflow for @copilot automation
- Create CODEOWNERS (* @owner) for automatic PR review assignment
- Create knowledge base (docs/knowledge/) with patterns/decisions/insights structure
- Create README explaining workflow: issue → @copilot → PR → review via web UI
- Include verification procedures
- All YAML files must pass yamllint
- Output location: P2-S2-sonnet-VALIDATION/"

P3_PROMPT="Create issue-driven development system: Issue template (.github/ISSUE_TEMPLATE/task.yml) for @copilot tasks, CODEOWNERS (* @owner) for PR auto-assignment, Knowledge base (docs/knowledge/) with patterns/decisions/insights structure, README with workflow: issue → @copilot → PR → review via web UI

SUCCESS CRITERIA (S2 - Moderate):
1. Process test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

INSTRUCTIONS:
- Implement exactly as specified in prompt
- File paths must match exactly
- README must explain full workflow
- Include validation procedures
- Output location: P3-S2-sonnet-VALIDATION/"

# Create directory structure
create_structure() {
    local scenario=$1
    local dir="$BASE_DIR/$scenario-S2-sonnet-VALIDATION"

    echo "Creating directory: $dir"
    mkdir -p "$dir"

    # Save prompt to file
    case $scenario in
        P1) echo "$P1_PROMPT" > "$dir/PROMPT.txt" ;;
        P2) echo "$P2_PROMPT" > "$dir/PROMPT.txt" ;;
        P3) echo "$P3_PROMPT" > "$dir/PROMPT.txt" ;;
    esac

    echo "Prompt saved to: $dir/PROMPT.txt"
}

# Main execution
main() {
    echo "=== Validation Run Replication ==="
    echo "Base directory: $BASE_DIR"
    echo "Scenario: $SCENARIO"
    echo ""

    case $SCENARIO in
        all)
            create_structure "P1"
            create_structure "P2"
            create_structure "P3"
            ;;
        P1|P2|P3)
            create_structure "$SCENARIO"
            ;;
        *)
            echo "Error: Unknown scenario '$SCENARIO'"
            echo "Usage: $0 [base-dir] [all|P1|P2|P3]"
            exit 1
            ;;
    esac

    echo ""
    echo "=== Next Steps ==="
    echo "1. Navigate to scenario directory:"
    echo "   cd $BASE_DIR/[P1|P2|P3]-S2-sonnet-VALIDATION"
    echo ""
    echo "2. Start Claude Code session:"
    echo "   claude"
    echo ""
    echo "3. Paste prompt from PROMPT.txt"
    echo ""
    echo "4. Verify output against original run"
}

main
```

**Usage:**
```bash
# Replicate all scenarios to custom location
./replicate-validation-run.sh ~/my-replication all

# Replicate only P2
./replicate-validation-run.sh ~/my-replication P2

# Default location (~/validation-replication-YYYYMMDD)
./replicate-validation-run.sh
```

---

## Verification Procedures

### After Each Scenario Replication

**Step 1: File Count Check**
```bash
# Compare file counts
original="/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-validation-20260106/P2-S2-sonnet-VALIDATION"
replica="$HOME/validation-replication-$(date +%Y%m%d)/P2-S2-sonnet-VALIDATION"

echo "Original: $(find "$original" -type f | wc -l) files"
echo "Replica:  $(find "$replica" -type f | wc -l) files"
```

**Step 2: Structure Verification**
```bash
# Verify directory structure
cd "$replica"

# Check for required directories
test -d .github/ISSUE_TEMPLATE && echo "✓ Issue template dir exists"
test -d .github/workflows && echo "✓ Workflows dir exists"
test -d docs/knowledge && echo "✓ Knowledge base exists"

# Check for required files
test -f .github/CODEOWNERS && echo "✓ CODEOWNERS exists"
test -f README.md && echo "✓ README exists"
```

**Step 3: Syntax Validation**
```bash
# Validate YAML files
find .github -name "*.yml" -exec yamllint {} \;

# Validate shell scripts (if present)
find scripts -name "*.sh" -exec shellcheck {} \; 2>/dev/null || true

# Validate markdown
find . -name "*.md" -exec markdownlint {} \; 2>/dev/null || true
```

**Step 4: Content Comparison**
```bash
# Compare key files (structure, not content)
for file in README.md .github/CODEOWNERS; do
    if [ -f "$original/$file" ] && [ -f "$replica/$file" ]; then
        echo "$file:"
        echo "  Original: $(wc -l < "$original/$file") lines"
        echo "  Replica:  $(wc -l < "$replica/$file") lines"
    fi
done
```

---

## Success Criteria for Replication

### Validation Checklist

**Functional Requirements:**
- [ ] All required files created
- [ ] GitHub directory structure (.github/) present
- [ ] Issue template exists and is valid YAML
- [ ] CODEOWNERS file exists
- [ ] Knowledge base structure (patterns/decisions/insights) present
- [ ] README explains workflow clearly
- [ ] All YAML files pass yamllint
- [ ] All shell scripts pass shellcheck (if present)

**Quality Indicators:**
- [ ] File count within 20% of original (±2-3 files)
- [ ] Total size within 30% of original
- [ ] No placeholder content (TODO, FIXME, etc.)
- [ ] No broken cross-references
- [ ] Documentation is coherent and complete

**Structural Compliance:**
- [ ] Proper directory hierarchy (no flat file structure)
- [ ] GitHub conventions followed (.github/ not github/)
- [ ] Knowledge base uses subdirectories (not underscores)
- [ ] File naming consistent

---

## Batch Replication (Full Matrix)

**Use Case:** Replicate complete 3×3 matrix (P1/P2/P3 × S1/S2/S3)

### Matrix Replication Script

```bash
#!/usr/bin/env bash
# replicate-full-matrix.sh - Replicate 3×3 prompt/criteria matrix

PROMPTS=("P1" "P2" "P3")
CRITERIA=("S1" "S2" "S3")
MODEL="sonnet"
BASE_DIR="$HOME/matrix-replication-$(date +%Y%m%d)"

mkdir -p "$BASE_DIR"

for prompt in "${PROMPTS[@]}"; do
    for criterion in "${CRITERIA[@]}"; do
        dir="$BASE_DIR/${prompt}-${criterion}-${MODEL}"
        mkdir -p "$dir"

        # Generate prompt file (would need full prompt templates for S1, S3)
        echo "Prompt: $prompt, Criteria: $criterion" > "$dir/PROMPT.txt"
        echo "Created: $dir"
    done
done

echo ""
echo "Matrix structure created in: $BASE_DIR"
echo "Total scenarios: 9"
echo ""
echo "To execute: cd into each directory and run Claude Code with PROMPT.txt"
```

---

## Common Issues and Solutions

### Issue: File Count Mismatch

**Problem:** Replica creates more/fewer files than original

**Diagnosis:**
```bash
# List files unique to each
diff <(cd "$original" && find . -type f | sort) \
     <(cd "$replica" && find . -type f | sort)
```

**Solutions:**
- More files: Agent over-elaborated, prompt too vague
- Fewer files: Agent under-delivered, check success criteria
- Try re-running with more specific constraints

### Issue: Flat File Structure

**Problem:** Files like `docs-knowledge-README.md` instead of `docs/knowledge/README.md`

**Diagnosis:**
```bash
# Check for files with multiple hyphens
find . -name "*-*-*.md"
```

**Solutions:**
- This indicates Haiku model behavior (systematic issue)
- Fix: Specify "Create proper directories, not flat files" in prompt
- Or: Use Opus/Sonnet models instead

### Issue: Missing .github Directory

**Problem:** GitHub files created at wrong location

**Diagnosis:**
```bash
# Check GitHub file locations
find . -name "CODEOWNERS" -o -name "*.yml"
```

**Solutions:**
- Add explicit file paths to prompt: `.github/CODEOWNERS`
- Include example in prompt: "Create .github/ directory first"
- Verify model is Opus or Sonnet (not Haiku)

---

## Advanced Replication Patterns

### Pattern 1: Incremental Refinement

**Use Case:** Replicate, then improve based on comparison

```bash
# 1. Initial replication
./replicate-validation-run.sh ~/refinement P2

# 2. Compare to original
diff -r "$original" "$replica"

# 3. Identify gaps
# 4. Re-run with enhanced prompt addressing gaps

# 5. Iterate until match
```

### Pattern 2: Multi-Model Comparison

**Use Case:** Test prompt portability across models

```bash
# Run same prompt with 3 models
for model in opus sonnet haiku; do
    dir="$HOME/model-comparison/P2-S2-$model"
    mkdir -p "$dir"
    echo "Run P2 prompt with $model model" > "$dir/INSTRUCTIONS.txt"
done

# Compare outputs
ls -la $HOME/model-comparison/*/
```

### Pattern 3: Prompt Engineering Validation

**Use Case:** Test prompt variations

```bash
# Create prompt variations
mkdir -p ~/prompt-variations/{minimal,moderate,detailed}

# Minimal (P1)
echo "$P1_PROMPT" > ~/prompt-variations/minimal/PROMPT.txt

# Moderate (P2)
echo "$P2_PROMPT" > ~/prompt-variations/moderate/PROMPT.txt

# Detailed (P3)
echo "$P3_PROMPT" > ~/prompt-variations/detailed/PROMPT.txt

# Execute all and compare
```

---

## Data Collection for Analysis

### Metrics to Capture

**Per Scenario:**
```bash
scenario="P2-S2-sonnet-VALIDATION"

# File count
find "$scenario" -type f | wc -l

# Total size
du -sh "$scenario"

# File types
find "$scenario" -type f | sed 's/.*\.//' | sort | uniq -c

# Line counts by file
find "$scenario" -type f -name "*.md" -exec wc -l {} +

# Directory depth
find "$scenario" -type d | awk -F/ '{print NF}' | sort -n | tail -1
```

**Save to JSON:**
```bash
cat > "$scenario/METRICS.json" <<EOF
{
  "scenario": "$scenario",
  "timestamp": "$(date -Iseconds)",
  "files": $(find "$scenario" -type f | wc -l),
  "size_kb": $(du -sk "$scenario" | cut -f1),
  "directories": $(find "$scenario" -type d | wc -l),
  "markdown_files": $(find "$scenario" -name "*.md" | wc -l),
  "yaml_files": $(find "$scenario" -name "*.yml" | wc -l)
}
EOF
```

---

## Integration with Existing Scripts

### Using Project Scripts

**From:** `/Users/bln/play/agentic-primer/scripts/`

```bash
# Create experiment run structure
cd /Users/bln/play/agentic-primer
./scripts/create-experiment-run.sh iteration-2 validation-replica

# This creates:
# experiments/iteration-2/runs/run-TIMESTAMP-validation-replica/
#   ├── config.json
#   ├── README.md
#   ├── logs/
#   └── results/

# Then replicate scenarios into results/
cd experiments/iteration-2/runs/run-TIMESTAMP-validation-replica/results
# Run replication scripts targeting this directory
```

---

## Reference Prompts Library

### P1 - Minimal Bootstrap (10 words)

```
Bootstrap @copilot issue automation with auto-review and knowledge base.
```

**Success Criteria S2:**
1. Process test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

### P2 - Moderate Specification (14 words)

```
Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base.
```

**Success Criteria S2:**
1. Process test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

### P3 - Detailed Requirements (35 words)

```
Create issue-driven development system: Issue template (.github/ISSUE_TEMPLATE/task.yml) for @copilot tasks, CODEOWNERS (* @owner) for PR auto-assignment, Knowledge base (docs/knowledge/) with patterns/decisions/insights structure, README with workflow: issue → @copilot → PR → review via web UI
```

**Success Criteria S2:**
1. Process test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

---

## Quick Start Commands

### Replicate Entire Validation Run (3 scenarios)

```bash
# 1. Create directories
mkdir -p ~/validation-replication/{P1,P2,P3}-S2-sonnet-VALIDATION

# 2. For each scenario, start Claude Code and paste corresponding prompt
cd ~/validation-replication/P1-S2-sonnet-VALIDATION
claude
# Paste P1 prompt + S2 criteria + instructions

cd ~/validation-replication/P2-S2-sonnet-VALIDATION
claude
# Paste P2 prompt + S2 criteria + instructions

cd ~/validation-replication/P3-S2-sonnet-VALIDATION
claude
# Paste P3 prompt + S2 criteria + instructions

# 3. Verify all outputs
for dir in P1 P2 P3; do
  echo "$dir: $(find ~/validation-replication/${dir}-S2-sonnet-VALIDATION -type f | wc -l) files"
done
```

### Replicate Single Scenario (P2 example)

```bash
# 1. Create directory
mkdir -p ~/test/P2-S2-sonnet-VALIDATION && cd ~/test/P2-S2-sonnet-VALIDATION

# 2. Start Claude
claude

# 3. Paste prompt
# [Copy P2 prompt from Reference Prompts Library section above]

# 4. Verify
ls -la
yamllint .github/**/*.yml
```

---

## Conclusion

This replication harness provides:

1. **Complete prompts** for all 3 validation scenarios
2. **Step-by-step instructions** for full or partial replication
3. **Automation scripts** for batch replication
4. **Verification procedures** to validate outputs
5. **Troubleshooting guides** for common issues
6. **Integration points** with existing project scripts

Use this harness to:
- Verify reproducibility of results
- Test prompt variations
- Compare model behaviors
- Validate prompt engineering techniques
- Train team members on bootstrap processes

**Repository:** `/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-validation-20260106`
**Created:** 2026-01-08
**Version:** 1.0
