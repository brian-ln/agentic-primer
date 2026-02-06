#!/bin/bash

# Analyze workflow files for automation vs notification patterns
# Success pattern: git operations (checkout -b, commit, push) + PR creation

BASE_DIR="/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix"

# Tested scenarios (skip these)
TESTED=(
  "P2-S2-opus"
  "P1-S3-opus"
  "P2-S2-sonnet"
  "P1-S1-opus"
  "P3-S2-opus"
)

# All scenarios
ALL_SCENARIOS=(
  P1-S1-haiku P1-S1-opus P1-S1-sonnet
  P1-S2-haiku P1-S2-opus P1-S2-sonnet
  P1-S3-haiku P1-S3-opus P1-S3-sonnet
  P2-S1-haiku P2-S1-opus P2-S1-sonnet
  P2-S2-haiku P2-S2-opus P2-S2-sonnet
  P2-S3-haiku P2-S3-opus P2-S3-sonnet
  P3-S1-haiku P3-S1-opus P3-S1-sonnet
  P3-S2-haiku P3-S2-opus P3-S2-sonnet
  P3-S3-haiku P3-S3-opus P3-S3-sonnet
)

echo "# Automation Pattern Analysis"
echo ""
echo "## Automation Indicators"
echo "- ✅ Full Automation: git checkout -b + commit + push + PR creation"
echo "- ⚠️  Partial Automation: Some git operations but incomplete"
echo "- ❌ Notification Only: No git operations, just comments"
echo ""

for scenario in "${ALL_SCENARIOS[@]}"; do
  # Skip tested scenarios
  skip=false
  for tested in "${TESTED[@]}"; do
    if [ "$scenario" = "$tested" ]; then
      skip=true
      break
    fi
  done

  if [ "$skip" = true ]; then
    continue
  fi

  # Find workflow files (both .github/workflows/*.yml and .github_workflows_*.yml)
  workflow_file=$(find "$BASE_DIR/$scenario" -name "*.yml" \( -path "*/.github/workflows/*" -o -name ".github_workflows_*" \) 2>/dev/null | head -n1)

  if [ -z "$workflow_file" ]; then
    echo "### $scenario: ⚠️ NO WORKFLOW FILE"
    echo ""
    continue
  fi

  # Check for automation patterns
  has_branch_create=$(grep -E "git checkout -b|checkout -b" "$workflow_file" 2>/dev/null | wc -l | tr -d ' ')
  has_git_commit=$(grep "git commit" "$workflow_file" 2>/dev/null | wc -l | tr -d ' ')
  has_git_push=$(grep "git push" "$workflow_file" 2>/dev/null | wc -l | tr -d ' ')
  has_pr_action=$(grep -E "peter-evans/create-pull-request|gh pr create" "$workflow_file" 2>/dev/null | wc -l | tr -d ' ')
  has_comment_only=$(grep "create-or-update-comment" "$workflow_file" 2>/dev/null | wc -l | tr -d ' ')

  # Determine automation level
  automation_score=$(( has_branch_create + has_git_commit + has_git_push + has_pr_action ))

  if [ $automation_score -ge 4 ]; then
    status="✅ FULL AUTOMATION"
  elif [ $automation_score -ge 2 ]; then
    status="⚠️  PARTIAL AUTOMATION"
  else
    status="❌ NOTIFICATION ONLY"
  fi

  echo "### $scenario: $status"
  echo ""
  echo "**Automation Score: $automation_score/4**"
  echo "- Branch create: $([ $has_branch_create -gt 0 ] && echo '✅' || echo '❌')"
  echo "- Git commit: $([ $has_git_commit -gt 0 ] && echo '✅' || echo '❌')"
  echo "- Git push: $([ $has_git_push -gt 0 ] && echo '✅' || echo '❌')"
  echo "- PR creation: $([ $has_pr_action -gt 0 ] && echo '✅' || echo '❌')"
  echo ""

  # Show workflow name
  workflow_name=$(grep "^name:" "$workflow_file" | head -n1 | sed 's/name: //')
  echo "**Workflow:** $workflow_name"
  echo "**File:** $(basename "$workflow_file")"
  echo ""

done

# Summary statistics
echo "---"
echo ""
echo "## Summary Statistics"
echo ""

full_auto=0
partial_auto=0
notification_only=0
no_workflow=0

for scenario in "${ALL_SCENARIOS[@]}"; do
  skip=false
  for tested in "${TESTED[@]}"; do
    if [ "$scenario" = "$tested" ]; then
      skip=true
      break
    fi
  done

  if [ "$skip" = true ]; then
    continue
  fi

  workflow_file=$(find "$BASE_DIR/$scenario" -name "*.yml" \( -path "*/.github/workflows/*" -o -name ".github_workflows_*" \) 2>/dev/null | head -n1)

  if [ -z "$workflow_file" ]; then
    no_workflow=$(( no_workflow + 1 ))
    continue
  fi

  has_branch_create=$(grep -E "git checkout -b|checkout -b" "$workflow_file" 2>/dev/null | wc -l | tr -d ' ')
  has_git_commit=$(grep "git commit" "$workflow_file" 2>/dev/null | wc -l | tr -d ' ')
  has_git_push=$(grep "git push" "$workflow_file" 2>/dev/null | wc -l | tr -d ' ')
  has_pr_action=$(grep -E "peter-evans/create-pull-request|gh pr create" "$workflow_file" 2>/dev/null | wc -l | tr -d ' ')

  automation_score=$(( has_branch_create + has_git_commit + has_git_push + has_pr_action ))

  if [ $automation_score -ge 4 ]; then
    full_auto=$(( full_auto + 1 ))
  elif [ $automation_score -ge 2 ]; then
    partial_auto=$(( partial_auto + 1 ))
  else
    notification_only=$(( notification_only + 1 ))
  fi
done

total=$(( full_auto + partial_auto + notification_only + no_workflow ))

echo "**Total untested scenarios:** 22"
echo "- ✅ Full automation: $full_auto"
echo "- ⚠️  Partial automation: $partial_auto"
echo "- ❌ Notification only: $notification_only"
echo "- ⚠️  No workflow file: $no_workflow"
echo ""

if [ $full_auto -gt 0 ]; then
  echo "## 🎯 Near-Misses Detected!"
  echo ""
  echo "Found $full_auto scenario(s) with full automation patterns (same as P3-S2-opus winner)."
  echo "These are HIGH PRIORITY for testing."
  echo ""
fi
