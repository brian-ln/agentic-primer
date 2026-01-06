#!/bin/bash
set -e

echo "Bootstrap Verification v1.0"
echo "============================"

# Check files exist
FILES=(
  ".github/workflows/issue-agent.yml"
  ".github/ISSUE_TEMPLATE/task.yml"
  "README.md"
  "docs/knowledge/README.md"
  "scripts/verify-bootstrap.sh"
)

for file in "${FILES[@]}"; do
  if [[ -f "$file" ]]; then
    echo "✓ $file exists"
  else
    echo "✗ $file MISSING"
    exit 1
  fi
done

# Validate YAML
if command -v yamllint &> /dev/null; then
  yamllint .github/workflows/issue-agent.yml || exit 1
  yamllint .github/ISSUE_TEMPLATE/task.yml || exit 1
  echo "✓ YAML syntax valid"
else
  echo "⚠ yamllint not installed, skipping syntax check"
fi

# Check workflow structure
grep -q "^on:" .github/workflows/issue-agent.yml || { echo "✗ Workflow missing 'on' trigger"; exit 1; }
grep -q "^jobs:" .github/workflows/issue-agent.yml || { echo "✗ Workflow missing 'jobs'"; exit 1; }
grep -q "permissions:" .github/workflows/issue-agent.yml || { echo "✗ Workflow missing permissions"; exit 1; }
echo "✓ Workflow structure valid"

# Check template structure
grep -q "^name:" .github/ISSUE_TEMPLATE/task.yml || { echo "✗ Template missing name"; exit 1; }
grep -q "^body:" .github/ISSUE_TEMPLATE/task.yml || { echo "✗ Template missing body"; exit 1; }
echo "✓ Template structure valid"

# Check knowledge base structure
[[ -d "docs/knowledge/patterns" ]] || { echo "✗ Missing patterns directory"; exit 1; }
[[ -d "docs/knowledge/decisions" ]] || { echo "✗ Missing decisions directory"; exit 1; }
[[ -d "docs/knowledge/insights" ]] || { echo "✗ Missing insights directory"; exit 1; }
echo "✓ Knowledge base structure valid"

echo ""
echo "============================"
echo "✓ ALL CHECKS PASSED"
echo "Bootstrap v1.0 is valid"
exit 0
