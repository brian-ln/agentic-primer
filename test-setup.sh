#!/bin/bash
# Test script to validate the issue-driven development setup

set -e

echo "Testing issue-driven development setup..."

# Check for required directories
echo "✓ Checking directory structure..."
test -d .github/workflows || { echo "✗ Missing .github/workflows"; exit 1; }
test -d .github/ISSUE_TEMPLATE || { echo "✗ Missing .github/ISSUE_TEMPLATE"; exit 1; }
test -d docs/knowledge || { echo "✗ Missing docs/knowledge"; exit 1; }

# Check for required files
echo "✓ Checking required files..."
test -f .github/workflows/auto-assign.yml || { echo "✗ Missing auto-assign.yml"; exit 1; }
test -f .github/ISSUE_TEMPLATE/config.yml || { echo "✗ Missing config.yml"; exit 1; }
test -f .github/ISSUE_TEMPLATE/research.yml || { echo "✗ Missing research.yml"; exit 1; }
test -f .github/ISSUE_TEMPLATE/implementation.yml || { echo "✗ Missing implementation.yml"; exit 1; }
test -f .github/ISSUE_TEMPLATE/test.yml || { echo "✗ Missing test.yml"; exit 1; }
test -f README.md || { echo "✗ Missing README.md"; exit 1; }
test -f docs/knowledge/README.md || { echo "✗ Missing knowledge base README"; exit 1; }

# Check for knowledge base subdirectories
echo "✓ Checking knowledge base structure..."
test -d docs/knowledge/conventions || { echo "✗ Missing conventions directory"; exit 1; }
test -d docs/knowledge/architecture || { echo "✗ Missing architecture directory"; exit 1; }
test -d docs/knowledge/workflows || { echo "✗ Missing workflows directory"; exit 1; }
test -d docs/knowledge/decisions || { echo "✗ Missing decisions directory"; exit 1; }

# Validate YAML syntax for workflows
echo "✓ Validating workflow YAML..."
if command -v python3 &> /dev/null; then
    python3 -c "import yaml; yaml.safe_load(open('.github/workflows/auto-assign.yml'))" 2>/dev/null || { echo "✗ Invalid YAML in auto-assign.yml"; exit 1; }
else
    echo "  (Python not available, skipping YAML validation)"
fi

# Validate YAML syntax for issue templates
echo "✓ Validating issue template YAML..."
if command -v python3 &> /dev/null; then
    for file in .github/ISSUE_TEMPLATE/*.yml; do
        python3 -c "import yaml; yaml.safe_load(open('$file'))" 2>/dev/null || { echo "✗ Invalid YAML in $file"; exit 1; }
    done
else
    echo "  (Python not available, skipping YAML validation)"
fi

echo ""
echo "✅ All tests passed!"
echo ""
echo "System is ready for issue-driven development:"
echo "  - Auto-assign workflow configured"
echo "  - Issue templates created"
echo "  - Knowledge base initialized"
echo ""
echo "Next steps:"
echo "  1. Create a test issue using the templates"
echo "  2. Mention @copilot in the issue"
echo "  3. Verify the workflow executes correctly"
