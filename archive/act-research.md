# act - GitHub Actions Local Testing

## Quick Overview

**act** (by nektos) is a CLI tool that runs GitHub Actions workflows locally using Docker containers. It simulates the GitHub Actions environment on your machine, allowing you to test workflows without pushing commits to GitHub.

**Key Value:** Fast feedback loops - test workflow changes immediately instead of committing, pushing, and waiting for GitHub's cloud runners.

## Installation

### Prerequisites
- **Docker** (required) - act runs workflows in Docker containers
- Sufficient disk space for Docker images (~500MB-20GB depending on runner image choice)

### Install Command

**macOS (Homebrew):**
```bash
brew install act
```

**Linux:**
```bash
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
```

**Windows:**
```bash
# Chocolatey
choco install act-cli

# Scoop
scoop install act
```

## Basic Usage

```bash
# List available workflows/jobs
act -l

# Run workflows triggered by 'push' event
act

# Run workflows for pull_request event
act pull_request

# Run a specific job
act -j job-name

# Run a specific workflow file
act -W .github/workflows/issue-agent.yml

# Dry-run (don't actually execute)
act -n
```

## Example Verification Script

```bash
#!/usr/bin/env bash
# verify-workflow.sh - Test generated GitHub Actions workflow

set -euo pipefail

WORKFLOW_FILE=".github/workflows/issue-agent.yml"

echo "=== Verifying GitHub Actions Workflow ==="

# 1. Check workflow file exists
if [[ ! -f "$WORKFLOW_FILE" ]]; then
    echo "ERROR: Workflow file not found: $WORKFLOW_FILE"
    exit 1
fi
echo "✓ Workflow file exists"

# 2. Validate YAML syntax (requires yq or similar)
if command -v yq &> /dev/null; then
    yq eval "$WORKFLOW_FILE" > /dev/null 2>&1
    echo "✓ Valid YAML syntax"
fi

# 3. Check if act is installed
if ! command -v act &> /dev/null; then
    echo "ERROR: act not installed. Install with: brew install act"
    exit 1
fi
echo "✓ act is installed"

# 4. List jobs in the workflow
echo ""
echo "Jobs found in workflow:"
act -W "$WORKFLOW_FILE" -l

# 5. Dry-run the workflow (validates structure without executing)
echo ""
echo "Running dry-run validation..."
act -W "$WORKFLOW_FILE" -n workflow_dispatch

# 6. Optional: Actually run the workflow (requires Docker)
# Uncomment to test actual execution
# echo ""
# echo "Executing workflow locally..."
# act -W "$WORKFLOW_FILE" workflow_dispatch

echo ""
echo "=== Workflow verification complete ==="
```

## Syntax vs Execution Validation

**Syntax Only:**
- Use `yq`, `yamllint`, or `action-validator` for pure YAML validation
- Fast, no Docker required
- Catches: invalid YAML, missing required fields, schema violations

**Syntax + Execution (act):**
- Validates workflow structure AND runs the actual steps
- Requires Docker, slower
- Catches: runtime errors, missing dependencies, incorrect step logic, environment issues

**For Bootstrap Verification:**
- **Minimum:** YAML syntax validation (fast, catches most errors)
- **Recommended:** `act -n` dry-run (validates job structure without execution)
- **Thorough:** `act` full execution (proves workflow actually works, but may require secrets/context)

## Limitations & Caveats

### Platform Limitations
- **Only supports Linux containers** - Windows and macOS runners are not supported
- Jobs using `runs-on: windows-latest` or `runs-on: macos-latest` will be skipped

### Environment Differences
- **Not a perfect replica** of GitHub's environment
- Some `github` context values may be incomplete or mocked
- Missing some pre-installed tools (unless using large runner images)
- No systemd or certain system services available in containers

### Secrets & Context
- **Secrets must be provided explicitly** via `-s` flag or `.secrets` file
- Example: `act -s GITHUB_TOKEN=ghp_xxx`
- Some context values (like `github.event`) require event payload files

### Execution Model
- Jobs typically run **sequentially** (not fully parallel like GitHub)
- Caching (`actions/cache`) works differently or may need configuration
- Hosted services integration may differ

### Docker Requirement
- Requires Docker daemon running
- First run downloads runner images (can be large)
- Runner image options: micro (~200MB), medium (~500MB), large (~20GB)

## Fits "Observable Outcomes" Strategy?

**YES, with qualifications:**

✓ **Can verify workflow exists and is syntactically valid** - Fast, deterministic
✓ **Can validate job structure without execution** - `act -n` is perfect for this
✓ **Can test basic execution locally** - Proves workflow logic works
✓ **Integrates easily into bash scripts** - Simple CLI interface
✓ **No GitHub push required** - Pure local verification

⚠ **Caveats:**
- Requires Docker (adds complexity to bootstrap environment)
- May not catch platform-specific issues (Windows/macOS runners)
- Full execution requires proper secrets/context setup
- Not identical to actual GitHub environment

**Recommendation for Bootstrap:**
Use `act -n` (dry-run) as the verification step:
```bash
# Quick, reliable, no secrets needed
act -W .github/workflows/issue-agent.yml -n workflow_dispatch
```

This validates:
1. Workflow file exists and is readable
2. YAML is valid
3. Job structure is correct
4. Steps are properly defined
5. No obvious syntax errors

**Does NOT validate:**
- Actual step execution success
- External dependencies (GitHub API, secrets)
- Runtime behavior

For bootstrap verification, this is **sufficient** - we're proving the workflow was created correctly, not that it will succeed in all runtime scenarios.

## Additional Tools

### action-validator
For pure syntax validation without Docker:
```bash
# Install
npm install -g action-validator

# Validate workflow
action-validator .github/workflows/issue-agent.yml
```

## References

- [nektos/act GitHub Repository](https://github.com/nektos/act)
- [Running GitHub Actions Locally with Act | Better Stack](https://betterstack.com/community/guides/scaling-docker/act-github-actions-tutorial/)
- [act Runners Documentation](https://actions-oss.github.io/act-docs/usage/runners.html)
- [Using Act to Test GitHub Workflows Locally | Microsoft](https://techcommunity.microsoft.com/blog/azureinfrastructureblog/using-act-to-test-github-workflows-locally-for-azure-deployments-cicd/4414310)
- [How to Run GitHub Actions Locally | freeCodeCamp](https://www.freecodecamp.org/news/how-to-run-github-actions-locally/)
- [GitHub Action Validator](https://github.com/mpalmer/action-validator)
- [Running GitHub Actions Locally | Baeldung](https://www.baeldung.com/ops/github-actions-workflow-locally)
- [Testing GitHub Actions Locally | BrowserStack](https://www.browserstack.com/guide/test-github-actions-locally)
