# GitHub Actions Workflow Patterns

Proven patterns for CI/CD automation using GitHub Actions.

## Table of Contents

1. [Issue-Triggered Workflows](#issue-triggered-workflows)
2. [PR Validation Pipeline](#pr-validation-pipeline)
3. [Knowledge Base Updates](#knowledge-base-updates)
4. [Workflow Error Handling](#workflow-error-handling)
5. [Matrix Builds](#matrix-builds)

---

## Issue-Triggered Workflows

### Problem

Automatically process issues when assigned to a specific user (e.g., @copilot) without manual intervention.

### Solution

Use `issues` event with assignment trigger and conditional execution.

### Example

```yaml
name: Copilot Issue Assignment

on:
  issues:
    types: [assigned, labeled]

jobs:
  process-issue:
    runs-on: ubuntu-latest
    if: |
      (github.event.assignee.login == 'copilot') ||
      contains(github.event.issue.labels.*.name, 'copilot')

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Parse issue
        uses: actions/github-script@v7
        with:
          script: |
            const body = context.payload.issue.body;
            // Parse structured data from issue forms
            // ...process and generate code...

      - name: Create PR
        run: |
          # Branch, commit, push, open PR
```

### Considerations

- **When to use**: Automating responses to issue assignments
- **Trade-offs**:
  - Event-driven (instant) vs polling (delayed)
  - Requires careful permission management
- **Alternatives**: Webhooks to external service

### Related

- See: [PR Validation Pipeline](#pr-validation-pipeline)
- Decision: [Why GitHub Actions](../decisions/001-architecture.md)

---

## PR Validation Pipeline

### Problem

Ensure all PRs pass syntax validation, tests, and security checks before merge.

### Solution

Multi-job workflow with validation gates and summary reporting.

### Example

```yaml
name: PR Validation

on:
  pull_request:
    types: [opened, synchronize, reopened]

jobs:
  validate-syntax:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install validators
        run: |
          pip install yamllint
          sudo apt-get install shellcheck
          npm install -g markdownlint-cli

      - name: Validate YAML
        run: find . -name "*.yml" | xargs yamllint

      - name: Validate Shell
        run: find . -name "*.sh" | xargs shellcheck

      - name: Validate Markdown
        run: find . -name "*.md" | xargs markdownlint

  security-scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Check for secrets
        run: |
          git diff origin/main...HEAD | \
            grep -E "password|secret|token" && exit 1 || exit 0

  final-status:
    runs-on: ubuntu-latest
    needs: [validate-syntax, security-scan]
    if: always()
    steps:
      - name: Report status
        uses: actions/github-script@v7
        with:
          script: |
            const failed = '${{ needs.validate-syntax.result }}' === 'failure' ||
                          '${{ needs.security-scan.result }}' === 'failure';
            if (failed) core.setFailed('Validation failed');
```

### Considerations

- **When to use**: Every PR, before merge
- **Trade-offs**:
  - Upfront validation cost vs post-merge fixes
  - Strict checks may slow velocity initially
- **Best practices**:
  - Fail fast (cheapest checks first)
  - Provide actionable error messages
  - Allow override for emergencies

### Related

- Pattern: [Error Handling](error-handling.md)
- Pattern: [Testing Strategies](testing.md)

---

## Knowledge Base Updates

### Problem

Extract learnings from merged PRs and populate knowledge base automatically.

### Solution

Post-merge workflow that analyzes changes and generates documentation.

### Example

```yaml
name: Knowledge Base Update

on:
  pull_request:
    types: [closed]
    branches: [main]

jobs:
  extract-learnings:
    if: github.event.pull_request.merged == true
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          ref: main

      - name: Extract PR metadata
        uses: actions/github-script@v7
        with:
          script: |
            const pr = context.payload.pull_request;
            core.setOutput('pr_number', pr.number);
            core.setOutput('title', pr.title);
            core.setOutput('labels', JSON.stringify(pr.labels));

      - name: Analyze changes
        run: |
          git diff HEAD~1 HEAD --stat
          # Categorize changes (workflows, scripts, code, etc.)

      - name: Generate insight
        run: |
          ./scripts/extract-learnings.sh ${{ steps.extract.outputs.pr_number }}

      - name: Commit knowledge base
        run: |
          git add docs/knowledge/
          git commit -m "docs: Update KB from PR #${{ steps.extract.outputs.pr_number }}"
          git push
```

### Considerations

- **When to use**: Post-merge automation for documentation
- **Trade-offs**:
  - Automated (consistent) vs manual (higher quality)
  - Risk of noisy commits if every PR generates updates
- **Best practices**:
  - Skip trivial PRs (e.g., typo fixes)
  - Human review before committing major insights
  - Use `[skip ci]` to avoid triggering downstream workflows

### Related

- Script: `scripts/extract-learnings.sh`
- Insight: Auto-generated PR summaries

---

## Workflow Error Handling

### Problem

Workflows fail silently or cryptically, making debugging difficult.

### Solution

Comprehensive error handling with notifications and cleanup.

### Example

```yaml
jobs:
  main-job:
    runs-on: ubuntu-latest
    steps:
      - name: Step that might fail
        id: risky-step
        run: |
          # Risky operation
          ./script.sh
        continue-on-error: true

      - name: Handle failure
        if: steps.risky-step.outcome == 'failure'
        run: |
          echo "Step failed, attempting recovery..."
          # Recovery logic

  handle-failure:
    runs-on: ubuntu-latest
    needs: main-job
    if: failure()
    steps:
      - name: Notify failure
        uses: actions/github-script@v7
        with:
          script: |
            await github.rest.issues.createComment({
              owner: context.repo.owner,
              repo: context.repo.repo,
              issue_number: context.payload.issue.number,
              body: '⚠️ Workflow failed. [View logs](...)'
            });
```

### Considerations

- **When to use**: All production workflows
- **Best practices**:
  - Always provide actionable error messages
  - Link to workflow run logs
  - Clean up artifacts on failure
  - Notify relevant parties

### Related

- Pattern: [Error Handling](error-handling.md)

---

## Matrix Builds

### Problem

Test across multiple environments (OS, language versions, etc.) efficiently.

### Solution

Use strategy matrix for parallel testing.

### Example

```yaml
jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        node: [18, 20, 22]
        exclude:
          - os: windows-latest
            node: 18
      fail-fast: false

    steps:
      - uses: actions/checkout@v4

      - name: Setup Node
        uses: actions/setup-node@v4
        with:
          node-version: ${{ matrix.node }}

      - name: Run tests
        run: npm test
```

### Considerations

- **When to use**: Multi-platform or multi-version support
- **Trade-offs**:
  - Cost (minutes) vs coverage
  - Parallel speed vs resource limits
- **Best practices**:
  - Use `fail-fast: false` to see all failures
  - Exclude known incompatible combinations
  - Cache dependencies for faster builds

### Related

- Pattern: [Testing Strategies](testing.md)

---

## Pattern Index

| Pattern | Complexity | Frequency |
|---------|-----------|-----------|
| Issue-Triggered | Medium | High |
| PR Validation | Medium | Very High |
| KB Updates | High | Medium |
| Error Handling | Low | Very High |
| Matrix Builds | Low | Medium |

## Anti-Patterns to Avoid

1. **Polling Instead of Events**: Use webhooks, not scheduled polling
2. **Hardcoded Secrets**: Always use GitHub Secrets
3. **Unpinned Actions**: Pin to commit SHA for security
4. **Missing Timeouts**: Set `timeout-minutes` to prevent runaway jobs
5. **Ignoring Failures**: Always handle `failure()` condition

---

**Last Updated**: January 8, 2026
**Status**: Active
**Source**: Auto-generated from PR analysis + manual curation
