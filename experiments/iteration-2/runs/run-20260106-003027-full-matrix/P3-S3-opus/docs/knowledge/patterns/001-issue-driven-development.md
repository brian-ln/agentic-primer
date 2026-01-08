# Issue-Driven Development Pattern

## Problem

How do we enable AI agents to work autonomously on development tasks while maintaining human oversight and quality control?

## Solution

Use GitHub issues as structured task definitions that AI agents can parse and execute. The workflow follows:

1. Human creates issue using structured template
2. Issue is labeled for agent processing
3. Agent reads issue, creates branch, implements solution
4. Agent creates PR with implementation
5. Human reviews PR in web UI
6. System captures learnings for future improvements

## Example

### Issue Template (task.yml)

```yaml
body:
  - type: textarea
    id: description
    attributes:
      label: Task Description
    validations:
      required: true

  - type: textarea
    id: success-criteria
    attributes:
      label: Success Criteria
    validations:
      required: true
```

### Workflow Trigger

```yaml
on:
  issues:
    types: [labeled]

jobs:
  process:
    if: github.event.label.name == 'copilot'
    steps:
      - name: Process Issue
        run: |
          # Parse issue body
          # Create branch
          # Implement solution
          # Create PR
```

## When to Use

- Tasks are well-defined with clear success criteria
- Implementation can be verified automatically (tests, lint)
- Human review is required before merge
- Continuous improvement is desired

## When NOT to Use

- Tasks require real-time human collaboration
- External system access is needed
- Success criteria cannot be objectively measured
- Security-sensitive operations

## Related Patterns

- [ADR-001: Use GitHub Native Features](../decisions/001-use-github-native-features.md)
- [Insight-001: Bootstrap Learnings](../insights/001-bootstrap-learnings.md)

## References

- GitHub Issue Templates: https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests
- GitHub Actions: https://docs.github.com/en/actions
