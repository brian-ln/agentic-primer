# @copilot Issue Automation Workflow

Complete guide to using GitHub Copilot for automated issue processing with auto-review and knowledge base integration.

## Quick Start

1. **Create an issue** using the [copilot-task template](.github/ISSUE_TEMPLATE/copilot-task.yml)
2. **Assign to @copilot** to trigger automation
3. **Review the PR** that @copilot creates
4. **Merge when ready** - reviewers auto-assigned via CODEOWNERS

## Overview

This system enables GitHub Copilot to autonomously process issues and create pull requests, leveraging an organizational knowledge base for context-aware code generation.

### System Components

```
┌─────────────────────────────────────────────────────────────┐
│                    Developer Creates Issue                   │
│              (Using Structured YAML Template)                │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│              Developer Assigns Issue to @copilot             │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│           GitHub Actions Workflow Triggered                  │
│   (Loads knowledge base, invokes @copilot processing)       │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│     @copilot Generates Code Using Knowledge Base Context    │
│   (Patterns, Decisions, Insights inform code generation)    │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│          @copilot Creates Draft Pull Request                │
│         (Branch: copilot/issue-N, Status: Draft)            │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│      CODEOWNERS Auto-Assigns Reviewers to PR                │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│         Team Reviews, Approves, and Merges PR               │
└─────────────────────────────────────────────────────────────┘
```

## Detailed Workflow

### Step 1: Create an Issue

Use the structured issue template to create a task for @copilot.

**Navigate to:**
- Issues → New Issue → "Copilot Task"

**Fill in:**
- **Task Title:** Brief description of what needs to be done
- **Detailed Description:** Comprehensive context for @copilot
- **Task Type:** Feature, Bug Fix, Refactoring, etc.
- **Priority:** Low, Medium, High, Critical
- **Acceptance Criteria:** Testable checkboxes
- **Additional Context:** Links, references, constraints
- **Knowledge Base References:** Check relevant categories
- **Implementation Notes:** Specific guidance

**Example:**
```yaml
title: "[Copilot]: Add rate limiting to API endpoints"

description: |
  Currently, our API endpoints have no rate limiting, making them
  vulnerable to abuse and DDoS attacks.

  Goal: Implement rate limiting using Redis-based token bucket algorithm

  Constraints:
  - Must work with existing auth middleware
  - Should be configurable per endpoint
  - Need different limits for authenticated vs anonymous users

acceptance_criteria: |
  - [ ] Rate limit middleware created
  - [ ] Limits enforced: 100 req/min (auth), 10 req/min (anon)
  - [ ] 429 status returned when limit exceeded
  - [ ] Rate limit headers included in response
  - [ ] Integration tests added
  - [ ] API docs updated

context: |
  See:
  - patterns/api-error-handling.md for error response format
  - decisions/001-use-rest-api.md for API conventions
  - insights/copilot-best-practices.md for issue structure tips

implementation_notes: |
  - Use express-rate-limit library
  - Store in Redis (existing connection in src/lib/redis.ts)
  - Files to modify:
    - src/middleware/rate-limit.ts (create)
    - src/api/routes/index.ts (apply middleware)
  - Edge cases:
    - Handle Redis connection failure gracefully
    - Test concurrent requests
```

### Step 2: Assign to @copilot

After creating the issue:
1. Click "Assignees" in the right sidebar
2. Type `@copilot` and select
3. Issue is now in @copilot's queue

**What happens next:**
- GitHub fires an `issues.assigned` event
- Workflow checks if assignee is @copilot
- If yes, automation workflow starts

### Step 3: Automation Workflow Executes

The GitHub Actions workflow (`.github/workflows/copilot-automation.yml`):

1. **Checks out the repository**
   ```yaml
   - uses: actions/checkout@v4
   ```

2. **Extracts issue metadata**
   - Issue number, title, body
   - Task type, priority
   - Acceptance criteria

3. **Creates copilot branch**
   ```bash
   git checkout -b copilot/issue-42
   ```

4. **Loads knowledge base**
   - Scans `docs/knowledge/`
   - Counts patterns, decisions, insights
   - Provides context to @copilot

5. **Comments on issue (processing started)**
   ```
   🤖 @copilot processing started...

   Issue: #42
   Branch: copilot/issue-42
   Knowledge Base: Available
   - Patterns: 1
   - Decisions: 1
   - Insights: 1

   I'll create a pull request shortly with my proposed solution.
   ```

6. **Invokes @copilot** (simulated in this version)
   - In production: Real @copilot generates code
   - In simulation: Placeholder commit created

7. **Pushes branch**
   ```bash
   git push origin copilot/issue-42
   ```

8. **Creates draft pull request**
   - Title: `[Copilot] Fix #42: [Issue Title]`
   - Body: Summary, changes, knowledge base context
   - Status: Draft (requires human review before merge)

9. **Comments on issue (PR created)**
   ```
   ✅ Pull request created: #43

   @copilot has finished processing this issue and created a draft PR.

   Next steps:
   1. Review the PR: #43
   2. Request changes if needed
   3. Approve and merge when ready

   The PR will be automatically assigned reviewers based on CODEOWNERS.
   ```

### Step 4: Auto-Review Assignment

When the PR is created, GitHub's CODEOWNERS feature:

1. **Reads `.github/CODEOWNERS` file**
   ```
   * @owner
   /docs/knowledge/ @owner @docs-team
   /.github/ @owner @devops-team
   ```

2. **Matches changed files to patterns**
   - If PR modifies any file: @owner is notified
   - If PR modifies docs/knowledge/: @docs-team is notified
   - If PR modifies .github/: @devops-team is notified

3. **Auto-requests reviews**
   - Matched users added as reviewers
   - They receive notification
   - PR shows "Review requested from @owner"

### Step 5: Human Review

Reviewers check the PR for:

**Code Quality:**
- Does it solve the problem?
- Is the implementation correct?
- Does it follow patterns and decisions?
- Are edge cases handled?

**Testing:**
- Are acceptance criteria met?
- Do tests pass?
- Is coverage adequate?
- Are edge cases tested?

**Knowledge Alignment:**
- Follows patterns from knowledge base?
- Respects architectural decisions?
- Avoids anti-patterns from insights?

**If changes needed:**
1. Request changes with specific feedback
2. @copilot can be re-invoked to address feedback
3. Or developer manually updates the PR

**If approved:**
1. Mark PR as "Ready for review" (remove draft status)
2. Approve the PR
3. Merge to main branch
4. Issue automatically closes (via "Fix #42" in PR title)

## Knowledge Base Integration

### What is the Knowledge Base?

Located in `docs/knowledge/`, it contains three types of organizational knowledge:

```
docs/knowledge/
├── patterns/          # How to implement solutions
│   ├── README.md
│   └── api-error-handling.md
├── decisions/         # Why we made architectural choices
│   ├── README.md
│   └── 001-use-rest-api.md
└── insights/          # What we've learned from experience
    ├── README.md
    └── copilot-best-practices.md
```

### How @copilot Uses It

1. **Patterns:** @copilot applies code templates and design patterns
2. **Decisions:** @copilot respects architectural constraints and choices
3. **Insights:** @copilot avoids known pitfalls and anti-patterns

### How to Reference Knowledge

In issue descriptions:
```markdown
Context:
- Follow patterns/api-error-handling.md for error responses
- Respect decisions/001-use-rest-api.md (we chose REST over GraphQL)
- See insights/copilot-best-practices.md for tips on issue structure
```

This helps @copilot find and apply the right context.

## Best Practices

### Writing Effective Issues

Based on `insights/copilot-best-practices.md`:

1. **Use the template** - 80% higher acceptance rate
2. **Reference knowledge base** - 92% pattern adherence
3. **Keep issues small** - <200 LOC changes = 85% acceptance
4. **Explicit acceptance criteria** - 78% test coverage average
5. **Provide file paths** - Reduces review cycles by 50%
6. **List edge cases** - 82% edge case coverage

### Knowledge Base Contribution

After merging a PR:

1. **Did you solve a reusable problem?**
   → Add a pattern to `docs/knowledge/patterns/`

2. **Did you make an architectural decision?**
   → Document it in `docs/knowledge/decisions/`

3. **Did you learn from a mistake or discovery?**
   → Capture it in `docs/knowledge/insights/`

This creates a virtuous cycle:
```
Good Issue → Good Code → Good Knowledge → Better Future Issues
```

## Troubleshooting

### Workflow Didn't Trigger

**Check:**
- Is issue assigned to @copilot? (not just labeled)
- Is the workflow file on the default branch?
- Does repository have GitHub Actions enabled?

**Debug:**
```bash
# View workflow runs
gh run list --workflow=copilot-automation.yml

# View specific run details
gh run view <run-id>
```

### @copilot Created Wrong PR

**Common causes:**
- Issue description too vague
- Missing knowledge base references
- No acceptance criteria
- Task too large

**Fix:**
1. Close the PR
2. Update issue with more detail
3. Re-assign to @copilot
4. Or manually fix the PR

### Reviewers Not Auto-Assigned

**Check:**
- Is CODEOWNERS file on default branch?
- Does CODEOWNERS pattern match changed files?
- Do specified users have repository access?

**Debug:**
```bash
# View CODEOWNERS
cat .github/CODEOWNERS

# Check if pattern matches
# (GitHub shows "Code owners" in PR if matched)
```

### Knowledge Base Not Loading

**Check:**
- Does `docs/knowledge/` directory exist?
- Are files in correct structure?
- Are files readable (permissions)?

**Debug:**
```bash
# Verify structure
ls -R docs/knowledge/

# Check workflow logs
gh run view <run-id> --log
```

## File Reference

### Core Files

- `.github/ISSUE_TEMPLATE/copilot-task.yml` - Issue template
- `.github/workflows/copilot-automation.yml` - Automation workflow
- `.github/CODEOWNERS` - Auto-reviewer assignment
- `docs/knowledge/` - Knowledge base directory
- This `README.md` - Workflow documentation

### Configuration

**Issue Template:**
- Location: `.github/ISSUE_TEMPLATE/copilot-task.yml`
- Customize: Add project-specific fields
- Validate: GitHub renders preview when you create issue

**Workflow:**
- Location: `.github/workflows/copilot-automation.yml`
- Customize: Change branch names, add steps
- Test: Trigger manually or by assigning test issue

**CODEOWNERS:**
- Location: `.github/CODEOWNERS`
- Customize: Replace `@owner` with actual usernames/teams
- Syntax: [CODEOWNERS documentation](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners)

### Knowledge Base

See individual READMEs:
- [Patterns README](docs/knowledge/patterns/README.md)
- [Decisions README](docs/knowledge/decisions/README.md)
- [Insights README](docs/knowledge/insights/README.md)

## Metrics and Monitoring

### Success Metrics

Track these to measure system effectiveness:

**@copilot Performance:**
- Acceptance rate (% of PRs merged without major changes)
- Review cycles (average rounds of review)
- Time to merge (issue creation → PR merge)

**Code Quality:**
- Test coverage in @copilot PRs
- Production bugs from @copilot code
- Pattern adherence rate

**Knowledge Base:**
- Coverage (% of decisions documented)
- Usage (references in issues)
- Recency (time since last update)

### Monitoring

**GitHub Insights:**
- Actions → Workflows → copilot-automation.yml
- View runs, success rate, execution time

**Issue Analysis:**
```bash
# List @copilot issues
gh issue list --assignee copilot

# View issue with PR
gh issue view 42
```

**PR Analysis:**
```bash
# List @copilot PRs
gh pr list --author copilot

# View PR details
gh pr view 43
```

## Advanced Usage

### Custom Issue Types

Create additional templates for:
- Bug fixes (`.github/ISSUE_TEMPLATE/copilot-bug.yml`)
- Documentation (`.github/ISSUE_TEMPLATE/copilot-docs.yml`)
- Refactoring (`.github/ISSUE_TEMPLATE/copilot-refactor.yml`)

### Workflow Customization

Extend the workflow to:
- Run additional checks (lint, tests)
- Notify on Slack/email
- Auto-label PRs by type
- Generate release notes

### Knowledge Base Automation

- Auto-generate pattern index
- Check for outdated ADRs
- Suggest related knowledge
- Validate cross-references

## FAQ

**Q: Can @copilot handle any type of task?**
A: @copilot works best for well-defined, isolated tasks. Complex, ambiguous, or exploratory work may need human intervention.

**Q: What if @copilot makes a mistake?**
A: Review the PR like any other. Request changes or close the PR and provide more detailed guidance in the issue.

**Q: Can I provide feedback to improve @copilot's output?**
A: Yes! Capture recurring issues in the knowledge base (insights/). This helps @copilot learn from mistakes.

**Q: Does this replace code review?**
A: No! Human review is still essential. CODEOWNERS ensures every PR gets reviewed.

**Q: How do I customize for my project?**
A: Update issue template, CODEOWNERS, and knowledge base with project-specific information.

**Q: Can I use this with private repositories?**
A: Yes! GitHub Copilot and Actions work with private repos (subscription required).

## Next Steps

1. **Test the system:**
   - Create a test issue
   - Assign to @copilot
   - Review the generated PR

2. **Customize for your project:**
   - Update CODEOWNERS with real usernames
   - Add project-specific issue template fields
   - Populate knowledge base

3. **Start using it:**
   - Create issues for @copilot
   - Build up knowledge base
   - Monitor and iterate

4. **Share feedback:**
   - What works well?
   - What could be improved?
   - Update this README with learnings

## Support

- **Documentation:** This README and knowledge base READMEs
- **Examples:** See `test-issue-example.md` for sample issue
- **Workflow logs:** Check Actions tab in GitHub
- **Team:** Ask in #copilot-automation Slack channel

## Resources

- [GitHub Copilot Documentation](https://docs.github.com/en/copilot)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [CODEOWNERS Documentation](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners)
- [Issue Templates Documentation](https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests)

---

**Ready to get started?** Create your first issue using the [copilot-task template](.github/ISSUE_TEMPLATE/copilot-task.yml)!
