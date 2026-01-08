# Insight: GitHub Copilot Best Practices

**Category:** Insights
**Domain:** AI-Assisted Development
**Last Updated:** 2026-01-08
**Source:** Team Experience, Production Usage

## Summary

Based on 3 months of using GitHub Copilot for issue automation, we've identified patterns that significantly improve code quality and reduce review cycles.

## Key Findings

### 1. Structured Issue Descriptions Improve Output Quality

**Observation:**
Issues with structured descriptions (using templates) resulted in significantly better code from Copilot.

**Data:**
- Structured issues: 80% acceptance rate on first PR
- Free-form issues: 35% acceptance rate on first PR
- Average review cycles: 1.2 vs 3.5

**Evidence:**
- Tracked 50 @copilot-generated PRs over 3 months
- Measured by: review comments, requested changes, merge time
- Controlled for task complexity

**Recommendation:**
✅ **Always use the issue template** when assigning to @copilot

**Example - Good Issue Structure:**
```markdown
Task: Add user authentication to /api/profile endpoint

Description:
- Currently: Endpoint is publicly accessible
- Goal: Require JWT authentication
- Constraints: Use existing auth middleware

Acceptance Criteria:
- [ ] Endpoint requires valid JWT token
- [ ] Returns 401 if unauthenticated
- [ ] Returns 403 if user lacks permission
- [ ] Existing tests still pass
- [ ] New auth tests added

Context:
- See patterns/api-error-handling.md for error format
- See decisions/001-use-rest-api.md for API design
- Use existing authMiddleware from src/middleware/auth.ts
```

**Example - Poor Issue Structure:**
```markdown
Add auth to profile endpoint
```

### 2. Knowledge Base References Reduce Inconsistencies

**Observation:**
When issues explicitly reference knowledge base entries, Copilot generates code that's consistent with existing patterns.

**Data:**
- With KB references: 92% pattern adherence
- Without KB references: 68% pattern adherence
- Pattern violations found in code review decreased by 60%

**Evidence:**
- Analyzed 30 PRs with explicit KB references
- Compared to 30 PRs without references
- Measured against coding standards checklist

**Recommendation:**
✅ **Reference relevant knowledge base entries** in issue descriptions

**Example:**
```markdown
Implement error handling following:
- patterns/api-error-handling.md (use standard error format)
- decisions/001-use-rest-api.md (REST conventions)
```

### 3. Smaller, Focused Issues Produce Better Results

**Observation:**
Breaking large issues into smaller, focused tasks results in higher-quality PRs and faster review cycles.

**Data:**
- Small issues (<200 LOC changes): 85% acceptance rate
- Medium issues (200-500 LOC): 62% acceptance rate
- Large issues (>500 LOC): 41% acceptance rate

**Evidence:**
- Analyzed 60 @copilot PRs by lines of code changed
- Tracked defects found in testing
- Measured time to merge

**Recommendation:**
✅ **Break large features into multiple focused issues**

**Example - Good Breakdown:**
```
Issue #1: Add user authentication middleware
Issue #2: Protect /api/profile endpoint with auth
Issue #3: Add authentication error handling tests
Issue #4: Update API documentation with auth requirements
```

**Example - Too Large:**
```
Issue #1: Implement complete authentication system
```

### 4. Explicit Acceptance Criteria Enable Better Testing

**Observation:**
Issues with clear, testable acceptance criteria resulted in Copilot generating more comprehensive tests.

**Data:**
- With explicit criteria: 78% test coverage average
- Without explicit criteria: 52% test coverage average
- Defects escaped to production reduced by 45%

**Evidence:**
- Measured test coverage of 40 @copilot PRs
- Tracked production bugs back to source issues
- Compared against team coding standards

**Recommendation:**
✅ **Write acceptance criteria as testable checkboxes**

**Example:**
```markdown
Acceptance Criteria:
- [ ] GET /api/users returns 200 with user list
- [ ] Returns 401 if no auth token provided
- [ ] Returns 403 if user lacks admin permission
- [ ] Pagination works with ?page=N&limit=M
- [ ] Response includes total count in metadata
- [ ] Integration tests cover happy path and errors
- [ ] API documentation updated
```

### 5. Code Review Comments Improve Future Generations

**Observation:**
When code review feedback is captured in knowledge base, Copilot doesn't repeat the same mistakes.

**Data:**
- Before capturing: Same issues recurred in 40% of PRs
- After capturing: Recurrence dropped to 8%
- Time spent on redundant review comments decreased 65%

**Evidence:**
- Tracked common review comments over 6 weeks
- Documented patterns and anti-patterns in KB
- Measured recurrence rate

**Recommendation:**
✅ **Capture recurring review feedback in knowledge base**

**Process:**
1. Identify recurring review comment
2. Create knowledge base entry (pattern or insight)
3. Reference in future issue descriptions
4. Monitor if issue recurs

**Example - Common Issue Captured:**
```markdown
# Anti-pattern: Not Validating Input Before Database Query

Problem: Copilot often skips input validation, leading to SQL injection risks.

Solution: Always validate and sanitize inputs before queries.

[Example code...]
```

### 6. Providing File Paths Improves Accuracy

**Observation:**
When issues specify exact file paths to modify, Copilot is more accurate and requires fewer review cycles.

**Data:**
- With file paths: 1.3 review cycles average
- Without file paths: 2.8 review cycles average
- Wrong files modified: 5% vs 28%

**Evidence:**
- Analyzed 50 @copilot PRs
- Tracked incorrect file modifications
- Measured review rounds required

**Recommendation:**
✅ **Specify exact files to modify** when possible

**Example:**
```markdown
Implementation Notes:
- Modify: src/api/routes/users.ts (add auth middleware)
- Modify: src/middleware/auth.ts (add new permission check)
- Create: tests/api/users.auth.test.ts (new test file)
- Update: docs/api/authentication.md (document changes)
```

### 7. Edge Cases Should Be Explicit

**Observation:**
Copilot handles happy paths well but often misses edge cases unless explicitly specified.

**Data:**
- Edge case coverage with explicit mention: 82%
- Edge case coverage without mention: 34%
- Production bugs from missed edge cases reduced 70%

**Evidence:**
- Analyzed 40 PRs for edge case handling
- Tracked production incidents to missing edge cases
- Compared against QA test plans

**Recommendation:**
✅ **List edge cases explicitly in acceptance criteria**

**Example:**
```markdown
Edge Cases to Handle:
- [ ] Empty request body
- [ ] Invalid email format
- [ ] Email already exists in database
- [ ] Database connection failure
- [ ] Concurrent registration attempts
- [ ] Special characters in input
- [ ] Maximum input length exceeded
```

## Anti-Patterns to Avoid

### ❌ Vague Issue Titles
```markdown
Bad: "Fix the bug"
Good: "Fix 500 error when user email is null in /api/profile"
```

### ❌ Missing Context
```markdown
Bad: "Add caching"
Good: "Add Redis caching to /api/users with 5min TTL (see decisions/002-use-redis.md)"
```

### ❌ No Acceptance Criteria
```markdown
Bad: "Make it faster"
Good: "Reduce /api/users response time from 800ms to <200ms (p95)"
```

### ❌ Mixing Multiple Concerns
```markdown
Bad: "Add auth and refactor database and update UI"
Good: Three separate issues for auth, database, and UI
```

## Metrics Dashboard

Track these metrics to measure @copilot effectiveness:

1. **Acceptance Rate:** % of PRs merged without major changes
2. **Review Cycles:** Average rounds of review before merge
3. **Time to Merge:** Days from issue creation to PR merge
4. **Test Coverage:** % code coverage in generated tests
5. **Production Bugs:** Defects traced to @copilot code
6. **Pattern Adherence:** % compliance with coding standards

**Target Metrics (as of 2026-01-08):**
- Acceptance rate: >75%
- Review cycles: <2
- Time to merge: <3 days
- Test coverage: >70%
- Production bugs: <5% of total
- Pattern adherence: >85%

## Team Workflow

### Before Creating Issue

1. Check if task can be broken into smaller issues
2. Identify relevant knowledge base entries
3. List edge cases and acceptance criteria
4. Specify files to modify (if known)

### In Issue Description

1. Use the issue template (always)
2. Reference knowledge base entries
3. Provide concrete examples
4. List explicit acceptance criteria
5. Specify edge cases to handle

### After PR Created

1. Review against acceptance criteria
2. Check for pattern adherence
3. Test edge cases explicitly mentioned
4. Provide specific, actionable feedback
5. Capture recurring issues in knowledge base

### After Merge

1. Monitor production for issues
2. Update metrics dashboard
3. Add insights to knowledge base
4. Refine issue template if needed

## Future Improvements

Based on ongoing experience, we plan to:

1. **Create issue templates for common task types**
   - Bug fix template
   - Feature addition template
   - Refactoring template
   - Documentation update template

2. **Build automated quality checks**
   - Lint PR descriptions for KB references
   - Check acceptance criteria format
   - Validate test coverage meets threshold
   - Auto-label issues by complexity

3. **Expand knowledge base**
   - Document all design patterns in use
   - Record every architectural decision
   - Capture insights from every incident
   - Cross-reference related entries

4. **Optimize workflow automation**
   - Auto-assign reviewers by expertise
   - Auto-tag issues with affected components
   - Generate release notes from merged PRs
   - Track metrics automatically

## Related Knowledge

- **Patterns:** All patterns should be referenced in issues
- **Decisions:** [001-use-rest-api.md](../decisions/001-use-rest-api.md) - API design guidelines
- **Tools:** GitHub Issue Templates, CODEOWNERS

## References

- [GitHub Copilot Documentation](https://docs.github.com/en/copilot)
- [Best Practices for AI Pair Programming](https://github.blog/2023-06-20-how-to-write-better-prompts-for-github-copilot/)
- [Issue Template Best Practices](https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests)

## Contributors

This insight is based on contributions from the entire engineering team. Special thanks to:
- QA team for production bug analysis
- Code reviewers for pattern adherence tracking
- DevOps for metrics dashboard

## Changelog

- **2026-01-08:** Initial insights documented
- **Future:** Add metrics from Q1 2026
- **Future:** Update with new patterns as discovered
