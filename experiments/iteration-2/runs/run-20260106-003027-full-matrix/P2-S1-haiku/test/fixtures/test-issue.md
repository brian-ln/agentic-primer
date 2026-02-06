# Test Issue Fixture for @copilot Workflow

This file represents a typical GitHub issue that @copilot would process.

## Issue Properties

**Title:** Create user authentication API endpoint

**Labels:** 
- `copilot-task`
- `enhancement`
- `api`

**Body:**

We need to add a user authentication API endpoint to allow clients to authenticate and receive JWT tokens.

### Acceptance Criteria

- [ ] Endpoint: POST /api/v1/auth/login
- [ ] Accept username and password in request body
- [ ] Return JWT token with 24-hour expiration
- [ ] Return 400 Bad Request if credentials are missing
- [ ] Return 401 Unauthorized if credentials are invalid
- [ ] Include rate limiting (max 5 attempts per minute)
- [ ] Document endpoint in API spec
- [ ] Include unit tests

### Additional Context

Follow the API design pattern for response format:

```json
{
  "data": {
    "token": "eyJhbGc...",
    "expires_at": "2026-01-09T12:00:00Z"
  },
  "meta": {
    "timestamp": "2026-01-08T12:00:00Z"
  }
}
```

Error responses should include code, message, and details fields.

### Implementation Notes

- Check docs/knowledge/patterns/api-design.md for conventions
- Consider the insights in docs/knowledge/insights/automation-learnings.md
- Review docs/knowledge/decisions/workflow-architecture.md for context

### Related Issues

- #123 (User model implementation)
- #124 (JWT library selection)

---

## Testing Instructions

To test this fixture with the @copilot workflow:

1. Create a GitHub issue with this exact title and body
2. Add the `copilot-task` label
3. Observe the workflow in Actions tab
4. Verify:
   - Issue auto-assigned to creator
   - Label `copilot-processing` added
   - Implementation file generated in `src/features/issue-{number}-implementation.md`
   - PR created with title "Copilot: Create user authentication API endpoint"
   - PR auto-assigned to issue creator
   - Issue commented with PR link
   - Labels updated: removed `copilot-processing`, added `copilot-completed`

## Expected Workflow Output

### 1. Workflow Triggered
- Issue has `copilot-task` label
- GitHub Actions workflow `copilot-issue-driven.yml` starts

### 2. Auto-Assignment
- Issue assigned to @creator (issue creator)
- Label `copilot-processing` added

### 3. Knowledge Base Scan
- Finds 1 pattern: api-design.md
- Finds 1 decision: workflow-architecture.md  
- Finds 1 insight: automation-learnings.md
- Passes context to @copilot

### 4. Implementation
- @copilot processes issue
- Generates: `src/features/issue-{number}-implementation.md`
- References API design pattern
- Includes acceptance criteria

### 5. Validation
- YAML syntax checked (workflow file)
- No shell scripts to validate
- Both checks pass

### 6. PR Creation
- Branch created: `copilot/issue-{number}`
- Changes committed
- PR created with:
  - Title: "Copilot: Create user authentication API endpoint"
  - Body: Includes KB context, validation results
  - Assigned to: @creator
  - Labeled: `copilot-generated`

### 7. Issue Update
- Comment added with PR link
- Labels updated:
  - Remove: `copilot-processing`
  - Add: `copilot-completed`

### 8. Complete
- Workflow succeeds
- Issue processed end-to-end
- Creator has PR to review

## Fixture Variants

You can modify this fixture to test different scenarios:

### Test Validation Failure
Add a YAML file to repository and include in output to test yamllint validation.

### Test Large Knowledge Base
Add more files to docs/knowledge/ directories to test KB scanning.

### Test Error Handling
Use partial data (missing title, body) to test error handling.

### Test Rate Limiting
Create multiple issues rapidly to test GitHub API rate limiting.

## Real-World Usage

Once validated with this fixture, the workflow should handle real issues:

```bash
# Create real issue from command line
gh issue create \
  --title "Create user authentication API endpoint" \
  --body "$(cat test/fixtures/test-issue.md)" \
  --label "copilot-task"
```

Or create manually in GitHub UI:
1. Click "New issue"
2. Copy title and body from fixture
3. Add `copilot-task` label
4. Submit

---

**Fixture Created:** 2026-01-08
**Version:** 1.0
**Purpose:** Validate @copilot workflow end-to-end
