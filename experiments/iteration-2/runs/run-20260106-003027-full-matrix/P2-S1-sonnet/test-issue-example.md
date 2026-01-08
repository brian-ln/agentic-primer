# Test Issue Example

This is an example test issue to verify the @copilot issue-driven development system works correctly.

## Issue Details

**Title:** Add user password reset endpoint

**Body:**
```
Users need the ability to reset forgotten passwords via email.

### Requirements

- POST /api/auth/reset-password endpoint to initiate reset
- Send reset token via email
- Token expires after 1 hour
- POST /api/auth/reset-password/confirm endpoint to complete reset
- Validate token before allowing password change

### Acceptance Criteria

- [ ] Endpoint accepts email and sends reset token
- [ ] Token can be validated
- [ ] Password can be updated with valid token
- [ ] Expired tokens are rejected
- [ ] Tests cover happy path and error cases
- [ ] Rate limiting prevents abuse

### Technical Notes

- Use crypto.randomBytes for token generation
- Store token hash in database (not plain token)
- Use nodemailer for sending emails
- Follow security best practices from knowledge base
```

**Labels:** `copilot-ready`, `enhancement`, `priority: medium`

**Assignee:** @copilot

## Expected Workflow

When this issue is assigned to @copilot:

1. **Issue Processor Workflow Triggers**
   - Detects assignment to @copilot
   - Parses issue body and extracts requirements
   - Identifies 6 requirements and 6 acceptance criteria
   - Priority: medium (default)

2. **Knowledge Base Search**
   - Query: "password reset authentication email token security"
   - Finds relevant patterns:
     - `kb-patterns-api-design.md`: Authentication patterns
     - `kb-procedures-security.md`: Token security best practices
     - `kb-patterns-error-handling.md`: API error responses
     - `kb-standards-code-style.md`: Code formatting

3. **Implementation Plan Generation**
   - Files to create:
     - `src/routes/auth.routes.js` (modify)
     - `src/services/password-reset.service.js` (new)
     - `src/models/PasswordResetToken.js` (new)
     - `tests/auth/password-reset.test.js` (new)
   - Dependencies:
     - nodemailer@^6.9.0
     - crypto (built-in)
   - Estimated effort: 6-8 hours

4. **Implementation**
   - Create feature branch: `copilot/issue-42-password-reset`
   - Generate code following patterns from knowledge base
   - Apply security best practices
   - Create comprehensive tests

5. **Pull Request Creation**
   - Title: `feat: add user password reset endpoint`
   - Body includes:
     - Summary of changes
     - List of files created/modified
     - Dependencies added
     - Testing approach
     - Security considerations
     - Reference to issue: "Closes #42"
   - Status: Draft

6. **Auto-Assignment**
   - PR assigned to issue creator as reviewer
   - Labels: `copilot-generated`, `needs-review`
   - Welcome comment posted explaining next steps

## Verification Steps

After workflow completes:

1. ✅ Check feature branch exists: `copilot/issue-42-password-reset`
2. ✅ Verify PR created and linked to issue
3. ✅ Confirm PR assigned to issue creator
4. ✅ Check implementation follows knowledge base patterns
5. ✅ Verify tests are included
6. ✅ Confirm no errors in workflow logs

## Success Criteria

System successfully processes the issue without errors if:

- All workflow steps complete without exceptions
- Feature branch created
- Draft PR opened
- PR correctly assigned to issue creator
- PR body includes "Closes #42"
- Knowledge base context applied to implementation

## Simulated Output

### Comment on Issue

```markdown
🤖 **@copilot has processed this issue**

✅ Implementation plan created
✅ Code generated and tested
✅ Pull request created: #87

**Next Steps:**
1. Review the implementation in PR #87
2. Run locally to verify functionality
3. Request changes if needed
4. Approve and merge when ready

**Knowledge Base Context Applied:**
patterns/api-design: Authentication Endpoints; procedures/security: Token Security Best Practices; patterns/error-handling: API Error Responses

Generated with ❤️ by @copilot
```

### PR Created

```markdown
# feat: add user password reset endpoint

## Summary
Implement password reset via email with expiring tokens following security best practices.

## Changes
- 🛣️ `src/routes/auth.routes.js` - Define API endpoints and route handlers
- ⚙️ `src/services/password-reset.service.js` - Implement business logic
- 📊 `src/models/PasswordResetToken.js` - Define data schema and model
- 🧪 `tests/auth/password-reset.test.js` - Unit and integration tests

## Technical Details

**Tech Stack:**
- framework: Express
- language: JavaScript
- apiType: REST

## Dependencies

**Production:**
- `nodemailer@^6.9.0` - Email sending

## Testing

**Unit Tests:**
- src/services/password-reset.service.js
  - Happy path with valid inputs
  - Error handling with invalid inputs
  - Edge cases and boundary conditions

**Integration Tests:**
- src/routes/auth.routes.js
  - Successful API requests
  - Authentication/authorization failures
  - Validation errors
  - Database integration

## Knowledge Base Context

This implementation follows our established patterns and standards:
- **patterns/api-design**: Authentication Endpoints
- **procedures/security**: Token Security Best Practices
- **patterns/error-handling**: API Error Responses

## Review Checklist

- [ ] Code follows project coding standards
- [ ] Tests pass locally
- [ ] No security vulnerabilities introduced
- [ ] Documentation updated
- [ ] Ready to merge

## Related Issues

Closes #42

---

🤖 *This PR was generated by @copilot*
⏱️  *Estimated effort: 8 hours*
```

---

**This test issue demonstrates the complete workflow from issue assignment to PR creation.**
