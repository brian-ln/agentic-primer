# @copilot - Autonomous GitHub Development Agent

## What is @copilot?

@copilot is an AI-powered autonomous development agent that:
- Analyzes GitHub issues you assign to it
- Generates complete, tested solutions
- Creates pull requests automatically
- Reviews its own code for quality
- Improves itself over time

## How It Works

### 1. Create an Issue

Use the **@copilot Task** issue template to describe what you want built:

```
Title: @copilot Add user authentication

Objective: Implement JWT-based user authentication

Requirements:
- Support email/password login
- Issue JWT tokens with 1-hour expiry
- Implement refresh token rotation
- Add logout endpoint

Acceptance Criteria:
- [ ] All unit tests pass (>90% coverage)
- [ ] Integration tests pass
- [ ] No security vulnerabilities (verified with npm audit)
- [ ] Documentation updated
- [ ] PR auto-reviewed and approved
```

### 2. @copilot Analyzes and Implements

The system will:
1. Analyze your requirements
2. Consult the knowledge base for patterns
3. Design a complete solution
4. Implement code with tests
5. Create a pull request

Expected time: 2-5 minutes

### 3. Review the PR

@copilot will create a pull request with:
- Implementation code
- Unit tests (>90% coverage)
- Integration tests
- Updated documentation
- Auto-review results

The PR will show:
- ✅ Syntax checks passed
- ✅ Tests passed
- ✅ Code quality grade
- ✅ Documentation complete

### 4. Merge or Request Changes

If you'd like changes:
1. Comment on the PR with feedback
2. @copilot will create a follow-up PR
3. Review and merge when satisfied

## Effective Issue Templates

### Do ✅
- **Be specific**: "Add JWT authentication with refresh token rotation"
- **List requirements**: Use bullet points for each requirement
- **Define success**: Clear acceptance criteria
- **Provide context**: Link to related issues or design docs
- **Set priority**: Mark as Simple/Medium/Complex

### Don't ❌
- **Be vague**: "Make auth better"
- **Skip requirements**: Hoping @copilot will guess
- **Unclear criteria**: "Make it work"
- **Contradictory requirements**: Different requirements in description and comments
- **Overly complex**: Break into multiple smaller issues

## Examples

### Example 1: Simple Feature

```
@copilot Add password reset functionality

Objective: Users should be able to reset forgotten passwords

Requirements:
- Email-based password reset with token
- Reset link expires after 1 hour
- Rate limit: 5 attempts per hour
- Send confirmation email after reset

Complexity: Simple
```

**Expected Result**: Complete solution in ~2 minutes

### Example 2: Medium Feature

```
@copilot Implement multi-factor authentication (MFA)

Objective: Support TOTP-based MFA for enhanced security

Requirements:
- Generate TOTP secrets using qrcode library
- Verify TOTP tokens on login
- Recovery codes for account lockout
- Admin dashboard to view user MFA status
- Detailed error messages for failed verification

Acceptance Criteria:
- [ ] 95%+ test coverage
- [ ] Works with Google Authenticator & Authy
- [ ] Security audit passed
- [ ] Migration guide for existing users
```

**Expected Result**: Complete solution with migration guide in ~10 minutes

### Example 3: Complex Feature

```
@copilot Build real-time collaboration system

Objective: Enable multiple users to edit documents simultaneously

Requirements:
- WebSocket-based real-time sync
- Operational Transform for conflict resolution
- Presence awareness (show who's editing)
- Undo/redo with sync
- Auto-save every 30 seconds
- Audit log of all changes

Acceptance Criteria:
- [ ] 90%+ test coverage including concurrency tests
- [ ] <100ms latency for updates
- [ ] Works with 10+ concurrent users
- [ ] Full documentation with examples
- [ ] Load tests showing scalability
```

**Expected Result**: Complete system with examples in ~30 minutes

## Understanding the Knowledge Base

@copilot learns from three documents in `docs/knowledge/`:

### PATTERNS.md
Reusable code patterns for common problems:
- JWT verification
- Error handling
- Testing templates
- Database queries
- API design

When @copilot encounters a new issue, it first searches PATTERNS.md to see if a pattern already exists. If yes, it reuses the pattern. If no, it implements a new pattern and can add it to PATTERNS.md.

### DECISIONS.md
Architecture Decision Records explaining why design choices were made:
- Why TypeScript?
- Why PostgreSQL?
- Why monorepo structure?
- Why JWT with refresh tokens?

When @copilot makes architectural decisions, it references these ADRs to maintain consistency.

### INSIGHTS.md
Performance tips, gotchas, and lessons learned:
- N+1 query optimization
- Security pitfalls to avoid
- Testing patterns that work
- Deployment insights

@copilot uses these insights to avoid past mistakes and write better code.

## Monitoring @copilot's Work

### View Execution Logs

```bash
# See recent issue processing logs
ls -la .copilot/logs/

# View specific issue log
cat .copilot/logs/issue-123-complete.json
```

### Check Success Rate

```bash
# Count successful vs total executions
grep -l '"status": "completed"' .copilot/logs/*.json | wc -l
```

### View Improvement PRs

@copilot automatically creates improvement PRs monthly:
1. Analyzes execution logs
2. Identifies patterns and opportunities
3. Creates 3+ improvement PRs

Check the PR list for titles starting with `[IMPROVEMENT]`.

## Customizing @copilot

### Update System Prompt

Edit `.copilot/system-prompt.md` to change @copilot's behavior:

```markdown
# @copilot System Prompt

You are @copilot. Additional instructions:
- Always use TypeScript, never JavaScript
- Test coverage must be >95%, not 90%
- Prefer functional style over OOP
```

### Update Validation Rules

Edit `.copilot/validation-rules.yaml` to change auto-review criteria:

```yaml
code_quality:
  complexity:
    cyclomatic_max: 8
  size:
    max_function_lines: 40
```

### Add Code Patterns

Edit `docs/knowledge/PATTERNS.md` to teach @copilot new patterns:

```markdown
## New Pattern: Caching Strategy

### When to use
For read-heavy endpoints that fetch same data

### Pattern
```typescript
const cache = new LRU({ max: 1000, ttl: 3600000 });
```
```

## Troubleshooting

### Issue not being processed?
- ✅ Did you use the @copilot Task template?
- ✅ Does the issue title contain `@copilot`?
- ✅ Are GitHub Actions enabled?
- ✅ Check `.copilot/logs/` for errors

### PR has issues?
- Comment on the PR with feedback
- @copilot will create a follow-up PR with fixes
- If persistent, escalate with `@need-human-review` label

### Want to disable @copilot?
- Remove `.github/workflows/copilot-issue-processor.yml`
- Or delete `CODEOWNERS` to stop auto-assignments

## Costs

### Model Costs (Anthropic API)
- **Haiku**: $0.001 per issue (simple issues)
- **Sonnet**: $0.01 per issue (medium issues)
- **Opus**: $0.03 per issue (complex issues)

Average: ~$0.01 per issue

### Expected Costs
- 100 issues/month = ~$1
- 1000 issues/month = ~$10
- 10000 issues/month = ~$100

You can set daily/monthly budget limits in `.copilot/agent-config.yaml`.

## Support

- 📚 Read `.github/COPILOT_WORKFLOW.md` for technical details
- 🔍 Check `docs/knowledge/INSIGHTS.md` for gotchas
- 💬 File an issue with `@need-help` label for assistance
- 🐛 Report bugs with `@need-human-review` label
