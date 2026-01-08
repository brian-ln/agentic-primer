# Git Workflow and Standards

## Branch Strategy

### Branch Naming

```
feature/<issue-number>-<short-description>
bugfix/<issue-number>-<short-description>
hotfix/<short-description>
release/<version>
```

Examples:
- `feature/123-user-authentication`
- `bugfix/456-fix-memory-leak`
- `hotfix/security-patch`
- `release/v1.2.0`

### Branch Lifecycle

1. Create branch from `main`
2. Work on feature/fix
3. Create pull request
4. Code review and approval
5. Merge to `main`
6. Delete feature branch

## Commit Messages

### Format

```
<type>(<scope>): <subject>

<body>

<footer>
```

### Types

- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, no logic change)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Build process, dependencies, tooling

### Examples

```
feat(auth): add JWT authentication

Implement JWT-based authentication with refresh tokens.
Tokens expire after 1 hour and can be refreshed.

Closes #123
```

```
fix(api): prevent memory leak in user service

Cache was not being cleared after requests, causing
memory to grow unbounded.

Fixes #456
```

### Rules

- First line max 72 characters
- Use imperative mood ("add" not "added")
- Don't end subject with period
- Separate subject from body with blank line
- Wrap body at 72 characters
- Use body to explain what and why, not how

## Pull Requests

### PR Title

Same format as commit messages:
```
feat(auth): add JWT authentication
```

### PR Description Template

```markdown
## Summary
Brief description of changes

## Changes
- List of specific changes
- File modifications
- New features

## Testing
- How to test
- Test results

## Related Issues
Closes #123
```

### PR Size

- Keep PRs small and focused
- Ideal: < 400 lines changed
- Large changes: break into multiple PRs

### Review Process

1. Self-review before requesting review
2. Request review from 1-2 team members
3. Address feedback
4. Approval required before merge
5. Squash or rebase before merge

## Code Review Guidelines

### For Authors

- Write clear PR description
- Add screenshots for UI changes
- Respond to all comments
- Keep discussion professional

### For Reviewers

- Review within 24 hours
- Be constructive and specific
- Approve when ready, request changes if needed
- Don't nitpick style (tools handle that)

## Merge Strategy

Use **squash and merge** for:
- Feature branches
- Small fixes
- Clean up messy commit history

Use **rebase and merge** for:
- Release branches
- Preserving detailed history

Never use **merge commit** unless absolutely necessary.

---

**Remember**: Clean git history helps future debugging and understanding.
