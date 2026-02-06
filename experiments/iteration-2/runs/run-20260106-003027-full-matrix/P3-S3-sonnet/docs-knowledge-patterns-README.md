# Code Patterns

Reusable code patterns and solutions discovered from past implementations.

## Purpose

This directory contains proven patterns that work well in this codebase. When implementing new features, search here for similar patterns to follow.

## What Goes Here

### Good Pattern Entries

✅ **Specific implementations**: "User authentication with JWT refresh tokens"
✅ **Reusable components**: "React form validation hook pattern"
✅ **API designs**: "RESTful pagination with cursor-based approach"
✅ **Error handling**: "Centralized error handling middleware pattern"
✅ **Testing strategies**: "Integration test setup for database operations"

### Not Pattern Entries

❌ **General advice**: "Write clean code" (too vague)
❌ **One-off solutions**: "Fixed typo in config file" (not reusable)
❌ **Architecture decisions**: Put in `decisions/` instead
❌ **Process learnings**: Put in `insights/` instead

## Pattern Template

```markdown
# Pattern Name

**Category**: [API Design / Component / Testing / etc.]
**Language**: [JavaScript / Python / etc.]
**Created**: YYYY-MM-DD
**Source**: PR #XXX

## Context

When working with [specific situation], we need to [accomplish goal].

## Problem

[Describe the specific problem this pattern solves]

## Solution

### Approach
[High-level description of the solution]

### Implementation
```language
// Code example showing the pattern
[reference implementation]
```

### Key Points
- Point 1: [Important aspect]
- Point 2: [Critical detail]
- Point 3: [Edge case handling]

## When to Use

- Use when [situation 1]
- Use when [situation 2]
- Do NOT use when [anti-pattern situation]

## Examples

### Example 1: [Use case]
[Code snippet or reference]

### Example 2: [Another use case]
[Code snippet or reference]

## Related Patterns

- [Link to related pattern 1]
- [Link to related pattern 2]

## Tradeoffs

**Pros**:
- Benefit 1
- Benefit 2

**Cons**:
- Limitation 1
- Limitation 2

## References

- PR: #XXX
- Issue: #YYY
- External docs: [URL]

---
_Extracted from PR #XXX_
```

## Categories

Organize patterns by category:

### API Design Patterns
- RESTful endpoint structures
- GraphQL query patterns
- Pagination strategies
- Rate limiting approaches
- Authentication/authorization

### Component Patterns
- React hooks and patterns
- Vue composition patterns
- Reusable UI components
- State management

### Data Patterns
- Database query optimization
- Caching strategies
- Data validation
- Migration patterns

### Testing Patterns
- Unit test structures
- Integration test setups
- Mocking strategies
- Test data generation

### Error Handling Patterns
- Exception handling
- Validation errors
- Retry logic
- Graceful degradation

### Performance Patterns
- Lazy loading
- Code splitting
- Resource optimization
- Background job processing

## Search Tips

### Find patterns by keyword
```bash
grep -r "authentication" patterns/
```

### Find recent patterns
```bash
ls -lt patterns/*.md | head -10
```

### Find patterns by file type
```bash
grep -l "JavaScript" patterns/*.md
```

### Find patterns from specific PR
```bash
grep -l "PR #123" patterns/*.md
```

## Usage Guide

### For AI Agents

When implementing a feature:

1. **Search** for similar patterns by keywords
2. **Read** relevant pattern documents
3. **Adapt** the pattern to your specific context
4. **Reference** the pattern in your PR description
5. **Update** the pattern if you discover improvements

### For Human Developers

1. Browse patterns before implementing new features
2. Contribute new patterns from successful PRs
3. Update patterns when better approaches discovered
4. Archive outdated patterns (mark as superseded)

## Example Patterns

### Example 1: JWT Authentication

```markdown
# JWT Authentication with Refresh Tokens

**Category**: API Design
**Language**: JavaScript (Node.js)
**Created**: 2026-01-06
**Source**: PR #42

## Context
Need secure user authentication for API endpoints with token refresh capability.

## Problem
Sessions don't work well for stateless APIs. Need token-based auth with security and UX balance.

## Solution

### Approach
- Short-lived access tokens (15 min)
- Long-lived refresh tokens (7 days)
- Secure httpOnly cookies
- Token rotation on refresh

### Implementation
```javascript
// middleware/auth.js
const jwt = require('jsonwebtoken');

const generateTokens = (userId) => {
  const accessToken = jwt.sign(
    { userId },
    process.env.ACCESS_SECRET,
    { expiresIn: '15m' }
  );

  const refreshToken = jwt.sign(
    { userId },
    process.env.REFRESH_SECRET,
    { expiresIn: '7d' }
  );

  return { accessToken, refreshToken };
};
```

[... rest of pattern]
```

### Example 2: React Form Validation

```markdown
# Custom React Form Validation Hook

**Category**: Component Pattern
**Language**: JavaScript (React)
**Created**: 2026-01-05
**Source**: PR #38

## Context
Need consistent form validation across application with good UX.

## Problem
Repetitive validation logic in every form component. Inconsistent error handling.

## Solution

### Approach
Custom hook that manages validation state and provides consistent API.

### Implementation
```javascript
// hooks/useFormValidation.js
import { useState } from 'react';

const useFormValidation = (initialValues, validationRules) => {
  const [values, setValues] = useState(initialValues);
  const [errors, setErrors] = useState({});

  const validate = (fieldName, value) => {
    const rule = validationRules[fieldName];
    if (!rule) return null;

    return rule(value);
  };

  // ... rest of hook

  return { values, errors, handleChange, handleSubmit };
};
```

[... rest of pattern]
```

## Contributing

### Adding New Patterns

1. Extract from successful PR
2. Follow template above
3. Include code examples
4. Document when to use/not use
5. Add references to source PR

### Updating Existing Patterns

1. Document version and changes
2. Mark old version as superseded if significant changes
3. Keep history for context
4. Update related patterns

### Deprecating Patterns

Don't delete - mark as deprecated:

```markdown
**Status**: ⚠️ DEPRECATED
**Superseded by**: new-pattern-name.md
**Reason**: [Why this pattern is no longer recommended]
```

## Maintenance

### Regular Review

- Review patterns quarterly
- Update with new learnings
- Consolidate similar patterns
- Archive obsolete patterns

### Quality Checks

- All patterns have code examples
- Clear context and use cases
- Documented tradeoffs
- References to source PRs
- Searchable keywords

---

**Pattern Count**: Auto-updated by workflow
**Last Reviewed**: 2026-01-06
**Maintainers**: @owner, automated workflows
