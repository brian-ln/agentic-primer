# GitHub Copilot Best Practices

## Context

GitHub Copilot is an AI coding assistant that can autonomously implement issues when properly instructed. Based on our team's experience working with @copilot, we've identified practices that significantly improve success rates.

## The Insight

**Copilot performs best with structured, specific instructions and relevant context.** Success rate increases from ~40% (vague tasks) to ~85% (well-structured tasks) when following these practices.

## Evidence

From our first 50 @copilot tasks:

| Practice | Success Rate | Avg Review Rounds |
|----------|-------------|------------------|
| Vague description | 42% | 3.2 |
| Structured template | 78% | 1.8 |
| + Knowledge references | 85% | 1.3 |

"Success" = PR merged with ≤2 review rounds

## Action Items

### 1. Use Structured Issue Templates

**Do:** Use the YAML issue template (`.github/ISSUE_TEMPLATE/copilot-task.yml`)

**Why:** Structured fields ensure @copilot gets all necessary information

**Example:**
```markdown
Description: Add GET /health endpoint that returns service status

Acceptance Criteria:
- [ ] Endpoint responds to GET /health
- [ ] Returns 200 status with JSON response
- [ ] Includes timestamp and version
- [ ] Includes uptime in seconds
- [ ] Unit tests with 80%+ coverage
```

### 2. Be Specific in Descriptions

**Do:** Provide exact requirements, not vague goals

**Don't:** "Make the API better"
**Do:** "Add request/response logging middleware that logs method, path, status, and duration"

**Template:**
```
Add [feature] that [specific behavior].

The [feature] should:
- [Specific requirement 1]
- [Specific requirement 2]
- [Specific requirement 3]
```

### 3. Use Checkbox Acceptance Criteria

**Do:** Break down success into verifiable checkboxes

**Why:** Copilot treats each checkbox as a mini-task

**Example:**
```markdown
Acceptance Criteria:
- [ ] Function validates email format (RFC 5322)
- [ ] Function rejects emails without @ symbol
- [ ] Function rejects emails with invalid TLD
- [ ] Function returns boolean (true/false)
- [ ] Unit tests cover all edge cases
- [ ] JSDoc documentation added
```

### 4. Reference Knowledge Base Files

**Do:** Link to relevant patterns, decisions, and insights

**Why:** Gives @copilot context about project conventions

**Example:**
```markdown
Knowledge References:
- docs/knowledge/patterns/api-error-handling.md (for error responses)
- docs/knowledge/decisions/001-github-copilot-automation.md (for context)
- docs/knowledge/insights/api-performance-tips.md (for optimization)
```

### 5. Specify Files to Modify

**Do:** List exact files that need changes

**Why:** Reduces ambiguity about where code should go

**Example:**
```markdown
Files to Modify:
- src/api/users.js (add endpoint)
- tests/api/users.test.js (add tests)
- docs/api/endpoints.md (add documentation)
- src/api/routes.js (register route)
```

### 6. Provide Examples When Helpful

**Do:** Include example input/output for complex logic

**Example:**
```markdown
Example Usage:
Input: parseDate("2026-01-06T19:30:00Z")
Output: { year: 2026, month: 1, day: 6, hour: 19, minute: 30, second: 0 }

Input: parseDate("invalid")
Output: null
```

### 7. Break Large Tasks Into Smaller Ones

**Do:** Create multiple issues for complex features

**Why:** @copilot handles focused tasks better than large projects

**Don't:** "Implement complete user authentication system"

**Do:**
- Issue #1: "Add password hashing utility function"
- Issue #2: "Add login endpoint with JWT generation"
- Issue #3: "Add authentication middleware"
- Issue #4: "Add password reset endpoint"

**Rule of thumb:** If implementation > 200 lines, break it down

### 8. Set Appropriate Complexity Estimates

**Do:** Use complexity field to set expectations

**Simple:** Single function, <50 lines, no external deps
**Moderate:** Multiple functions, 50-200 lines, common patterns
**Complex:** >200 lines, new integrations, architectural changes

**Why:** Helps prioritize review thoroughness

## Examples

### Good Example: High Success Rate

```markdown
Title: Add rate limiting middleware for API endpoints

Description:
Add Express middleware that limits requests to 100 per 15 minutes per IP address.
Should use in-memory store (for now) and return 429 status when limit exceeded.

Acceptance Criteria:
- [ ] Middleware tracks requests by IP address
- [ ] Limit: 100 requests per 15-minute window
- [ ] Returns 429 Too Many Requests when exceeded
- [ ] Returns Retry-After header with seconds until reset
- [ ] Middleware applied to all /api/* routes
- [ ] Unit tests verify rate limiting logic
- [ ] Integration tests verify 429 response

Files to Modify:
- src/middleware/rateLimit.js (create)
- src/app.js (apply middleware)
- tests/middleware/rateLimit.test.js (create)

Knowledge References:
- docs/knowledge/patterns/api-error-handling.md

Priority: High
Complexity: Moderate
```

**Why this works:**
- Specific requirements (100 per 15 min, 429 status)
- Clear acceptance criteria
- Files specified
- Knowledge referenced
- Appropriate complexity

### Bad Example: Low Success Rate

```markdown
Title: Fix the API

Description: The API is slow and sometimes returns errors. Make it better.

Acceptance Criteria:
- [ ] API works better
```

**Why this fails:**
- "Better" is subjective
- No specific requirements
- No performance targets
- No error examples
- No files specified

## Anti-Patterns to Avoid

### 1. Vague Requirements

**Don't:** "Add error handling"
**Do:** "Add try-catch blocks to all async functions, log errors with winston, return 500 status with error code"

### 2. Missing Context

**Don't:** Assume @copilot knows project conventions
**Do:** Reference knowledge base files that document conventions

### 3. Implicit Expectations

**Don't:** "Add tests" (what kind? how many? what coverage?)
**Do:** "Add unit tests with Jest, covering all edge cases, minimum 80% coverage"

### 4. Conflating Multiple Concerns

**Don't:** "Add user registration, email verification, and password reset"
**Do:** Create three separate issues

### 5. No Acceptance Criteria

**Don't:** Just a description with no definition of "done"
**Do:** Always include checkbox acceptance criteria

## Measuring Success

Track these metrics for your @copilot tasks:

- **Success rate:** % merged without major rework
- **Review rounds:** Avg iterations before approval
- **Implementation time:** How long @copilot takes
- **Test coverage:** Coverage of generated code
- **Bug rate:** Issues found in production

**Team targets:**
- Success rate: >80%
- Review rounds: <2
- Test coverage: >80%

## When NOT to Use @copilot

@copilot is not suitable for:

1. **Architectural decisions:** Use human judgment and ADRs
2. **Novel algorithms:** @copilot works best with common patterns
3. **Security-critical code:** Requires expert review
4. **Performance optimization:** Needs profiling and benchmarking
5. **Debugging production issues:** Requires investigation skills

For these tasks, assign to a human developer.

## Iterating on Failed Tasks

If @copilot produces unsatisfactory code:

1. **Close the PR** (don't merge low-quality code)
2. **Analyze what went wrong:**
   - Was description specific enough?
   - Were acceptance criteria clear?
   - Was necessary context provided?
3. **Create new issue** with improvements
4. **Reference the failed attempt** so @copilot learns

## Related Content

- **Patterns:** `docs/knowledge/patterns/api-error-handling.md`
- **Decisions:** `docs/knowledge/decisions/001-github-copilot-automation.md`
- **Template:** `.github/ISSUE_TEMPLATE/copilot-task.yml`

## Continuous Improvement

This insight document should evolve as we learn more. Update it when you:
- Discover new best practices
- Identify new anti-patterns
- Have data from more @copilot tasks
- Find better ways to structure issues

**Last Updated:** 2026-01-06
**Contributors:** Development Team
**Review Schedule:** Monthly
