# Background Agent Clarification Protocol - Examples

Quick reference guide with concrete examples of clarification requests.

---

## Example 1: System Ambiguity

### Ambiguous Task:
```
"Analyze the performance of the API"
```

### Clarification Request (Recommended Format):

```markdown
🔴 CLARIFICATION REQUIRED 🔴

**Question ID:** clr-001-which-api
**Category:** Context
**Question:** Which API should be analyzed?

**Context Found:**
- REST API endpoints in /src/api/
- GraphQL API in /src/graphql/
- Internal RPC API in /src/rpc/

**Need to know:**
A) REST API only
B) GraphQL API only
C) Both REST and GraphQL
D) All three including RPC

**Current Status:** BLOCKED - Waiting for answer
**Blocking since:** 2026-01-11 08:45:23 UTC
```

---

## Example 2: Scope Ambiguity

### Ambiguous Task:
```
"Fix the authentication bugs"
```

### Clarification Request:

```markdown
🔴 CLARIFICATION REQUIRED 🔴

**Question ID:** clr-002-which-bugs
**Category:** Goals
**Question:** Which authentication bugs should be fixed?

**Bugs Found:**
1. Issue #123: OAuth token refresh fails after 24 hours
2. Issue #156: Password reset emails not sent
3. Issue #189: Session cookies expire too quickly
4. Issue #201: 2FA bypass vulnerability (CRITICAL)

**Need to know:**
- Should I fix all bugs? (estimated 8 hours)
- Should I prioritize by severity? (fix #201 first)
- Is there a specific bug number you meant?

**Current Status:** BLOCKED - Waiting for answer
**Blocking since:** 2026-01-11 09:15:42 UTC
```

---

## Example 3: Format Ambiguity

### Ambiguous Task:
```
"Document the database schema"
```

### Clarification Request:

```markdown
🔴 CLARIFICATION REQUIRED 🔴

**Question ID:** clr-003-doc-format
**Category:** Metrics/Deliverables
**Question:** What format should the documentation use?

**Options:**
A) Entity-Relationship Diagram (Mermaid in Markdown)
B) SQL DDL export (CREATE TABLE statements)
C) Documentation comments in migration files
D) Structured markdown tables with relationships
E) OpenAPI/JSON Schema format
F) Database-specific tool (e.g., dbdocs.io)

**Additional Questions:**
- Should this include sample data?
- Should indexes and constraints be documented?
- What level of detail for relationships?

**Current Status:** BLOCKED - Waiting for answer
**Blocking since:** 2026-01-11 09:30:11 UTC
```

---

## Example 4: Multiple Dependencies

### Ambiguous Task:
```
"Prepare the release"
```

### Clarification Request:

```markdown
🔴 CLARIFICATION REQUIRED 🔴

**Question ID:** clr-004-release-prep
**Category:** Actionability
**Question:** What does "prepare the release" include?

**Possible Steps Identified:**
- [ ] Run test suite
- [ ] Update version numbers
- [ ] Generate changelog
- [ ] Build production assets
- [ ] Tag commit in git
- [ ] Create GitHub release
- [ ] Update documentation
- [ ] Notify stakeholders
- [ ] Deploy to staging
- [ ] Deploy to production

**Need to know:**
1. Which steps should I execute? (check all that apply)
2. What version number? (current is v2.3.1)
3. Should I deploy or just prepare artifacts?
4. Are there any blockers? (pending PRs, failed tests, etc.)

**Current Status:** BLOCKED - Waiting for answers
**Blocking since:** 2026-01-11 10:00:00 UTC
```

---

## Example 5: Quality Standards

### Ambiguous Task:
```
"Optimize the database queries"
```

### Clarification Request:

```markdown
🔴 CLARIFICATION REQUIRED 🔴

**Question ID:** clr-005-optimization-scope
**Category:** Metrics/Success Criteria
**Question:** What optimization goals should I target?

**Current Performance Baseline:**
- Average query time: 245ms
- Slowest query: 3.2s (user dashboard)
- 95th percentile: 890ms
- Database size: 12GB

**Need to know:**
1. **Target metrics:**
   - Reduce average to: ___ ms?
   - Reduce slowest to: ___ ms?
   - Improve 95th percentile to: ___ ms?

2. **Optimization scope:**
   - [ ] Add missing indexes
   - [ ] Rewrite inefficient queries
   - [ ] Introduce caching layer
   - [ ] Partition large tables
   - [ ] Denormalize for read performance

3. **Constraints:**
   - Maximum implementation time: ___ hours?
   - Can I change table schemas? (requires migrations)
   - Can I introduce new dependencies? (Redis, etc.)

**Current Status:** BLOCKED - Waiting for answers
**Blocking since:** 2026-01-11 10:15:33 UTC
```

---

## Example 6: File Location Ambiguity

### Ambiguous Task:
```
"Create a configuration file for the deployment"
```

### Clarification Request:

```markdown
🔴 CLARIFICATION REQUIRED 🔴

**Question ID:** clr-006-config-location
**Category:** Context + Deliverables
**Question:** Where should the configuration file be created and what format?

**Project Structure Found:**
```
/config/
  ├── development.json
  ├── staging.json
  └── production.json
/deploy/
  ├── kubernetes/
  │   └── *.yaml
  └── docker/
      └── docker-compose.yml
/.env.example
```

**Need to know:**
1. **Location:**
   - A) Add to /config/ directory (JSON format)
   - B) Add to /deploy/kubernetes/ (YAML format)
   - C) Add to /deploy/docker/ (Docker Compose format)
   - D) Create as .env file
   - E) Other: _______________

2. **Environment:**
   - Development
   - Staging
   - Production
   - All three (create separate files)

3. **What should be configured:**
   - [ ] Database connection strings
   - [ ] API keys and secrets
   - [ ] Resource limits (CPU, memory)
   - [ ] Feature flags
   - [ ] Service endpoints
   - [ ] Other: _______________

**Current Status:** BLOCKED - Waiting for answers
**Blocking since:** 2026-01-11 10:30:45 UTC
```

---

## Best Practices for Clarification Requests

### DO:

✓ **Be specific** - Reference exact files, line numbers, or options found
✓ **Show context** - Demonstrate you've explored and identified options
✓ **Provide structure** - Use checkboxes, numbering, or multiple choice
✓ **Include timestamps** - Help parent agent prioritize blocked agents
✓ **Suggest defaults** - "I'll assume X unless told otherwise"
✓ **Estimate impact** - "Affects N files" or "Estimated M hours"

### DON'T:

✗ **Ask vague questions** - "What should I do?" (too broad)
✗ **Ask one thing at a time** - Bundle related questions together
✗ **Proceed with guesses** - If blocked, stay blocked until clarified
✗ **Hide assumptions** - Make implicit assumptions explicit
✗ **Skip reconnaissance** - Show you've explored before asking

---

## Decision Framework: When to Ask vs. Proceed

```
Is the requirement ambiguous?
  │
  ├─ NO ──> Proceed with implementation
  │
  └─ YES ──> Can I make a reasonable assumption?
              │
              ├─ YES ──> Document assumption clearly + Proceed
              │           (flag for review in output)
              │
              └─ NO ──> BLOCK and request clarification
                        (multiple interpretations possible)
```

### Examples:

| Scenario | Decision | Rationale |
|----------|----------|-----------|
| "Fix typos in docs" | Proceed | Typos are objective errors |
| "Improve error messages" | Ask | "Improve" is subjective |
| "Add logging to API" | Ask | Where? What level? What format? |
| "Update README version" | Proceed | Version likely in package.json |
| "Refactor the codebase" | Ask | Massive scope ambiguity |
| "Run the tests" | Proceed | Test command usually standard |

---

## Template for Clarification Requests

Copy/paste this template:

```markdown
🔴 CLARIFICATION REQUIRED 🔴

**Question ID:** clr-[number]-[short-desc]
**Category:** [Context|Goals|Metrics|Actionability]
**Question:** [Your specific question]

**Context Found:**
[What you discovered while investigating]

**Options Identified:**
A) [Option 1]
B) [Option 2]
C) [Option 3]

**Need to know:**
1. [First decision point]
2. [Second decision point]

**Impact Assessment:**
- Affects: [files/systems/users]
- Estimated time: [hours]
- Risk level: [low/medium/high]

**Current Status:** BLOCKED - Waiting for answer
**Blocking since:** [ISO timestamp]
```

---

## Clarification Response Format (For Parent Agents)

When answering clarification requests:

```markdown
✓ CLARIFICATION ANSWER

**Question ID:** clr-003-doc-format
**Answer:** Option D - Structured markdown tables with relationships

**Additional Context:**
- Include indexes and constraints
- Skip sample data for now
- Focus on foreign key relationships
- Target audience: new developers

**Priority:** High - unblock agent immediately
**Answered by:** [parent-agent-id or username]
**Timestamp:** 2026-01-11 09:35:22 UTC
```

---

## Next Steps

1. **For agents that need clarification:**
   - Use template above
   - Output to stdout (visible in TaskOutput)
   - Set internal state to BLOCKED
   - Poll for answer or exit

2. **For parent agents monitoring:**
   - Run `TaskOutput agent-id=X block=false` regularly
   - Parse output for 🔴 marker
   - Use AskUserQuestion to get answer from human
   - Communicate answer back to agent

3. **For users:**
   - Review clarification requests promptly
   - Provide complete answers (not just "yes")
   - Consider adding answers to agent's context for re-launch

---

**See Also:** CLARIFICATION_PROTOCOL_TEST.md for full analysis and implementation recommendations
