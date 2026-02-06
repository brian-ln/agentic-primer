# Issue-Driven Development with @copilot

This project demonstrates an automated issue-driven development system where @copilot autonomously processes GitHub issues and creates pull requests for human review.

## Quick Start

### For Creating Tasks (Human)
1. Create a new issue using the [@copilot Task Template](.github/ISSUE_TEMPLATE/task.yml)
2. Fill in all required fields:
   - Task Description
   - Acceptance Criteria
   - Priority
   - Issue Type
3. Assign to: `@copilot`
4. Add label: `copilot`
5. Submit the issue

The system will automatically:
- Parse the issue template
- Check the knowledge base for relevant patterns
- Generate implementation code
- Create a pull request
- Auto-assign for review based on `CODEOWNERS`

### For Reviewing PRs (Human)
1. GitHub notification arrives when @copilot creates a PR
2. Open the PR in GitHub web UI
3. Review code changes:
   - Does it solve the issue?
   - Is implementation consistent with codebase?
   - Are tests included?
4. Add comments or approve
5. If approved, merge or request changes

### For Understanding the System (@copilot)
Refer to the knowledge base in `docs/knowledge/` for patterns, decisions, and insights.

---

## Issue-to-PR Workflow

```
┌─────────────────────────────────────────────────────────────┐
│  STEP 1: HUMAN CREATES ISSUE                                │
│                                                               │
│  - Uses .github/ISSUE_TEMPLATE/task.yml template             │
│  - Provides clear description and acceptance criteria       │
│  - Assigns to @copilot                                       │
│                                                               │
│  Example: "Add deprecation warning to /api/v1/users"        │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 2: @COPILOT RECEIVES ISSUE                            │
│                                                               │
│  @copilot webhook/automation:                               │
│  1. Receives issue payload                                   │
│  2. Parses template structure                                │
│  3. Extracts: title, description, acceptance criteria       │
│  4. Reads issue type and priority                            │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 3: CONSULT KNOWLEDGE BASE                             │
│                                                               │
│  @copilot searches:                                          │
│  - docs/knowledge/patterns/ for relevant patterns           │
│  - docs/knowledge/decisions/ for architectural context      │
│  - docs/knowledge/insights/ for lessons learned             │
│                                                               │
│  If issue mentions "deprecation", finds and uses the        │
│  "API Deprecation Warning" pattern                           │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 4: IMPLEMENT SOLUTION                                 │
│                                                               │
│  @copilot:                                                   │
│  1. Plans implementation based on pattern                    │
│  2. Creates feature branch                                   │
│  3. Implements changes:                                      │
│     - Code modifications                                     │
│     - Tests (unit + integration)                             │
│     - Documentation updates                                  │
│  4. Verifies acceptance criteria are met                     │
│  5. Commits with clear message                               │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 5: CREATE PULL REQUEST                                │
│                                                               │
│  @copilot creates PR with:                                   │
│  - Clear title and description                               │
│  - Link to original issue (#123)                             │
│  - Checklist of implementation steps                         │
│  - Test results                                              │
│  - Knowledge base references                                 │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 6: AUTO-ASSIGNMENT VIA CODEOWNERS                     │
│                                                               │
│  GitHub CODEOWNERS rules automatically request review from:  │
│  - Matching path owners (@owner, @frontend-team, etc.)      │
│  - Ensures appropriate subject-matter expertise             │
│  - Prevents bottlenecks                                      │
│                                                               │
│  Example: Changes to src/api/ → @backend-team @owner       │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 7: HUMAN REVIEW & FEEDBACK                            │
│                                                               │
│  Reviewer opens PR in GitHub web UI:                         │
│  - ✅ Review code changes                                     │
│  - ✅ Run checks and tests                                    │
│  - ✅ Add comments or approve                                 │
│  - ✅ Request changes if needed                               │
│                                                               │
│  Feedback options:                                           │
│  A) APPROVE → Go to Step 8                                   │
│  B) REQUEST CHANGES → @copilot responds in issue comments   │
│                                                               │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 8: MERGE & CLOSE                                      │
│                                                               │
│  When approved:                                              │
│  - Reviewer clicks "Merge" in GitHub web UI                 │
│  - PR merges to main branch                                  │
│  - Issue automatically closes                                │
│  - Changes go to production via normal deployment pipeline   │
│                                                               │
│  Deployment:                                                 │
│  - CI/CD pipeline runs on main branch                        │
│  - All checks pass                                           │
│  - Code deploys to production                                │
└─────────────────────────────────────────────────────────────┘
```

---

## Knowledge Base Structure

The project includes a structured knowledge base at `docs/knowledge/` to ensure consistent implementation across all tasks:

### `docs/knowledge/patterns/` - Implementation Patterns
Proven solutions for common implementation tasks:
- **API Deprecation Warning** - How to deprecate an endpoint with smooth migration
- **Configuration Management** - Environment-specific config with validation
- **Error Handling** - Consistent error responses and logging
- **Database Migrations** - Schema changes with up/down migrations

Use these patterns when implementing similar features. If your task doesn't match an existing pattern, create a new one.

### `docs/knowledge/decisions/` - Architecture Decision Records (ADRs)
Important architectural decisions and trade-offs:
- **ADR-001**: Monorepo vs Polyrepo (we chose monorepo)
- **ADR-002**: TypeScript vs JavaScript (we chose TypeScript)
- **ADR-003**: Testing Strategy (unit + integration tests)
- **ADR-004**: Logging and Observability (structured logging with correlation IDs)

Understand these decisions before proposing architectural changes.

### `docs/knowledge/insights/` - Project-Specific Learnings
Lessons learned from experience:
- **Testing Insights** - Practical testing lessons (isolation, mocking, async handling)
- **Performance Considerations** - Caching strategies and optimization patterns
- **Common Pitfalls** - Issues to avoid based on past experience

Review relevant insights before starting implementation.

### `docs/knowledge/STRUCTURE.md`
Navigation guide for the knowledge base and instructions for adding new patterns, decisions, and insights.

---

## For @copilot: Processing a Task

1. **Parse the Issue**
   - Read the task.yml template fields
   - Extract: description, acceptance criteria, priority, type

2. **Consult Knowledge Base**
   - Search patterns/ for similar tasks
   - Check decisions/ for architectural context
   - Review insights/ for lessons learned

3. **Create Implementation Plan**
   - Map acceptance criteria to code changes
   - Identify files to modify
   - Plan test coverage

4. **Implement**
   - Create feature branch
   - Write code following patterns
   - Add tests
   - Update documentation

5. **Create Pull Request**
   ```
   Title: Implement <task description>

   Closes #<issue-number>

   ## Summary
   <What was implemented and why>

   ## Changes
   - File 1: <change description>
   - File 2: <change description>

   ## Testing
   - [ ] Unit tests added/updated
   - [ ] Integration tests added/updated
   - [ ] All tests passing

   ## Related Documentation
   - Pattern: docs/knowledge/patterns/xxx
   - Decision: docs/knowledge/decisions/adr-xxx

   ## Acceptance Criteria Met
   - [x] Criterion 1
   - [x] Criterion 2
   - [x] Criterion 3
   ```

6. **Verify**
   - All tests pass
   - Code follows patterns
   - Documentation updated
   - Acceptance criteria met

---

## CODEOWNERS - Automatic Reviewer Assignment

The `CODEOWNERS` file controls which users/teams are automatically requested for review when PRs touch their areas:

```
# All changes
* @owner

# Frontend changes
src/ui/** @frontend-team @owner

# Backend changes
src/api/** @backend-team @owner

# Database changes
src/db/** @database-team @owner
migrations/** @database-team @owner

# Documentation
docs/** @owner
```

GitHub automatically requests reviews from matching paths when PRs are created. No manual assignment needed.

---

## Example: Adding a Feature

### Issue Created
```
Title: Add deprecation warning to legacy API endpoint

Description:
The /api/v1/users endpoint is deprecated. We need to warn clients about the migration path.

Acceptance Criteria:
- [ ] Endpoint logs deprecation warning on each call
- [ ] HTTP Deprecation header is set in response
- [ ] Tests verify warning is issued
- [ ] Documentation updated with migration guide

Priority: High
Issue Type: Feature
```

### @copilot Processes:
1. **Finds pattern**: `docs/knowledge/patterns/implementation-patterns.md#api-deprecation-warning`
2. **Reviews decision**: `docs/knowledge/decisions/adr-004-logging.md` for logging approach
3. **Implements**:
   - Updates endpoint handler
   - Adds deprecation header
   - Adds structured logging
   - Creates tests
   - Updates migration documentation
4. **Creates PR** with reference to pattern and decision

### Reviewer Actions:
1. Opens PR in GitHub web UI
2. Reviews changes against pattern
3. Checks tests
4. Approves or requests changes
5. Merges to main

---

## Files in This System

- `.github/ISSUE_TEMPLATE/task.yml` - Structured issue template for @copilot
- `CODEOWNERS` - Automatic reviewer assignment rules
- `docs/knowledge/STRUCTURE.md` - Knowledge base navigation
- `docs/knowledge/patterns/implementation-patterns.md` - Reusable patterns
- `docs/knowledge/decisions/adr-001-architecture.md` - Architecture decisions
- `docs/knowledge/insights/testing-insights.md` - Lessons learned
- `README.md` - This file, workflow documentation

---

## Success Criteria

This system successfully demonstrates:

- ✅ **Issue Template**: Structured input that can be reliably parsed
- ✅ **@copilot Processing**: Can read issues and understand requirements
- ✅ **Knowledge Base**: Contains patterns, decisions, insights for consistent work
- ✅ **PR Creation**: Generates pull requests with clear descriptions
- ✅ **Auto-Assignment**: CODEOWNERS routes PRs to appropriate reviewers
- ✅ **Human Review**: Web UI workflow for feedback and approval
- ✅ **Workflow Documentation**: Clear end-to-end process for humans and machines

---

## Next Steps

To use this system:

1. **Create a test issue** using the task template
2. **@copilot processes** the issue (in this simulation, documented in PR)
3. **Reviewer checks** the PR in GitHub web UI
4. **Approve and merge** to complete the workflow

The knowledge base (`docs/knowledge/`) grows as new patterns, decisions, and insights are discovered.
