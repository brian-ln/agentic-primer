# Copilot Bootstrap Simulation: 14-Word Prompt Analysis

**Date:** 2026-01-05
**Scenario:** GitHub Copilot (@copilot) receives 14-word bootstrap request in bare repository
**Request:** "Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base."

---

## What Copilot WOULD Create

### File Inventory

Copilot would create **5 primary files** in this order:

1. `.github/ISSUE_TEMPLATE/task.yml` - Issue template
2. `.github/CODEOWNERS` - Auto-assign configuration
3. `.github/workflows/copilot-task.yml` - Automation workflow
4. `README.md` - Usage documentation
5. `docs/knowledge/README.md` - Knowledge base structure

---

## Detailed File Analysis

### 1. `.github/ISSUE_TEMPLATE/task.yml` (48-65 lines)

**What Copilot would infer:**

The request mentions "issue-driven development" + "@copilot". Copilot would recognize this means:
- GitHub Issues should become work items
- Copilot processes them via mentions or assignment
- Structure via YAML template (GitHub standard)

**File content Copilot would create:**

```yaml
name: Development Task
description: Task for Copilot to execute
labels: [copilot-task]
body:
  - type: input
    id: title
    attributes:
      label: Task Title
      placeholder: "What needs to be done?"
    validations:
      required: true

  - type: textarea
    id: description
    attributes:
      label: Description
      placeholder: "Detailed explanation of the task..."
      value: |
        ## Background

        ## What needs to be done

        ## Context
    validations:
      required: true

  - type: textarea
    id: acceptance_criteria
    attributes:
      label: Acceptance Criteria
      placeholder: "How to verify this is complete"
      value: |
        - [ ] Requirement 1
        - [ ] Requirement 2
        - [ ] Requirement 3
    validations:
      required: true

  - type: textarea
    id: notes
    attributes:
      label: Additional Notes (optional)
      placeholder: "Links, context, constraints..."
    validations:
      required: false
```

**Copilot reasoning:**
- "Issue-driven development" → template needed
- "Development task" is standard GitHub terminology
- Required fields: title (implicit), description, acceptance criteria
- Optional: notes for context
- Auto-label with `copilot-task` to trigger workflow

**Completeness:** 85% - Template is usable but misses:
- Priority field (would help triage)
- Estimated effort (would help planning)
- Dependency tracking (issues blocking issues)

---

### 2. `.github/CODEOWNERS` (3-5 lines)

**What Copilot would infer:**

Request explicitly states: "Auto-assign PRs to owner"

Copilot knows CODEOWNERS triggers automatic review assignment on PRs.

**File content Copilot would create:**

```
# All PRs auto-assign to repository owner
* @USERNAME
```

**Copilot reasoning:**
- "Owner" = repository owner (inferred from context)
- CODEOWNERS is GitHub's standard mechanism
- Wildcard `*` catches all files
- `@USERNAME` = repository owner (Copilot would prompt for this OR note it needs manual replacement)

**Problem:** Copilot likely wouldn't know the actual username. Would either:
- Prompt: "What's your GitHub username?"
- Leave placeholder: `* @OWNER` (requires manual edit)
- Create as conditional: note in README to replace

**Completeness:** 70% - Works but requires manual username substitution

---

### 3. `.github/workflows/copilot-task.yml` (45-80 lines)

**What Copilot would infer:**

The 14-word request doesn't explicitly say "create workflow", but:
- "issue-driven development" + "@copilot" implies automation
- "Auto-assign" implies some triggering mechanism
- Copilot knows GitHub Actions is the standard tool

**File content Copilot would create:**

```yaml
name: Copilot Task Automation
on:
  issues:
    types: [opened, labeled, assigned]
  issue_comment:
    types: [created]

permissions:
  contents: write
  pull-requests: write
  issues: write
  repository-projects: read

jobs:
  process_copilot_task:
    if: |
      (github.event.action == 'opened' ||
       contains(github.event.issue.labels.*.name, 'copilot-task') ||
       github.event.issue.assignee.login == 'copilot')

    runs-on: ubuntu-latest

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Get Issue Details
        id: issue
        run: |
          echo "title=${{ github.event.issue.title }}" >> $GITHUB_OUTPUT
          echo "body=${{ github.event.issue.body }}" >> $GITHUB_OUTPUT
          echo "number=${{ github.event.issue.number }}" >> $GITHUB_OUTPUT

      - name: Comment with Acknowledgment
        uses: actions/github-script@v7
        with:
          script: |
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: '✓ Task received. Copilot will begin work...'
            })

      - name: Create Task Branch
        run: |
          git config user.name "copilot-bot"
          git config user.email "copilot@github.com"
          git checkout -b copilot/issue-${{ steps.issue.outputs.number }}

      - name: Mark Issue as In Progress
        uses: actions/github-script@v7
        with:
          script: |
            github.rest.issues.addLabels({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              labels: ['in-progress']
            })
```

**Copilot reasoning:**
- Triggers: `opened` (new issue), `labeled` (add copilot-task), `assigned` (assign to copilot)
- Also handles `issue_comment` for mentions like "@copilot help"
- Creates feature branch for each issue
- Adds comments to track progress
- Adds labels for workflow visibility

**Limitations Copilot would NOT address:**
- Actually calling Copilot API (would need authentication)
- Executing the real task implementation
- Creating the PR (partially stubbed)
- Error handling and rollback

**Completeness:** 30-40% - Skeleton is created but NOT functional. Would need:
- API integration to call actual Copilot
- Implementation execution logic
- PR creation with real changes
- Error handling and retry logic

---

### 4. `README.md` (60-100 lines)

**What Copilot would infer:**

Standard practice: README explains what users do next.

The 14-word request implies:
1. Users create issues
2. Copilot processes them
3. PRs are created/assigned automatically

**File content Copilot would create:**

```markdown
# Issue-Driven Development with Copilot

This repository uses GitHub Issues as executable work items. Create an issue, mention @copilot, and let automation handle the rest.

## Quick Start

### 1. Create an Issue

Use the "Development Task" template:

1. Go to **Issues** → **New Issue**
2. Select **"Development Task"** template
3. Fill in:
   - **Task Title**: Brief description
   - **Description**: Detailed explanation
   - **Acceptance Criteria**: How to verify completion
4. Click **Create Issue**

### 2. Trigger Copilot

Either:
- **Mention:** Comment on issue: `@copilot help`
- **Label:** Add `copilot-task` label
- **Assign:** Assign issue to @copilot

### 3. Wait for PR

Copilot will:
1. Analyze the issue
2. Create a feature branch
3. Implement the solution
4. Create a PR with the changes
5. PR is auto-assigned to you (CODEOWNERS)

### 4. Review & Merge

1. Review the PR in GitHub web UI
2. Request changes if needed (comment)
3. Approve and merge

## How It Works

```
Issue Created
    ↓
Copilot triggers (label/mention/assign)
    ↓
Workflow reads issue details
    ↓
Copilot analyzes requirements
    ↓
Copilot creates feature branch & implements
    ↓
PR created automatically
    ↓
CODEOWNERS auto-requests your review
    ↓
You review and merge
```

## Requirements

- GitHub repository with Actions enabled
- GitHub Copilot subscription
- CODEOWNERS file configured (already done)

## Troubleshooting

### Copilot doesn't respond

**Check:**
1. Does the issue have the `copilot-task` label?
2. Is Copilot mentioned in a comment?
3. Is the issue assigned to @copilot?

### PR not created

Check GitHub Actions logs:
```
Issues → Your Issue → Workflow runs → [Run name] → logs
```

## Knowledge Base

See `docs/knowledge/README.md` for patterns, decisions, and learnings discovered during development.

## Support

For issues with this automation, create an issue with label `infra` instead of `copilot-task`.
```

**Copilot reasoning:**
- "Quick Start" pattern (standard UX)
- Clear step-by-step flow
- Troubleshooting section (common need)
- Links to existing files (CODEOWNERS, knowledge base)
- Visual workflow diagram

**Completeness:** 75% - Functional but assumes:
- Users know GitHub web UI
- Copilot subscription exists
- CODEOWNERS already configured
- Doesn't explain: how to set up API key, Copilot workspace, etc.

---

### 5. `docs/knowledge/README.md` (30-50 lines)

**What Copilot would infer:**

Request explicitly includes: "Include knowledge base"

Copilot would interpret this as:
- Structured docs directory for learnings
- Best practice pattern in enterprise repos

**File content Copilot would create:**

```markdown
# Knowledge Base

This directory stores patterns, decisions, and insights discovered during issue-driven development.

## Purpose

- **Capture:** Document patterns and learnings as we discover them
- **Reuse:** Reference patterns in future issues
- **Decide:** Record architectural decisions
- **Learn:** Share observations and constraints

## Structure

### `/patterns`

Reusable patterns discovered in implementation.

**File format:** `PATTERN-NAME.md`

Example:
- `github-actions-caching.md` - How to cache dependencies in GH Actions
- `testing-with-mocks.md` - Mocking patterns for unit tests
- `error-handling-http.md` - HTTP error handling pattern

### `/decisions`

Architecture Decision Records (ADRs) - "Why we chose X over Y"

**File format:** `ADR-NNN-TITLE.md`

Structure:
```
# ADR-001: Use GitHub Actions for CI/CD

## Status: Accepted

## Context
[Why this decision was needed]

## Decision
[What we decided]

## Consequences
[Trade-offs and impact]
```

### `/insights`

Learnings and observations - less formal than ADRs.

**File format:** `DATE-TITLE.md` or `PROJECT-TITLE.md`

Examples:
- `2026-01-scaling-issues.md` - Observed bottleneck with large batches
- `2026-01-memory-profiling.md` - How to profile memory in GitHub Actions

## Contributing

When you implement a task:

1. **Extract pattern:** If you discover a reusable approach, create `patterns/PATTERN-NAME.md`
2. **Record decision:** If you chose between options, create `decisions/ADR-NNN.md`
3. **Note learning:** If you discover a constraint or best practice, create `insights/TITLE.md`

## Using the Knowledge Base

When creating issues, reference relevant docs:

```
## References
- See `patterns/github-actions-caching.md` for optimization approach
```

This prevents re-solving solved problems.
```

**Copilot reasoning:**
- "Knowledge base" = docs structure (not code)
- Enterprise pattern: capture decisions and patterns
- Organize by: patterns, decisions, insights
- ADR format is industry standard
- Make it easy to contribute

**Completeness:** 80% - Well-structured, but:
- No automation to surface/cross-link knowledge
- No process for deprecating outdated patterns
- No search/discovery mechanism

---

## Summary Table: What Would Be Created

| File | Lines | Completeness | Key Gap |
|------|-------|--------------|---------|
| `.github/ISSUE_TEMPLATE/task.yml` | 48 | 85% | Missing priority, effort, dependencies |
| `.github/CODEOWNERS` | 3 | 70% | Requires manual username substitution |
| `.github/workflows/copilot-task.yml` | 65 | 35% | No actual Copilot API integration or implementation logic |
| `README.md` | 95 | 75% | Assumes prerequisites; doesn't explain Copilot setup |
| `docs/knowledge/README.md` | 45 | 80% | No automation for cross-linking or discovery |

**Total files created:** 5
**Total lines of code/config:** ~256
**Average completeness:** 69%

---

## What Copilot Would MISS or INFER INCORRECTLY

### 1. **Workflow Execution** (Critical Gap)

The request says "Setup issue-driven development" but doesn't explain how Copilot actually runs.

Copilot would likely:
- ✅ Create the workflow trigger
- ❌ NOT implement the actual execution engine
- ❌ NOT call Copilot API
- ❌ NOT generate code from requirements
- ❌ NOT create functional PR

**Why:** The 14 words don't specify _how_ Copilot integrates. Copilot would create a skeleton but leave the core logic as TODO comments.

### 2. **Authentication & Secrets**

The request doesn't mention API keys.

Copilot would likely:
- ✅ Note in README: "Set up GitHub Copilot subscription"
- ❌ NOT explain where ANTHROPIC_API_KEY should come from
- ❌ NOT create workflow that authenticates to external APIs
- ❌ NOT handle token rotation or scoping

**Why:** The 14 words focus on GitHub integration, not external AI APIs.

### 3. **Multi-Agent Support**

The request is specific to "@copilot" but doesn't mention:
- What if you want multiple agents (Claude, Gemini, etc.)?
- What if your org uses different agents for different tasks?

Copilot would:
- ✅ Hardcode "@copilot" only
- ❌ NOT create extensible architecture
- ❌ NOT support agent routing/selection
- ❌ NOT handle fallback agents

**Why:** Request is Copilot-specific. Would need extended prompt to address multi-agent.

### 4. **Error Handling & Retries**

The request doesn't mention failure cases.

Copilot would:
- ✅ Create basic workflow
- ❌ NOT add retry logic
- ❌ NOT handle API timeouts
- ❌ NOT create issue comments on failure
- ❌ NOT log detailed diagnostics

**Why:** 14 words focus on happy path only.

### 5. **Knowledge Base Integration**

The request includes "knowledge base" but doesn't explain how it's used.

Copilot would:
- ✅ Create directory structure
- ❌ NOT explain how tasks reference it
- ❌ NOT create automation to suggest relevant patterns
- ❌ NOT link completed issues to relevant docs
- ❌ NOT create PR that updates docs

**Why:** "Include knowledge base" is passive; doesn't specify _how_ it's integrated into workflow.

---

## Quality Assessment: 14-Word vs 10-Word Request

### Pressure Test Findings

**14-word request:** "Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base."

**Clarity score:** 7/10

**What the 14 words provide:**
- ✅ Explicit mention of "@copilot" (agent selection)
- ✅ Explicit mention of "auto-assign PRs to owner" (CODEOWNERS requirement)
- ✅ Explicit mention of "knowledge base" (docs structure)
- ❌ Does NOT explain workflow execution
- ❌ Does NOT mention authentication
- ❌ Does NOT clarify template structure
- ❌ Does NOT specify which files to create
- ❌ Does NOT mention README necessity

**What would be needed for 90%+ completeness:** 30+ words:

```
Setup issue-driven development with GitHub Issues as executable work items.
Include GitHub Copilot for task processing.
Auto-assign PRs to repository owner via CODEOWNERS.
Create issue template with title, description, acceptance criteria.
Add GitHub Actions workflow that listens for issue events.
Include knowledge base structure in docs/knowledge with patterns, decisions, insights.
Create README explaining the workflow and quick start.
```

---

## Simulation Conclusion

### Would Copilot Complete This Successfully?

**Short answer:** 40% success rate

**What would work:**
- ✅ Issue template (usable)
- ✅ CODEOWNERS (functional, but requires manual edit)
- ✅ README (clear documentation)
- ✅ Knowledge base structure (well-organized)

**What would NOT work:**
- ❌ Workflow would be non-functional skeleton
- ❌ No Copilot API integration
- ❌ No code generation/implementation
- ❌ No PR creation logic
- ❌ No authentication setup

**Why the gap:** The 14-word request doesn't specify HOW Copilot integrates with the workflow. It only specifies WHAT files to create (implicitly) and functional requirements (auto-assign, knowledge base).

### Comparison: 14 Words vs 10 Words

| Aspect | 10 Words | 14 Words | Winner |
|--------|----------|----------|--------|
| Specificity | Low | Medium | 14 words |
| Agent clarity | Not specified | @copilot explicit | 14 words |
| PR handling | Unclear | Auto-assign to owner | 14 words |
| Knowledge base | Missing | Explicit | 14 words |
| Workflow details | Not mentioned | Not mentioned | Tie |
| Completeness | ~30% | ~40% | 14 words |

**Verdict:** **14 words is better for pressure test**, but both fall short without explanation of:
1. Workflow execution mechanism
2. Authentication/API integration
3. How knowledge base is used in task flow
4. Error handling and recovery

For a 90%+ complete bootstrap, need 40+ carefully-chosen words, OR follow an established template/pattern that Copilot recognizes.

---

## What This Reveals About Prompting

### The Gap Between Implicit and Explicit

**Copilot's challenge with 14 words:**
- Knows GitHub standards (issue templates, CODEOWNERS, workflows)
- Knows Copilot exists and how it's used
- BUT doesn't know if you want:
  - GitHub-native Copilot Workspace?
  - Copilot CLI integration?
  - External API calling Copilot?
  - Local testing setup?
  - Multi-environment support?

**Result:** Creates the "obvious" files (template, CODEOWNERS, README) but leaves the hard part (workflow execution) incomplete.

### Why 14 Words > 10 Words

**10 words might be:** "Setup issue-driven development and automate PR reviews"
- Generic, could mean anything
- No agent specified
- No knowledge base mentioned
- Would generate even less complete solution

**14 words is:** "Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base."
- Specifies agent (@copilot)
- Specifies assignment mechanism (CODEOWNERS)
- Specifies knowledge capture
- More structured sentences

**Improvement:** +15-20% completeness by being more specific, but still insufficient for production use.

### The Hard Limit

Even with 50+ carefully-chosen words, Copilot would struggle with:
- **Implementation mechanism:** How does the workflow actually call Copilot and get code back?
- **Integration details:** API endpoints, authentication, retry logic, error handling
- **Architectural choices:** Single vs multi-agent, task queue vs direct execution, approval workflows

**These require:** Either (a) linking to a reference architecture, or (b) very detailed specification (100+ words), or (c) interaction (asking clarifying questions).

---

## Files Copilot WOULD Create (Summary)

### If Given 14 Words Without Additional Context:

1. **`.github/ISSUE_TEMPLATE/task.yml`** ✅
   - Functional but basic
   - Missing: priority, effort estimates, dependency tracking

2. **`.github/CODEOWNERS`** ⚠️
   - Skeleton created (needs manual owner username)
   - Functional but incomplete

3. **`.github/workflows/copilot-task.yml`** ❌
   - Trigger logic: 70% complete
   - Execution logic: 0% complete
   - Would be non-functional without additional code

4. **`README.md`** ✅
   - Good quality documentation
   - 75% complete (assumes some setup already done)

5. **`docs/knowledge/README.md`** ✅
   - Well-structured
   - 80% complete (doesn't explain integration with workflow)

**NOT created (but should be):**
- `.github/workflows/knowledge-sync.yml` - Auto-link completed issues to relevant docs
- `docs/knowledge/CONTRIBUTION_GUIDE.md` - How to add patterns/decisions
- `.github/PULL_REQUEST_TEMPLATE.md` - Auto-populate PR description from issue
- `scripts/validate-bootstrap.sh` - Verify setup is correct
- `.github/workflows/validate.yml` - Test the bootstrap works

**Estimate:** Copilot would deliver ~60% of a production-ready system with the 14-word request, creating 5 files totaling ~256 lines, but leaving critical execution logic unimplemented.

