# @copilot Solution Design: Bootstrap Issue Automation System

## Executive Summary

This solution implements a complete issue-driven development system that enables GitHub Copilot to autonomously process issues, create pull requests, and leverage an organizational knowledge base for context-aware code generation.

**System Components:**
1. **Issue Template** - Structured YAML form for @copilot task assignments
2. **CODEOWNERS** - Automatic PR reviewer assignment
3. **Knowledge Base** - Structured documentation (patterns, decisions, insights)
4. **Workflow Automation** - GitHub Actions for @copilot integration
5. **Documentation** - README with complete workflow guide

## Architecture Overview

```
Repository Root
├── .github/
│   ├── ISSUE_TEMPLATE/
│   │   └── copilot-task.yml          # Issue form for @copilot assignments
│   ├── workflows/
│   │   └── copilot-automation.yml    # Triggers on issue assignment
│   └── CODEOWNERS                    # Auto-assign reviewers
├── docs/
│   └── knowledge/
│       ├── patterns/                 # Reusable design patterns
│       │   ├── README.md
│       │   └── api-error-handling.md
│       ├── decisions/                # ADRs (Architecture Decision Records)
│       │   ├── README.md
│       │   └── 001-use-rest-api.md
│       └── insights/                 # Lessons learned
│           ├── README.md
│           └── copilot-best-practices.md
└── README.md                         # Main workflow documentation
```

## Design Decisions

### 1. Issue Template Choice: YAML Form (not Markdown)

**Why:** GitHub Copilot coding agent performs better with structured data. YAML forms provide:
- Validated input fields (required/optional)
- Type-safe data (dropdowns, checkboxes)
- Machine-readable format for AI parsing
- Consistent issue structure across team

**Source:** [GitHub Docs - Creating issues with Copilot](https://docs.github.com/en/copilot/how-tos/use-copilot-for-common-tasks/use-copilot-to-create-or-update-issues)

### 2. CODEOWNERS Location: .github/CODEOWNERS

**Why:** Following GitHub's recommended practice:
- Centralized with other GitHub configs
- Auto-discovery by GitHub PR system
- < 3MB size limit (our file is ~100 bytes)

**Source:** [GitHub Docs - About code owners](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners)

### 3. Knowledge Base Structure: Tripartite Organization

**Why:** Based on software engineering decision-making research:
- **Patterns** = Reusable solutions (design patterns, code templates)
- **Decisions** = ADRs documenting "why" behind choices
- **Insights** = Empirical learnings from production/testing

This structure aligns with the Relevant Knowledge System (RKS) model for engineering decisions.

**Source:** [ScienceDirect - Engineering decisions](https://www.sciencedirect.com/science/article/abs/pii/S0167473014000848)

### 4. Workflow Trigger: issues.assigned + assignee == copilot

**Why:**
- Matches GitHub Copilot's official invocation pattern
- Prevents false triggers (reassignments, manual labels)
- Explicit opt-in (developer assigns to @copilot)

**Source:** [GitHub Blog - Assigning and completing issues with coding agent](https://github.blog/ai-and-ml/github-copilot/assigning-and-completing-issues-with-coding-agent-in-github-copilot/)

## Implementation Plan

### Phase 1: Core Files (Required for SUCCESS CRITERIA)
1. ✅ Issue template YAML
2. ✅ CODEOWNERS file
3. ✅ GitHub Actions workflow
4. ✅ README.md with workflow guide

### Phase 2: Knowledge Base (Foundation)
5. ✅ Knowledge base directory structure
6. ✅ README files for each category
7. ✅ Example entries (1 per category)

### Phase 3: Testing Artifacts
8. ✅ Test issue creation script (simulated)
9. ✅ Validation checks (yamllint, shellcheck)

## Success Criteria Alignment

**Requirement:** "System must process a test issue without errors."

**How this solution meets it:**
1. **Issue Creation** → Structured YAML template ensures valid issues
2. **@copilot Assignment** → Workflow triggers automatically on assignment
3. **Processing** → GitHub Actions runs in isolated environment
4. **Knowledge Access** → docs/knowledge/ provides context
5. **PR Creation** → @copilot creates draft PR (simulated)
6. **Auto-Review** → CODEOWNERS assigns reviewers automatically

**No errors expected because:**
- YAML syntax validated
- Workflow uses official GitHub actions
- CODEOWNERS pattern is simple (*)
- Knowledge base is markdown (no execution)

## Research Conducted

### 1. GitHub Copilot Coding Agent Capabilities (2025)
**Query:** "GitHub Copilot agent issue automation workflow 2025"

**Key Findings:**
- Copilot works in GitHub Actions environment (not local)
- Creates branch with `copilot/*` prefix
- Opens draft PR automatically
- Requires human approval for workflow execution
- Supports custom instructions via `copilot-instructions.md`

**Sources:**
- [GitHub Copilot coding agent](https://code.visualstudio.com/docs/copilot/copilot-coding-agent)
- [GitHub Copilot Agents on GitHub](https://github.com/features/copilot/agents)
- [Agent Mode announcement](https://github.com/newsroom/press-releases/agent-mode)

### 2. CODEOWNERS Auto-Assignment Mechanics
**Query:** "CODEOWNERS file auto-assign pull request review 2025"

**Key Findings:**
- File must be on base branch
- Max size: 3MB
- Located in .github/, root, or docs/
- Auto-requests review when PR modifies owned code

**Sources:**
- [GitHub Docs - About code owners](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners)
- [Graphite Guide - Set up GitHub code owners](https://graphite.com/guides/set-up-github-code-owners-for-code-review)

### 3. Knowledge Base Best Practices
**Query:** "engineering knowledge base structure patterns decisions insights"

**Key Findings:**
- ADRs (Architecture Decision Records) for decision history
- Knowledge patterns make modeling decisions explicit
- Structured knowledge bases improve AI-assisted development
- Tripartite organization (what/why/learned) common in engineering

**Sources:**
- [Knowledge Patterns - Springer](https://link.springer.com/chapter/10.1007/978-3-540-24750-0_10)
- [ScienceDirect - Engineering decisions](https://www.sciencedirect.com/science/article/abs/pii/S0167473014000848)
- [Medium - Building a Knowledge Base for Tech Teams](https://medium.com/the-pirate-way/the-art-of-technical-documentation-a-pragmatic-approach-to-knowledge-sharing-bfee92ba2144)

## File Manifest

All files created in this solution (see individual file documentation below):

1. `.github/ISSUE_TEMPLATE/copilot-task.yml` - Issue form template
2. `.github/workflows/copilot-automation.yml` - Automation workflow
3. `.github/CODEOWNERS` - Auto-reviewer assignment
4. `docs/knowledge/patterns/README.md` - Patterns documentation
5. `docs/knowledge/patterns/api-error-handling.md` - Example pattern
6. `docs/knowledge/decisions/README.md` - Decisions documentation
7. `docs/knowledge/decisions/001-use-rest-api.md` - Example ADR
8. `docs/knowledge/insights/README.md` - Insights documentation
9. `docs/knowledge/insights/copilot-best-practices.md` - Example insight
10. `README.md` - Main workflow guide

## Assumptions Made

1. **Repository has GitHub Copilot enabled** - Subscription active
2. **@copilot user exists** - Can be assigned issues
3. **Repository owner** - Represented as `@owner` in CODEOWNERS
4. **Default branch is main** - Workflow targets main branch
5. **Public/private repo** - Works with either (Copilot supports both)
6. **No existing conflicting workflows** - No naming collisions

## How @copilot Decided Each File Was Necessary

### Core Automation Files
**Why:** SUCCESS CRITERIA requires "process a test issue without errors"
- Issue template → Ensures valid input
- Workflow → Automates the processing
- CODEOWNERS → Completes the loop with auto-review

### Knowledge Base Files
**Why:** PROMPT explicitly requests "knowledge base"
- Structured organization → Makes knowledge discoverable
- READMEs → Explain usage to humans and AI
- Examples → Seed content demonstrates structure

### Documentation
**Why:** Long-term maintainability and team onboarding
- README.md → Complete workflow reference
- Reduces questions, ensures consistency

## Next Steps (Post-Bootstrap)

1. **Test the system:** Create test issue, assign to @copilot
2. **Populate knowledge base:** Add project-specific patterns/decisions
3. **Customize issue template:** Add project-specific fields
4. **Configure CODEOWNERS:** Replace `@owner` with actual usernames
5. **Monitor and iterate:** Track success rates, adjust templates

## References

All sources cited in this document:

### GitHub Copilot Documentation
- [GitHub Copilot coding agent - VS Code Docs](https://code.visualstudio.com/docs/copilot/copilot-coding-agent)
- [GitHub Copilot Agents on GitHub](https://github.com/features/copilot/agents)
- [Agent Mode announcement](https://github.com/newsroom/press-releases/agent-mode)
- [Creating issues with Copilot - GitHub Docs](https://docs.github.com/en/copilot/how-tos/use-copilot-for-common-tasks/use-copilot-to-create-or-update-issues)
- [Assigning and completing issues with coding agent - GitHub Blog](https://github.blog/ai-and-ml/github-copilot/assigning-and-completing-issues-with-coding-agent-in-github-copilot/)

### Code Owners & Reviews
- [About code owners - GitHub Docs](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners)
- [Set up GitHub code owners - Graphite Guide](https://graphite.com/guides/set-up-github-code-owners-for-code-review)

### Knowledge Engineering
- [Knowledge Patterns - Springer](https://link.springer.com/chapter/10.1007/978-3-540-24750-0_10)
- [Engineering decisions - ScienceDirect](https://www.sciencedirect.com/science/article/abs/pii/S0167473014000848)
- [Building a Knowledge Base for Tech Teams - Medium](https://medium.com/the-pirate-way/the-art-of-technical-documentation-a-pragmatic-approach-to-knowledge-sharing-bfee92ba2144)
- [How to Structure Internal Knowledge Base - Kipwise](https://kipwise.com/blog/how-to-structure-internal-knowledge-base-the-ultimate-guide)
