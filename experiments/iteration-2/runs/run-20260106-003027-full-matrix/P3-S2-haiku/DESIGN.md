# Issue-Driven Development System Design
## @copilot Autonomous Workflow

**Design Date:** 2026-01-06
**Agent:** @copilot (Haiku 4.5 Simulation)
**Task:** Create autonomous issue-driven development with GitHub integration

---

## Problem Statement

Traditional development workflows require manual coordination:
1. Create issue
2. Manually assign work
3. Switch context to implementation
4. Create PR manually
5. Coordinate review

**Goal:** Automate the entire pipeline: issue → @copilot → PR → review

---

## Success Criteria (Observable Outcomes)

✅ System processes test issue end-to-end without errors
✅ Issue template validates with yamllint
✅ GitHub workflow validates (valid YAML syntax)
✅ CODEOWNERS syntax correct
✅ Test issue can be created using template
✅ Workflow triggers on issue creation
✅ Knowledge base structure is searchable
✅ README provides complete workflow reference

---

## Architecture: Files to Create

### Tier 1: Issue Template (GitHub UX)
**File:** `.github/ISSUE_TEMPLATE/task.yml`
- **Purpose:** Standardize copilot task format
- **Format:** GitHub issue template (YAML)
- **Why needed:** Ensures consistent input data for @copilot

### Tier 2: Code Review Routing (GitHub UX)
**File:** `.github/CODEOWNERS`
- **Purpose:** Auto-assign PRs for review
- **Format:** GitHub CODEOWNERS file (text)
- **Why needed:** Ensures PRs don't get lost, consistent review assignment

### Tier 3: Workflow Automation (GitHub Actions)
**Files:**
- `.github/workflows/copilot-task.yml` - Main execution pipeline
- `.github/workflows/validate-system.yml` - System health checks
- `.github/pull_request_template.md` - Auto-populate PR descriptions

### Tier 4: Knowledge Base (Organizational Memory)
**Files:**
- `docs/knowledge/README.md` - Knowledge base structure
- `docs/knowledge/patterns/.gitkeep` - Pattern storage
- `docs/knowledge/decisions/.gitkeep` - ADR storage
- `docs/knowledge/insights/.gitkeep` - Learning storage

### Tier 5: Documentation & Validation
**Files:**
- `README.md` - User-facing workflow documentation
- `scripts/verify-bootstrap.sh` - System verification
- `.gitattributes` - Consistent line endings

---

## Design Decisions

### 1. Issue Template Format
**Decision:** Use GitHub's native YAML format
**Rationale:**
- Standard GitHub feature (no custom tools)
- Enforces required fields
- Auto-labels issues
- Lower barrier for users
**Fields Included:**
- Title (implicit)
- Description
- Acceptance Criteria
- Priority (P0-P3)
- Effort estimate
- Skills required
- Dependencies

### 2. Workflow Trigger Strategy
**Decision:** Multiple triggers (opened, labeled, assigned)
**Rationale:**
- Opened: Immediate response
- Labeled: Allows batch processing
- Assigned: Handles reassignment
- Comment: Ad-hoc requests

### 3. Knowledge Base Structure
**Decision:** Three-tier (Patterns / Decisions / Insights)
**Rationale:**
- **Patterns:** Reusable solutions (lowest barrier, highest reuse)
- **Decisions:** Why we chose X (formal, traceable)
- **Insights:** Lessons learned (informal, evolutionary)
- Mirrors real team knowledge capture

### 4. CODEOWNERS Handling
**Decision:** Placeholder @owner requires manual substitution
**Rationale:**
- Can't infer from git metadata reliably
- Users understand need to configure
- Clear documentation in README
**Alternative Considered:** Auto-detect from git origin (rejected: too fragile)

### 5. Validation Strategy
**Decision:** Separate validation workflow
**Rationale:**
- Decoupled from main execution
- Can run on schedule (e.g., daily)
- Can be manual-triggered for debugging
- Prevents cascading failures

---

## Workflow Execution Flow

```
┌─ Issue Created (test-issue.md)
│
├─ GitHub Actions Trigger
│  ├─ Receive issue metadata
│  ├─ Extract title, body, labels
│  └─ Add "received" label
│
├─ @copilot Analysis Phase
│  ├─ Parse acceptance criteria
│  ├─ Check knowledge base for patterns
│  ├─ Estimate complexity
│  └─ Plan implementation
│
├─ @copilot Implementation Phase
│  ├─ Create feature branch (copilot/issue-NNN)
│  ├─ Make code changes
│  ├─ Add tests if needed
│  └─ Update documentation
│
├─ Pull Request Creation
│  ├─ Create PR on main branch
│  ├─ Link to issue (#NNN)
│  ├─ Add auto-populated description
│  └─ Assign reviewer via CODEOWNERS
│
├─ Review Phase (Manual)
│  ├─ Repository owner reviews
│  ├─ Request changes or approve
│  └─ @copilot resolves feedback
│
└─ Merge & Knowledge Capture
   ├─ Merge PR to main
   ├─ Close issue
   └─ Opportunity to document patterns
```

---

## File Manifest with Rationale

| File | Type | Purpose | Why @copilot Created |
|------|------|---------|----------------------|
| `.github/ISSUE_TEMPLATE/task.yml` | GitHub Config | Input format | Needed for consistent issue capture |
| `.github/CODEOWNERS` | GitHub Config | Auto-assign reviews | Prevents PR orphans |
| `.github/workflows/copilot-task.yml` | Workflow | Main execution | Core automation |
| `.github/workflows/validate-system.yml` | Workflow | Health checks | Catch configuration errors |
| `.github/pull_request_template.md` | Template | PR consistency | Ensures good PR descriptions |
| `docs/knowledge/README.md` | Documentation | KB structure | Capture organizational learning |
| `docs/knowledge/patterns/.gitkeep` | Directory marker | Pattern storage | Reusable solutions |
| `docs/knowledge/decisions/.gitkeep` | Directory marker | ADR storage | Formal decisions |
| `docs/knowledge/insights/.gitkeep` | Directory marker | Learning storage | Evolutionary knowledge |
| `README.md` | Documentation | User workflow | Complete setup reference |
| `scripts/verify-bootstrap.sh` | Validation Script | System check | Detect setup problems |
| `.gitattributes` | Git Config | Line endings | Consistency across OS |

---

## Implementation Notes

### Assumptions Made
1. **GitHub environment:** Assumes Actions are enabled
2. **Git workflow:** main branch is primary, feature branches from main
3. **User familiarity:** Users know GitHub basics (issues, PRs, labels)
4. **Automation scope:** This is a GitHub-native system (no external APIs)

### How @copilot Decided These Were Necessary

1. **Issue Template** - "process test issue" requires consistent input format
2. **CODEOWNERS** - "PR → review" requires auto-assignment
3. **Workflows** - "issue → @copilot → PR" requires automation
4. **Knowledge Base** - "remember everything" requires persistent capture
5. **README** - "workflow" requires documentation
6. **Validation Script** - "without errors" requires verification

### What's NOT Included (And Why)

- ❌ **External Copilot API calls** - Not possible in pure GitHub Actions
- ❌ **Code generation templates** - Keep flexible for any task type
- ❌ **Automatic testing** - Dependent on project tech stack
- ❌ **Slack integration** - Out of scope (GitHub-only system)

---

## Validation Checklist

Before claiming success:

- [ ] All YAML files validate with yamllint
- [ ] Workflow syntax is valid
- [ ] Issue template appears in GitHub UI
- [ ] CODEOWNERS path is correct
- [ ] Bash scripts pass shellcheck
- [ ] README provides complete workflow walkthrough
- [ ] Knowledge base directories exist
- [ ] All files have proper permissions
- [ ] No placeholder @OWNER left unaddressed in documentation

---

## Next Steps

1. Create design summary (this file)
2. Implement all configuration files
3. Implement all workflow files
4. Implement knowledge base structure
5. Implement documentation and verification scripts
6. Validate all files with appropriate linters
7. Test with sample issue

