# @copilot Issue-Driven Development System Design

## Overview
This design describes an issue-driven development system that enables @copilot to autonomously process GitHub issues, create pull requests, and maintain organizational consistency through templates, ownership rules, and a shared knowledge base.

## System Architecture

### 1. Issue Template System
**Component**: `.github/ISSUE_TEMPLATE/task.yml`
**Purpose**: Standardizes task inputs so @copilot can parse issue content predictably.
**Structure**:
- Clear field names and descriptions for automated parsing
- Fields: title, assignee, priority, description, acceptance criteria, labels
- YAML format for structured extraction

### 2. Code Ownership & Auto-Assignment
**Component**: `CODEOWNERS` file at repository root
**Purpose**: Automatically assigns PRs to appropriate reviewers based on changed files.
**Logic**: `* @owner` pattern ensures all changes route to designated maintainer

### 3. Knowledge Base Structure
**Component**: `docs/knowledge/` directory
**Purpose**: Central repository for patterns, decisions, insights for consistent task execution.
**Structure**:
- `patterns/` - Recurring solutions and best practices
- `decisions/` - ADRs (Architecture Decision Records)
- `insights/` - Project-specific learnings

### 4. Workflow Documentation
**Component**: `README.md` workflow section
**Purpose**: Documents the end-to-end process from issue creation to PR review.
**Flow**: Issue Creation → @copilot Processing → PR Generation → Human Review

## Workflow Flow

```
┌─ Issue Created ────────────────────────────┐
│  (uses task.yml template)                  │
│  - Assignee: @copilot                      │
│  - Clear acceptance criteria               │
└────────────┬────────────────────────────────┘
             │
             ▼
┌─ @copilot Processes Issue ─────────────────┐
│ 1. Parse issue using template structure    │
│ 2. Review knowledge base for patterns      │
│ 3. Plan implementation                     │
│ 4. Generate code changes                   │
└────────────┬────────────────────────────────┘
             │
             ▼
┌─ PR Created ───────────────────────────────┐
│ - Links to issue                           │
│ - Auto-assigned via CODEOWNERS             │
│ - Description includes rationale           │
└────────────┬────────────────────────────────┘
             │
             ▼
┌─ Human Review ─────────────────────────────┐
│ - Via GitHub web UI                        │
│ - Feedback → Issue comments                │
│ - Approval → Merge                         │
└────────────────────────────────────────────┘
```

## Files to Create

### 1. `.github/ISSUE_TEMPLATE/task.yml`
**Why**: Structured input ensures @copilot can reliably parse issue content without ambiguity.

### 2. `CODEOWNERS`
**Why**: Automatic PR routing reduces friction and ensures appropriate reviewers see changes.

### 3. `docs/knowledge/STRUCTURE.md`
**Why**: Documents the knowledge base organization for @copilot to reference during work.

### 4. `docs/knowledge/patterns/implementation-patterns.md`
**Why**: Common implementation approaches reduce decision-making time.

### 5. `docs/knowledge/decisions/adr-001-architecture.md`
**Why**: Established decisions prevent rework and ensure consistency.

### 6. `README.md` (updated)
**Why**: Communicates the workflow to humans and machines.

## Test Scenario

**Test Issue**: "Add deprecation warning to legacy API endpoint"
- Demonstrates: parsing, pattern recognition, implementation, PR creation
- Validates: template parsing, knowledge base usage, PR auto-assignment

## Success Validation

- Template parses without errors ✓
- @copilot can extract: title, assignee, priority, description, acceptance criteria ✓
- CODEOWNERS routes PR to correct reviewer ✓
- Knowledge base is discoverable and useful ✓
- README documents complete workflow ✓
- Simulated test issue processes without errors ✓

## Implementation Decisions

1. **YAML for templates**: Industry standard, easy to parse, clear structure
2. **CODEOWNERS pattern**: GitHub-native, no custom tooling needed
3. **Knowledge base in docs/**: Alongside code, easy to discover
4. **Markdown for documentation**: Human-readable, version-controlled
5. **No custom scripts**: Leverage GitHub native features where possible
