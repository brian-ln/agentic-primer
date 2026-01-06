# Knowledge Base

This directory contains the git-tracked knowledge base for the agentic-primer project.

## Structure

- **research/** - Research findings and investigations
- **planning/** - Architecture documents and implementation plans
- **implementation/** - Implementation tracking and technical documentation

## Purpose

The knowledge base serves as:
- A permanent record of decisions and rationale
- A resource for understanding system evolution
- A foundation for future work
- A learning resource for contributors

## How It Works

When issues with `research`, `planning`, or `implementation` labels are created:

1. **Research Issues** → Generate documents in `research/`
2. **Planning Issues** → Generate documents in `planning/`
3. **Implementation Issues** → Generate tracking docs in `implementation/`

All documents are:
- Git-tracked for history
- Linked to their originating issues
- Structured for easy navigation
- Automatically created by workflows

## Usage

### Creating Documents Manually

You can also create documents manually following the same structure:

```markdown
# Title

**Issue:** #XX
**Date:** YYYY-MM-DD
**Status:** In Progress/Complete

## Content sections...
```

### Linking Documents

Reference other documents and issues:
- `See research/issue-XX.md`
- `Related to #XX`
- `Implements planning from planning/issue-YY.md`

## Maintenance

- Keep documents updated as work progresses
- Archive completed work appropriately
- Link related documents
- Update status fields
