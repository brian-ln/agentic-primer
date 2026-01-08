# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records that document significant technical decisions.

## Overview

ADRs help @copilot understand architectural constraints and design rationale when implementing features.

## ADR Template

When adding a new decision, use this template:

```markdown
# ADR-NNN: Title

**Status:** Proposed | Accepted | Deprecated | Superseded

**Date:** YYYY-MM-DD

## Context

What is the issue that we're seeing that is motivating this decision?

## Decision

What is the change that we're proposing and/or doing?

## Consequences

What becomes easier or more difficult to do because of this change?

### Positive
- Benefit 1
- Benefit 2

### Negative
- Tradeoff 1
- Tradeoff 2

## Alternatives Considered

What other options were considered and why were they rejected?

### Alternative 1
- Pros: ...
- Cons: ...
- Rejected because: ...
```

## Decisions Index

| ADR | Title | Status | Date |
|-----|-------|--------|------|
| (none yet) | Add decisions as they are made | - | - |

## How to Add Decisions

1. Create a new file: `docs/knowledge/decisions/ADR-NNN-<title>.md`
2. Follow the ADR template above
3. Update this index
4. Reference in relevant issues

## Decision Categories

- **Architecture** - System structure and component design
- **Technology** - Framework and library choices
- **API** - Interface design decisions
- **Data** - Storage and data model decisions
- **Security** - Authentication, authorization, encryption
- **Operations** - Deployment, monitoring, scaling
