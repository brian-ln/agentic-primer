# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records documenting important architectural and design choices for the event-driven system.

## Format

Each ADR follows this structure:
- **Status**: Proposed / Accepted / Deprecated / Superseded
- **Date**: When the decision was made
- **Context**: What led to this decision
- **Decision**: What was decided
- **Consequences**: Impact and trade-offs
- **Alternatives Considered**: Other options and why they were rejected

## Index

### ADR-001: Functions vs Actors Architecture
**Status:** Proposed (Decision Pending)
**Date:** 2026-01-11
**Related Bead:** agentic-primer-jv9

Should user-defined functions be real Hewitt-style actors or remain stateless callbacks?

**Options:**
1. Pure actor model (everything is an actor)
2. Current stateless functions
3. Hybrid (both stateless functions + function actors)

**Recommendation:** Hybrid approach (80% stateless functions, 20% function actors for stateful needs)

[Read full ADR](./001-functions-vs-actors.md)

---

## Contributing

When adding a new ADR:

1. **Number sequentially**: Use next available number (e.g., 002-decision-title.md)
2. **Use descriptive title**: Clear, concise description of the decision
3. **Follow the format**: Use the standard ADR template
4. **Update this index**: Add entry above with key details
5. **Link from main docs**: Update README.md if it's a major decision
6. **Create related bead**: Track implementation in beads system

## Decision States

- **Proposed**: Under discussion, not yet finalized
- **Accepted**: Approved and being implemented
- **Deprecated**: No longer recommended, kept for historical reference
- **Superseded**: Replaced by newer decision (link to replacement)

## Related Documentation

- [Detailed Analyses](../insights/) - In-depth research and analysis
- [Patterns](../patterns/) - Implementation patterns derived from decisions
- [Knowledge Base README](../README.md) - Overview of knowledge structure
