# Research and Design Exploration

This directory contains research, design explorations, and session summaries from the actor system development process.

## Design Exploration

- **MESSAGE_FLOWS.md** - Comparison of Erlang vs our routing model
- **ACTOR_API_SCENARIOS.md** - External vs internal API design scenarios
- **ACTOR_SEND_SEMANTICS.datalog** - Earlier exploration of send semantics
- **ARCHITECTURE.md** - Separation between core actor system and application layer

## Session Summaries

- **SESSION_SUMMARY.md** - Complete summary of functional actors + graph systems work
- **GRAPH_ACTOR_SYSTEM.md** - Virtual actor pattern exploration
- **EVENT_SOURCING_SUMMARY.md** - Event sourcing approaches

## Final Spec

The definitive specification is in `src/actors/`:
- `ACTOR_SYSTEM.spec.md` - Complete human-readable spec
- `ACTOR_SYSTEM.spec.datalog` - Formal verification model
- `SPEC.md` - Quick reference and verification guide

These research documents capture the design journey but are superseded by the final spec.
