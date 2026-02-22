# Agent Instructions

## Project Context

**Agentic Primer** is a protocol-first convergence platform for multi-agent actor systems. Key components:

- **@agentic-primer/protocols** - Published npm package (63+ domain types: Address, Node, Edge, Agent, Task, etc.)
- **WIT Protocols** - Language-agnostic interface definitions using WebAssembly Interface Types
- **SEAG** - Reference implementation of protocol-driven actor system (Simplify Environment for Agentic Growth)
- **Path-Based Addressing** - Hierarchical routing for actor systems (POC complete in `simplify/`)

**Documentation:**
- `packages/protocols/` - Protocol package source
- `docs/protocols/` - WIT migration plans, integration strategy
- `CURRENT_STATE.md` - Project evolution and current focus
- `docs/archive/bootstrap-phase/` - Historical bootstrap experiments (Jan 2026)

## Issue Tracking

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Knowledge Systems

**Session Knowledge** - Learn from past work:
```bash
./scripts/know decisions recent 10       # Recent architectural decisions
./scripts/know learnings category technical  # Technical insights
./scripts/know errors type <ErrorType>   # Known error patterns
```

When you complete significant work, extract knowledge:
```bash
/extract-knowledge <session-id>
```

See `.claude/rules/session-knowledge.md` for detailed usage patterns.

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
