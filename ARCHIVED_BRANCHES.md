# Archived Branches

## archive/event-system-20260111 (feature/event-system)

**Archived**: 2026-02-06  
**Original dates**: 2026-01-11 to 2026-01-14  
**Commits**: 9 commits, 30,741 lines added

**Content**:
- Event Capture System MVP (actor-based event sourcing)
- 6 core actors: Daemon, EventLog, PatternMatcher, FunctionRegistry, HTTPServer, FunctionExecutor
- Comprehensive specifications: BDD scenarios, FIT decision tables, state machines
- CLI, daemon, HTTP server implementation
- Loop prevention mechanisms
- JSONL event logs

**Why archived**:
- Experimental architecture from before SEAG evolution
- Main diverged by 128 commits since this work
- SEAG/Simplify chosen as canonical TypeScript implementation
- Merging would create architectural confusion

**Future options**:
- Extract specification patterns for SEAG
- Revive if event processing architecture is needed
- Reference for actor design patterns

**Access**:
```bash
git checkout archive/event-system-20260111
# or
git show archive/event-system-20260111:path/to/file
```
