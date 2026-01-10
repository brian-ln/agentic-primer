# Compaction Recovery Protocol

## Purpose
This protocol ensures seamless context recovery after session compaction. It enables Claude Code to reconstruct complete working context from compact logs and resume work without information loss.

## When to Use
- After any session compaction
- When resuming work in a compacted session
- When user asks "where are we?" or "what's next?"
- At the start of any session where you need to verify current state

## The Protocol

### Phase 1: Establish Temporal Context
**Goal**: Know exactly when "now" is and what timezone we're in.

```bash
date
```

**Output**: Current date, time, and timezone. This grounds all temporal reasoning.

### Phase 2: Identify Active Work Context
**Goal**: Discover what work is currently in flight.

```bash
# Check current directory and git branch
pwd
git branch --show-current

# Check for background agents/tasks
# Look for .task files or background processes
ls -la | grep -E '\.(task|log)$'
```

**Interpret**:
- Current directory = active work area
- Git branch = specific feature/task context
- Background task files = work running in parallel

### Phase 3: Check Bead System (Issue Tracking)
**Goal**: Discover created issues, their status, and dependencies.

```bash
# List all beads (issues)
bd list

# Check for beads in specific states
bd list --status=open
bd list --status=in-progress

# Look for recently created beads (check .beads/ directory)
ls -lt .beads/issues/ | head -20
```

**Interpret**:
- Open beads = planned work not yet started
- In-progress beads = current active tasks
- Recent beads (by timestamp) = newly discovered issues

### Phase 4: Discover Architecture/Documentation
**Goal**: Find any architectural decisions, plans, or documentation files.

```bash
# List markdown files (common for architecture docs)
ls -la *.md

# Check for architecture-specific files
ls -la | grep -iE 'arch|design|plan|protocol|readme'

# List experiment or analysis directories
ls -la experiments/ scripts/ archive/ 2>/dev/null || true
```

**Interpret**:
- ARCHITECTURE.md, DESIGN.md = system design
- README.md = project overview
- PLAN.md, PROTOCOL.md = execution strategy
- experiments/, scripts/ = implementation work

### Phase 5: Check Git History
**Goal**: Understand recent activity and decisions.

```bash
# Recent commits
git log --oneline -10

# Files changed recently
git status

# Uncommitted work
git diff --stat
```

**Interpret**:
- Recent commits = completed work
- Staged changes = work ready to commit
- Unstaged changes = work in progress

### Phase 6: Reconstruct TODO List
**Goal**: Build actionable next steps from discovered context.

**Process**:
1. Combine information from phases 2-5
2. Identify:
   - Work explicitly marked as "next steps"
   - Open beads that need action
   - Incomplete items in architecture/plan files
   - Background tasks that need checking
3. Order by:
   - Explicit priority (from beads or docs)
   - Logical dependencies
   - User's stated goals (from compact log)

### Phase 7: Synthesize Status Summary
**Goal**: Present complete picture to user.

**Structure**:
```
## Current Status (as of [DATE] [TIME] [TZ])

**Active Context**:
- Directory: [path]
- Branch: [branch-name]
- Background Work: [task status]

**Recent Progress**:
- [Key accomplishments from git log]
- [Completed beads or milestones]

**Open Work**:
- [Bead ID]: [Title] ([Status])
- [Bead ID]: [Title] ([Status])

**Next Steps**:
1. [Highest priority action]
2. [Next action]
3. [Following action]

**Architecture Artifacts**:
- [Key design docs discovered]

**Questions/Blockers**:
- [Anything unclear or blocking progress]
```

## Example Execution

```bash
# Phase 1: Temporal context
date
# Output: Fri Jan 10 09:15:42 PST 2026

# Phase 2: Active work
pwd
# Output: /Users/bln/play/agentic-primer/.wt/event-system

git branch --show-current
# Output: genesis

ls -la | grep .task
# Output: .task_afde589 (completed)

# Phase 3: Bead system
bd list
# Output: Shows 3 open beads (protocol-definition, event-flow, state-machine)

# Phase 4: Architecture
ls -la *.md
# Output: README.md, ARCHITECTURE.md, SIMULATION_HARNESS.md, etc.

# Phase 5: Git history
git log --oneline -5
# Output: Recent commits about bootstrap, metrics, documentation

git status
# Output: Deleted old docs, untracked new docs

# Phase 6-7: Synthesize and present
# [Construct TODO list and status summary]
```

## Success Criteria

A successful recovery includes:
- User feels no information loss
- All active work threads identified
- Next steps are clear and actionable
- No need to ask user "what were we working on?"
- User responds with "yes, exactly" or similar confirmation

## Anti-Patterns to Avoid

**DON'T**:
- Guess or assume context without checking
- Skip phases because you "think you know"
- Present vague next steps ("continue working on X")
- Ignore background tasks or parallel work
- Forget to check temporal context (date/time)

**DO**:
- Run every phase systematically
- Present specific, verifiable facts
- Include file paths, bead IDs, timestamps
- Acknowledge uncertainty ("no beads found" vs "didn't check")
- Update TODO list based on discovered reality

## Compaction Best Practices (For Users)

To maximize recovery success, when compacting:

1. **State your continuation intent**:
   - "Make sure you can resume work on [specific task]"
   - "Ensure you remember [key context]"

2. **Mention key artifacts**:
   - "We created ARCHITECTURE.md and several beads"
   - "Background task is running for [purpose]"

3. **Highlight process/workflow**:
   - "Remember our status process"
   - "Keep using the bead workflow we established"

4. **Specify what NOT to lose**:
   - "Don't lose the understanding of [technical decision]"
   - "Make sure you can pick up the TODO list"

## Maintenance

Update this protocol when:
- A recovery fails and new phase is needed
- New tools/systems are added (beyond beads)
- Success criteria change
- Anti-patterns discovered

---

**Version**: 1.0
**Last Updated**: 2026-01-10
**Based On**: Successful recovery in session [compacted 2026-01-10]
