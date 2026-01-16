# Development Mode Guide

**Current Mode:** Aggressive (as of 2026-01-16)

This document explains the two development modes for tk-agents and when to use each.

## Overview

tk-agents uses a mode-based approach to balance speed and safety:

- **Aggressive Mode** (current): Fast iteration with git safety nets
- **Careful Mode** (future): Backward compatibility and migration planning

The current mode is tracked in `.dev-mode` at the project root.

---

## Aggressive Mode

**When to use:** Pre-1.0, experimental features, early prototyping, no external users

### What It Means

- Breaking changes are acceptable
- Fast iteration is prioritized over stability
- "Just fix it" approach - if something is wrong, change it directly
- Use version control as the safety net
- No deprecation phases needed
- Focus on getting to the right design quickly

### Techniques

1. **Direct API Changes**
   - Rename methods/interfaces when semantics are wrong
   - Example: `send()` → `receive()` in Hewitt migration
   - Don't maintain old API alongside new one (yet)

2. **Git Worktrees for Parallel Work**
   ```bash
   # Create worktree for experimental branch
   git worktree add ../tk-agents-experiment -b experiment/new-feature

   # Work in isolation without blocking main development
   cd ../tk-agents-experiment
   # Make aggressive changes here

   # When ready, merge or discard
   git worktree remove ../tk-agents-experiment
   ```

3. **Version Control Rollback**
   - Make changes, test, commit if good
   - If bad: `git reset --hard HEAD~1` or revert
   - Branches are cheap - use them liberally
   - Tag stable points: `git tag -a v0.0.1.0 -m "Stable before Hewitt migration"`

4. **"Just Fix It" Approach**
   - See a problem? Fix it immediately
   - Don't accumulate technical debt
   - Refactor aggressively while codebase is small
   - Example: Registry → System conversion in one go

5. **Tests as Checkpoints**
   - Run tests before starting work (baseline)
   - Run tests after changes (validation)
   - If tests fail, fix them or revert changes
   - Don't skip or disable failing tests (see CLAUDE.md)

### Safety Nets

Even in aggressive mode, we use safety nets:

- **Git history**: Complete record of all changes
- **Git branches**: Experiment without risk
- **Git worktrees**: Multiple working directories from one repo
- **Test suite**: Prevents regressions
- **Version tags**: Mark stable points
- **Documentation**: Record decisions in ADRs

### Example Workflow

```bash
# Pre-flight check
bun test  # Baseline: all passing

# Create branch for aggressive change
git checkout -b feature/aggressive-refactor

# Make breaking changes
# - Rename Actor.send() → Actor.receive()
# - Update all implementations
# - Update all tests

# Validate
bun test  # Must pass

# Commit if good
git add .
git commit -m "Refactor: Actor.send() → Actor.receive()"

# Merge to main
git checkout main
git merge feature/aggressive-refactor

# If something goes wrong later
git revert <commit-hash>
# or
git reset --hard <previous-tag>
```

### When Aggressive Mode Works

- Team size: Small (1-3 developers)
- User base: None or internal only
- Version: Pre-1.0
- Codebase size: Small to medium
- Change frequency: High
- Discovery phase: Still finding the right abstractions

---

## Careful Mode

**When to use:** Post-1.0, production systems, public APIs, external users exist

### What It Means

- Backward compatibility is required
- Breaking changes need migration plans
- Deprecation phases for old APIs
- Phased rollout of changes
- Clear communication about changes
- Minimize disruption to users

### Techniques

1. **Dual API Support**
   ```typescript
   // Keep old API
   /** @deprecated Use newMethod() instead. Will be removed in v3.0 */
   async oldMethod(arg: string): Promise<Result> {
     console.warn('oldMethod is deprecated, use newMethod');
     return this.newMethod(arg);
   }

   // Introduce new API
   async newMethod(arg: string): Promise<Result> {
     // New implementation
   }
   ```

2. **Migration Plans**
   - Document: What's changing, why, how to migrate
   - Example: `MIGRATION_PLAN.md` for Hewitt model
   - Phased approach: Phase 1-6 over weeks
   - Each phase maintains backward compatibility until Phase 5

3. **Feature Flags**
   ```typescript
   const USE_NEW_API = process.env.USE_NEW_API === 'true';

   if (USE_NEW_API) {
     // New behavior
   } else {
     // Old behavior (default)
   }
   ```

4. **Versioning Strategy**
   - Semantic versioning: MAJOR.MINOR.PATCH
   - MAJOR: Breaking changes
   - MINOR: New features (backward compatible)
   - PATCH: Bug fixes
   - Document changes in CHANGELOG.md

5. **Adapter Pattern**
   ```typescript
   // Wrap new API for legacy code
   class LegacyAdapter {
     constructor(private newSystem: System) {}

     // Old API signature
     send(actorId: string, message: Message): Promise<Response> {
       // Translate to new API
       return this.newSystem.receive({
         type: 'route',
         payload: { targetId: actorId, message }
       });
     }
   }
   ```

### Safety Nets

- **Comprehensive test coverage**: Catch regressions
- **Deprecation warnings**: Guide users to new API
- **Feature flags**: Roll out gradually
- **Rollback procedures**: Emergency escape hatch
- **Communication**: Announce changes in advance
- **Documentation**: Migration guides for users

### Example Workflow

See `MIGRATION_PLAN.md` for a complete example of careful mode in action (Hewitt Actor Model migration).

```bash
# Phase 1: Add new API alongside old
# - No breaking changes
# - All tests still pass

# Phase 2: Implement new API
# - Old API still works
# - Deprecation warnings added

# Phase 3: Update internal usage
# - Our code uses new API
# - External code still uses old API (works with warnings)

# Phase 4: Communication
# - Announce deprecation
# - Provide migration guide
# - Set removal timeline (e.g., 6 months)

# Phase 5: Remove old API
# - MAJOR version bump (v2.0.0)
# - Breaking change clearly documented
```

### When Careful Mode Is Required

- Team size: Medium to large
- User base: External users, production deployments
- Version: Post-1.0
- Codebase size: Large, widely used
- Change frequency: Lower, more deliberate
- Stability phase: APIs are stable and published

---

## Switching Between Modes

### How to Switch

1. **Update `.dev-mode` file**
   ```
   mode=careful  # Change from aggressive
   last_updated=2026-XX-XX
   reason=Reached v1.0, have external users
   ```

2. **Update development practices**
   - Start using migration plans for breaking changes
   - Introduce deprecation warnings
   - Version bumps follow semantic versioning strictly
   - Create CHANGELOG.md if it doesn't exist

3. **Communicate to team**
   - Announce mode switch
   - Update workflow documentation
   - Review PRs for backward compatibility

### Triggers for Switching to Careful Mode

- First external user/customer
- Version 1.0 release
- Public API announcement
- When rollback becomes expensive
- When breaking users becomes unacceptable

### Can You Switch Back?

Yes, but rarely needed:

- Major version bump (v2.0, v3.0) might allow temporary aggressive mode
- But even then, maintain migration paths for users

---

## Current Project Status

**Mode:** Aggressive

**Reasoning:**
- Version: 0.0.1.0-pre (pre-release)
- Users: None (internal development only)
- Team: Small (1-3 developers)
- Phase: Design discovery - still finding right abstractions
- Example: Hewitt Actor Model migration shows we're still iterating on core concepts

**Active Work:**
- Hewitt migration (breaking change to core API)
- Bootstrap system (new feature, experimental)
- Mailbox integration (refactoring internal architecture)

**When we'll switch to Careful Mode:**
- Version 1.0 release
- First external user/integrator
- Public API announcement
- Or: when team decides stability is more important than speed

---

## FAQ

### Q: Does aggressive mode mean no testing?
**A:** No! Tests are critical even in aggressive mode. They're the safety net that lets you iterate quickly. Never skip tests.

### Q: Does aggressive mode mean no documentation?
**A:** No! Document decisions (ADRs), architecture (DESIGN.md), and interfaces (ACTOR_INTERFACE.md). But don't spend time documenting APIs that might change tomorrow.

### Q: Can I use aggressive mode for some modules and careful mode for others?
**A:** Yes, but document it clearly. Example: stable core (careful) vs experimental features (aggressive).

### Q: How do I know which mode to use?
**A:** Check `.dev-mode`. If unsure, ask the user or check project version/user base.

### Q: Does the Hewitt migration plan contradict aggressive mode?
**A:** No! The MIGRATION_PLAN.md shows what careful mode would look like. But since we're pre-1.0 with no users, we can collapse phases or skip deprecation warnings. We're documenting the *process* for learning, even if we execute more aggressively.

---

## Summary

| Aspect | Aggressive Mode | Careful Mode |
|--------|----------------|--------------|
| Breaking changes | Acceptable | Require migration plans |
| API stability | Iterate rapidly | Maintain backward compat |
| Deprecation | Not needed | Phased deprecation |
| Version bumps | Flexible | Strict semantic versioning |
| Safety nets | Git, tests, worktrees | Git, tests, feature flags, adapters |
| Speed | Fast | Deliberate |
| User impact | None (no users) | Minimize disruption |
| Phase | Discovery | Stability |

**Current mode:** Aggressive

**Check:** `.dev-mode` for current settings
