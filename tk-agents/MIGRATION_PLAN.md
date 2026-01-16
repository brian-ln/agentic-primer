# Migration Plan: Hewitt Actor Model Semantics

**Status:** Planning
**Target Date:** TBD
**Owner:** Core Development Team

## Overview

This document provides a detailed, phased migration plan to transition tk-agents from the current actor implementation to proper Hewitt Actor Model semantics. The migration will be backward-compatible through deprecation phases to minimize disruption.

**Key Changes:**
- `Actor.send()` → `Actor.receive()` (semantic correctness)
- `Registry` → `System extends Actor` (uniform composition)
- Private `send()` method for actors to send through System
- No direct actor-to-actor communication

## Current State Analysis

### Test Coverage Baseline
- **Total Tests:** ~100 tests passing (as of 2026-01-16)
- **Test Files:** 7 test files
- **Coverage Areas:** Actor implementations, Registry, Mailbox, Examples

**Pre-Migration Requirement:** ALL tests must pass before starting any migration work.

### Affected Files Inventory

#### Core Actor System (Critical Path)
| File | Lines | Purpose | Migration Complexity |
|------|-------|---------|---------------------|
| `src/actors/base.ts` | 71 | Actor interface, Message/Response types | HIGH - Core interface change |
| `src/actors/registry.ts` | 377 | Registry → System conversion | HIGH - Major refactor |
| `src/actors/mailbox-manager.ts` | Unknown | Mailbox management | MEDIUM - Integration with System |

#### Actor Implementations (12 actors)
| File | Type | Migration Complexity | Notes |
|------|------|---------------------|-------|
| `src/actors/bash.ts` | Deterministic | LOW | Simple: rename send → receive |
| `src/actors/claude.ts` | Agent | MEDIUM | Has streaming, session management |
| `src/actors/human.ts` | Agent | LOW | Simple message queue |
| `src/actors/mock.ts` | Test | LOW | Used in tests only |
| `src/actors/chain.ts` | Orchestration | HIGH | Calls other actors, needs System ref |
| `src/actors/mailbox.ts` | Infrastructure | MEDIUM | Integration with System |

#### Examples and Documentation
| File | Purpose | Migration Complexity |
|------|---------|---------------------|
| `examples/minimal-actors.ts` | Teaching examples | MEDIUM - Update all 7 examples |
| `examples/self-managing-dev.ts` | Bootstrap demo | HIGH - Complex orchestration |
| `ACTOR_INTERFACE.md` | Interface docs | MEDIUM - Complete rewrite |
| `DESIGN.md` | Architecture docs | MEDIUM - Update diagrams/concepts |
| `README.md` | Getting started | LOW - Update code samples |

#### Test Files (Critical for Validation)
| File | Coverage Area | Migration Complexity |
|------|---------------|---------------------|
| `src/actors/actors.test.ts` | Core actor tests | HIGH - Many send() calls |
| `src/actors/mailbox.test.ts` | Mailbox tests | MEDIUM |
| `examples/minimal-actors.test.ts` | Example validation | MEDIUM |
| `src/bootstrap/bootstrap.test.ts` | Bootstrap tests | HIGH - Complex flows |

**Total Estimated Files to Modify:** ~25 files

## Breaking Changes Analysis

### API Surface Changes

#### Actor Interface
```typescript
// BEFORE (Current - WRONG)
interface Actor {
  send(message: Message): Promise<Response>;  // CONFUSING NAME
}

const response = await actor.send(message);  // Looks like actor is SENDING

// AFTER (Hewitt - CORRECT)
interface Actor {
  receive(message: Message): Promise<Response>;  // SEMANTICALLY CORRECT
}

const response = await actor.receive(message);  // Clear: actor RECEIVES
```

#### Registry → System
```typescript
// BEFORE (Current)
import { registry } from './actors/registry';

registry.register(actor);
await registry.send(actorId, message);
await registry.sendTo(actorId, type, payload);

// AFTER (Hewitt)
import { System } from './actors/system';

const system = new System();
system.register(actor);
await system.receive({
  type: 'route',
  payload: { targetId: actorId, message }
});
```

#### Actor-to-Actor Communication
```typescript
// BEFORE (Current - BAD)
class MyActor implements Actor {
  constructor(private registry: Registry) {}

  async send(message: Message): Promise<Response> {
    // Actor calls registry directly
    return this.registry.send('other-actor', someMessage);
  }
}

// AFTER (Hewitt - GOOD)
class MyActor extends BaseActor {
  async receive(message: Message): Promise<Response> {
    // Actor uses protected send() that goes through System
    return this.send('other-actor', someMessage);
  }
}
```

### Backward Compatibility Strategy

We will maintain backward compatibility during migration using:

1. **Dual Method Support (Phase 1-2)**
   - Actor interface has both `send()` and `receive()`
   - `send()` delegates to `receive()` with deprecation warning
   - No breaking changes yet

2. **Registry Shim (Phase 1-2)**
   - Keep Registry class as thin wrapper around System
   - Registry delegates to System internally
   - Singleton still works, but deprecated

3. **Deprecation Warnings (Phase 2)**
   - Console warnings when old API used
   - Guide users to new API in warnings
   - Collect usage telemetry if needed

4. **Adapter Pattern (Phase 2-3)**
   - Provide `LegacyActor` adapter if needed
   - Wraps new API for legacy code
   - Emergency escape hatch

## Phased Migration Strategy

### Phase 1: Foundation (Week 1) - No Breaking Changes

**Goal:** Add new interfaces alongside old ones, verify tests still pass.

**Tasks:**

1. **Update Actor Interface** (`src/actors/base.ts`)
   ```typescript
   export interface Actor {
     readonly id: string;
     readonly type: ActorType;

     // NEW: Semantically correct method
     receive(message: Message): Promise<Response>;

     // DEPRECATED: Keep for backward compat
     /** @deprecated Use receive() instead */
     send?(message: Message): Promise<Response>;

     // Optional
     stream?(message: Message): AsyncGenerator<StreamEvent, Response>;
     start?(): Promise<void>;
     stop?(): Promise<void>;
   }
   ```

2. **Create System Class** (`src/actors/system.ts`)
   ```typescript
   export class System implements Actor {
     readonly id = "system";
     readonly type = "deterministic" as const;

     private registry: Registry; // Wrap existing Registry

     async receive(message: Message): Promise<Response> {
       // Route messages to Registry for now
     }
   }
   ```

3. **Create BaseActor** (`src/actors/base-actor.ts`)
   ```typescript
   export abstract class BaseActor implements Actor {
     protected system?: System;

     setSystem(system: System): void {
       this.system = system;
     }

     abstract receive(message: Message): Promise<Response>;

     // Protected send for actor-to-actor
     protected async send(targetId: string, message: Message): Promise<Response> {
       if (!this.system) {
         throw new Error("Actor not registered with System");
       }
       return this.system.receive({
         id: crypto.randomUUID(),
         type: 'route',
         payload: { targetId, message },
         sender: this.id
       });
     }
   }
   ```

4. **Verification**
   - Run full test suite: `bun test`
   - ALL 100 tests must still pass
   - No behavior changes yet

**Exit Criteria:**
- ✅ All tests pass (100/100)
- ✅ New interfaces added but not yet used
- ✅ No breaking changes

---

### Phase 2: Implementation Migration (Week 2-3)

**Goal:** Update all actor implementations to use `receive()`, deprecate `send()`.

**Approach:** Migrate one module at a time, run tests after each.

#### Module 1: Simple Deterministic Actors (Day 1-2)

**Files:**
- `src/actors/bash.ts`
- `src/actors/mock.ts`

**Steps:**
1. Rename `send()` → `receive()`
2. Add deprecation shim: `send(msg) { console.warn('deprecated'); return this.receive(msg); }`
3. Update corresponding tests
4. Run tests: `bun test src/actors/bash.test.ts`

**Example:**
```typescript
// src/actors/bash.ts
export class BashActor implements Actor {
  // NEW: Semantically correct
  async receive(message: Message): Promise<Response> {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }
    // ... existing logic
  }

  // DEPRECATED: Backward compat shim
  /** @deprecated Use receive() instead */
  async send(message: Message): Promise<Response> {
    console.warn('[BashActor] send() is deprecated, use receive()');
    return this.receive(message);
  }
}
```

#### Module 2: Agent Actors (Day 3-4)

**Files:**
- `src/actors/claude.ts`
- `src/actors/human.ts`

**Complexity:** Medium - has streaming interface

**Steps:**
1. Rename `send()` → `receive()`
2. Keep `stream()` as-is (no semantic issue)
3. Add deprecation shim
4. Update tests

#### Module 3: Orchestration Actors (Day 5-7)

**Files:**
- `src/actors/chain.ts`
- Any actors that call other actors

**Complexity:** HIGH - these actors send to other actors

**Steps:**
1. Extend `BaseActor` instead of raw `Actor`
2. Rename `send()` → `receive()`
3. Update internal sends to use `this.send(targetId, msg)`
4. Inject System reference during registration
5. Update tests with System instance

**Example:**
```typescript
// src/actors/chain.ts
export class ChainActor extends BaseActor {
  async receive(message: Message): Promise<Response> {
    // Process message
    const intermediateResult = this.processStep1(message);

    // Send to next actor in chain
    const response = await this.send('next-actor-id', {
      id: crypto.randomUUID(),
      type: 'process',
      payload: intermediateResult
    });

    return response;
  }
}
```

#### Module 4: Infrastructure (Day 8-10)

**Files:**
- `src/actors/mailbox-manager.ts`
- `src/actors/mailbox.ts`

**Steps:**
1. Integrate with System class
2. Update mailbox delivery to call `actor.receive()`
3. Update tests

**Exit Criteria:**
- ✅ All actor implementations use `receive()`
- ✅ Deprecation warnings in place
- ✅ All tests pass (100/100)
- ✅ `send()` shims work but warn

---

### Phase 3: Test Migration (Week 4)

**Goal:** Update all tests to use new API, verify behavior unchanged.

**Approach:** One test file at a time.

#### Test Migration Checklist Per File

```typescript
// BEFORE
const actor = new BashActor({ id: 'bash-1' });
const response = await actor.send(message);

// AFTER
const actor = new BashActor({ id: 'bash-1' });
const response = await actor.receive(message);
```

**Files in Order:**
1. `src/actors/actors.test.ts` - Core actor tests
2. `src/actors/mailbox.test.ts` - Mailbox tests
3. `examples/minimal-actors.test.ts` - Example tests
4. `src/bootstrap/bootstrap.test.ts` - Bootstrap tests

**Steps Per File:**
1. Find all `actor.send(` → replace with `actor.receive(`
2. Find all `registry.send(` → replace with System routing
3. Run tests: `bun test <file>`
4. Fix any failures
5. Commit when green

**Exit Criteria:**
- ✅ All test files updated
- ✅ All tests pass (100/100)
- ✅ No usage of old `send()` API in tests

---

### Phase 4: Registry → System Migration (Week 5)

**Goal:** Replace Registry singleton with System instances.

**Approach:** Gradual replacement with feature flag.

#### Steps

1. **Add Feature Flag**
   ```typescript
   // src/actors/config.ts
   export const USE_SYSTEM_API = process.env.USE_SYSTEM_API === 'true';
   ```

2. **Update Registry to Use System Internally**
   ```typescript
   // src/actors/registry.ts
   export class Registry {
     private system: System;

     constructor() {
       this.system = new System();
     }

     /** @deprecated Use System directly */
     register(actor: Actor): void {
       console.warn('Registry.register() is deprecated, use System');
       this.system.register(actor);
     }

     /** @deprecated Use System.receive() with route message */
     async send(actorId: string, message: Message): Promise<Response> {
       console.warn('Registry.send() is deprecated');
       return this.system.receive({
         id: crypto.randomUUID(),
         type: 'route',
         payload: { targetId: actorId, message }
       });
     }
   }
   ```

3. **Update Examples to Use System**
   - `examples/minimal-actors.ts` - rewrite all 7 examples
   - `examples/self-managing-dev.ts` - update bootstrap

4. **Update Documentation**
   - `ACTOR_INTERFACE.md` - complete rewrite
   - `DESIGN.md` - update architecture diagrams
   - `README.md` - update getting started examples

**Exit Criteria:**
- ✅ Registry is thin wrapper around System
- ✅ Examples use System directly
- ✅ Documentation updated
- ✅ All tests pass (100/100)

---

### Phase 5: Remove Deprecated APIs (Week 6)

**Goal:** Remove old `send()` method and Registry singleton.

**Prerequisites:**
- ✅ All code uses `receive()` API
- ✅ All code uses System instead of Registry
- ✅ Deprecation warnings confirmed working
- ✅ No critical blockers identified

#### Steps

1. **Remove `send()` from Actor Interface**
   ```typescript
   // src/actors/base.ts
   export interface Actor {
     readonly id: string;
     readonly type: ActorType;
     receive(message: Message): Promise<Response>;  // Only this remains
     // send() method removed
   }
   ```

2. **Remove Registry Singleton**
   - Keep Registry class as internal implementation detail
   - Remove `export const registry` singleton
   - System uses Registry internally if needed

3. **Remove Deprecation Shims**
   - Remove all `send()` compatibility methods from actors
   - Clean up deprecation warnings

4. **Final Test Run**
   ```bash
   bun test
   # ALL tests must pass
   ```

5. **Update package.json Version**
   ```json
   {
     "version": "2.0.0"  // MAJOR version bump - breaking change
   }
   ```

**Exit Criteria:**
- ✅ Old API completely removed
- ✅ All tests pass (100/100)
- ✅ No deprecation warnings
- ✅ Clean semantic model
- ✅ Version bumped to 2.0.0

---

### Phase 6: Documentation and Polish (Week 7)

**Goal:** Complete documentation, create migration guide, polish UX.

#### Tasks

1. **Create Migration Guide** (`docs/MIGRATION_GUIDE.md`)
   - How to upgrade from 1.x to 2.0
   - Code examples for common patterns
   - Troubleshooting common issues

2. **Update All Documentation**
   - [x] ADR created: `docs/decisions/HEWITT_ACTOR_MODEL.md`
   - [ ] `ACTOR_INTERFACE.md` - Complete rewrite
   - [ ] `DESIGN.md` - Update architecture
   - [ ] `README.md` - Update examples
   - [ ] `PROJECT_CONTEXT.md` - Update project overview

3. **Create New Example** (`examples/hewitt-model-example.ts`)
   - Demonstrate System as actor
   - Show nested systems (supervision)
   - Show proper receive/send semantics

4. **Blog Post / Announcement**
   - Explain why we migrated
   - Highlight benefits
   - Code examples showing improvements

5. **Performance Verification**
   - Benchmark before/after
   - Confirm no performance regression
   - Document any improvements

**Exit Criteria:**
- ✅ All documentation updated
- ✅ Migration guide complete
- ✅ New examples created
- ✅ Performance verified
- ✅ Ready for release

---

## Risk Management

### Risk Matrix

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Tests fail during migration | MEDIUM | HIGH | Migrate one module at a time, run tests after each |
| Breaking changes affect users | HIGH | HIGH | Phased approach, deprecation warnings, clear migration guide |
| Performance regression | LOW | MEDIUM | Benchmark before/after, optimize if needed |
| Hidden dependencies on old API | MEDIUM | MEDIUM | Comprehensive search, deprecation warnings catch usage |
| Team bandwidth insufficient | MEDIUM | HIGH | Timebox phases, can pause between phases if needed |

### Rollback Strategy

**At any phase, if critical issues found:**

1. **Phase 1-2:** Revert commits, no breaking changes yet
2. **Phase 3-4:** Keep deprecation shims longer, extend timeline
3. **Phase 5+:** Emergency patch release, restore compatibility layer

**Rollback Trigger Conditions:**
- Test failures that cannot be resolved within 2 days
- Critical bug discovered in production
- Major performance regression (>20% slower)
- Team consensus that migration should pause

### Success Metrics

**Pre-Migration Baseline:**
- Tests: 100 passing, 0 failing
- Coverage: TBD%
- Performance: TBD ms/operation

**Post-Migration Target:**
- Tests: 100 passing, 0 failing (maintain coverage)
- Coverage: >= baseline (no reduction)
- Performance: <= 105% of baseline (max 5% slower)
- Zero deprecation warnings in codebase
- All documentation updated

## Timeline Summary

| Phase | Duration | Milestone |
|-------|----------|-----------|
| Phase 1: Foundation | Week 1 | New interfaces added, tests pass |
| Phase 2: Implementations | Week 2-3 | All actors use receive() |
| Phase 3: Tests | Week 4 | All tests updated |
| Phase 4: System Migration | Week 5 | Registry → System complete |
| Phase 5: Remove Old API | Week 6 | Breaking change deployed |
| Phase 6: Documentation | Week 7 | Ready for release |

**Total Estimated Time:** 7 weeks (can be compressed or extended based on team capacity)

## Open Questions

1. **Should we support both Registry and System APIs indefinitely?**
   - Recommendation: No, clean break in 2.0
   - Keep Registry as internal detail only

2. **Do we need adapter for third-party code?**
   - Recommendation: Provide `LegacyActorAdapter` if demand exists
   - Document in migration guide

3. **Should System support remote actors now?**
   - Recommendation: Not in scope for this migration
   - Future enhancement, Hewitt model enables it

4. **Version numbering: 2.0 or 1.x?**
   - Recommendation: 2.0 (breaking change)
   - Clearly signals API change

## Next Steps

1. **Review this plan** with team
2. **Get approval** to proceed
3. **Assign owners** for each phase
4. **Create project board** to track progress
5. **Start Phase 1** when ready

## References

- [ADR: Hewitt Actor Model](docs/decisions/HEWITT_ACTOR_MODEL.md)
- [Actor Interface Documentation](ACTOR_INTERFACE.md)
- [Current Registry Implementation](src/actors/registry.ts)
- [Current Actor Interface](src/actors/base.ts)
