# Exploration Roadmap: tk-agents

**Status**: Active Strategic Guide
**Last Updated**: 2026-01-15
**Purpose**: Synthesize analysis insights into actionable exploration priorities

---

## Executive Summary

This roadmap synthesizes insights from critical analysis, Socratic questioning, Erlang architecture exploration, and various deep dives (error models, event sourcing, latency tiers, pressure testing) into a prioritized plan for conceptual exploration and learning.

**Core Insight**: tk-agents is at a critical decision point. The current ~600 lines of simple TypeScript work fine for exploration, but the 2000+ lines of proposed Erlang patterns, event sourcing, and distributed systems features represent months of work. We need to be honest about what we're building: a pragmatic exploration vehicle or production infrastructure?

**Recommended Path**: Focus on **high-learning-value, low-complexity explorations** that validate core concepts before committing to complex infrastructure.

---

## Current State (As of 2026-01-15)

### What We Have (Working Code)
- **Core Protocol**: Graph.send() message routing (~100 lines)
- **Domain Nodes**: TaskNode, KnowledgeNode (~550 lines)
- **Actors**: ClaudeActor, BashActor (~310 lines)
- **Registry**: Actor management (~100 lines)
- **Tests**: Basic protocol and actor tests (19 passing)
- **Total**: ~600 lines of working TypeScript

### What We've Analyzed (Documents)
- **ERLANG_ARCHITECTURE_ANALYSIS.md**: 2000+ lines proposing supervision, mailboxes, OTP patterns
- **CRITICAL_ANALYSIS.md**: Identifies over-engineering risks and pragmatic concerns
- **SOCRATIC_QUESTIONS.md**: 57 probing questions exposing assumptions
- **EVENT_SOURCING_EXPLORATION.md**: Event log, replay, time travel design
- **ERROR_MODEL_EXPLORATION.md**: Error taxonomy, retry patterns, circuit breakers
- **LATENCY_LOCALITY_TIERS.md**: Hot/warm/cold actor placement strategies
- **PRESSURE_TEST_SCENARIOS.md**: 12 concurrency failure modes
- **TEST_PROTOCOL_DESIGN.md**: Layered testing strategy
- **SIMPLE_IMPROVEMENTS_PLAN.md**: 3 tactical improvements (rename spawn, death detection, task-to-executor edges)
- **CONCEPT_GRAPH/**: 50 concepts, 61 relationships mapped

### The Gap
**Analysis to Implementation Ratio**: ~15:1 (analysis documents vastly exceed working code)

**Critical Questions**:
1. Are we building a **research prototype** to explore ideas?
2. Are we building an **actor framework** with production aspirations?
3. Are we building a **pragmatic task/knowledge system** that happens to use actors?

**The answer determines everything that follows.**

---

## Exploration Themes

Based on all analysis, explorations cluster into 5 major themes:

### Theme 1: Core Coordination Patterns
**Questions**: How do actors coordinate? What breaks under concurrency? How do we prevent deadlocks and race conditions?

**Key Documents**: PRESSURE_TEST_SCENARIOS.md, ERLANG_ARCHITECTURE_ANALYSIS.md sections 1-4

**Learning Value**: High - foundational to any actor system

### Theme 2: Fault Tolerance & Recovery
**Questions**: What happens when actors crash? How do we restart gracefully? What's recoverable vs fatal?

**Key Documents**: ERROR_MODEL_EXPLORATION.md, ERLANG_ARCHITECTURE_ANALYSIS.md section 1

**Learning Value**: High - systems fail, must handle it

### Theme 3: Time & History
**Questions**: Can we replay execution? What is event sourcing good for? How do we debug "how did we get here?"

**Key Documents**: EVENT_SOURCING_EXPLORATION.md

**Learning Value**: Medium - interesting but potentially over-complex

### Theme 4: Scale & Distribution
**Questions**: How do we scale beyond one machine? What are the latency/locality tradeoffs? How do we tier actors?

**Key Documents**: LATENCY_LOCALITY_TIERS.md, ERLANG_ARCHITECTURE_ANALYSIS.md section 3

**Learning Value**: Low for MVP - premature optimization

### Theme 5: Developer Experience
**Questions**: How do we test this? How do we debug it? How do we make it comprehensible?

**Key Documents**: TEST_PROTOCOL_DESIGN.md, CRITICAL_ANALYSIS.md section 2

**Learning Value**: Very High - unusable systems aren't valuable

---

## Prioritized Exploration Roadmap

### Immediate Priority (Next 1-2 Weeks)

These explorations have high learning value, low complexity, and directly address critical gaps.

#### 1. Error Classification & Result Types
**Why**: Current error handling (throw exceptions) is insufficient. Need explicit error categories.

**What to Learn**:
- How to distinguish transient vs permanent errors
- Whether Result types improve or complicate the API
- If error classification helps or just adds verbosity

**Approach**:
1. Add ActorError type with categories (validation, transient, fatal, permanent)
2. Update Response interface
3. Convert throws to error returns in TaskNode, KnowledgeNode
4. Measure: Does this make debugging easier?

**Effort**: 1-2 days
**Success Criteria**: All message handlers return structured errors, no throws (except truly unexpected)

**Documents**: ERROR_MODEL_EXPLORATION.md sections 1-3, SIMPLE_IMPROVEMENTS_PLAN.md

---

#### 2. Death Detection (Exception Handling)
**Why**: Actors can crash silently. Need to detect and report failures.

**What to Learn**:
- How to detect actor crashes via exceptions
- Whether heartbeat monitoring adds value
- What to do with death events (log, restart, escalate?)

**Approach**:
1. Wrap Registry.send() with try/catch
2. Emit 'actor_died' events
3. Remove dead actors from registry
4. Optional: Add heartbeat with configurable interval

**Effort**: 0.5-1 day
**Success Criteria**: Death events are emitted and logged, registry stays clean

**Documents**: SIMPLE_IMPROVEMENTS_PLAN.md section 2

---

#### 3. Pressure Testing (Concurrency Reality Check)
**Why**: Current synchronous design masks concurrency issues. Need to expose problems early.

**What to Learn**:
- Which coordination problems are real vs theoretical
- Whether async mailboxes reveal new failure modes
- How to test concurrent actor systems

**Approach**:
1. Implement testing infrastructure (concurrent(), withJitter())
2. Run Scenarios 1-3 (state mutations, write conflicts, circular deps)
3. Document which failures occur and why
4. Decide: Do we need async mailboxes or is sync good enough?

**Effort**: 2-3 days
**Success Criteria**: 3 critical scenarios pass or reveal specific coordination gaps

**Documents**: PRESSURE_TEST_SCENARIOS.md scenarios 1-3, TEST_PROTOCOL_DESIGN.md

---

#### 4. Simple Improvements (Low-Hanging Fruit)
**Why**: Three simple changes with clear value and minimal risk.

**What to Learn**:
- Whether "spawn" → "create_task" improves comprehension
- If task-to-executor edges clarify assignment
- Whether these changes inform larger patterns

**Approach**:
1. Rename spawn message to create_task (honest naming)
2. Implement ActorNode, ExecutionNode, WorkerPoolNode
3. Add execution edges (task → executor)
4. Test assignment queries work

**Effort**: 2-3 hours
**Success Criteria**: Clearer semantics, queryable execution relationships

**Documents**: SIMPLE_IMPROVEMENTS_PLAN.md all 3 improvements

---

### Medium-Term Explorations (Weeks 3-6)

These build on immediate work and explore deeper patterns.

#### 5. State Machine Formalization (gen_statem Pattern)
**Why**: TaskNode has implicit state machine. Making it explicit may reveal design issues.

**What to Learn**:
- Does explicit state machine prevent bugs?
- Is gen_statem pattern worth the complexity?
- How to handle invalid transitions gracefully

**Approach**:
1. Define valid state transitions explicitly
2. Implement state handler functions per state
3. Add transition validation
4. Test all state combinations

**Effort**: 1-2 weeks
**Success Criteria**: All state transitions are validated, invalid transitions return errors

**Documents**: ERLANG_ARCHITECTURE_ANALYSIS.md section 2.2, DESIGN.md state machine

**Depends On**: Pressure testing to reveal which state races are real

---

#### 6. Minimal Event Logging (Not Full Event Sourcing)
**Why**: Audit trail is valuable. Full event sourcing is complex. Start simple.

**What to Learn**:
- Does event logging help debugging?
- What's the storage/performance cost?
- Do we ever actually replay events?

**Approach**:
1. Log all graph.send() calls to JSONL file
2. Add basic query tools (filter by actor, by time)
3. Use for debugging actual problems
4. Measure: Do we reference logs? Or are they write-only?

**Effort**: 1-2 days
**Success Criteria**: Event log captures all operations, logs are actually used for debugging

**Documents**: EVENT_SOURCING_EXPLORATION.md sections 1-3, 9

**Defer**: Full replay, time travel UI, compensation events

---

#### 7. Timeout Strategies (Prevent Hanging)
**Why**: ClaudeActor can hang indefinitely. Need timeouts for external operations.

**What to Learn**:
- What timeout values work in practice?
- How to handle timeout errors gracefully
- Whether cancellation is needed or timeout alone suffices

**Approach**:
1. Add withTimeout() utility
2. Wrap ClaudeActor.send() with 2-minute timeout
3. Add configurable timeouts to BashActor
4. Test with deliberately slow operations

**Effort**: 0.5-1 day
**Success Criteria**: No more infinite hangs, timeouts return clear errors

**Documents**: ERROR_MODEL_EXPLORATION.md section 3.4

---

#### 8. Layered Test Suite (Foundation for Quality)
**Why**: Current tests are ad-hoc. Need systematic coverage.

**What to Learn**:
- Does layered testing (graph protocol → domain → actor lifecycle) improve clarity?
- Which layer has most bugs?
- How to test concurrent operations?

**Approach**:
1. Implement Layer 1: Graph Protocol Tests (send/receive, edges)
2. Implement Layer 2: Domain Protocol Tests (task lifecycle, knowledge ops)
3. Implement Layer 3: Actor Lifecycle Tests (death detection, monitoring)
4. Measure coverage

**Effort**: 1-2 weeks
**Success Criteria**: 80%+ test coverage, tests catch real bugs

**Documents**: TEST_PROTOCOL_DESIGN.md all layers

---

### Long-Term Questions (Weeks 7+)

These are research directions, not immediate implementations.

#### 9. Supervision Trees (If Needed)
**When**: Only if death detection reveals frequent crashes requiring auto-restart

**What to Learn**:
- Does supervision actually improve reliability?
- What restart strategies work in TypeScript (vs Erlang)?
- Is the complexity justified by value?

**Approach**:
1. Prototype SupervisorActor with one-for-one strategy
2. Test with deliberately failing actors
3. Measure: Does it restart correctly? Are there restart storms?

**Effort**: 1-2 weeks
**Success Criteria**: Supervisor restarts failed actors without cascading failures

**Documents**: ERLANG_ARCHITECTURE_ANALYSIS.md section 1, ERROR_MODEL_EXPLORATION.md section 3.5

**Defer Until**: Multiple actor crashes observed in practice

---

#### 10. Async Mailboxes (If Concurrency Becomes Real)
**When**: Only if pressure testing reveals that synchronous messaging is insufficient

**What to Learn**:
- Do async mailboxes add value or just complexity?
- How to handle backpressure and queue overflow?
- Is selective receive useful in practice?

**Approach**:
1. Implement Mailbox with priority queuing
2. Convert one actor (TaskNode) to async
3. Compare synchronous vs async performance
4. Measure: Does async improve throughput? Or just complicate debugging?

**Effort**: 2-3 weeks
**Success Criteria**: Async actor handles high message rate without blocking

**Documents**: ERLANG_ARCHITECTURE_ANALYSIS.md section 4, PRESSURE_TEST_SCENARIOS.md scenario 4

**Defer Until**: Synchronous design proves to be a bottleneck

---

#### 11. Tiered Actor Placement (Far Future)
**When**: Only if actor count exceeds memory limits or distribution is needed

**What to Learn**:
- Do access patterns actually correlate with hot/warm/cold tiers?
- Is automatic promotion/demotion worth the complexity?
- How much does SQLite persistence cost?

**Approach**:
1. Implement AccessLogger to track access patterns
2. Add SQLite warm tier for infrequent actors
3. Test with 10,000+ actors
4. Measure: Hit rates, latency, storage savings

**Effort**: 3-4 weeks
**Success Criteria**: Hot tier contains <10% of actors, 80%+ hit rate

**Documents**: LATENCY_LOCALITY_TIERS.md all sections

**Defer Until**: Actor count exceeds 1000 and memory becomes a concern

---

#### 12. Full Event Sourcing (Research Only)
**When**: Only if event log proves valuable and replay is needed

**What to Learn**:
- Does event replay actually work for deterministic actors?
- How to handle non-deterministic actors (ClaudeActor)?
- Is time travel useful or just novelty?

**Approach**:
1. Build on minimal event logging
2. Implement replay engine for TaskNode
3. Test reconstruction accuracy
4. Measure: Do we ever use replay? Or is it a curiosity?

**Effort**: 2-3 weeks
**Success Criteria**: Replay produces identical state for deterministic operations

**Documents**: EVENT_SOURCING_EXPLORATION.md sections 4-8

**Defer Until**: Event logging is proven valuable and replay use cases are clear

---

### Experiments Worth Running

These are quick experiments (1-3 days) to test specific hypotheses.

#### Experiment A: Message Passing vs Direct Calls
**Hypothesis**: Message passing adds debugging overhead without architectural benefit

**Method**:
1. Create parallel implementations: MessagePassingTask vs DirectCallTask
2. Implement same workflow in both
3. Compare: Code clarity, debuggability, performance

**Learn**: Is message passing absolutism justified?

**Effort**: 1 day
**Documents**: CRITICAL_ANALYSIS.md section 2.2, SOCRATIC_QUESTIONS.md Q1-Q2

---

#### Experiment B: Result Types vs Exceptions
**Hypothesis**: Result types are more verbose but improve error handling

**Method**:
1. Convert TaskNode to use Result types
2. Measure: Lines of code, error handling clarity, caller verbosity
3. Survey: Which is easier to debug?

**Learn**: Is functional error handling worth it in TypeScript?

**Effort**: 1 day
**Documents**: ERROR_MODEL_EXPLORATION.md section 3.1

---

#### Experiment C: SQLite Persistence Overhead
**Hypothesis**: SQLite adds <10ms latency, making warm tier viable

**Method**:
1. Benchmark in-memory Map vs SQLite for 1000 actors
2. Measure: Read latency, write latency, storage size
3. Compare: Memory usage vs disk usage

**Learn**: Is SQLite fast enough for warm tier?

**Effort**: 0.5 day
**Documents**: LATENCY_LOCALITY_TIERS.md section 1, EVENT_SOURCING_EXPLORATION.md section 4.3

---

#### Experiment D: Temporal Pattern Detection
**Hypothesis**: Sequential access patterns are predictable and prefetchable

**Method**:
1. Log access sequences for 100 task executions
2. Detect patterns (task A → task B with >70% probability)
3. Implement prefetch based on patterns
4. Measure: Hit rate, latency reduction

**Learn**: Does prefetching hide latency in practice?

**Effort**: 2-3 days
**Documents**: LATENCY_LOCALITY_TIERS.md sections 5-6

---

#### Experiment E: Compression Ratios
**Hypothesis**: Knowledge nodes compress 3-5x, making storage cheaper

**Method**:
1. Collect 100 realistic knowledge nodes (text, code, embeddings)
2. Test compression: gzip, zstd, delta encoding
3. Measure: Compression ratio, CPU overhead, decompression speed

**Learn**: Is compression worth the CPU cost?

**Effort**: 0.5 day
**Documents**: LATENCY_LOCALITY_TIERS.md direction 3

---

## Decision Framework: When to Stop

For each exploration, apply these filters:

### Go/No-Go Criteria

**GO if**:
✅ Learning value is clear and specific
✅ Effort is bounded and measurable
✅ Success criteria are objective
✅ Failure teaches us something valuable
✅ Aligns with "exploration" phase goals

**STOP if**:
❌ "Might need it someday" is the only justification
❌ Effort exceeds 2 weeks without clear learning milestones
❌ Success depends on multiple other unproven components
❌ Problem is hypothetical, not observed
❌ Simpler alternative exists and hasn't been tried

### Complexity Budget

**Total complexity budget for exploration phase**: ~10 weeks of focused work

**Already spent**: ~2 weeks (current codebase + analysis)
**Remaining**: ~8 weeks

**Current allocation**:
- Immediate (weeks 1-2): 4 days ← Highest ROI
- Medium-term (weeks 3-6): 15 days ← Build on immediate work
- Long-term (weeks 7+): Defer until immediate/medium prove valuable

### The "Finish One Thing" Rule

**Before starting new explorations, complete one from previous tier.**

- ✅ Don't start error model until death detection works
- ✅ Don't start supervision until error model is validated
- ✅ Don't start async mailboxes until pressure tests reveal need

**Rationale**: Completing builds confidence. Spreading effort across 12 partial explorations teaches nothing.

---

## Immediate Next Steps (Top 3)

Based on all analysis, these are the **highest value, lowest risk** explorations to start with:

### 1. Error Classification + Result Types (1-2 days)
**Why First**: Foundation for all error handling. Immediately improves debugging.

**Action Items**:
- [ ] Create src/actors/errors.ts with ActorError type
- [ ] Update Response interface
- [ ] Convert TaskNode throws to error returns
- [ ] Add tests for error categories
- [ ] Document: Did this make debugging easier? More or less verbose?

**Success**: All actors return structured errors, no uncaught exceptions

---

### 2. Death Detection + Simple Improvements (1 day)
**Why Second**: Combines two low-effort, high-value changes.

**Action Items**:
- [ ] Wrap Registry.send() with try/catch, emit actor_died
- [ ] Rename "spawn" → "create_task" in TaskNode
- [ ] Add tests for death detection
- [ ] Verify: Do death events help catch problems?

**Success**: Dead actors are detected and removed, clearer naming

---

### 3. Pressure Testing Scenarios 1-3 (2-3 days)
**Why Third**: Reality check. Exposes which coordination problems are real.

**Action Items**:
- [ ] Implement testing infrastructure (concurrent(), chaos tools)
- [ ] Run Scenario 1: Concurrent task state mutations
- [ ] Run Scenario 2: Knowledge node write conflicts
- [ ] Run Scenario 3: Circular task dependencies
- [ ] Document: Which failures occur? What coordination mechanisms are missing?

**Success**: Know which concurrency problems are real vs theoretical

---

## What NOT to Do (Yet)

Based on critical analysis, defer these until prerequisites are met:

### Defer: Full Erlang Patterns
- ❌ OTP behaviors (gen_server, gen_statem) - Complex, uncertain value
- ❌ Process groups - No need until multiple actor types discovered
- ❌ ETS-like shared tables - JavaScript Map works fine for exploration
- ❌ Hot code swapping - Not needed for prototyping

**Why**: 2000 lines of Erlang analysis for 600 lines of code is backwards. Build first, then pattern-match to Erlang if needed.

### Defer: Full Event Sourcing
- ❌ CQRS projections - Over-engineered for exploration
- ❌ Snapshotting - Premature optimization
- ❌ Schema evolution - Single version sufficient
- ❌ Time travel UI - Novelty, not necessity

**Why**: Start with simple event logging. Full ES is a commitment, not a feature.

### Defer: Distribution
- ❌ Distributed registry - Single process is fine
- ❌ Network actors - No use case yet
- ❌ Consensus protocols - Overkill for MVP
- ❌ Edge caching - Premature

**Why**: "Might scale someday" is not a reason to add complexity now.

### Defer: Advanced Patterns
- ❌ Circuit breakers - No evidence of cascading failures
- ❌ Complex retry logic - Start with simple timeout
- ❌ Compression/delta encoding - Storage is not a bottleneck
- ❌ Content-addressable storage - Interesting but speculative

**Why**: YAGNI. Add when problems are observed, not anticipated.

---

## How to Use This Roadmap

### For Agents Implementing Features
1. **Start with immediate priority** (sections 1-4)
2. **Check dependencies** (some explorations require others to complete first)
3. **Follow success criteria** (objective measures of completion)
4. **Document learnings** (update this roadmap with what you discovered)

### For Strategic Planning
1. **Review every 2 weeks** (adjust priorities based on learnings)
2. **Apply decision framework** (Go/No-Go criteria for new explorations)
3. **Respect complexity budget** (don't exceed 10 weeks without shipping something)
4. **Finish one thing before starting another**

### For Research Questions
1. **Consult analysis documents** (linked throughout this roadmap)
2. **Run experiments** (quick hypothesis tests before big implementations)
3. **Update Socratic questions** (new questions emerge from new learnings)

---

## Success Metrics for Exploration Phase

Track these to know if explorations are valuable:

### Learning Metrics
- [ ] Can explain actor model benefits/costs clearly to others
- [ ] Can predict which patterns apply to new problems
- [ ] Have evidence (not just theory) for design decisions
- [ ] Understand tradeoffs, not just patterns

### Quality Metrics
- [ ] Test coverage >80% for implemented features
- [ ] No known crashes or hangs in basic workflows
- [ ] Error messages are actionable
- [ ] Debugging is not significantly harder than direct calls

### Pragmatism Metrics
- [ ] Code-to-analysis ratio improves (currently 1:15, target 1:3)
- [ ] Features complete within estimated effort
- [ ] Can demo working system in <5 minutes
- [ ] Codebase is comprehensible to new contributors

### "Ship Something" Metric
- [ ] **By week 8**: Have working demo that showcases core concepts
  - Multi-step task decomposition
  - Knowledge querying
  - Basic error handling
  - Actor coordination

**If we can't demo this by week 8, we've over-complicated.**

---

## Alignment with Project Goals

Recall from PROJECT_CONTEXT.md:

> "tk-agents is a conceptual exploration and evolution of connected ideas, NOT a production product."

This roadmap respects that by:
- **Prioritizing learning** over completeness
- **Accepting incomplete implementations** as exploration vehicles
- **Valuing rapid iteration** over production hardening
- **Focusing on idea clarity** not optimization

**But** it also challenges the tendency to:
- **Over-design** before implementing
- **Reference patterns** without testing them
- **Add complexity** without measuring value
- **Defer the hard questions** about what we're actually building

---

## Conclusion: The Core Question

The roadmap ultimately depends on answering SOCRATIC_QUESTIONS.md Q57:

> **What problem are you actually solving?**

If the answer is:
- **"Exploring actor patterns"** → Focus on coordination, supervision, mailboxes
- **"Building task/knowledge system"** → Focus on task lifecycle, knowledge queries, practical UX
- **"Learning distributed systems"** → Focus on event sourcing, tiering, failure modes
- **"Creating AI agent infrastructure"** → Focus on ClaudeActor integration, execution patterns

**The current roadmap assumes**: Exploring actor patterns in context of task/knowledge coordination, with pragmatic focus on what actually works.

**If the answer changes, this roadmap must be revisited.**

---

## Document Maintenance

**Update frequency**: Every 2 weeks or after completing 3 explorations

**Update process**:
1. Mark completed explorations with ✅
2. Document learnings and decisions
3. Reprioritize based on new information
4. Add new explorations if clear need emerges
5. Remove explorations if shown to be unnecessary

**Next review**: 2026-01-29 (after immediate priority explorations complete)

---

## References

All analysis documents synthesized into this roadmap:

- **ERLANG_ARCHITECTURE_ANALYSIS.md** - Supervision, OTP, mailboxes
- **CRITICAL_ANALYSIS.md** - Pragmatism checks, over-engineering risks
- **SOCRATIC_QUESTIONS.md** - Probing assumptions, uncovering gaps
- **PROJECT_CONTEXT.md** - Goals, philosophy, current state
- **SIMPLE_IMPROVEMENTS_PLAN.md** - Tactical next steps
- **EVENT_SOURCING_EXPLORATION.md** - Event log, replay, time travel
- **ERROR_MODEL_EXPLORATION.md** - Error taxonomy, retry, circuit breakers
- **LATENCY_LOCALITY_TIERS.md** - Actor placement, hot/warm/cold
- **PRESSURE_TEST_SCENARIOS.md** - Concurrency failure modes
- **TEST_PROTOCOL_DESIGN.md** - Layered testing strategy
- **DESIGN.md** - Core architecture
- **CONCEPT_GRAPH/** - Interconnected concepts map

---

**This roadmap is a living document. It should evolve as we learn.**

**Start with immediate priority. Build one thing at a time. Ship something.**
