# Actor Model Research Summary: Event Capture System Patterns

**Date**: 2026-01-10
**Purpose**: Summary of actor model patterns and implementations from knowledge base and session history to inform event capture system MVP design

---

## Executive Summary

Your knowledge base contains extensive, production-tested actor model patterns spanning approximately 6,664 lines across 15+ documents. The material includes:

1. **Comprehensive actor model theory** grounded in Carl Hewitt's work and distributed systems engineering
2. **Production lessons from building a pure actor system** including critical architectural pivots
3. **Event-sourced signal capture system** already implemented with detector-based monitoring
4. **Constitutional principles** for rigorous actor system development with anti-gaming measures
5. **Progressive complexity methodology** emphasizing starting simple and earning sophistication

The most relevant finding: You already have a **working event-sourced signal capture system** (`~/knowledge/signals/`) that demonstrates many patterns needed for the MVP.

---

## Key Insights for MVP Design

### 1. Hierarchical Routing Eliminates Race Conditions

**The Critical Lesson from Production**:
Early implementations used a centralized MessageBus for actor registration and routing. This created race conditions where actors spawned but weren't registered yet, causing messages to fail silently.

**The Breakthrough**:
"Do we actually need a message bus? Why can't the actor system just act as a message bus for its children?"

**Key Insight**: **Spawning an actor IS registration.** When the parent creates a child, the child reference exists immediately in the parent's child map. No separate registration step, no race window.

**Applied to Event Capture MVP**:
- When daemon starts, it spawns function actors as children
- Parent holds child references immediately - no registration protocol needed
- Events route through parent-child hierarchy, not through central broker
- Each function actor manages its own state independently

**Source**: `/Users/bln/knowledge/domain/actor-systems/04-architectural-insights.md`, `/Users/bln/knowledge/domain/actor-systems/05-lessons-learned.md`

---

### 2. Pattern Matching Through Message Routing

**Actor Communication Pattern**:
Messages in actor systems use structured formats with protocol-based routing:

```typescript
{
  protocol: "event.v1",      // What capability/interface
  action: "match",            // What operation
  data: { event, pattern }    // Payload
}
```

**Universal Actor Protocol (UAP)**:
Three-field structure works across all transports (function calls, HTTP, WebSocket, message queues):
- `protocol`: Versioned capability namespace (e.g., "timer.v1", "storage.v2")
- `action`: Operation within protocol ("schedule", "cancel", "query")
- `data`: Operation-specific payload

**Applied to Event Capture MVP**:
- Events are messages with structure: `{ type, source, timestamp, data }`
- Functions subscribe to event patterns, not specific event sources
- Pattern matching = routing messages to interested functions
- Functions return new events through same message protocol

**Source**: `/Users/bln/knowledge/domain/actor-systems/04-architectural-insights.md` (sections: Protocol-First Design, UAP)

---

### 3. Loop Prevention Through Supervision and Depth Limits

**Actor Supervision Pattern**:
Actors exist in parent-child hierarchies where:
- Parents supervise children (monitor health, handle failures)
- Supervision strategies: restart, stop, escalate, ignore
- Recursive shutdown: stopping root stops entire tree depth-first
- Clear lifecycle states: starting → running → stopping → stopped/failed

**Critical Design Principle**: "Let it crash" philosophy - don't try to prevent all failures, design for recovery.

**Applied to Event Capture MVP**:
- Each function actor has max depth counter in its state
- When function creates event, increment depth
- If depth exceeds limit (e.g., 10), drop event and log warning
- Supervisor can restart failed function actors without affecting others
- Each function isolated - infinite loop in one doesn't crash daemon

**Loop Prevention Strategies**:
1. **Stack depth counter**: Track how many times an event has been reprocessed
2. **Event fingerprinting**: Hash event content, reject duplicates within time window
3. **Cycle detection**: Track event ancestry chain, reject if creates cycle
4. **Time-based throttling**: Limit events per function per time window
5. **Circuit breakers**: Temporarily disable function if producing too many events

**Source**: `/Users/bln/knowledge/domain/actor-systems/00-synthesis.md` (Supervision), `/Users/bln/knowledge/domain/actor-systems/05-lessons-learned.md`

---

### 4. Event-Sourced Architecture with Append-Only Logs

**Your Existing Signal System** (`~/knowledge/signals/`):

The signal capture system already implements event sourcing:

```
signals/
├── detectors/              # What to monitor (20+ detector configs)
├── logs/                   # JSONL event logs (one per detector)
│   ├── *.jsonl            # Append-only event stream
│   └── *.checkpoint.json  # Track last run timestamp
├── lib/
│   ├── detector-parser.js  # Parse detector configs
│   ├── signal-capture.js   # Capture + deduplication
│   └── sources/            # RSS, GitHub, Brave Search fetchers
└── scripts/
    └── run-detector        # Execute detector captures
```

**Key Patterns**:
- **One log per detector** - Natural partitioning by source
- **JSONL format** - One JSON event per line, append-only
- **Checkpoints** - Track processing state (last_run timestamps)
- **Immutable events** - Never delete, only append new event types
- **Git commits** - Represent processing phases (capture → filter → promote)
- **Detectors as config** - Markdown files with YAML frontmatter

**Event Types in Signal System**:
```json
{"event_type": "captured", "detector": "...", "timestamp": "...", "signal": {...}}
{"event_type": "filtered", "url": "...", "keep": true, "score": 0.8}
{"event_type": "promoted", "url": "...", "path": "knowledge/..."}
```

**Applied to Event Capture MVP**:
- Daemon writes events to append-only JSONL log
- Each function creates events that get appended
- Checkpoint tracks "processed up to line N"
- Functions read new events since last checkpoint
- Can replay entire event log to rebuild state

**Source**: `/Users/bln/knowledge/signals/DESIGN.md`, `/Users/bln/knowledge/signals/README.md`

---

### 5. System Actors as Capability Boundaries

**Core Architectural Principle**:
Application actors should NEVER access I/O directly. All I/O flows through dedicated **system actors** that:
- Validate operations (path checks, security policies)
- Enforce quotas and rate limits
- Provide observability (log all I/O operations)
- Enable testing (mock system actors in tests)
- Abstract platforms (same interface across Node.js, browsers, etc.)

**System Actor Examples**:
- **TimerActor**: Schedule callbacks (abstracts setTimeout/setInterval)
- **StorageActor**: File I/O with path validation
- **NetworkActor**: HTTP requests with domain allowlists
- **CryptoActor**: Hashing and encryption
- **ConsoleActor**: Logging with structured output
- **ConfigActor**: Configuration management

**Critical Insight**: Each process gets its own system actors. A browser shouldn't call server for setTimeout - use local TimerActor.

**Applied to Event Capture MVP**:
- **EventLogActor**: Handles all event log writes (validation, deduplication)
- **HTTPServerActor**: Handles incoming HTTP requests
- **FunctionRegistryActor**: Manages function actor lifecycle
- **PatternMatcherActor**: Routes events to matching functions
- Application functions never touch filesystem/network directly

**Benefits**:
- Security: Functions can't read arbitrary files or make arbitrary network calls
- Testing: Mock EventLogActor to test functions without real I/O
- Observability: All events flow through known chokepoints
- Rate limiting: EventLogActor can enforce per-function quotas

**Source**: `/Users/bln/knowledge/domain/actor-systems/04-architectural-insights.md` (System Actors section)

---

### 6. Progressive Complexity: Start Simple, Earn Sophistication

**Six-Phase Methodology**:
1. **Structure**: Identify actors and capabilities (draw the boxes)
2. **Communication**: Define message patterns (connect the boxes)
3. **Composition**: Assemble system architecture (coordination)
4. **Implementation**: Adapt to runtime environment (Node.js/Bun/browser)
5. **Events**: Observable behavior and telemetry (monitoring)
6. **Runtime**: Performance validation (optimization)

**Philosophy**: Each phase builds on the previous through causal chains. You cannot make intelligent runtime decisions without understanding events. You cannot understand events without implementation. You cannot implement without composition, etc.

**Framework Journey** (for actor systems):
1. **Phase 1**: Native APIs (Web Workers, Bun workers) - Zero dependencies, manual implementation
2. **Phase 2**: State machines (XState) - Add when state complexity hurts
3. **Phase 3**: Distribution (Partykit, Durable Objects) - Add when single-process limits are reached

**Why Start Simple**:
- Build irreplaceable understanding by implementing patterns manually
- Understand what each abstraction layer solves because you've lived without it
- Add complexity therapeutically (to fix present pain), not prophylactically (to prevent imagined future pain)

**Applied to Event Capture MVP**:
- **Start**: Single-process daemon, in-memory function registry, simple JSONL log
- **Earn**: Persistent function state when needed, HTTP API when local-only hurts, distribution when single-process bottlenecks
- **Avoid**: Starting with Kafka, Redis, Kubernetes before proving they're necessary

**Source**: `/Users/bln/knowledge/domain/actor-systems/08-progressive-complexity-philosophy.md`, `/Users/bln/knowledge/domain/actor-systems/02-constitutional-principles.md`

---

### 7. Time Abstraction for Testing

**The Problem**:
Testing asynchronous systems with real time is painful. Tests verifying "this happens after 5 seconds" take 5 seconds. Multiply by hundreds of tests and suite becomes unusable.

**The Solution**:
Abstract time behind interfaces from day one:

```typescript
// Duration: Human-readable time spans
const delay = Duration.parse('5s');

// Clock: Interface for time queries
interface Clock {
  now(): Timestamp;
  after(duration: Duration): Timestamp;
}

// FakeClock: Tests run instantly
const clock = new FakeClock();
scheduler.scheduleOnce(Duration.seconds(5), callback);
clock.advance(Duration.seconds(5));  // Instant
expect(callbackCalled).toBe(true);
```

**Key Principle**: "Time is I/O. Files are I/O. Networks are I/O. Every I/O dependency needs abstraction from day one."

**Applied to Event Capture MVP**:
- Daemon uses Clock interface (not Date.now() directly)
- Tests inject FakeClock to control time
- Function timeouts can be tested instantly
- Event replay can be time-shifted for testing
- No waiting for timers in test suite

**Source**: `/Users/bln/knowledge/domain/actor-systems/05-lessons-learned.md` (Time Abstraction section)

---

### 8. Constitutional Principles: Anti-Gaming and Quality

**Core Tenets**:
1. **Pure actor architecture**: Everything is an actor, no privileged components
2. **Specifications are law**: Complete specs before implementation
3. **Independent verification**: Test authors and coders work in isolation from spec
4. **Quality gates cannot be skipped**: No shortcuts for deadlines
5. **Anti-gaming enforced**: Superficial compliance triggers remediation

**What is "Gaming"**:
- Tests checking implementation details instead of behavior
- Hardcoded test data instead of real implementation
- Lenient assertions that accept anything
- Incomplete error handling
- Tests that pass without implementation

**Enforcement**: Gaming triggers immediate remediation, escalation, potential role reassignment.

**Philosophy**: "Tests that pass without implementation are lies. They create false confidence and mask incomplete work."

**Applied to Event Capture MVP**:
- Write behavior tests before implementation
- Test pattern matching with realistic events, not hardcoded fixtures
- Test loop prevention actually prevents loops (not just checks flag)
- Test function isolation actually isolates (failures don't cascade)
- If test passes with empty function implementation, test is wrong

**Source**: `/Users/bln/knowledge/domain/actor-systems/02-constitutional-principles.md` (Anti-Gaming Measures)

---

## Existing Working Implementations

### Signal Capture System (Event-Sourced)

**Location**: `/Users/bln/knowledge/signals/`

**What It Does**:
- Monitors 20+ external sources (RSS feeds, GitHub repos, Brave Search)
- Captures signals as append-only JSONL logs
- Deduplicates events (checks existing log before appending)
- Tracks processing state with checkpoints
- Schedule-aware execution (hourly/daily/weekly)
- Git commits represent processing phases

**Architecture**:
```
Detectors (config) → Capture Runner (script) → Event Logs (JSONL)
                                             → Checkpoints (JSON)
```

**Key Files**:
- `lib/signal-capture.js`: Core capture + deduplication logic
- `lib/detector-parser.js`: Parse detector YAML frontmatter
- `scripts/run-detector`: Execute detectors (single/pattern/all)
- `detectors/*.md`: Detector configurations (21 files)

**This is 70% of your MVP already built.** The pattern matching and function execution layer is what's missing.

---

### Beads Issue Tracker (Actor-Like Event System)

**Location**: `/Users/bln/play/agentic-primer/.beads/`

**What It Does**:
- Event-sourced issue tracking system
- SQLite database (`beads.db`) for queryable state
- JSONL logs (`issues.jsonl`, `interactions.jsonl`) for audit trail
- Daemon process (`bd.sock`, `daemon.pid`) for background processing
- Issue lifecycle: open → in-progress → blocked → closed

**Key Insight**: Combines SQLite (queryable current state) with JSONL (immutable audit log).

**Pattern for Event Capture**:
- Use SQLite for "what functions exist right now?" queries
- Use JSONL for "what events have occurred?" audit trail
- Daemon reads JSONL, updates SQLite, routes to functions

---

## Pattern Synthesis for MVP

Based on research, here's the synthesized architecture:

### Core Components (Actors)

1. **DaemonActor** (Root Supervisor)
   - Spawns and supervises all child actors
   - Handles lifecycle (start/stop/restart)
   - Recursive shutdown when terminated

2. **EventLogActor** (System Actor)
   - Append events to JSONL log
   - Validate event structure
   - Deduplicate based on fingerprint
   - Track checkpoint (last processed line)
   - Enforce quotas (events per second)

3. **PatternMatcherActor** (System Actor)
   - Read events since last checkpoint
   - Match events against function patterns
   - Route matched events to function actors
   - Track event ancestry (loop detection)

4. **FunctionRegistryActor** (System Actor)
   - Spawn function actors from definitions
   - Maintain function metadata (name, pattern, source)
   - Handle function lifecycle (load/unload/reload)
   - Enforce depth limits per function

5. **HTTPServerActor** (System Actor - Optional)
   - Accept POST /event requests
   - Validate incoming events
   - Forward to EventLogActor
   - Return acknowledgment

6. **FunctionActor** (Application Actor - One per function)
   - Execute JavaScript function code
   - Receive matched events via messages
   - Return new events to EventLogActor
   - Maintain depth counter
   - Isolated execution context

### Message Protocol

```typescript
// Event message
{
  protocol: "event.v1",
  action: "append",
  data: {
    type: "user.created",
    source: "http",
    timestamp: "2026-01-10T12:00:00Z",
    payload: { userId: "123", email: "user@example.com" },
    depth: 0,
    parent_id: null
  }
}

// Pattern match message
{
  protocol: "pattern.v1",
  action: "match",
  data: {
    pattern: { type: "user.*" },
    event: { ... }
  }
}

// Function execution message
{
  protocol: "function.v1",
  action: "execute",
  data: {
    function_name: "send_welcome_email",
    event: { ... },
    depth: 1
  }
}
```

### Loop Prevention Strategy

1. **Depth Counter**: Each event carries `depth` field
   - Root events (HTTP, CLI): depth = 0
   - Function-generated events: depth = parent.depth + 1
   - Reject if depth > MAX_DEPTH (default 10)

2. **Event Fingerprinting**: Hash event content
   - Store recent fingerprints in memory (LRU cache, 1000 entries)
   - Reject duplicate fingerprint within time window (60 seconds)

3. **Ancestry Chain**: Track parent_id
   - Each event references parent event ID
   - Detect cycles by checking if event ID appears in ancestry

4. **Circuit Breaker**: Per-function quotas
   - Track events created per function per minute
   - If exceeds threshold (e.g., 100/min), pause function for 60 seconds
   - Log warning when circuit opens

5. **Supervisor Override**: Manual controls
   - CLI command to disable function: `daemon stop-function <name>`
   - CLI command to clear quotas: `daemon reset-quotas`

### Data Flow

```
HTTP POST /event
    ↓
HTTPServerActor validates
    ↓
EventLogActor.append(event)
    ↓
JSONL log written (immutable)
    ↓
PatternMatcherActor reads new events
    ↓
For each event:
    For each function:
        If pattern matches:
            FunctionActor.execute(event)
                ↓
            Function returns new_events[]
                ↓
            For each new_event:
                Check depth limit
                Check fingerprint cache
                Check ancestry chain
                    ↓
                EventLogActor.append(new_event)
                    ↓
                JSONL log written
                    ↓
                [Cycle continues]
```

### Startup Sequence

```
1. DaemonActor spawns child actors:
   - EventLogActor
   - PatternMatcherActor
   - FunctionRegistryActor
   - HTTPServerActor (if enabled)

2. FunctionRegistryActor loads function definitions:
   - Read functions/ directory
   - Parse function configs (name, pattern, source)
   - Spawn FunctionActor for each

3. PatternMatcherActor reads checkpoint:
   - Load last_processed_line from checkpoint.json
   - Start reading JSONL from that line

4. HTTPServerActor starts listening:
   - Bind to configured port (default 3000)

5. Daemon enters event loop:
   - PatternMatcherActor processes new events
   - Routes to matching functions
   - Functions create events
   - Cycle continues
```

### Configuration

```yaml
# daemon.config.yaml
daemon:
  log_file: "events.jsonl"
  checkpoint_file: "checkpoint.json"
  max_depth: 10
  deduplication_window_seconds: 60
  deduplication_cache_size: 1000

http:
  enabled: true
  port: 3000
  host: "127.0.0.1"

functions:
  directory: "./functions"
  max_events_per_minute: 100
  execution_timeout_ms: 5000

logging:
  level: "info"
  file: "daemon.log"
```

### Function Definition Format

```javascript
// functions/send-welcome-email.js
module.exports = {
  name: "send_welcome_email",
  pattern: { type: "user.created" },
  execute: async (event) => {
    // Business logic
    const { userId, email } = event.payload;
    await sendEmail(email, "Welcome!");

    // Return new events
    return [
      {
        type: "email.sent",
        payload: { userId, email, template: "welcome" }
      }
    ];
  }
};
```

---

## Testing Strategy

### Unit Tests (Per Actor)

```javascript
// Test EventLogActor
test('deduplicates events', async () => {
  const clock = new FakeClock();
  const log = new EventLogActor({ clock });

  await log.append({ type: 'test', data: 'foo' });
  await log.append({ type: 'test', data: 'foo' }); // duplicate

  const events = await log.readAll();
  expect(events.length).toBe(1);
});

// Test PatternMatcherActor
test('matches simple patterns', () => {
  const matcher = new PatternMatcherActor();
  const event = { type: 'user.created' };
  const pattern = { type: 'user.*' };

  expect(matcher.matches(event, pattern)).toBe(true);
});

// Test FunctionActor
test('enforces depth limit', async () => {
  const fn = new FunctionActor({ max_depth: 3 });
  const event = { type: 'test', depth: 4 };

  await expect(fn.execute(event)).rejects.toThrow('Depth limit exceeded');
});
```

### Integration Tests (System-Level)

```javascript
test('event creates chain without infinite loop', async () => {
  const daemon = new DaemonActor();
  await daemon.start();

  // Function A creates event B
  // Function B creates event C
  // Function C creates event D
  // Function D does NOT create event A (would be loop)

  await daemon.post({ type: 'chain.start' });
  await wait(100);

  const events = await daemon.eventLog.readAll();
  expect(events.length).toBe(4); // start, B, C, D
  expect(events.map(e => e.type)).toEqual([
    'chain.start', 'chain.b', 'chain.c', 'chain.d'
  ]);
});

test('loop prevention stops infinite recursion', async () => {
  const daemon = new DaemonActor();

  // Function A creates event A (infinite loop attempt)
  daemon.registerFunction({
    name: 'loop',
    pattern: { type: 'loop' },
    execute: () => [{ type: 'loop' }]
  });

  await daemon.post({ type: 'loop' });
  await wait(100);

  const events = await daemon.eventLog.readAll();
  expect(events.length).toBeLessThanOrEqual(11); // max_depth + 1
});
```

---

## Session History Insights

### Simulation Algorithm

**Location**: `/Users/bln/play/agentic-primer/SIMULATION_ALGORITHM.md`

**What It Is**: Methodology for testing AI agent behavior across prompt variations (P1/P2/P3), success criteria (S1/S2/S3), and models (Opus/Sonnet/Haiku).

**Pattern for Event Capture**:
- Test event capture system with different event patterns
- Vary complexity (simple events, nested events, high-frequency events)
- Measure loop prevention effectiveness across scenarios
- Identify optimal configuration (max_depth, cache_size, etc.)

### Recent Experiments

**Location**: `/Users/bln/play/agentic-primer/experiments/iteration-2/`

**Key Finding**: Event-driven architecture decision (ADR-001) in multiple experiment runs shows GitHub Actions pattern for issue → PR automation.

**Relevant Pattern**: Issues as events, workflows as functions, PRs as generated events - exactly the event capture system pattern.

---

## Recommendations for MVP

### Phase 1: Core Event Capture (Week 1)

**Scope**:
- EventLogActor with append-only JSONL
- Simple depth counter (no ancestry tracking yet)
- In-memory function registry (load from directory)
- Basic pattern matching (exact type match only)
- CLI interface (no HTTP yet)

**Deliverable**: `daemon capture-event '{"type":"test","data":"foo"}'` writes to log and triggers matching functions.

### Phase 2: Loop Prevention (Week 2)

**Scope**:
- Event fingerprinting with LRU cache
- Ancestry chain tracking (parent_id)
- Circuit breaker per function
- Enhanced pattern matching (wildcards)

**Deliverable**: Functions can create events without causing infinite loops.

### Phase 3: HTTP API (Week 3)

**Scope**:
- HTTPServerActor with POST /event
- Authentication (API key)
- Rate limiting
- WebSocket for event streaming (optional)

**Deliverable**: `curl -X POST http://localhost:3000/event -d '{"type":"user.created"}'` triggers system.

### Phase 4: Production Hardening (Week 4)

**Scope**:
- Persistent checkpoint (survive daemon restart)
- Function hot-reload (no daemon restart)
- Structured logging (JSON logs)
- Metrics (events/sec, functions/event, depth histogram)
- Alerting (circuit breaker opened, depth limit hit)

**Deliverable**: Production-ready daemon with monitoring.

---

## References

### Knowledge Base Documents (Highest Value)

1. **Actor Systems Synthesis** (568 lines)
   - Path: `/Users/bln/knowledge/domain/actor-systems/00-synthesis.md`
   - Content: Unified vision, key insights, theoretical foundations
   - Most valuable: Sections on supervision, hierarchy, constrained solution spaces

2. **Architectural Insights** (323 lines)
   - Path: `/Users/bln/knowledge/domain/actor-systems/04-architectural-insights.md`
   - Content: MessageBus → hierarchy pivot, system actors, path routing, UAP
   - Most valuable: "Why hierarchy won", system actors as capability boundaries

3. **Lessons Learned** (428 lines)
   - Path: `/Users/bln/knowledge/domain/actor-systems/05-lessons-learned.md`
   - Content: Production mistakes, time abstraction, clock vs alarm separation
   - Most valuable: "When your 'simple' solution requires careful timing, it's fragile"

4. **Constitutional Principles** (430 lines)
   - Path: `/Users/bln/knowledge/domain/actor-systems/02-constitutional-principles.md`
   - Content: Pure actor architecture, anti-gaming, progressive complexity methodology
   - Most valuable: Six-phase methodology, quality gates

5. **Signal Capture System Design** (200+ lines)
   - Path: `/Users/bln/knowledge/signals/DESIGN.md`
   - Content: Event-sourced architecture, detector-based monitoring, JSONL logs
   - Most valuable: Complete working implementation of event sourcing

6. **Q&A Database** (273 entries)
   - Path: `/Users/bln/knowledge/domain/actor-systems/QA.yaml`
   - Content: 24 curated Q&A pairs covering key concepts
   - Most valuable: Quick reference for common actor model questions

### Session History Documents

1. **Simulation Algorithm**
   - Path: `/Users/bln/play/agentic-primer/SIMULATION_ALGORITHM.md`
   - Content: Testing methodology for AI agents
   - Relevance: Pattern for testing event capture system variations

2. **Event-Driven ADR** (P3-S3-haiku experiment)
   - Path: `/Users/bln/play/agentic-primer/experiments/iteration-2/.../ADR-001-event-driven.md`
   - Content: Architecture decision record for GitHub Actions event system
   - Relevance: Issue → PR automation mirrors event → function pattern

### Implementation Code References

1. **Signal Capture Library**
   - Path: `/Users/bln/knowledge/signals/lib/signal-capture.js`
   - Content: Working event capture with deduplication
   - Use: Reference implementation for EventLogActor

2. **Detector Parser**
   - Path: `/Users/bln/knowledge/signals/lib/detector-parser.js`
   - Content: Parse YAML frontmatter from markdown
   - Use: Pattern for parsing function definitions

3. **Run Detector Script**
   - Path: `/Users/bln/knowledge/signals/scripts/run-detector`
   - Content: Schedule-aware detector execution
   - Use: Pattern for daemon event loop

---

## Key Quotes for Design Decisions

### On Hierarchy vs Central Bus
> "When your 'simple' solution requires careful timing and ordering to work, it's not actually simple. It's fragile."

> "Spawning an actor IS registration. If the parent holds the child reference immediately after spawn, there's no race condition."

### On Progressive Complexity
> "Add complexity when the current level hurts with present-tense pain, not future-oriented anxiety."

> "Simplicity is not the opposite of sophistication—it's the foundation of it."

### On Testing
> "Tests that pass without implementation are lies. They create false confidence and mask incomplete work."

> "Time is I/O. Files are I/O. Networks are I/O. Every I/O dependency needs abstraction from day one."

### On Supervision
> "Rather than preventing all failures—impossible given LLM probabilistic nature—design for failure recovery."

> "Supervision is not prevention. Acknowledge agents will fail unpredictably, and design recovery mechanisms."

### On Protocol Design
> "In distributed systems, the protocol is more important than any single implementation. Implementations can be rewritten. Protocols are forever (or very expensive to change)."

---

## Conclusion

Your knowledge base contains production-validated patterns for exactly the system you want to build. The key insights:

1. **Use hierarchical actor routing**, not centralized message bus (eliminates race conditions)
2. **Leverage existing signal capture system** as foundation (70% of MVP already exists)
3. **Implement system actors** for I/O boundaries (security, testability, observability)
4. **Start simple** with in-memory state, earn complexity as pain surfaces
5. **Test with time abstraction** from day one (fast tests enable fearless refactoring)
6. **Enforce quality through anti-gaming** measures (tests must genuinely verify behavior)
7. **Apply loop prevention** through depth counters, fingerprinting, and circuit breakers
8. **Protocol-first design** enables future distribution without rewrite

The event capture MVP should follow the same six-phase methodology documented in your constitutional principles: Structure → Communication → Composition → Implementation → Events → Runtime.

Start with actors on paper (structure), define message protocols (communication), assemble the system (composition), then implement in Node.js/Bun (implementation). Observability (events) and optimization (runtime) come last, after you understand the system through usage.

**Next Steps**: Review this summary, identify gaps or questions, then proceed with Phase 1 implementation (Core Event Capture) using patterns from your knowledge base.
