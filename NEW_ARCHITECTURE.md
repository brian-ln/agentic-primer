# Architecture: Protocol-First Convergence Platform

**Version:** 0.1.0 (February 2026)
**Status:** Active Development

---

## Overview

Agentic Primer is a **protocol-first convergence platform** for multi-agent actor systems. The architecture is built on three core principles:

1. **Protocol-First Design** - WIT interfaces define contracts, implementations follow
2. **Universal Domain Model** - Shared types across all implementations (63+ primitives)
3. **Actor-Based Concurrency** - Message-passing actors with hierarchical addressing

---

## Architectural Layers

### Layer 1: Protocol Definitions (WIT + JSON Schema)

**Purpose:** Language-agnostic interface definitions and type specifications

**Components:**
- **JSON Schema** - Source of truth for domain types
- **WIT Interfaces** - WASM Component Model protocol definitions
- **Generated Artifacts** - TypeScript types, Zod validators, language bindings

**Key Files:**
- `packages/protocols/schema/domain.schema.json` - Complete domain model (63+ types)
- `packages/protocols/wit/domain.wit` - WIT interface definitions
- `packages/protocols/src/domain.types.ts` - Generated TypeScript types
- `packages/protocols/src/domain.validators.ts` - Generated Zod validators

**Design Decisions:**

1. **JSON Schema as Source of Truth**
   - **Why:** Universal format, excellent tooling, generates to multiple targets
   - **Trade-offs:** Less expressive than TypeScript, requires code generation
   - **Alternatives Considered:** TypeScript-first (harder WIT generation), WIT-first (limited tooling)

2. **WASM Component Model + WIT**
   - **Why:** Language-agnostic, WASM ecosystem alignment, future-proof for multi-language
   - **Trade-offs:** Newer ecosystem, fewer examples, learning curve
   - **Alternatives Considered:** Protocol Buffers (less WASM-native), JSON Schema only (no bindings)

### Layer 2: Implementation (TypeScript + Future Rust)

**Purpose:** Concrete implementations of protocol-defined interfaces

**Components:**

1. **@agentic-primer/protocols** (npm package)
   - Published TypeScript/Zod implementation
   - Runtime validation and type safety
   - Multiple export paths (types, validators, schema, wit)
   - Version: 0.1.0

2. **SEAG** (Simplify Environment for Agentic Growth)
   - Reference implementation of actor system
   - Path-based hierarchical addressing
   - Message routing and delivery
   - Actor lifecycle management
   - Location: `simplify/` subdirectory (branch: feature/path-addressing)

3. **Future: Rust WASM Components** (Optional)
   - Performance-critical algorithms
   - Convergence detection
   - Cost estimation
   - Triage classification

**Design Decisions:**

1. **TypeScript as Primary Implementation Language**
   - **Why:** Rapid development, excellent tooling, runs on Bun/Node/Workers/Browser
   - **Trade-offs:** Performance vs Rust, no compile-time guarantees beyond types
   - **When to use Rust:** Performance-critical paths, proven bottlenecks only

2. **Zod for Runtime Validation**
   - **Why:** Type-safe validation, excellent error messages, TypeScript integration
   - **Trade-offs:** Bundle size (mitigated by tree-shaking), runtime overhead
   - **Alternatives Considered:** io-ts (less ergonomic), custom validation (maintenance burden)

### Layer 3: Runtime Environments

**Purpose:** Execution environments for implementations

**Supported Runtimes:**

1. **Bun.js**
   - Primary development runtime
   - Fast startup, excellent DX
   - Used for: SEAG, CLI tools, local development

2. **Cloudflare Workers**
   - Production deployment target
   - Durable Objects for actor persistence
   - Used for: Signal Hub, event routing, distributed actors

3. **Browser**
   - Client-side processing
   - Extensions, interactive tools
   - Future: WASM components for performance

**Design Decisions:**

1. **Multi-Runtime Support**
   - **Why:** Different use cases need different runtimes (dev vs prod vs client)
   - **Trade-offs:** Must avoid runtime-specific APIs, careful feature detection
   - **Strategy:** Use protocol layer for portability, runtime adapters for specifics

---

## Core Domain Model

### Entity Hierarchy

```text
Entity (abstract)
├── Agent          - AI agents (execution, reasoning)
├── Human          - Human participants (commands, feedback)
├── Information    - Data artifacts (documents, code, context)
├── Model          - AI models (capabilities, configs)
├── Program        - Automated programs (scripts, services)
├── Provider       - Service providers (APIs, infrastructure)
├── Session        - Execution contexts (conversations, workflows)
└── Task           - Work units (goals, status, outputs)
```

### Graph Primitives

**Address** - Universal addressing for nodes and actors
```typescript
{
  id: string;           // Unique identifier
  scope: "node" | "edge" | "entity";
  path?: string;        // Optional hierarchical path (e.g., "system/actors/agent-123")
}
```

**Node** - Graph nodes (entities, references, data)
```typescript
{
  id: string;
  address: Address;
  type: NodeType;       // Entity types + graph metadata nodes
  data?: Record<string, unknown>;
  metadata?: Record<string, unknown>;
}
```

**Edge** - Directed relationships between nodes
```typescript
{
  id: string;
  source: Address;
  target: Address;
  type: EdgeType;       // "depends_on", "references", "produces", etc.
  metadata?: Record<string, unknown>;
}
```

### Message Protocol

**Message** - Async communication between actors
```typescript
{
  id: string;
  from: Address;        // Sender address
  to: Address;          // Recipient address
  type: MessageType;    // "command", "query", "event", "reply"
  payload: unknown;
  timestamp: string;    // ISO-8601
  correlationId?: string;
}
```

**MessageEnvelope** - Delivery metadata
```typescript
{
  message: Message;
  retryCount: number;
  deliveredAt?: string;
  failedAt?: string;
  error?: string;
}
```

### Actor Model

**ActorRef** - Reference to an actor
```typescript
{
  address: Address;
  path: string;         // Hierarchical path (e.g., "system/actors/agent-123")
  capabilities: string[];
}
```

**Actor Lifecycle:**
1. **Created** - Actor instantiated, address assigned
2. **Started** - Actor accepts messages
3. **Processing** - Actor handling messages
4. **Stopped** - Actor no longer accepts messages
5. **Failed** - Actor encountered unrecoverable error

---

## Path-Based Addressing

### Hierarchical Addressing Design

**Problem:** Flat IDs are opaque and don't reflect system structure

**Solution:** Support both IDs and hierarchical paths in Address type

**Example Paths:**
```text
system/actors/agent-123           - Actor in system hierarchy
workflows/orchestration/phase-1   - Workflow phase
sessions/2026-02-06/abc123        - Session with date context
topics/ai/completions             - Pub/Sub topic hierarchy
```

**Implementation:**

1. **PathResolver Utility**
   - Resolves paths to addresses
   - Handles relative/absolute paths
   - Validates path syntax
   - Performance: <1μs per resolution

2. **Hierarchical Routing**
   - Routes messages based on path hierarchy
   - Supports wildcard subscriptions (e.g., "topics/ai/*")
   - Enables scoped routing (e.g., all actors under "system/actors/")

3. **Zero-Copy Optimization**
   - Path stored as string, split on-demand
   - Memoization for frequently accessed paths
   - Lazy resolution when possible

**Performance Baseline (Feb 5-6):**
- Simple resolution: <1μs average
- Complex resolution (wildcards): <5μs average
- Throughput: 100,000+ paths/sec on M1 Mac
- Memory: <100 bytes per path on average

**Design Decisions:**

1. **Paths as Optional Enhancement**
   - **Why:** Backward compatibility, gradual adoption, IDs still valid
   - **Trade-offs:** Two addressing modes (complexity), validation required
   - **Migration Strategy:** Add paths gradually, never remove ID support

2. **String-Based Paths (not arrays)**
   - **Why:** Ergonomics, URL-like familiarity, easy to log/debug
   - **Trade-offs:** Parsing overhead (mitigated by memoization)
   - **Alternatives Considered:** Array<string> (less ergonomic), separate Path type (more complexity)

---

## SEAG Integration Architecture

**SEAG (Simplify Environment for Agentic Growth)** is the reference implementation of the protocol-driven actor system.

### SEAG Components

1. **Actor System**
   - Actor creation and lifecycle management
   - Message delivery and routing
   - Supervision and fault tolerance
   - Hierarchy management (parent/child actors)

2. **Message Bus**
   - Pub/Sub topic-based messaging
   - Point-to-point message delivery
   - Wildcard subscriptions
   - Message persistence (optional)

3. **Storage Layer**
   - Actor state persistence
   - Message queue durability
   - Event sourcing support
   - Query layer for actor discovery

4. **REPL & CLI**
   - Interactive development interface
   - Actor inspection and debugging
   - Message tracing and logging
   - AI-assisted help commands

### SEAG Architecture Diagram

```text
┌─────────────────────────────────────────────────────────────┐
│                        SEAG System                           │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Actor System Layer                       │  │
│  │  • Actor Registry (PathResolver)                      │  │
│  │  • Lifecycle Management (create/start/stop)           │  │
│  │  • Supervision (restart policies)                     │  │
│  └────────────────┬─────────────────────────────────────┘  │
│                   │                                          │
│  ┌────────────────▼─────────────────────────────────────┐  │
│  │              Message Bus Layer                        │  │
│  │  • Routing (path-based + wildcard)                    │  │
│  │  • Pub/Sub Topics (e.g., seag://system/topic/trace)   │  │
│  │  • Delivery Guarantees (at-least-once)                │  │
│  └────────────────┬─────────────────────────────────────┘  │
│                   │                                          │
│  ┌────────────────▼─────────────────────────────────────┐  │
│  │              Storage Layer                            │  │
│  │  • Actor State (Durable Objects / KV)                 │  │
│  │  • Message Queue (R2 / Queue)                         │  │
│  │  • Event Log (append-only)                            │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Interface Layer                          │  │
│  │  • REPL (interactive commands)                        │  │
│  │  • CLI (scripting, automation)                        │  │
│  │  • HTTP API (remote access)                           │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### SEAG Integration with Protocols

**Before Protocol Integration:**
- Custom types per module
- Inconsistent validation
- No shared type definitions
- Manual serialization

**After Protocol Integration:**
- Uses `@agentic-primer/protocols` types everywhere
- Zod validation at boundaries
- Type-safe message passing
- Automatic JSON serialization

**Example Usage:**
```typescript
import { Address, Message, Agent, addressSchema } from '@agentic-primer/protocols';

// Create actor address
const addr: Address = {
  id: "agent-123",
  scope: "entity",
  path: "system/actors/agent-123"
};

// Validate at runtime
const result = addressSchema.safeParse(addr);
if (!result.success) {
  throw new Error(`Invalid address: ${result.error}`);
}

// Send message
const msg: Message = {
  id: crypto.randomUUID(),
  from: senderAddr,
  to: addr,
  type: "command",
  payload: { action: "process", data: {...} },
  timestamp: new Date().toISOString()
};

await actorSystem.send(msg);
```

---

## Cross-System Integration Strategy

### Target Systems

1. **Signal Hub** (Cloudflare Workers)
   - Event routing and aggregation
   - Durable Objects for actor persistence
   - Pub/Sub topic management
   - Integration: Use protocols package, implement WIT interfaces

2. **Convergence Framework**
   - Multi-model orchestration
   - Decision fusion algorithms
   - Cost-aware routing
   - Integration: Share protocol types, implement convergence detection

3. **AI Capacity Tracking**
   - Usage monitoring and quotas
   - Cost estimation
   - Provider abstraction
   - Integration: Usage-tracking protocol, ModelConfig/ProviderConfig types

### Integration Phases

**Phase 1: Type Sharing** (Current)
- Install @agentic-primer/protocols in all projects
- Replace custom types with protocol types
- Add Zod validation at boundaries
- Verify type compatibility

**Phase 2: Protocol Conformance** (Next)
- Implement WIT interfaces in each system
- Validate protocol conformance with tests
- Ensure consistent message formats
- Document integration patterns

**Phase 3: Cross-System Messaging** (Future)
- Enable message passing between systems
- Implement routing across system boundaries
- Add distributed tracing
- Test end-to-end scenarios

**Phase 4: Distributed Actor System** (Vision)
- Actors can move between systems
- Transparent location (local vs remote)
- Distributed supervision
- Global addressing and discovery

---

## Design Principles

### 1. Protocol-First

**Definition:** Protocols define interfaces, implementations follow

**Why:**
- Enables multiple implementations (TypeScript, Rust, etc.)
- Clear contracts prevent integration issues
- Language-agnostic specifications

**How:**
- Start with WIT/JSON Schema definitions
- Generate code from protocols
- Validate conformance with tests

### 2. Type Safety End-to-End

**Definition:** Static types at compile-time, runtime validation at boundaries

**Why:**
- Catch errors early in development
- Prevent invalid data from entering system
- Self-documenting code

**How:**
- TypeScript strict mode
- Zod validators at all external boundaries
- Generated types from schemas

### 3. Actor-Based Concurrency

**Definition:** Message-passing actors, no shared mutable state

**Why:**
- Simplifies reasoning about concurrency
- Natural fault tolerance (actor isolation)
- Scalable to distributed systems

**How:**
- All communication via messages
- Actors own their state
- Supervision hierarchies for fault recovery

### 4. Progressive Enhancement

**Definition:** Start simple, add complexity only when needed

**Why:**
- Faster initial development
- Easier to understand and maintain
- Optimize based on real measurements

**How:**
- TypeScript first, Rust only for proven bottlenecks
- IDs work, paths are optional enhancement
- Local first, distributed when necessary

### 5. Observability by Default

**Definition:** Built-in tracing, logging, metrics

**Why:**
- Debug production issues effectively
- Understand system behavior
- Performance analysis and optimization

**How:**
- Structured logging everywhere
- Trace message flow with correlation IDs
- Publish metrics to topic streams
- REPL for live inspection

---

## Performance Considerations

### Current Performance Baseline

**Path Resolution:**
- Simple paths: <1μs average
- Complex wildcards: <5μs average
- Throughput: 100k+ paths/sec

**Message Routing:**
- Local delivery: <10μs average
- Pub/Sub fanout (10 subscribers): <100μs average
- Queue persistence: <1ms average (Cloudflare Queue)

**Actor Operations:**
- Actor creation: <100μs average
- Message send: <50μs average (in-memory)
- State persistence: <5ms average (Durable Objects)

### Optimization Strategies

1. **Lazy Evaluation**
   - Don't parse paths until needed
   - Delay validation to boundaries
   - Cache parsed structures

2. **Memoization**
   - Cache frequently resolved paths
   - Reuse validated objects
   - Shared immutable data structures

3. **Zero-Copy Operations**
   - Pass references, not copies
   - Strings over arrays for paths
   - Structured sharing for large payloads

4. **Batch Processing**
   - Batch message deliveries
   - Batch persistence operations
   - Amortize overhead across operations

5. **WASM for Hotspots**
   - Compile critical algorithms to WASM
   - Call from TypeScript via WIT bindings
   - Rust for performance + safety

---

## Security Considerations

### Threat Model

1. **Malicious Messages**
   - Threat: Crafted messages bypass validation
   - Mitigation: Zod validation at all boundaries, schema enforcement

2. **Actor Impersonation**
   - Threat: Messages forged with fake sender addresses
   - Mitigation: Cryptographic signatures (future), trusted system paths only

3. **Resource Exhaustion**
   - Threat: Flood system with messages, create infinite actors
   - Mitigation: Rate limiting, quotas, supervision timeouts

4. **Injection Attacks**
   - Threat: Path traversal, command injection via paths
   - Mitigation: Path validation, allowlist characters, escape user input

### Security Principles

1. **Validate Everything**
   - All external input validated with Zod
   - Path syntax validated before resolution
   - Message payloads validated against schemas

2. **Principle of Least Privilege**
   - Actors only access their own state
   - Capabilities required for privileged operations
   - Supervision hierarchy enforces boundaries

3. **Defense in Depth**
   - Multiple validation layers
   - Runtime checks + static types
   - Monitoring + alerting for anomalies

---

## Future Directions

### Near-Term (Q1 2026)

- Complete SEAG integration with protocols
- Implement cross-system messaging (Signal Hub ↔ SEAG)
- Add protocol conformance tests
- Document integration patterns

### Mid-Term (Q2 2026)

- Rust WASM components for performance-critical paths
- Distributed actor system (actors across Workers/Bun)
- Advanced routing (content-based, policy-based)
- Observability dashboards

### Long-Term (Q3+ 2026)

- Multi-language implementations (Rust, Go, Python)
- Actor persistence and migration
- Distributed supervision
- Protocol versioning and evolution

---

## Related Documentation

**Protocol Definitions:**
- `packages/protocols/README.md` - Protocol package documentation
- `packages/protocols/schema/domain.schema.json` - JSON Schema source
- `packages/protocols/wit/domain.wit` - WIT interface definitions

**Implementation Guides:**
- `docs/protocols/INTEGRATION_STRATEGY.md` - Integration approach
- `docs/protocols/WIT_PLATFORM_MIGRATION_PLAN.md` - Comprehensive migration plan
- `docs/protocols/PHASE1_VALIDATION_REPORT.md` - Phase 1 validation results

**SEAG Documentation:**
- `simplify/` - SEAG reference implementation (branch: feature/path-addressing)
- Path addressing design docs (in simplify/)
- Performance baseline reports (in simplify/)

**Project Context:**
- `CURRENT_STATE.md` - Project evolution and milestones
- `AGENTS.md` - Agent instructions and workflows
- `README.md` - Quick start and overview

---

*This architecture document describes the current design (Feb 2026) and will evolve as the project develops. For implementation details, see the linked documentation and source code.*
