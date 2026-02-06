# Agentic Primer WIT Protocol Definitions

**Version:** 0.1.0
**Status:** Phase 1 Complete
**Created:** 2026-02-05

## Overview

This directory contains WIT (WebAssembly Interface Types) protocol definitions for the Agentic Primer platform. These interfaces provide language-agnostic protocol specifications for AI provider integrations, event-driven architecture, message-graph communication, and usage tracking.

### Protocol-First Architecture

The WIT definitions serve as the **single source of truth** for cross-project protocols:

- **Signal Hub**: AI triage with event sourcing
- **Convergence Framework**: Multi-agent coordination
- **Simplify**: Browser extension actors
- **AI Capacity**: Usage tracking and quotas

## WIT Packages

### 📦 types (364 LOC)

**Package:** `agentic-primer:types@0.1.0`
**Source:** Adapted from UGS `types.wit`

Shared type system providing foundation for all interfaces:

- **Error handling**: `error-info`, severity levels, retry strategies
- **Tracing**: `span-context` for distributed tracing (OpenTelemetry compatible)
- **Security**: `security-context`, authentication credentials
- **Configuration**: Type-safe config values (string, int, float, JSON)
- **Observability**: Health checks, metrics, quota information

**Interfaces:**
- `types`: Core type definitions
- `version`: Semantic version utilities
- `utils`: UUID generation, timestamp validation

### 🤖 provider (524 LOC)

**Package:** `agentic-primer:provider@0.1.0`
**Source:** Adapted from UGS `adapter.wit`

AI provider integration with cost tracking and capability detection:

**Supported Providers:**
- Anthropic (Messages API with prompt caching)
- OpenAI (Chat Completions)
- Google Gemini
- Cohere
- Custom protocols

**Key Features:**
- Cost estimation before API calls
- Token usage tracking (input/output/cached)
- Rate limiting metadata
- Provider capabilities (streaming, vision, function-calling)
- Model metadata and recommendations

**Interfaces:**
- `provider`: Core provider operations
- `metadata`: Model discovery and selection

**Example Use Cases:**
- Signal Hub: AI triage cost tracking
- /ai skill: Multi-provider routing
- Convergence: Worker cost monitoring

### 📡 event (299 LOC)

**Package:** `agentic-primer:event@0.1.0`
**Source:** New design for universal event protocol

Universal event envelope and pub/sub protocol for event-driven integration:

**Event Types:**
- User interactions: `user-message`, `agent-response`
- Signal Hub: `signal-created`, `signal-triaged`, `action-required`
- Convergence: `iteration-started`, `iteration-completed`, `convergence-detected`
- Usage: `usage-tracked`, `quota-exceeded`
- System: `system-health`, `system-error`

**Key Features:**
- UUID v7 event IDs (time-sortable)
- Distributed tracing integration
- Priority levels (low, normal, high, critical)
- Correlation IDs for request tracing
- Flexible subscription filters

**Interfaces:**
- `event`: Event envelope and types
- `handler`: Event processing
- `bus`: Pub/sub protocol
- `store`: Event persistence and querying

**Example Use Cases:**
- Signal Hub: Event sourcing for signals
- Convergence: Iteration tracking
- AI Capacity: Usage event streaming

### 📨 message (347 LOC)

**Package:** `agentic-primer:message@0.1.0`
**Source:** Merged from UGS `component.wit` and `gateway.wit`

Message-graph protocol for actor communication and routing:

**Node Types:**
- `producer`: Emits messages (e.g., Convergence supervisor)
- `consumer`: Processes messages (e.g., Convergence worker)
- `relay`: Forwards messages
- `processor`: Transforms messages
- `hybrid`: Multiple roles

**Key Features:**
- Message routing with policy control
- Node lifecycle management
- Health monitoring
- Delivery tracking with receipts
- Retry logic for failed messages

**Interfaces:**
- `message`: Message envelope and node types
- `node`: Node implementation
- `router`: Message routing and registry
- `delivery`: Tracked delivery with confirmation

**Example Use Cases:**
- Convergence: Supervisor ↔ worker communication
- Simplify: Actor system backbone

### 💰 usage-tracking (323 LOC)

**Package:** `agentic-primer:usage-tracking@0.1.0`
**Source:** New design for AI capacity tracking

AI capacity tracking across all projects:

**Key Features:**
- Token usage with cache support (Anthropic prompt caching)
- Cost calculation per provider/model
- Budget management with alerts
- Quota enforcement
- Usage statistics by provider, model, user, source

**Pricing Support:**
- Input/output token costs
- Cache write/read costs (Anthropic)
- Batch pricing tiers
- Custom pricing models

**Interfaces:**
- `tracking`: Usage events and statistics
- `budget`: Budget configuration and enforcement
- `estimation`: Cost estimation and pricing info

**Example Use Cases:**
- AI Capacity: Track all AI usage
- Signal Hub: Triage cost monitoring
- Convergence: Run cost attribution
- /ai skill: User quota management

### 🌍 worlds (255 LOC)

**Package:** `agentic-primer:worlds@0.1.0`
**Source:** Adapted from UGS `world.wit`

Component world definitions for WASM composition:

**Worlds Defined:**

1. **provider-component**: AI provider implementations
2. **event-handler-component**: Event processors
3. **message-node-component**: Message-graph nodes
4. **usage-tracking-component**: AI capacity trackers
5. **signal-hub-service**: Complete Signal Hub (all capabilities)
6. **convergence-service**: Convergence Framework deployment
7. **browser-extension**: Client-side (Simplify)
8. **cli-tool**: Command-line tools (/ai skill)
9. **development**: Testing and development

## Architecture Patterns

### Event Sourcing (Signal Hub)

```wit
// Signal created → Event published
event-envelope {
  id: "01JK...",
  event-type: signal-created,
  source: "signal-hub",
  payload: { signal-id, channel-id, message, ... }
}

// Event handler processes
handler.handle(event) -> result<option<event-envelope>>

// Derived event published
event-envelope {
  event-type: signal-triaged,
  correlation-id: "01JK...", // Links to signal-created
  payload: { triage-result, action-required, ... }
}
```

### Message Routing (Convergence)

```wit
// Supervisor sends task to worker
message-envelope {
  source: "supervisor-1",
  destination: "worker-2",
  message-type: "task-assignment",
  correlation-id: "run-123"
}

// Router resolves destination
router.route(message) -> list<route-info>

// Worker processes and responds
node.process(message) -> result<option<message-envelope>>
```

### Cost Tracking (All Projects)

```wit
// Before API call
provider.estimate-cost(request) -> cost-estimate {
  estimated-input-tokens: 1500,
  estimated-output-tokens: 500,
  total-estimated-cost: 0.0045 // USD
}

// After API call
usage-event {
  provider: "anthropic",
  model: "claude-sonnet-4-5",
  tokens: { input: 1523, output: 487, cached: 200 },
  cost-usd: 0.0042,
  source: "signal-hub"
}

// Track usage
tracking.track(usage-event)

// Check quota
budget.check-budget("user-123") -> quota-status {
  exceeded: false,
  usage-percentage: 67.3
}
```

## Integration Guide

### Using WIT Interfaces

#### 1. TypeScript (via jco)

```bash
# Generate TypeScript bindings
jco types core/wit/provider/ --out-dir src/types/

# Import in TypeScript
import type { Provider, ProviderRequest } from './types/provider';
```

#### 2. Rust (via wit-bindgen)

```bash
# Generate Rust bindings
wit-bindgen rust core/wit/provider/

# Use in Rust
use agentic_primer::provider::*;
```

#### 3. Validation

```bash
# Validate all packages
wasm-tools component wit core/wit/types/
wasm-tools component wit core/wit/provider/
wasm-tools component wit core/wit/event/
wasm-tools component wit core/wit/usage-tracking/
wasm-tools component wit core/wit/message/
```

## Source Attribution

This WIT protocol design is adapted from the Universal Graph System (UGS) project:

**Source:** `/Users/bln/play/projects/proj-20260131-090153/schemas/wit/`

| Agentic Primer | UGS Source | Changes |
|----------------|------------|---------|
| `types.wit` | `types.wit` | Namespace changed, same structure |
| `provider.wit` | `adapter.wit` | Focused on AI providers, added cost tracking |
| `event.wit` | New design | Universal event protocol |
| `message.wit` | `component.wit` + `gateway.wit` | Merged into message-graph protocol |
| `usage-tracking.wit` | New design | AI capacity tracking |
| `worlds.wit` | `world.wit` | Adapted for agentic-primer components |

**Total:** 2,112 lines of WIT definitions

## Validation Status

See [VALIDATION.md](./VALIDATION.md) for detailed validation status.

**Summary:**
- ✅ All 5 core packages validate independently
- ✅ 2,112 LOC of protocol definitions
- ⚠️ Cross-package world composition (Phase 3)

## Next Steps

### Phase 2: Repository Reorganization
- Create `services/` and `tooling/` directories
- Move Signal Hub implementation
- Set up monorepo structure

### Phase 3: TypeScript Integration
- Generate TypeScript bindings with jco
- Replace existing interfaces with generated types
- Update Signal Hub to use WIT types
- Integrate Convergence Framework
- Connect /ai skill

### Phase 4: WASM Components (Optional)
- Compile performance-critical algorithms to WASM
- Convergence detection algorithm
- Cost estimation
- Triage classification

## References

- **WIT Specification**: https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md
- **WASM Component Model**: https://github.com/WebAssembly/component-model
- **jco (TypeScript bindings)**: https://github.com/bytecodealliance/jco
- **wasm-tools**: https://github.com/bytecodealliance/wasm-tools
- **Migration Plan**: [WIT_PLATFORM_MIGRATION_PLAN.md](../../WIT_PLATFORM_MIGRATION_PLAN.md)
