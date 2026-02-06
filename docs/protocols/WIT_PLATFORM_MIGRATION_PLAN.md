# WIT Platform Migration Plan

**Created:** 2026-02-05
**Purpose:** Transform agentic-primer into unified protocol-first platform
**Status:** Planning Phase

---

## Executive Summary

Transform agentic-primer into a unified, protocol-first platform by extracting WIT interfaces from the UGS project and adapting them for Signal Hub, Convergence Framework, and AI capacity tracking. This plan uses WASM Component Model + WIT for language-agnostic protocol definitions while maintaining TypeScript implementations initially.

### Key Goals

1. **Extract & adapt** 6 WIT files from UGS project (1,208 LOC)
2. **Reorganize** agentic-primer repository for protocol-first architecture
3. **Generate TypeScript bindings** from WIT definitions using `jco`
4. **Integrate** across Signal Hub, Convergence, Simplify, AI tracking
5. **Optional:** Compile performance-critical algorithms to WASM Components

### Success Metrics

- ✅ All WIT interfaces validated with `wasm-tools`
- ✅ TypeScript bindings generated and used across 3+ projects
- ✅ Zero breaking changes to existing Signal Hub functionality
- ✅ Protocol definitions serve as single source of truth
- ✅ Cross-project integration working (Signal Hub → Convergence → /ai)

### Timeline

- **Week 1:** WIT extraction and adaptation (Phase 1)
- **Week 2:** Repository reorganization (Phase 2)
- **Week 3:** TypeScript integration (Phase 3)
- **Week 4+:** Optional WASM components (Phase 4)
- **Week 5+:** Cross-system integration (Phase 5)

---

## Architecture Overview

### Three-Layer Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Protocol Layer (WIT)                       │
│  • provider.wit (AI provider interfaces)                     │
│  • event.wit (event protocol)                                │
│  • message.wit (message-graph protocol)                      │
│  • usage-tracking.wit (AI capacity)                          │
│  • types.wit (shared types)                                  │
│  • world.wit (component worlds)                              │
└────────────────────┬────────────────────────────────────────┘
                     │
                     v
┌─────────────────────────────────────────────────────────────┐
│           Implementation Layer (TypeScript/Rust)             │
│  TypeScript (primary):                                       │
│  • Signal Hub providers                                      │
│  • Simplify actors                                           │
│  • Convergence nodes                                         │
│  • AI capacity trackers                                      │
│                                                              │
│  Rust WASM (optional):                                       │
│  • Convergence detection algorithm                           │
│  • Cost estimation                                           │
│  • Triage classification                                     │
└────────────────────┬────────────────────────────────────────┘
                     │
                     v
┌─────────────────────────────────────────────────────────────┐
│              Runtime Layer (Bun/Workers/Browser)             │
│  • Cloudflare Workers (Durable Objects)                      │
│  • Bun.js (CLI tools, local development)                     │
│  • Browser (extensions, client-side processing)              │
└─────────────────────────────────────────────────────────────┘
```

### Repository Structure (Target)

```
agentic-primer/
├── core/
│   ├── wit/                      # WIT protocol definitions
│   │   ├── types.wit             # Shared types
│   │   ├── provider.wit          # Provider interface
│   │   ├── event.wit             # Event protocol
│   │   ├── message.wit           # Message-graph protocol
│   │   ├── usage-tracking.wit    # AI capacity tracking
│   │   └── world.wit             # Component worlds
│   │
│   ├── bindings/                 # Generated bindings
│   │   ├── typescript/           # Generated from WIT
│   │   └── rust/                 # Generated from WIT (optional)
│   │
│   └── wasm/                     # WASM components (optional)
│       ├── convergence/          # Convergence detection
│       └── cost-estimator/       # Cost estimation
│
├── services/                     # Deployed services
│   ├── signal-hub/               # Signal Hub (moved from brianln.ai)
│   ├── simplify/                 # Simplify (existing)
│   └── convergence/              # Convergence Framework (new)
│
├── tooling/                      # Development tools
│   ├── bmad-method/              # bmad-method (moved)
│   ├── spec-kit/                 # spec-kit (moved)
│   └── harness/                  # Simulation harness
│
├── experiments/                  # Experiments (existing)
├── scripts/                      # Build & deployment scripts
├── docs/                         # Documentation
└── package.json                  # Workspace root
```

---

## Phase 1: WIT Extraction & Adaptation

### Overview

Extract 6 WIT files from UGS project and adapt for agentic-primer use cases:
- Signal Hub (event sourcing, provider adapters)
- Convergence Framework (message-graph, iteration tracking)
- AI Capacity Tracking (usage events, cost estimation)

### Source Files (UGS Project)

Location: `/Users/bln/play/projects/proj-20260131-090153/schemas/wit/`

| File | LOC | Purpose | Adaptation Needed |
|------|-----|---------|-------------------|
| `types.wit` | ~270 | Shared types, errors, telemetry | Minimal - reuse as-is |
| `adapter.wit` | ~390 | Protocol adapters (OpenAI ↔ Anthropic) | Adapt to `provider.wit` |
| `component.wit` | ~70 | Component interface, lifecycle | Adapt to `message.wit` |
| `gateway.wit` | ~50 | Message routing | Merge into `message.wit` |
| `transport.wit` | ~40 | Transport abstraction | Merge into `message.wit` |
| `world.wit` | ~388 | World definitions (18 types) | Adapt for new use cases |
| **Total** | **~1,208** | | |

### Target Files (Agentic Primer)

Location: `/Users/bln/play/agentic-primer/core/wit/`

| File | Purpose | Key Features |
|------|---------|--------------|
| `types.wit` | Shared types | Error handling, tracing, encoding |
| `provider.wit` | AI provider interface | Capability detection, cost estimation |
| `event.wit` | Event protocol | Universal event envelope, pub/sub |
| `message.wit` | Message-graph protocol | Node interface, routing |
| `usage-tracking.wit` | AI capacity tracking | Usage events, quota management |
| `world.wit` | Component worlds | Provider, handler, tracker worlds |

### Adaptation Strategy

#### 1. types.wit → types.wit (Direct Copy)

**Changes:**
- Copy as-is from UGS
- Add any missing types for AI capacity tracking
- Ensure compatibility with Signal Hub event schema

**Key Types:**
- `error-info` (error handling)
- `trace-info` (distributed tracing)
- `payload-metadata` (encoding, compression)
- `content-encoding` (JSON, MessagePack, etc.)

#### 2. adapter.wit → provider.wit (Major Adaptation)

**Changes:**
- Rename `adapter` → `provider`
- Adapt `protocol-type` enum for AI providers:
  ```wit
  enum protocol-type {
      openai-chat,
      anthropic-messages,
      google-gemini,
      cohere,
      ugs-native,  // Keep for compatibility
      custom,
  }
  ```
- Add cost estimation methods:
  ```wit
  interface provider {
      estimate-cost: func(request: provider-request) -> result<cost-estimate, error-info>;
  }
  ```
- Add usage tracking integration

**Use Cases:**
- Signal Hub: Anthropic, OpenAI, Google providers
- AI Capacity: Usage extraction from responses
- Convergence: Model routing and cost tracking

#### 3. component.wit → message.wit (Significant Adaptation)

**Changes:**
- Keep `message` record (already perfect for message-graph)
- Rename `component` interface → `message-node` interface
- Add routing capabilities:
  ```wit
  interface message-node {
      id: func() -> string;
      send: func(message: message) -> result<_, error-info>;
      receive: func(message: message) -> result<_, error-info>;
      process: func(message: message) -> result<option<message>, error-info>;
  }
  ```
- Add `message-router` interface (from gateway.wit)

**Use Cases:**
- Convergence: Supervisor ↔ worker communication
- Simplify: Actor system backbone
- Signal Hub: Event-driven message passing (optional)

#### 4. gateway.wit + transport.wit → message.wit (Merge)

**Changes:**
- Extract routing logic from `gateway.wit`
- Extract transport abstraction from `transport.wit`
- Create unified `message-router` interface:
  ```wit
  interface message-router {
      register: func(node-id: string, capabilities: node-capabilities) -> result<_, error-info>;
      route: func(message: message) -> result<routing-decision, error-info>;
      send: func(message: message) -> result<_, error-info>;
  }
  ```

**Use Cases:**
- Message-graph: Route messages between nodes
- Convergence: Distribute tasks to workers
- Signal Hub: Route events to handlers (optional)

#### 5. NEW: usage-tracking.wit (Create from Scratch)

**Purpose:**
Track AI usage across Signal Hub, Convergence, /ai skill

**Key Interfaces:**
```wit
interface usage-tracker {
    track: func(event: usage-event) -> result<_, error-info>;
    get-stats: func(start: string, end: string) -> result<usage-stats, error-info>;
    check-quota: func(user-id: string) -> result<quota-status, error-info>;
}

record usage-event {
    timestamp: string,
    provider: string,
    model: string,
    tokens: token-usage,
    cost-usd: f64,
    session-id: option<string>,
    convergence-run-id: option<string>,
}
```

**Use Cases:**
- AI Capacity: Track usage from all sources
- Signal Hub: Track AI triage costs
- Convergence: Track convergence run costs

#### 6. world.wit → world.wit (Significant Adaptation)

**Changes:**
- Remove UGS-specific worlds (18 component types)
- Add worlds for new use cases:
  ```wit
  world provider-component {
      export provider;
  }

  world event-handler-component {
      export event-handler;
      import event-bus;
  }

  world message-node-component {
      export message-node;
      import message-router;
  }

  world usage-tracking-component {
      export usage-tracker;
      import event-bus;
  }
  ```

**Use Cases:**
- Define component boundaries
- Enable WASM component composition
- Document system architecture

### Validation Steps

After adaptation, validate all WIT files:

```bash
# Install wasm-tools
cargo install wasm-tools

# Validate WIT syntax
wasm-tools component wit /Users/bln/play/agentic-primer/core/wit/

# Generate TypeScript bindings (test)
npm install -g @bytecodealliance/jco
jco types /Users/bln/play/agentic-primer/core/wit/ --out-dir ./test-bindings/
```

Expected output:
- ✅ All WIT files pass validation
- ✅ TypeScript types generated successfully
- ✅ No circular dependencies
- ✅ All imports resolve correctly

---

## Phase 2: Repository Reorganization

### Overview

Reorganize agentic-primer to support protocol-first architecture:
1. Create `core/` directory for WIT and bindings
2. Move `simplify/` to `services/`
3. Prepare for Signal Hub migration
4. Reorganize tooling (bmad-method, spec-kit)

### Directory Structure Changes

#### Current Structure

```
agentic-primer/
├── simplify/                     # Actor system (115 files)
├── bmad-method/                  # Formula-based development
├── spec-kit/                     # Specification toolkit
├── experiments/                  # Simulation experiments
├── scripts/                      # Utilities
└── docs/                         # Knowledge base
```

#### Target Structure

```
agentic-primer/
├── core/
│   ├── wit/                      # Protocol definitions (NEW)
│   ├── bindings/                 # Generated bindings (NEW)
│   └── wasm/                     # WASM components (NEW, optional)
│
├── services/
│   ├── simplify/                 # Actor system (MOVED)
│   ├── signal-hub/               # Signal Hub (FUTURE)
│   └── convergence/              # Convergence Framework (FUTURE)
│
├── tooling/
│   ├── bmad-method/              # Formula-based development (MOVED)
│   ├── spec-kit/                 # Specification toolkit (MOVED)
│   └── harness/                  # Simulation harness (MOVED)
│
├── experiments/                  # Experiments (KEEP)
├── scripts/                      # Build & deployment scripts (KEEP)
├── docs/                         # Documentation (KEEP)
├── package.json                  # Workspace root (UPDATE)
└── pnpm-workspace.yaml           # Workspace config (NEW)
```

### Migration Steps

#### Step 1: Create Core Structure

```bash
cd /Users/bln/play/agentic-primer

# Create core directories
mkdir -p core/wit
mkdir -p core/bindings/typescript
mkdir -p core/bindings/rust
mkdir -p core/wasm

# Create services directory
mkdir -p services

# Create tooling directory
mkdir -p tooling
```

#### Step 2: Move Simplify

```bash
# Move simplify to services
mv simplify services/

# Update internal references
# (Handled in beads wit-002 through wit-004)
```

#### Step 3: Move Tooling

```bash
# Move bmad-method and spec-kit
mv bmad-method tooling/
mv spec-kit tooling/

# Create harness directory from experiments
mkdir -p tooling/harness
# Move simulation harness files (specific files in beads)
```

#### Step 4: Update Package Configuration

Create `pnpm-workspace.yaml`:
```yaml
packages:
  - 'core/bindings/typescript'
  - 'services/*'
  - 'tooling/*'
```

Update root `package.json`:
```json
{
  "name": "@agentic-primer/root",
  "private": true,
  "workspaces": [
    "core/bindings/typescript",
    "services/*",
    "tooling/*"
  ],
  "scripts": {
    "build:wit": "wasm-tools component wit core/wit/",
    "gen:types": "jco types core/wit/ --out-dir core/bindings/typescript",
    "build": "pnpm -r build",
    "test": "pnpm -r test"
  }
}
```

### Reference Updates

After reorganization, update all internal references:

1. **Import paths** in Simplify:
   ```typescript
   // Before
   import { ... } from '../some-module'

   // After
   import { ... } from '@agentic-primer/core'
   import { ... } from '@agentic-primer/simplify'
   ```

2. **Documentation links**:
   - Update all `README.md` files
   - Update `docs/` references
   - Update experiment references

3. **Build scripts**:
   - Update `scripts/` to handle new structure
   - Update CI/CD workflows (if any)

---

## Phase 3: TypeScript Integration

### Overview

Generate TypeScript bindings from WIT and integrate across projects:
1. Generate types with `jco types`
2. Update Signal Hub providers (if migrated)
3. Update Simplify actors
4. Update Convergence framework (new development)
5. Write protocol conformance tests

### WIT → TypeScript Generation

#### Step 1: Install Tools

```bash
# Install jco (WASM Component Model tooling)
npm install -g @bytecodealliance/jco

# Verify installation
jco --version
```

#### Step 2: Generate TypeScript Types

```bash
cd /Users/bln/play/agentic-primer

# Generate types from WIT
jco types core/wit/ --out-dir core/bindings/typescript/src

# Expected output structure:
# core/bindings/typescript/src/
#   ├── types.d.ts
#   ├── provider.d.ts
#   ├── event.d.ts
#   ├── message.d.ts
#   ├── usage-tracking.d.ts
#   └── world.d.ts
```

#### Step 3: Create TypeScript Package

Create `core/bindings/typescript/package.json`:
```json
{
  "name": "@agentic-primer/types",
  "version": "0.1.0",
  "type": "module",
  "main": "./dist/index.js",
  "types": "./dist/index.d.ts",
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "default": "./dist/index.js"
    },
    "./provider": {
      "types": "./dist/provider.d.ts",
      "default": "./dist/provider.js"
    },
    "./event": {
      "types": "./dist/event.d.ts",
      "default": "./dist/event.js"
    },
    "./message": {
      "types": "./dist/message.d.ts",
      "default": "./dist/message.js"
    },
    "./usage-tracking": {
      "types": "./dist/usage-tracking.d.ts",
      "default": "./dist/usage-tracking.js"
    }
  },
  "scripts": {
    "build": "tsc",
    "clean": "rm -rf dist"
  },
  "devDependencies": {
    "typescript": "^5.3.0"
  }
}
```

### Integration Examples

#### Example 1: Signal Hub Provider (if migrated)

```typescript
// services/signal-hub/providers/anthropic.ts
import type {
    Provider,
    ProviderRequest,
    ProviderResponse,
    Capability,
    ErrorInfo
} from '@agentic-primer/types/provider';

export class AnthropicProvider implements Provider {
    supports(cap: Capability): boolean {
        const supported: Capability[] = [
            'chat-completion',
            'function-calling',
            'vision',
            'streaming',
            'tool-use',
        ];
        return supported.includes(cap);
    }

    async fetch(request: ProviderRequest): Promise<Result<ProviderResponse, ErrorInfo>> {
        // Implementation using Anthropic API
        // Types are auto-generated from WIT
    }

    // ... other methods
}
```

#### Example 2: Simplify Actor with Message Types

```typescript
// services/simplify/actors/example-actor.ts
import type {
    Message,
    MessageNode,
    NodeCapabilities
} from '@agentic-primer/types/message';

export class ExampleActor implements MessageNode {
    async process(message: Message): Promise<Message | null> {
        // Type-safe message processing
        console.log(`Processing message: ${message.type}`);

        return {
            id: generateId(),
            version: '1.0.0',
            source: this.id,
            destination: message.source,
            'message-type': 'response',
            payload: new TextEncoder().encode(JSON.stringify({ status: 'ok' })),
            timestamp: new Date().toISOString(),
            'correlation-id': message.id,
            metadata: [],
        };
    }

    capabilities(): NodeCapabilities {
        return {
            'supported-message-types': ['example-request'],
            'async-support': true,
            'batch-support': false,
        };
    }
}
```

#### Example 3: Convergence Framework

```typescript
// services/convergence/tracker.ts
import type {
    UsageEvent,
    UsageTracker
} from '@agentic-primer/types/usage-tracking';

export class ConvergenceUsageTracker implements UsageTracker {
    async track(event: UsageEvent): Promise<void> {
        // Track usage from convergence iterations
        console.log(`Tracked: ${event.provider} ${event.model} - $${event['cost-usd']}`);

        // Store in database
        await this.db.insert('usage_events', {
            timestamp: event.timestamp,
            provider: event.provider,
            model: event.model,
            tokens_in: event.tokens['input-tokens'],
            tokens_out: event.tokens['output-tokens'],
            cost_usd: event['cost-usd'],
            run_id: event['convergence-run-id'],
        });
    }
}
```

### Protocol Conformance Tests

Create tests to verify protocol compliance:

```typescript
// core/bindings/typescript/tests/provider.test.ts
import { describe, it, expect } from 'vitest';
import type { Provider, ProviderRequest } from '@agentic-primer/types/provider';
import { AnthropicProvider } from '@agentic-primer/signal-hub/providers/anthropic';

describe('Provider Protocol Conformance', () => {
    it('should implement Provider interface', () => {
        const provider: Provider = new AnthropicProvider();
        expect(provider.supports).toBeDefined();
        expect(provider.fetch).toBeDefined();
        expect(provider.metadata).toBeDefined();
    });

    it('should support chat-completion capability', () => {
        const provider = new AnthropicProvider();
        expect(provider.supports('chat-completion')).toBe(true);
    });

    it('should return valid ProviderResponse', async () => {
        const provider = new AnthropicProvider();
        const request: ProviderRequest = {
            model: 'claude-sonnet-4-5',
            messages: [{ role: 'user', content: 'Hello' }],
            temperature: 0.7,
            'max-tokens': 1024,
            stream: false,
            tools: null,
            metadata: [],
        };

        const result = await provider.fetch(request);
        expect(result.ok).toBe(true);
        if (result.ok) {
            expect(result.value).toHaveProperty('id');
            expect(result.value).toHaveProperty('model');
            expect(result.value).toHaveProperty('content');
            expect(result.value).toHaveProperty('usage');
        }
    });
});
```

---

## Phase 4: WASM Components (Optional)

### Overview

Compile performance-critical algorithms to WASM Components:
- Convergence detection (Rust)
- Cost estimation (Rust/Go)
- Triage classification (Rust with ML)

**Note:** This phase is optional and can be deferred. Start with TypeScript implementations using WIT-generated types.

### Candidate Algorithms

| Algorithm | Language | Reason | Speedup Expected |
|-----------|----------|--------|------------------|
| Convergence detection | Rust | CPU-intensive math | 3-5x |
| Cost estimation | Rust | Complex calculations | 2-3x |
| Triage ML model | Rust | ML inference | 5-10x |

### Implementation Steps (Example: Convergence Detection)

#### Step 1: Define WIT Interface

```wit
// core/wit/convergence-algorithm.wit
package convergence:algorithm@0.1.0;

interface detector {
    record metric {
        iteration: u32,
        value: f64,
        timestamp: string,
    }

    enum convergence-status {
        converging,
        converged,
        diverging,
        stagnant,
    }

    record detection-result {
        status: convergence-status,
        confidence: f64,
        trend: f64,
        iterations-remaining: option<u32>,
        reason: string,
    }

    detect: func(metrics: list<metric>) -> detection-result;
}

world convergence-algorithm {
    export detector;
}
```

#### Step 2: Implement in Rust

```rust
// core/wasm/convergence/src/lib.rs
wit_bindgen::generate!({
    world: "convergence-algorithm",
    exports: {
        "convergence:algorithm/detector": ConvergenceDetector,
    },
});

use exports::convergence::algorithm::detector::*;

struct ConvergenceDetector;

impl Guest for ConvergenceDetector {
    fn detect(metrics: Vec<Metric>) -> DetectionResult {
        // Linear regression for trend
        let trend = calculate_trend(&metrics);

        // Variance for stability
        let variance = calculate_variance(&metrics);

        // Detect status
        let status = if variance < 0.01 && trend.abs() < 0.001 {
            ConvergenceStatus::Converged
        } else if trend > 0.01 {
            ConvergenceStatus::Converging
        } else if trend < -0.01 {
            ConvergenceStatus::Diverging
        } else {
            ConvergenceStatus::Stagnant
        };

        DetectionResult {
            status,
            confidence: calculate_confidence(&metrics, variance),
            trend,
            iterations_remaining: estimate_remaining(&metrics, trend),
            reason: format_reason(status, trend, variance),
        }
    }
}
```

#### Step 3: Build WASM Component

```bash
# Install cargo-component
cargo install cargo-component

# Build WASM component
cd core/wasm/convergence
cargo component build --release

# Output: target/wasm32-wasip1/release/convergence_detector.wasm
```

#### Step 4: Transpile to TypeScript

```bash
# Transpile WASM to ES module (for Bun/Node.js/Browser)
jco transpile target/wasm32-wasip1/release/convergence_detector.wasm \
    -o ../../bindings/typescript/src/wasm/convergence/

# Output: core/bindings/typescript/src/wasm/convergence/convergence_detector.js
```

#### Step 5: Use in TypeScript

```typescript
// services/convergence/detector.ts
import { detect } from '@agentic-primer/types/wasm/convergence/convergence_detector';

export class ConvergenceDetector {
    async detectStatus(iterations: Iteration[]): Promise<ConvergenceStatus> {
        const metrics = iterations.map((iter, index) => ({
            iteration: index,
            value: iter.quality_score || 0.0,
            timestamp: iter.timestamp,
        }));

        // Call WASM algorithm (runs at near-native speed)
        const result = detect(metrics);

        console.log(`Status: ${result.status}, Confidence: ${result.confidence}`);
        return result.status;
    }
}
```

---

## Phase 5: Cross-System Integration

### Overview

Integrate all systems using WIT protocols:
1. Signal Hub → Convergence (event protocol)
2. Browser extension → API server (usage tracking)
3. Convergence → /ai skill (message protocol)
4. End-to-end testing

### Integration Points

#### 1. Signal Hub → Convergence

```typescript
// Signal Hub publishes event when signal is triaged
const event: UniversalEvent = {
    id: ulid(),
    type: 'signal-triaged',
    timestamp: new Date().toISOString(),
    source: '@brain',
    payload: {
        signalId: signal.id,
        urgency: 'high',
        action: 'notify-convergence',
    },
};

// Publish to event bus (using event.wit protocol)
await eventBus.publish(event);

// Convergence subscribes to 'signal-triaged' events
class ConvergenceHandler implements EventHandler {
    'can-handle'(eventType: EventType): boolean {
        return eventType === 'signal-triaged';
    }

    async handle(event: Event): Promise<Event | null> {
        // Launch convergence run based on signal
        const runId = await this.launchConvergenceRun(event.payload);

        return {
            ...event,
            type: 'convergence-started',
            payload: { runId, originalSignalId: event.payload.signalId },
        };
    }
}
```

#### 2. Browser Extension → API Server

```typescript
// Browser extension tracks usage (using usage-tracking.wit)
class BrowserUsageTracker implements UsageTracker {
    async track(event: UsageEvent): Promise<void> {
        // Send to API server via WebSocket
        this.ws.send(JSON.stringify({
            type: 'usage-event',
            data: event,
        }));
    }
}

// API server receives and processes
class APIUsageProcessor extends EventProcessor<UsageEvent> {
    protected async storeInD1(event: UsageEvent): Promise<void> {
        await this.env.DB.prepare(`
            INSERT INTO usage_events (
                id, timestamp, provider, model, tokens_in, tokens_out, cost_usd
            ) VALUES (?, ?, ?, ?, ?, ?, ?)
        `).bind(
            ulid(),
            event.timestamp,
            event.provider,
            event.model,
            event.tokens['input-tokens'],
            event.tokens['output-tokens'],
            event['cost-usd']
        ).run();
    }
}
```

#### 3. Convergence → /ai Skill

```typescript
// Convergence sends message to /ai skill (using message.wit)
class ConvergenceSupervisor implements MessageNode {
    async launchIteration(task: ConvergenceTask): Promise<void> {
        const message: Message = {
            id: ulid(),
            version: '1.0.0',
            source: '@convergence-supervisor',
            destination: '@ai-skill',
            'message-type': 'execute-iteration',
            payload: encodePayload(task),
            timestamp: new Date().toISOString(),
            'correlation-id': task.runId,
            metadata: [],
        };

        // Send via message router
        await this.router.send(message);
    }

    async process(message: Message): Promise<Message | null> {
        if (message['message-type'] === 'iteration-complete') {
            const result = decodePayload(message.payload);

            // Track usage
            await this.usageTracker.track({
                timestamp: new Date().toISOString(),
                provider: result.provider,
                model: result.model,
                tokens: result.usage,
                'cost-usd': result.cost,
                'convergence-run-id': message['correlation-id'],
            });

            // Check convergence
            const converged = await this.checkConvergence(result);

            if (!converged) {
                await this.launchIteration(result.nextTask);
            }
        }

        return null;
    }
}
```

### End-to-End Test Scenario

```typescript
// E2E test: Signal → Convergence → Usage Tracking
describe('E2E: Signal Hub → Convergence → Usage Tracking', () => {
    it('should flow from signal to tracked usage', async () => {
        // 1. Signal Hub receives signal
        const signal = await signalHub.ingest({
            source: 'email',
            sender: 'test@example.com',
            subject: 'Urgent: Deploy needed',
            body: 'Production is down!',
        });

        // 2. Signal Hub triages with AI
        const triage = await signalHub.triage(signal.id);
        expect(triage.urgency).toBe('high');

        // 3. Event published to Convergence
        await waitFor(() => convergence.hasReceivedEvent('signal-triaged'));

        // 4. Convergence launches run
        const run = await convergence.getLatestRun();
        expect(run.signalId).toBe(signal.id);

        // 5. Convergence iterations tracked
        await waitFor(() => run.iterations.length > 0);

        // 6. Usage tracked in AI Capacity
        const usage = await aiCapacity.getUsageForRun(run.id);
        expect(usage.length).toBeGreaterThan(0);
        expect(usage[0].provider).toBe('anthropic');
        expect(usage[0].cost_usd).toBeGreaterThan(0);

        // 7. Total cost calculated
        const totalCost = usage.reduce((sum, u) => sum + u.cost_usd, 0);
        expect(totalCost).toBeGreaterThan(0);
        console.log(`Convergence run cost: $${totalCost.toFixed(4)}`);
    });
});
```

---

## Risk Assessment & Mitigation

### Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| WIT syntax errors | Medium | High | Validate early with wasm-tools |
| TypeScript binding issues | Medium | Medium | Test generation incrementally |
| Breaking changes to Simplify | Low | High | Backward compatibility layer |
| WASM component overhead | Low | Medium | Benchmark before migration |
| Cross-project integration bugs | High | Medium | Comprehensive E2E tests |

### Process Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Scope creep (too many WIT files) | Medium | Medium | Focus on 6 core files only |
| Over-engineering (premature WASM) | Medium | Low | Phase 4 is optional |
| Repository chaos during reorg | High | High | Use git worktrees for safety |
| Documentation drift | High | Low | Update docs in same PR |

### Mitigation Strategies

1. **Incremental Migration**
   - One WIT file at a time
   - Test after each extraction
   - Keep old and new side-by-side initially

2. **Backward Compatibility**
   - Don't break existing Simplify code
   - Use adapter pattern for old interfaces
   - Gradual migration over 2-3 weeks

3. **Validation Gates**
   - WIT syntax validation (automated)
   - TypeScript type checking (automated)
   - Integration tests (manual + automated)
   - Performance benchmarks (manual)

4. **Rollback Plan**
   - Git branches for each phase
   - Can revert any phase independently
   - Keep experiments/ directory untouched

---

## Success Criteria (Detailed)

### Phase 1 Success (WIT Extraction)

- ✅ All 6 WIT files pass `wasm-tools component wit` validation
- ✅ No circular dependencies in WIT imports
- ✅ Types adapted for Signal Hub, Convergence, AI Capacity use cases
- ✅ Documentation explains each WIT file's purpose
- ✅ Git commit with clean diff (only new files)

### Phase 2 Success (Reorganization)

- ✅ All files moved to correct directories
- ✅ No broken imports in Simplify
- ✅ Workspace configuration (pnpm-workspace.yaml) working
- ✅ All existing tests still pass
- ✅ Documentation updated with new structure

### Phase 3 Success (TypeScript Integration)

- ✅ TypeScript bindings generated successfully
- ✅ @agentic-primer/types package builds without errors
- ✅ At least 1 provider implements generated Provider interface
- ✅ At least 1 actor uses generated Message types
- ✅ Protocol conformance tests pass (100% coverage)

### Phase 4 Success (WASM Components, Optional)

- ✅ At least 1 WASM component compiled (convergence detector)
- ✅ Component transpiled to ES module successfully
- ✅ TypeScript can import and use WASM component
- ✅ Performance benchmark shows 2x+ speedup
- ✅ WASM component <500KB

### Phase 5 Success (Integration)

- ✅ Signal Hub → Convergence event flow working
- ✅ Browser → API server usage tracking working
- ✅ Convergence → /ai skill messaging working
- ✅ E2E test passes (signal → convergence → usage tracking)
- ✅ All systems use shared WIT protocols
- ✅ Zero breaking changes to existing functionality

---

## Implementation Beads Structure

See `BEADS_SUMMARY.md` for complete breakdown of 27 beads across 5 phases.

**High-Level Bead Groups:**

- **wit-001 to wit-009:** Phase 1 (WIT Extraction) - 9 beads
- **reorg-001 to reorg-006:** Phase 2 (Reorganization) - 6 beads
- **ts-001 to ts-005:** Phase 3 (TypeScript Integration) - 5 beads
- **wasm-001 to wasm-004:** Phase 4 (WASM Components, Optional) - 4 beads
- **integ-001 to integ-003:** Phase 5 (Integration) - 3 beads

**Total:** 27 beads, ~320 hours estimated effort

---

## References

### Source Projects

- **UGS WIT Interfaces:** `/Users/bln/play/projects/proj-20260131-090153/schemas/wit/`
- **Signal Hub:** `/Users/bln/play/brianln.ai/services/signal-hub/`
- **Convergence Framework:** `/Users/bln/play/projects/proj-20260204-083827/`
- **Agentic Primer:** `/Users/bln/play/agentic-primer/`

### Documentation

- **WASM Protocol Architecture:** `/Users/bln/play/projects/proj-20260204-083827/WASM_PROTOCOL_ARCHITECTURE.md`
- **Project Convergence Analysis:** `/Users/bln/play/projects/proj-20260204-083827/PROJECT_CONVERGENCE_ANALYSIS.md`
- **UGS WIT README:** `/Users/bln/play/projects/proj-20260131-090153/schemas/wit/README.md`

### Tools

- **wasm-tools:** https://github.com/bytecodealliance/wasm-tools
- **jco:** https://github.com/bytecodealliance/jco
- **cargo-component:** https://github.com/bytecodealliance/cargo-component
- **WIT Specification:** https://component-model.bytecodealliance.org/design/wit.html

---

## Git & Worktree Workflow

### Overview

This migration uses **git worktrees** for parallel phase development and **feature branches** for clean integration. Each phase works in an isolated worktree, maintains full git history as documentation, and cleans up after merge.

### Core Principles

1. **Git history is documentation** - Commit messages explain what, why, and reference bead IDs
2. **Worktrees enable parallelism** - Work on Phase N+1 while Phase N is in review
3. **Archive, don't delete** - Preserve old code with context for reference
4. **Clean up religiously** - Remove worktrees and branches after phase completion

---

### Branch Strategy

**Branch Structure:**
```
main                               # Production-ready state
├─ feature/phase1-wit-extraction   # Phase 1 work (worktree)
├─ feature/phase2-reorganization   # Phase 2 work (worktree)
├─ feature/phase3-typescript       # Phase 3 work (worktree)
├─ feature/phase4-wasm             # Phase 4 work (worktree)
└─ feature/phase5-integration      # Phase 5 work (worktree)
```

**Naming Convention:**
- `feature/phase<N>-<short-name>` for phase branches
- `hotfix/bead-<id>` for urgent fixes
- `experiment/<description>` for explorations (kept separate)

---

### Worktree Setup (Phase Start)

**Phase 1 Example:**
```bash
# From agentic-primer main directory
cd ~/play/agentic-primer

# Ensure main is clean and up-to-date
git checkout main
git pull
git status  # Must be clean

# Create feature branch and worktree
git worktree add ../agentic-primer-wit feature/phase1-wit-extraction

# Verify worktree created
git worktree list
# main         /Users/bln/play/agentic-primer         [main]
# wit-phase    ../agentic-primer-wit                  [feature/phase1-wit-extraction]

# Switch to worktree
cd ../agentic-primer-wit

# Verify branch
git branch --show-current  # Should show: feature/phase1-wit-extraction

# Ready to work on Phase 1 beads (bb4.1 through bb4.9)
```

**All Phases:**
```bash
# Phase 1
git worktree add ../agentic-primer-wit feature/phase1-wit-extraction

# Phase 2 (can start while Phase 1 is in review)
git worktree add ../agentic-primer-reorg feature/phase2-reorganization

# Phase 3
git worktree add ../agentic-primer-ts feature/phase3-typescript

# Phase 4 (optional)
git worktree add ../agentic-primer-wasm feature/phase4-wasm

# Phase 5
git worktree add ../agentic-primer-integ feature/phase5-integration
```

---

### Commit Message Format

**Standard Format:**
```
<type>(<scope>): <subject line - what>

<body - why, context, approach>

<footer - bead references, co-authorship>
```

**Types:**
- `feat:` New feature or capability
- `refactor:` Code restructuring without behavior change
- `docs:` Documentation only
- `test:` Test additions or changes
- `chore:` Build, tooling, dependencies

**Scopes:**
- `core:` Core WIT interfaces
- `services:` Signal Hub, Simplify, Convergence
- `tooling:` bmad-method, spec-kit
- `bindings:` Generated TypeScript/Rust
- `repo:` Repository structure changes

**Example Commits:**

```
feat(core): Extract types.wit from UGS project

Copied and adapted shared types from UGS schemas/wit/types.wit:
- Error types (error-info, error-severity)
- Trace types (trace-info for distributed tracing)
- Payload metadata (encoding, compression)

Changes from UGS version:
- Removed WASI-specific types (not needed for browser)
- Added cost-tracking types for AI capacity
- Simplified security types (use runtime auth instead)

Files:
- core/wit/types.wit (new, 156 lines)

Refs: bb4.2
Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
```

```
refactor(services): Move Signal Hub from brianln.ai

Moved Signal Hub from ~/play/brianln.ai/services/signal-hub/
to agentic-primer/services/signal-hub/ as part of platform
unification.

Preserved git history via subtree merge. All commit SHAs
maintained for traceability.

Changes:
- Updated import paths (brianln.ai → @agentic-primer)
- Updated wrangler.toml paths
- Preserved all tests (137/142 passing)

Migration context preserved in:
- services/signal-hub/MIGRATION.md
- archive/2026-02-pre-unification/

Refs: reorg-002
Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
```

---

### Code Comments for Reorganization

**Document every move and change:**

```typescript
// ============================================================================
// MIGRATION CONTEXT
// ============================================================================
// MOVED FROM: ~/play/brianln.ai/services/signal-hub/src/providers/anthropic.ts
// MOVED TO:   agentic-primer/services/signal-hub/src/providers/anthropic.ts
// DATE:       2026-02-05
// PHASE:      Phase 2 - Repository Reorganization
// BEAD:       reorg-002
// REASON:     Consolidating all AI platform services into unified repo
//
// UPDATED:    Changed interface from hand-written TypeScript to WIT-generated
// OLD:        interface Provider { ... } (local definition)
// NEW:        import type { Provider } from '@agentic-primer/types/provider'
// WHY:        Protocol-first architecture - WIT as single source of truth
// BEAD:       ts-002
// ============================================================================

import type { Provider, ProviderRequest, ProviderResponse } from '@agentic-primer/types/provider';

/**
 * Anthropic AI provider implementation.
 *
 * Implements WIT provider interface defined in core/wit/provider.wit.
 * Generated types: core/bindings/typescript/provider.d.ts
 *
 * @see https://docs.anthropic.com/claude/reference
 */
export class AnthropicProvider implements Provider {
  // Implementation...
}
```

**For deleted code:**
```typescript
// ============================================================================
// DEPRECATED: Old provider interface
// ============================================================================
// This hand-written interface was replaced by WIT-generated types.
//
// REPLACED BY:  @agentic-primer/types/provider (generated from core/wit/provider.wit)
// REASON:       Protocol-first architecture
// BEAD:         ts-002
// DATE:         2026-02-05
// PRESERVED IN: archive/2026-02-pre-wit/providers/types.ts
//
// To reference old implementation patterns, see archive with full context.
// ============================================================================
```

---

### Archival Strategy (Don't Delete)

**Instead of deleting, archive with context:**

```bash
# DON'T DO THIS:
rm -rf old-implementation/

# DO THIS:
# 1. Create archive directory with timestamp
mkdir -p archive/2026-02-pre-wit-migration/

# 2. Move (don't delete) old code
git mv src/providers/old-types.ts archive/2026-02-pre-wit-migration/

# 3. Create README explaining archive
cat > archive/2026-02-pre-wit-migration/README.md <<'EOF'
# Pre-WIT Migration Archive

**Date:** 2026-02-05
**Phase:** Phase 3 - TypeScript Integration
**Beads:** ts-001 through ts-005

## What Changed

This archive preserves code from before the WIT protocol-first migration:

- **Hand-written TypeScript interfaces** → WIT-generated bindings
- **Local type definitions** → Core protocol definitions
- **Direct implementations** → Protocol-conformant implementations

## Files Preserved

- `old-types.ts` - Original Provider interface (pre-WIT)
- `old-adapters.ts` - Original adapter implementations
- `old-tests.ts` - Original test suite (for comparison)

## Why Preserved

1. **Reference:** Compare old vs new implementation patterns
2. **Verification:** Ensure no regressions during migration
3. **Documentation:** Understand pre-WIT architecture decisions
4. **Rollback:** Quick reference if migration issues found

## Migration Path

Old → New:
- `Provider` interface → `core/wit/provider.wit` → generated bindings
- `supports(cap: string)` → `supports(cap: Capability)` (typed enum)
- `fetch(request: any)` → `fetch(request: ProviderRequest)` (structured)

## Related Documentation

- Main migration plan: `WIT_PLATFORM_MIGRATION_PLAN.md`
- Phase 3 details: `BEADS_SUMMARY.md` (ts-001 to ts-005)
- WIT interfaces: `core/wit/provider.wit`

## Access

This archive will be maintained for 6 months post-migration, then moved
to long-term storage (S3/R2) if unneeded.
EOF

# 4. Commit archive with context
git add archive/2026-02-pre-wit-migration/
git commit -m "chore(repo): Archive pre-WIT implementation

Preserved old TypeScript interfaces and implementations before
WIT protocol-first migration.

Files archived:
- old-types.ts (Provider interface)
- old-adapters.ts (adapter implementations)
- old-tests.ts (test suite)

Archive includes comprehensive README explaining what changed,
why preserved, and how to reference.

This enables rollback if needed and serves as implementation
reference during migration.

Refs: ts-002"
```

---

### Phase Execution Workflow

**Complete Lifecycle for One Phase:**

#### 1. Phase Start

```bash
# From main repo
cd ~/play/agentic-primer
git checkout main
git pull

# Create worktree
git worktree add ../agentic-primer-wit feature/phase1-wit-extraction
cd ../agentic-primer-wit

# Verify clean state
git status  # Should show: On branch feature/phase1-wit-extraction

# Ready to work on beads
bd list --status=ready  # Show Phase 1 beads
```

#### 2. Work on Beads

```bash
# Update bead status
bd update bb4.1 --status=in_progress

# Do the work (extract WIT file, test, validate)
# ...

# Commit with bead reference
git add core/wit/types.wit
git commit -m "feat(core): Extract types.wit from UGS

<full commit message with what/why>

Refs: bb4.1
Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"

# Close bead
bd close bb4.1

# Repeat for remaining beads in phase
```

#### 3. Phase Review

```bash
# Ensure all phase beads complete
bd list --status=completed | grep "bb4\."

# Run all tests
bun test

# Validate WIT (if Phase 1)
wasm-tools component wit core/wit/

# Generate bindings (if Phase 1)
jco types core/wit/ --out-dir core/bindings/typescript/

# Push phase branch
git push origin feature/phase1-wit-extraction
```

#### 4. Phase Merge

```bash
# Switch to main in main repo
cd ~/play/agentic-primer
git checkout main
git pull

# Merge phase branch (no fast-forward to preserve phase history)
git merge --no-ff feature/phase1-wit-extraction -m "Merge Phase 1: WIT Extraction

Completed beads: bb4.1 through bb4.9

Summary:
- Extracted 6 WIT files from UGS project
- Adapted for Signal Hub, Convergence, AI Capacity
- Validated with wasm-tools
- Generated TypeScript bindings with jco
- Documented all interfaces

Success metrics:
✅ All WIT files validate
✅ TypeScript bindings generated
✅ No breaking changes
✅ Documentation complete

Total effort: 19 hours
Phase duration: 5 days

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"

# Push merged main
git push origin main
```

#### 5. Phase Cleanup

```bash
# Remove worktree
git worktree remove ../agentic-primer-wit

# If worktree removal fails (locked files, etc.)
git worktree remove --force ../agentic-primer-wit

# Manual cleanup if needed
rm -rf ../agentic-primer-wit
git worktree prune

# Delete feature branch (local)
git branch -d feature/phase1-wit-extraction

# Delete feature branch (remote)
git push origin --delete feature/phase1-wit-extraction

# Clean up remote tracking branches
git fetch --prune

# Verify clean state
git worktree list
# Should only show main worktree

git branch -a
# feature/phase1-wit-extraction should be gone

# Update beads to mark phase complete
bd update bb4 --notes="Phase 1 merged to main, worktree cleaned up"
```

---

### Parallel Work Strategies

**Worktrees enable parallelism at multiple levels:**

#### Parallel Phases

Start Phase 2 while Phase 1 is in review:

```bash
# Phase 1 pushed for review, waiting for merge
cd ~/play/agentic-primer
git worktree add ../agentic-primer-reorg feature/phase2-reorganization

# Phase 2 worktree based on current main
cd ../agentic-primer-reorg

# Start Phase 2 beads while Phase 1 is in review
bd update reorg-001 --status=in_progress

# Work on Phase 2...
# When Phase 1 merges, rebase Phase 2 onto new main

cd ~/play/agentic-primer-reorg
git fetch origin
git rebase origin/main

# Resolve conflicts if any, continue Phase 2 work
```

#### Parallel Beads Within Phase

**Check dependencies first:**
```bash
# See which beads are ready (no blockers)
bd ready

# Example output:
# bb4.2 - Extract types.wit (no dependencies)
# bb4.3 - Adapt provider.wit (depends on bb4.2)
# bb4.4 - Create event.wit (no dependencies)
# bb4.7 - Adapt world.wit (depends on bb4.2, bb4.3, bb4.4)
```

**Parallel Strategy:** Work on **independent beads** (bb4.2, bb4.4) simultaneously using worktrees:

```bash
# From main Phase 1 worktree
cd ~/play/agentic-primer-wit  # Phase 1 worktree

# Create sub-worktree for independent bead
git worktree add ../ap-wit-types feature/phase1-wit-extraction-types
git worktree add ../ap-wit-event feature/phase1-wit-extraction-event

# Terminal 1: Work on bb4.2 (types.wit)
cd ../ap-wit-types
bd update bb4.2 --status=in_progress
# Extract types.wit, commit
git add core/wit/types.wit
git commit -m "feat(core): Extract types.wit

Refs: bb4.2"

# Terminal 2: Work on bb4.4 (event.wit) IN PARALLEL
cd ../ap-wit-event
bd update bb4.4 --status=in_progress
# Create event.wit, commit
git add core/wit/event.wit
git commit -m "feat(core): Create event.wit

Refs: bb4.4"

# Back to main Phase 1 worktree
cd ~/play/agentic-primer-wit

# Merge sub-worktree work
git merge feature/phase1-wit-extraction-types --no-ff
git merge feature/phase1-wit-extraction-event --no-ff

# Clean up sub-worktrees
git worktree remove ../ap-wit-types
git worktree remove ../ap-wit-event
git branch -d feature/phase1-wit-extraction-types
git branch -d feature/phase1-wit-extraction-event
```

**When to use parallel beads:**
- ✅ Beads have no dependencies on each other
- ✅ Beads work on different files (no merge conflicts)
- ✅ Beads can be tested independently
- ❌ Beads are sequential (one builds on another)
- ❌ Beads affect same files
- ❌ Testing requires both beads complete

**Example Parallel Work (Phase 1):**

```
bb4.2 (types.wit) ──────────┐
                            ├──> bb4.8 (validate)
bb4.4 (event.wit) ──────────┤
                            └──> bb4.9 (docs)
bb4.6 (usage-tracking.wit) ─┘

All three WIT files (bb4.2, bb4.4, bb4.6) are independent
and can be extracted in parallel worktrees.
```

#### Parallel Agents on Independent Beads

```bash
# Launch multiple background agents for parallel beads
/bg Work on bb4.2: Extract types.wit from UGS to core/wit/
/bg Work on bb4.4: Create event.wit with Signal Hub, Convergence events
/bg Work on bb4.6: Create usage-tracking.wit for AI capacity

# Each agent works in isolation
# Merge results when all complete
```

---

### Recovery Procedures

#### Worktree Won't Remove

```bash
# Error: "fatal: 'remove' cannot be used with unregistered working trees"

# Force remove
git worktree remove --force ../agentic-primer-wit

# Manual cleanup
rm -rf ../agentic-primer-wit

# Prune references
git worktree prune

# Verify
git worktree list
```

#### Worktree Metadata Corrupted

```bash
# Error: "fatal: '.git' at main working tree is not the repository directory"

# Repair worktree metadata
git worktree repair

# If that fails, manual repair
cd ~/play/agentic-primer
rm .git/worktrees/<worktree-name> -rf
git worktree prune
```

#### Accidentally Deleted Worktree Directory

```bash
# Worktree directory gone but git still tracks it

# Prune stale worktrees
git worktree prune

# Recreate if needed
git worktree add ../agentic-primer-wit feature/phase1-wit-extraction

# Or checkout work from branch
git checkout feature/phase1-wit-extraction
```

#### Merge Conflicts

```bash
# During merge or rebase

# See conflict files
git status

# Resolve conflicts in editor
# ...

# Mark resolved
git add <resolved-files>

# Continue merge
git merge --continue

# Or continue rebase
git rebase --continue

# If merge becomes too complex, abort and get help
git merge --abort
# or
git rebase --abort
```

---

### End-of-Phase Checklist

**Run this checklist after each phase merge:**

```bash
# 1. Verify phase work merged
git log --oneline -1
# Should show: "Merge Phase N: <description>"

# 2. Remove worktree
git worktree remove ../agentic-primer-<phase-name>
# Success: "Worktree removed"

# 3. Delete local branch
git branch -d feature/phaseN-<name>
# Success: "Deleted branch feature/phaseN-<name>"

# 4. Delete remote branch
git push origin --delete feature/phaseN-<name>
# Success: "- [deleted] feature/phaseN-<name>"

# 5. Prune worktree references
git worktree prune
# No output = success

# 6. Clean remote tracking
git fetch --prune
# Prunes stale remote branches

# 7. Verify clean state
git worktree list
# Should only show main worktree

git branch -a | grep phaseN
# Should return nothing (branch deleted)

# 8. Update phase epic bead
bd close bb4  # Phase 1 epic
bd update bb4 --notes="Phase complete, branch cleaned up, ready for Phase 2"

# ✅ Phase cleanup complete
```

---

### Quick Reference Commands

**Setup:**
```bash
git worktree add ../agentic-primer-<phase> feature/phase<N>-<name>
```

**Cleanup:**
```bash
git worktree remove ../agentic-primer-<phase>
git branch -d feature/phase<N>-<name>
git push origin --delete feature/phase<N>-<name>
git worktree prune
git fetch --prune
```

**Status:**
```bash
git worktree list                    # Show all worktrees
git branch -a                        # Show all branches
bd list --status=in_progress         # Show active beads
```

**Recovery:**
```bash
git worktree remove --force <path>   # Force remove stuck worktree
git worktree repair                  # Fix corrupted metadata
git worktree prune                   # Remove stale references
```

---

**Next Steps:** Create beads in agentic-primer repository using `bd` CLI (see execution below).
