# Cloudflare Actor & Durable Objects Inventory

**Date:** 2026-02-15
**Purpose:** Comprehensive inventory of all Cloudflare actor/Durable Objects work across projects and branches
**Status:** Complete survey of 5 major locations + branches

---

## Executive Summary

Found **5 distinct categories** of Cloudflare actor/Durable Objects implementations across your codebase:

1. **agentic-primer/packages/cloudflare** - Production-ready actor system package
2. **brianln.ai/services** - Production Durable Objects for Signal Hub, CMS, Auth
3. **proj-20260211-140744** (current) - Server-side actor system with DO integration
4. **oauth-server** - OAuth flow Durable Objects
5. **actor-lab** - Experimental actor modeling system

**Key Finding:** You have TWO mature actor systems:
- **@agentic-primer/cloudflare** - Reusable package for wrapping ActorSystem in DurableObjects
- **brianln.ai production services** - Hand-crafted DOs for specific business logic

---

## 1. Agentic Primer - Cloudflare Package

**Location:** `/Users/bln/play/agentic-primer/packages/cloudflare`
**Status:** Production-ready package (26/26 tests passing)
**Purpose:** Reusable adapter layer between ActorSystem and Cloudflare Durable Objects

### Key Files

| File | Purpose | Lines |
|------|---------|-------|
| `src/do-actor-system.ts` | Abstract base class for DurableObject + ActorSystem integration | 240 |
| `src/do-actor-checkpoint.ts` | Persistence layer using DO SQLite | 112 |
| `src/transports/websocket-bridge.ts` | WebSocket transport for DO hibernation API | ~150 |
| `src/transports/do-transport.ts` | HTTP transport for DO-to-DO communication | ~100 |
| `src/provisioner/miniflare-provisioner.ts` | Local dev testing with Miniflare | ~200 |
| `src/storage/*` | VectorizeStore + BlobStorage implementations | ~300 |

### What It Does

Provides **DOActorSystem** abstract class that:
- Wraps ActorSystem in a DurableObject
- Maps DO lifecycle (constructor, fetch, alarm, webSocketMessage) to actor messages
- Persists actor state using DO SQLite via DOActorCheckpoint
- Supports WebSocket hibernation API for real-time connections
- Enables DO-to-DO communication via HTTP transport
- Schedules recurring alarms for actor tasks

### Usage Pattern

```typescript
export class BrainDO extends DOActorSystem<Env> {
  configure(system: ActorSystem) {
    system.spawn(triageBehavior, initialState, 'triage');
    this.scheduleAlarm({
      targetActor: 'briefing',
      messageType: 'GENERATE',
      nextRunAt: getNextBriefingTime(),
      interval: 24 * 60 * 60 * 1000,
    });
  }
}
```

### Branches Checked

- **main** - Current stable version
- **feature/entangled-actor-migration** - Empty packages/cloudflare (migrated to monorepo)
- **feature/shared-storage-interfaces** - Storage abstraction work
- **genesis** - Earlier versions

**Recommendation:** This is the **canonical Cloudflare actor adapter**. Use this for future DO-based actor systems.

---

## 2. brianln.ai Production Services

**Location:** `/Users/bln/play/brianln.ai/services` and `/Users/bln/play/worktrees/bl-week1/services`
**Status:** Production Durable Objects (actively deployed)
**Purpose:** Real-world Signal Hub, CMS, Auth, Agent services

### 2.1 Signal Hub Brain

**Location:** `brianln.ai/services/signal-hub/src/brain.ts`
**Type:** Hand-crafted DurableObject
**Purpose:** Central intelligence for Signal Hub

**Features:**
- Queue consumption and AI triage
- Signal storage in DO SQLite
- Action item extraction
- Daily briefing alarms
- WebSocket push for critical signals
- Hibernatable WebSocket API

**Key Code:**
```typescript
export class Brain extends DurableObject<Env> {
  private connections: Set<WebSocket> = new Set();

  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);
    this.connections = new Set(ctx.getWebSockets()); // Hibernation

    ctx.blockConcurrencyWhile(async () => {
      await this.initSchema();
    });
  }

  async alarm() {
    // Daily briefing generation
  }
}
```

### 2.2 Agent Hub

**Location:** `brianln.ai/services/agent-hub/src/agent.ts`
**Type:** Per-user DurableObject
**Purpose:** Conversational agent with tool calling

**Features:**
- Conversation state management
- Tool execution via service binding to signal-hub
- WebSocket connections for real-time chat
- Artifact management

### 2.3 CMS Actors

**Location:** `brianln.ai/services/cms/src/actors/`
**Type:** Three DurableObject classes
**Purpose:** Git-style content management

| DurableObject | Purpose |
|---------------|---------|
| `ContentManager.ts` | Manages collections, handles CRUD |
| `Collection.ts` | Per-collection metadata and indexing |
| `ContentItem.ts` | Git-style branching, versioning (R2 storage) |

**Pattern:** Each content item is a separate DO with:
- Branch management (main, feature branches)
- Version history (parent-child DAG)
- R2 storage for large content blobs
- DO SQLite for metadata

### 2.4 Auth DurableObjects

**Location:** `brianln.ai/services/auth/src/durable-objects/DeviceSession.ts`
**Type:** Session management DurableObject
**Purpose:** Device authentication sessions

### 2.5 Email Services

**Location:** `brianln.ai/services/email/*/src/index.ts`
**Type:** Multiple DurableObjects
**Purpose:** Email processing pipeline

| Service | DurableObject |
|---------|---------------|
| `sender` | Email send queue |
| `notification-manager` | WebSocket notification delivery |
| `cleanup-manager` | Scheduled cleanup alarms |

### 2.6 Relay Service

**Location:** `brianln.ai/services/relay/src/index.ts`
**Type:** Message relay DurableObject
**Purpose:** Real-time message routing between services

### Configuration

All services use `wrangler.jsonc` (not `.toml`) with:
- Durable Object bindings
- Service bindings between services
- R2, D1, Queue bindings
- Migrations for SQLite schema

---

## 3. Current Project - Server-Side Actor System

**Location:** `/Users/bln/play/projects/proj-20260211-140744`
**Status:** Active development
**Purpose:** Server-side actor system with Cloudflare DO backend

### Key Files

| File | Purpose |
|------|---------|
| `actor-system.ts` | Server-side ActorSystem implementation |
| `actor-system-ws.ts` | WebSocket actor transport |
| `actor-system-queue.ts` | Queue-based actor messaging |
| `actor-system-session.ts` | Session management actor |
| `actor-system-events.ts` | Event system integration |
| `actor-example.ts` | Example CounterActor DurableObject |
| `wrangler.jsonc` | DO configuration |

### What's Different

This is a **server-side actor system** (different from agentic-primer):
- No dependency on `@agentic-primer/actors`
- Custom ActorSystem implementation
- Focus on WebSocket and Queue transports
- Example DurableObject: `CounterActor`

### Pattern

```typescript
export class CounterActor extends DurableObject {
  async fetch(request: Request): Promise<Response> {
    const url = new URL(request.url);

    if (url.pathname === '/increment') {
      // Actor logic
    }
  }
}
```

**Note:** This appears to be experimental work - not using the agentic-primer package.

---

## 4. OAuth Server Project

**Location:** `/Users/bln/play/projects/oauth-server/implementation/src/durable-objects`
**Status:** Production-ready OAuth implementation
**Purpose:** OAuth 2.0 Authorization Code Flow with PKCE

### Durable Objects

| DurableObject | Purpose | Features |
|---------------|---------|----------|
| `AuthorizationSession.ts` | OAuth session management | PKCE validation, one-time codes, 10min expiry |
| `DeviceFlowSession.ts` | Device authorization flow | QR code auth, polling |
| `RateLimiter.ts` | Rate limiting | Token bucket algorithm |

### Pattern

```typescript
export class AuthorizationSession extends DurableObject<Env> {
  async fetch(request: Request): Promise<Response> {
    const url = new URL(request.url);

    if (url.pathname === '/create' && request.method === 'POST') {
      // Initialize OAuth session
    }

    if (url.pathname === '/approve') {
      // User approves, generate auth code
    }

    if (url.pathname === '/exchange') {
      // Exchange code for tokens (validate PKCE)
    }
  }

  async alarm() {
    // Auto-cleanup expired sessions
  }
}
```

**Key Features:**
- Security-focused (PKCE, replay prevention, expiry)
- Alarm-based cleanup
- State machine pattern (pending → approved → consumed)

---

## 5. Actor Lab - Experimental

**Location:** `/Users/bln/play/projects/proj-20260211-065911/actor-lab-v04`
**Status:** Research/experimentation
**Purpose:** Actor modeling methodology

### Structure

```
actor-lab-v04/
└── actors/
    ├── agent-router/
    │   ├── SPEC.md
    │   ├── INTERFACE.md
    │   ├── src/
    │   └── verify.js
    ├── saga-coordinator/
    └── rate-limiter/
```

### What It Is

Systematic approach to actor design:
- Write SPEC.md (behavior specification)
- Define INTERFACE.md (message protocol)
- Generate implementation
- Verify against spec

**Actors Explored:**
- Agent router (message routing)
- Saga coordinator (distributed transactions)
- Rate limiter (token bucket)

**Note:** This is **research/tooling**, not production code. Useful for designing new actors.

---

## 6. Additional Findings

### 6.1 Signal Element Actors

**Location:** `/Users/bln/play/projects/signal-element-actors`
**Type:** Frontend web components library
**Purpose:** Reactive web components using TC39 Signals

**NOT Cloudflare actors** - this is client-side reactive UI components.

### 6.2 Cloudflare Research Docs

**Location:** `brianln.ai/design-explorations/`
**Key Documents:**
- `2026-02-05-cloudflare-agents-workflows-research.md` - Evaluation of Cloudflare Agents SDK
- `2026-02-06-signal-hub-actor-architecture.md` - Signal Hub actor integration plan
- `2026-02-05-service-binding-event-log-prototype.ts` - Service binding patterns

### 6.3 CMS Prototype

**Location:** `/Users/bln/play/projects/proj-20260212-074612/cms-prototype`
**Type:** Standalone CMS with DurableObjects
**Purpose:** Markdown CMS with graph-based orchestration

**Features:**
- Content versioning with DOs
- Agent orchestration for content workflows
- A2UI (Agent-to-UI) protocol
- Graph-based task execution

---

## Architecture Patterns Observed

### Pattern 1: Hand-Crafted DurableObjects

**Examples:** Signal Hub Brain, CMS actors, OAuth sessions
**When to use:** Custom business logic, complex state machines
**Pros:** Full control, optimized for specific use case
**Cons:** Boilerplate, manual lifecycle management

```typescript
export class CustomActor extends DurableObject<Env> {
  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);
    ctx.blockConcurrencyWhile(async () => {
      await this.initSchema();
    });
  }

  async fetch(request: Request): Promise<Response> {
    // Custom HTTP handler
  }

  async alarm() {
    // Custom alarm logic
  }

  async webSocketMessage(ws: WebSocket, message: string | ArrayBuffer) {
    // Custom WebSocket handler
  }
}
```

### Pattern 2: DOActorSystem Wrapper

**Example:** `@agentic-primer/cloudflare`
**When to use:** Want ActorSystem features (pub/sub, supervision, etc.)
**Pros:** Reusable, battle-tested, minimal boilerplate
**Cons:** Abstraction layer overhead

```typescript
export class MyActorDO extends DOActorSystem<Env> {
  configure(system: ActorSystem) {
    system.spawn(myBehavior, initialState, 'my-actor');
  }
}
```

### Pattern 3: Service Bindings

**Example:** Agent Hub → Signal Hub
**Pattern:** DOs communicate via service bindings

```typescript
// wrangler.jsonc
{
  "services": [
    { "binding": "SIGNAL_HUB", "service": "signal-hub" }
  ]
}

// agent.ts
const signals = await this.env.SIGNAL_HUB.getSignals();
```

---

## Comparison Table

| Location | Type | Purpose | Status | Actor System | Tests |
|----------|------|---------|--------|--------------|-------|
| agentic-primer/cloudflare | Package | Reusable DO+ActorSystem adapter | Production | ✅ Full ActorSystem | 26/26 ✅ |
| brianln.ai/signal-hub | Service | Signal triage & briefing | Production | ❌ Hand-crafted | ✅ Deployed |
| brianln.ai/cms | Service | Content management | Production | ❌ Hand-crafted | ✅ Deployed |
| brianln.ai/agent-hub | Service | Conversational agents | Production | ❌ Hand-crafted | ✅ Deployed |
| oauth-server | Library | OAuth 2.0 flows | Ready | ❌ Hand-crafted | ⚠️ Unit tests |
| proj-20260211-140744 | Experiment | Server-side actors | Development | ✅ Custom impl | ⚠️ WIP |
| actor-lab | Research | Actor modeling | Research | ❌ Specs only | N/A |
| cms-prototype | Prototype | Graph-orchestrated CMS | Prototype | ⚠️ Graph-based | ⚠️ WIP |

---

## Recommendations

### 1. **Consolidation Strategy**

**For new Durable Object projects:**
1. Start with `@agentic-primer/cloudflare` if you need ActorSystem features
2. Use hand-crafted DOs for simple stateful services (like OAuth sessions)
3. Reference `brianln.ai/services` for production patterns

**For existing code:**
1. **DO NOT** migrate working production DOs to ActorSystem unless there's a clear benefit
2. Extract common patterns into `@agentic-primer/cloudflare` utilities
3. Standardize on `wrangler.jsonc` configuration format

### 2. **Package Publishing**

Consider publishing `@agentic-primer/cloudflare` as a reusable npm package:
- Currently only used internally
- Could benefit other DO+ActorSystem projects
- Well-tested (26/26) and documented

### 3. **Pattern Library**

Create a pattern library documenting:
- When to use hand-crafted DOs vs DOActorSystem
- Service binding patterns (brianln.ai services)
- Hibernatable WebSocket patterns (Signal Hub)
- Alarm-based scheduling patterns (all services)
- SQLite schema patterns (consistent across projects)

### 4. **Testing Strategy**

Standardize on:
- Miniflare for local DO testing (agentic-primer uses this)
- Vitest for unit tests (current project uses this)
- DO SQLite for state persistence (all projects use this)

### 5. **Migration Path**

**DO NOT migrate unless:**
- Need ActorSystem features (pub/sub, supervision, actor hierarchy)
- Have complex actor coordination requirements
- Want location transparency (local vs remote actors)

**DO migrate if:**
- Building multi-actor systems with complex messaging
- Need actor supervision and restart policies
- Want to reuse actor behaviors across different deployments

---

## Key Files by Category

### Core Cloudflare Actor Infrastructure

```
/Users/bln/play/agentic-primer/packages/cloudflare/src/
├── do-actor-system.ts          ⭐ Main adapter class
├── do-actor-checkpoint.ts      ⭐ Persistence layer
├── transports/
│   ├── websocket-bridge.ts     ⭐ WebSocket hibernation
│   └── do-transport.ts         ⭐ DO-to-DO HTTP
├── storage/
│   ├── vectorize-store.ts
│   └── blob-storage.ts
└── provisioner/
    └── miniflare-provisioner.ts ⭐ Local testing
```

### Production Services

```
/Users/bln/play/brianln.ai/services/
├── signal-hub/src/brain.ts                    ⭐ Complex DO with alarms
├── agent-hub/src/agent.ts                     ⭐ Per-user DO
├── cms/src/actors/
│   ├── ContentManager.ts                      ⭐ Manager pattern
│   ├── Collection.ts                          ⭐ Collection pattern
│   └── ContentItem.ts                         ⭐ Git-style versioning
├── auth/src/durable-objects/DeviceSession.ts  ⭐ Session pattern
└── email/*/src/index.ts                       ⭐ Service pipeline
```

### OAuth Examples

```
/Users/bln/play/projects/oauth-server/implementation/src/durable-objects/
├── AuthorizationSession.ts  ⭐ State machine pattern
├── DeviceFlowSession.ts     ⭐ Polling pattern
└── RateLimiter.ts          ⭐ Token bucket pattern
```

---

## Next Steps

1. **Document** the differences between hand-crafted DOs and DOActorSystem
2. **Extract** common utilities from brianln.ai services into reusable helpers
3. **Standardize** on configuration format (wrangler.jsonc)
4. **Test** DOActorSystem with production Signal Hub workload
5. **Publish** @agentic-primer/cloudflare to npm (optional)

---

## Conclusion

You have **two mature approaches** to Cloudflare actors:

1. **@agentic-primer/cloudflare** - Reusable ActorSystem wrapper for complex multi-actor systems
2. **Hand-crafted DurableObjects** - Production-tested patterns in brianln.ai services

Both are valid. Use ActorSystem when you need actor model features. Use hand-crafted DOs for simpler stateful services.

The oauth-server and brianln.ai services provide excellent real-world patterns for:
- State machines (OAuth sessions)
- Alarm-based scheduling (daily briefings)
- Hibernatable WebSockets (real-time notifications)
- Service bindings (inter-service communication)
- SQLite persistence (all services)

**Recommendation:** Keep both approaches. They serve different use cases and both are production-ready.
