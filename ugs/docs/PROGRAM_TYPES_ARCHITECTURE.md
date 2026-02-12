# Program Types Architecture

Everything is a Program. All actors in the graph share the same base with different behaviors.

---

## Core Insight

```
Program (base executable actor)
├── code           - runs JavaScript/TypeScript
├── provider       - routes to LLM APIs
├── model          - inference config, references provider
├── session        - JSONL conversation log, routes to models
├── agent          - control harness around sessions
├── task           - work specification, trackable
├── information    - structured data, queryable
└── human          - human actor, receives notifications
```

**All share:**
- Graph node representation
- Lifecycle machine (draft → published → deprecated, or type-specific)
- Event emission on all mutations
- Addressable via `@(id)`
- Invokable
- `config` property for settings (not `data`)

---

## Architectural Layers

```
┌─────────────────────────────────────────────────────────────────┐
│                    Application Layer                            │
│  Agent logic, Task specs, Session conversations, Human approval │
│  (This document - Program Types)                                │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                    Actor Runtime Layer                          │
│  Message routing, ExecutionContext propagation                  │
│  In-process: AsyncLocalStorage | Distributed: token serialization│
│  (Future: agentic-primer-64k)                                   │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                    Capability Layer                             │
│  Principal identity, secrets, authorization, delegation         │
│  (Future: agentic-primer-737)                                   │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                    Storage Layer                                │
│  GraphStore (entities), WAL (events), JSONL (sessions)          │
│  (Implemented: src/graph.ts)                                    │
└─────────────────────────────────────────────────────────────────┘
```

**Key Distinction:**
- **Entity** (Graph Node): Human, Agent, Session - the "thing" that exists
- **Principal** (Runtime): Security identity for authorization - who is executing
- An entity **assumes** a principal when it executes

---

## Program Types

### 1. Provider (`programType: "provider"`)

Routes API calls to LLM services.

```typescript
{
  id: "cf-gateway",
  type: "program",
  programType: "provider",
  lifecycle: "published",  // draft → published → deprecated
  config: {
    providerType: "cloudflare-ai-gateway",
    accountId: "${CLOUDFLARE_ACCOUNT_ID}",
    gatewayId: "${CLOUDFLARE_GATEWAY_ID}",
    // API key from env, not stored
  }
}
```

**Events:** PROVIDER_CREATED, PROVIDER_CONFIGURED

---

### 2. Model (`programType: "model"`)

Inference configuration. References a Provider.

```typescript
{
  id: "claude-balanced",
  type: "program",
  programType: "model",
  lifecycle: "published",  // draft → published → deprecated
  config: {
    name: "Claude Balanced",
    modelId: "claude-sonnet-4-5",
    provider: "@(cf-gateway)",

    // Default inference params
    temperature: 0.7,
    maxTokens: 4000,
    topP: 0.9,

    // Situational overrides
    situations: {
      "coding": { temperature: 0, maxTokens: 8000 },
      "creative": { temperature: 0.95 },
      "quick": { maxTokens: 500 }
    }
  }
}
```

**Invoke:** Do inference with messages
**Events:** MODEL_CREATED, MODEL_CONFIGURED, MODEL_INFERENCE_REQUESTED, MODEL_INFERENCE_COMPLETED

---

### 3. Session (`programType: "session"`)

Conversation channel. JSONL log of messages with metadata.

```typescript
{
  id: "session-abc123",
  type: "program",
  programType: "session",
  lifecycle: "active",  // created → active ↔ paused → completed
  config: {
    owner: "@(agent-researcher)",
    defaultModel: "@(claude-balanced)",
    logFile: "sessions/abc123.jsonl",  // Or inline messages
  }
}
```

**Session Log Entry (JSONL format, per Claude Code pattern):**

```typescript
interface SessionLogEntry {
  uuid: string;
  parentUuid?: string;
  sessionId: string;
  timestamp: string;

  message: {
    role: "user" | "assistant";
    content: string;
    model?: string;           // Which model generated (assistant only)
    usage?: {
      inputTokens: number;
      outputTokens: number;
      cacheReadTokens?: number;
      cacheWriteTokens?: number;
    };
  };

  // Request metadata
  requestId?: string;
  provider?: string;
  latencyMs?: number;
  situation?: string;         // If situational params used
}
```

**Invoke:** Send message, optionally specify model

```typescript
await session.invoke({
  message: "Research quantum computing"
});

await session.invoke({
  message: "What does gemini think?",
  model: "@(gemini-flash)"  // Override for this message
});
```

**Events:** SESSION_CREATED, SESSION_MESSAGE, SESSION_MODEL_SWITCHED, SESSION_COMPLETED

---

### 4. Agent (`programType: "agent"`)

Autonomous actor with control harness around sessions.

```typescript
{
  id: "researcher",
  type: "program",
  programType: "agent",
  state: "idle",  // idle → thinking → executing → waiting → paused → error (runtime state)
  config: {
    name: "Research Agent",
    systemPrompt: "You are a research assistant...",
    tools: ["@(web-search)", "@(summarize)"],  // Program IDs
    defaultModel: "@(claude-balanced)",

    harness: {
      maxTurns: 50,
      reflectOnFailure: true,
      checkpointEvery: 5,
    },

    // Runtime state (populated when task assigned)
    currentTask: "task-123",           // Active task ID
    currentSession: "researcher_task-123_1706...",  // Session for this task
    turnCount: 5,
    lastCheckpoint: 5
  }
}
```

**Agent does NOT own inference params** - those are on the Model.
**Agent creates Session on task assignment** - `assignTask()` creates a new Session.
**Agent.step() uses Session** - preserves conversation history across turns.

**Events:** AGENT_CREATED, AGENT_TASK_ASSIGNED, AGENT_THINKING, AGENT_EXECUTING, AGENT_COMPLETED, AGENT_CHECKPOINT, AGENT_RESET

---

### 5. Task (`programType: "task"`)

Work specification with formal success criteria.

```typescript
{
  id: "task-research-quantum",
  type: "program",
  programType: "task",
  lifecycle: "pending",  // pending → active → completed | failed | blocked
  config: {
    title: "Research quantum computing",
    spec: {
      inputs: ["topic: quantum computing"],
      outputs: ["summary document"],
      constraints: ["max 2000 words"],
      successCriteria: [
        { type: "file_exists", path: "quantum-research.md" },
        { type: "word_count", min: 1000 }
      ]
    },
    assignee: "@(researcher)",
    priority: "P1"
  }
}
```

**Events:** TASK_CREATED, TASK_ASSIGNED, TASK_STARTED, TASK_COMPLETED, TASK_FAILED

---

### 6. Information (`programType: "information"`)

Structured knowledge nodes.

```typescript
{
  id: "info-quantum-basics",
  type: "program",
  programType: "information",
  lifecycle: "validated",  // draft → validated → active ↔ archived
  config: {
    infoType: "fact" | "schema" | "workflow" | "pattern",
    content: { ... },
    schema: "@(quantum-schema)",  // Optional validation
    sources: ["@(task-research-quantum)"],
  }
}
```

**Events:** INFORMATION_CREATED, INFORMATION_VALIDATED, INFORMATION_UPDATED

---

### 7. Human (`programType: "human"`)

Human actor in the system.

```typescript
{
  id: "bln",
  type: "program",
  programType: "human",
  state: "available",  // available → busy → away → offline (presence state)
  config: {
    name: "Brian",
    preferences: {
      notificationChannel: "terminal",
    },
    permissions: ["approve", "assign", "configure"]
  }
}
```

**Invoke:** Send notification, request approval
**Events:** HUMAN_NOTIFIED, HUMAN_APPROVED, HUMAN_REJECTED

---

## ExecutionContext

Implicit credential and config passing through async call chains using `AsyncLocalStorage`.

```typescript
import { getCurrentContext, runWithContextAsync, createContextFromEnv } from './context';

// At entry point (CLI, HTTP handler, etc.)
const ctx = createContextFromEnv({ id: 'cli', type: 'human' });

await runWithContextAsync(ctx, async () => {
  // Anywhere in the call chain:
  const current = getCurrentContext();
  const apiKey = current.getCredential('ANTHROPIC_API_KEY');  // Falls back to process.env
  const timeout = current.getConfig('timeout', 30000);
});
```

**Principal types:** `human`, `agent`, `session`, `system`

**Credential lookup:** Context → Environment variable (fallback)

**Child contexts:** `ctx.child({ credentials: {...} })` for scope narrowing

**Delegation:** `ctx.delegate({ id: 'agent-1', type: 'agent' })` tracks `onBehalfOf`

---

## Inference Chain

```
ExecutionContext (credentials, config, principal)
    │
    └──► Human/Agent
            │
            └──► Session (conversation log)
                    │
                    ├── tracks history (JSONL)
                    ├── routes to model (default or per-message)
                    │
                    └──► Model (inference config)
                            │
                            ├── applies params (temp, maxTokens, situations)
                            ├── gets credentials from ExecutionContext
                            │
                            └──► Provider (API routing)
                                    │
                                    ├── cf-aig-authorization header (CF token)
                                    ├── x-api-key / Authorization (provider key)
                                    │
                                    └──► LLM (Cloudflare AI Gateway)
```

**Example flow:**

```typescript
// 1. Human sends to session
await session.invoke({ message: "Research quantum computing" });

// 2. Session routes to its default model
// 3. Model applies params, calls provider
// 4. Provider routes to Cloudflare AI Gateway → Claude
// 5. Response logged to session with metadata
// 6. Response returned

// 7. Human asks for different perspective
await session.invoke({
  message: "What does gemini think?",
  model: "@(gemini-flash)"
});

// 8. Session routes to specified model instead
// 9. Both responses in session history
```

---

## Implementation Stack

**LLM Access:**
- Vercel AI SDK (`ai`, `@ai-sdk/openai`, `@ai-sdk/anthropic`)
- Cloudflare AI Gateway (unified endpoint with `cf-aig-authorization` header)
- Provider-specific SDKs auto-selected based on model name

**Context:**
- `ExecutionContext` class with `AsyncLocalStorage` (Node.js/Bun)
- Credential lookup: context → environment variable fallback
- Principal tracking for audit trails

**Storage:**
- Sessions: JSONL files (per Claude Code pattern)
- Programs: Graph nodes (existing UGS)
- Events: WAL (existing UGS event log)

**Inference:**
- Batch mode (`generateText`) and streaming mode (`streamText`)
- `stream: true` option with `onToken` callback
- Situational param overrides
- Multi-model sessions
- Agent creates Session on task assignment
- Session preserves conversation history across agent turns

**Embeddings:**
- Cloudflare Workers AI (`@cf/baai/bge-base-en-v1.5`, 768 dimensions)
- `embedNode()` stores vectors in node properties
- `findSimilar()`, `findSimilarToNode()`, `findSimilarToText()`
- Brute-force cosine similarity (O(n), fine for <10k nodes)

**Tags:**
- Any node can have tags: `addTags()`, `removeTags()`, `getByTag()`, `getByTags()`
- Events: `NodeTagged`, `NodeUntagged`

**Deferred:**
- Tool calling through agent harness
- Browser-compatible context (TC39 AsyncContext proposal)
- Vector index for large-scale similarity (ANN)

---

## CLI Commands

```bash
# Providers
./ugs provider create cf-gateway --type cloudflare-ai-gateway
./ugs provider list

# Models
./ugs model create claude-fast --backend claude-sonnet-4-5 --provider cf-gateway
./ugs model configure claude-fast --temperature 0 --situation coding
./ugs model invoke claude-fast --message "Hello"
./ugs model invoke claude-fast --message "Hello" --stream  # Streaming mode

# Sessions
./ugs session create --model claude-fast
./ugs session send <session-id> "Hello"
./ugs session send <session-id> "What does gemini think?" --model gemini-flash
./ugs session history <session-id>

# Agents
./ugs agent create researcher --prompt "You are..." --model claude-fast
./ugs agent assign researcher <task-id>
./ugs agent status researcher

# Tasks
./ugs task create "Research quantum computing" --assignee researcher
./ugs task list --status in_progress

# Tags (any node)
./ugs tag add <node-id> tag1 tag2
./ugs tag remove <node-id> tag1
./ugs tag list <node-id>
./ugs nodes --tag trusted

# Embeddings
./ugs embed <node-id>                    # Embed a node
./ugs embed <node-id> --text "custom"    # Embed with custom text
./ugs similar <node-id> --limit 5        # Find similar nodes
./ugs similar --text "query" --limit 5   # Find by text
./ugs embed-stats                        # Embedding statistics

# All are programs
./ugs program list --type model
./ugs program get claude-fast
./ugs program invoke <any-program-id>
```
