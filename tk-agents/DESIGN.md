# tk-agents: Design & Architecture

## Overview

A unified actor-based system for task and knowledge management. All interactions happen through message passing (`SEND`). Actors can be deterministic (scripts, recipes) or non-deterministic (LLM agents).

```mermaid
graph TB
    subgraph "External"
        H[Human] --> R[Router]
    end

    subgraph "Registry"
        R --> REG[Actor Registry]
        REG --> A1[ClaudeActor]
        REG --> A2[BashActor]
        REG --> A3[TaskNode]
        REG --> A4[KnowledgeNode]
    end

    subgraph "Infrastructure"
        A1 --> CLI[Claude CLI]
        A2 --> BASH[Shell]
    end
```

---

## Core Concepts

### Everything is an Actor

```mermaid
classDiagram
    class Actor {
        <<interface>>
        +id: string
        +type: deterministic | agent
        +send(Message) Response
        +stream(Message) AsyncGenerator
    }

    class ClaudeActor {
        +sessionId: string
        +turnCount: number
        +send() spawns CLI
        +stream() yields events
    }

    class BashActor {
        +cwd: string
        +timeout: number
        +send() runs command
    }

    class TaskNode {
        +state: TaskState
        +goal: string
        +criteria: Criterion[]
        +spawn() creates child
        +eval() checks criteria
    }

    class KnowledgeNode {
        +content: string
        +sources: string[]
        +query() answers questions
        +append() adds data
    }

    Actor <|-- ClaudeActor
    Actor <|-- BashActor
    Actor <|-- TaskNode
    Actor <|-- KnowledgeNode
```

### Actor Spectrum

```
Deterministic                                              Non-deterministic
     │                                                              │
     ▼                                                              ▼
BashActor ──── RecipeActor ──── TaskNode ──── KnowledgeNode ──── ClaudeActor
     │              │              │               │                  │
  "run it"     "follow it"    "manage it"    "query it"        "figure it out"
```

---

## State Machines

### Task Lifecycle

```mermaid
stateDiagram-v2
    [*] --> created: createTask()

    created --> active: start
    active --> blocked: block
    blocked --> active: unblock
    active --> completed: complete (if eval passes)
    active --> failed: fail

    completed --> [*]
    failed --> [*]

    note right of active
        Can spawn child tasks
        Can update criteria
        Can link to knowledge
    end note

    note right of blocked
        reason: string
        requiredKnowledge: string[]
    end note
```

### Claude Session Lifecycle

```mermaid
stateDiagram-v2
    [*] --> idle: new ClaudeActor()

    idle --> running: send()
    running --> idle: response received

    idle --> streaming: stream()
    streaming --> streaming: yield event
    streaming --> idle: final result

    note right of idle
        sessionId persisted
        turnCount tracked
    end note

    note right of running
        --session-id (turn 0)
        --resume (turn 1+)
    end note
```

---

## Message Protocol

### Standard Messages (All Actors)

| Message | Payload | Response | Description |
|---------|---------|----------|-------------|
| `get` | `{}` | `{id, type, properties, edges}` | Get actor state |
| `observe` | `{}` | `{state, observations, metadata}` | Get observations |
| `update` | `{properties}` | `{success, updatedProperties}` | Update properties |
| `link` | `{toId, edgeType}` | `{edgeId, success}` | Create edge |
| `unlink` | `{edgeId}` | `{success}` | Remove edge |
| `delete` | `{}` | `{success}` | Remove actor |

### Task Messages

| Message | Payload | Response | Description |
|---------|---------|----------|-------------|
| `start` | `{context?}` | `{success, state}` | Begin execution |
| `spawn` | `{goal, criteria, ...}` | `{childTaskId, success}` | Create child task |
| `eval` | `{}` | `{score, passed, criteria, observations}` | Evaluate criteria |
| `complete` | `{result, artifacts?}` | `{success, finalState}` | Mark complete |
| `block` | `{reason, requiredKnowledge?}` | `{success, state}` | Mark blocked |
| `query_status` | `{}` | `{state, progress, blockers, children}` | Get full status |

### Knowledge Messages

| Message | Payload | Response | Description |
|---------|---------|----------|-------------|
| `append` | `{data, source?}` | `{success, version}` | Add content |
| `query` | `{question, context?}` | `{answer, confidence, sources}` | Ask question |
| `synthesize` | `{fromNodes}` | `{synthesis, sources}` | Combine knowledge |

---

## Scenarios (BDD Style)

### Scenario: Task Delegation

```gherkin
Feature: Task delegation between actors

  Scenario: Coordinator delegates to worker
    Given a "coordinator" ClaudeActor with system prompt "You are a task coordinator"
    And a "worker" BashActor
    And the coordinator is registered in the registry
    And the worker is registered in the registry

    When I send "plan" message to coordinator with payload "deploy the app"
    Then the response should contain action "delegate"
    And the response should contain target actor "worker"

    When I send the delegated task to the worker
    Then the worker should execute the command
    And the response should indicate success
```

### Scenario: Resumable Session

```gherkin
Feature: Multi-turn conversation with session persistence

  Scenario: Claude remembers previous turns
    Given a ClaudeActor with id "tutor" and model "haiku"

    When I send "What is 15 + 27?"
    Then I receive a response containing "42"
    And the session turnCount is 1

    When I send "Double that number"
    Then I receive a response containing "84"
    And the session turnCount is 2
    And the same sessionId is used

    When I send "What was my first question?"
    Then I receive a response mentioning "15" and "27"
```

### Scenario: Chained Pipeline

```gherkin
Feature: Stream-chaining between Claude actors

  Scenario: Planner to Executor pipeline
    Given a ChainedActors with:
      | name     | prompt                    |
      | planner  | Break down the task       |
      | executor | Execute the plan steps    |

    When I send "Build a REST API" to the chain
    Then the planner receives the input
    And the planner's output streams to the executor
    And the executor receives full context from planner
    And I receive the final executor response
```

---

## Test Fixtures

### Actor Fixtures

```typescript
// fixtures/actors.ts

export const fixtures = {
  // Echo actor - returns payload as-is
  echoActor: () => createEchoMock("echo"),

  // Failing actor - always fails
  failingActor: (error: string) => createFailingMock("fail", error),

  // Claude-like actor with canned responses
  claudeActor: (responses: string[]) => createClaudeMock("claude", responses),

  // Bash actor with custom cwd
  bashActor: (cwd: string) => createBashActor({ id: "bash", cwd }),
};
```

### Message Fixtures

```typescript
// fixtures/messages.ts

export const messages = {
  // Standard task messages
  startTask: (context = {}) => createMessage("start", { context }),
  evalTask: () => createMessage("eval", {}),
  completeTask: (result: unknown) => createMessage("complete", { result }),

  // Spawn with criteria
  spawnTask: (goal: string, criteria: Criterion[]) => createMessage("spawn", {
    goal,
    deliverables: [],
    criteria,
  }),

  // Knowledge queries
  query: (question: string) => createMessage("query", { question }),
  append: (data: string, source?: string) => createMessage("append", { data, source }),
};
```

### Scenario Table (FIT-style)

| Scenario | Actor Type | Input | Expected Output | Expected State |
|----------|------------|-------|-----------------|----------------|
| Echo | MockActor | `{x: 1}` | `{x: 1}` | - |
| Bash pwd | BashActor(cwd=/tmp) | `pwd` | `/tmp\n` | - |
| Bash fail | BashActor | `exit 1` | error | - |
| Bash timeout | BashActor(timeout=100) | `sleep 5` | timeout error | - |
| Task start | TaskNode(state=created) | `start` | success | active |
| Task spawn | TaskNode(state=active) | `spawn{goal}` | childId | - |
| Task eval pass | TaskNode(criteria met) | `eval` | passed=true | - |
| Task eval fail | TaskNode(criteria not met) | `eval` | passed=false | - |
| Knowledge query | KnowledgeNode | `query{q}` | answer, confidence | - |

---

## Architecture Patterns

### Pattern 1: Resumable Session

```mermaid
sequenceDiagram
    participant C as Caller
    participant A as ClaudeActor
    participant CLI as Claude CLI

    C->>A: send("First prompt")
    A->>CLI: claude -p --session-id X "First prompt"
    CLI-->>A: response
    A-->>C: {success, data, sessionId}

    Note over A: turnCount = 1

    C->>A: send("Follow up")
    A->>CLI: claude -p --resume X "Follow up"
    CLI-->>A: response (with full history)
    A-->>C: {success, data, sessionId}

    Note over A: turnCount = 2
```

### Pattern 2: Chained Pipeline

```mermaid
sequenceDiagram
    participant C as Caller
    participant CH as ChainedActors
    participant P as Planner (Claude)
    participant E as Executor (Claude)

    C->>CH: send("Build feature X")
    CH->>P: claude -p --output-format stream-json
    P-->>E: stdout | stdin (stream-json)
    E-->>CH: final response
    CH-->>C: {success, data}

    Note over P,E: Full context flows through pipe
```

### Pattern 3: Task Decomposition

```mermaid
graph TD
    T1[Parent Task] -->|spawn| T2[Child 1]
    T1 -->|spawn| T3[Child 2]
    T1 -->|spawn| T4[Child 3]

    T2 -->|requires| K1[Knowledge Node]
    T3 -->|requires| K1

    T4 -->|uses| B1[BashActor]

    T2 -->|eval| E1{Pass?}
    T3 -->|eval| E2{Pass?}
    T4 -->|eval| E3{Pass?}

    E1 -->|yes| C1[Complete]
    E2 -->|yes| C2[Complete]
    E3 -->|yes| C3[Complete]

    C1 --> T1
    C2 --> T1
    C3 --> T1

    T1 -->|all children done| EVAL{Eval Parent}
    EVAL -->|pass| DONE[Completed]
```

---

## Directory Structure

```
tk-agents/
├── src/
│   ├── actors/
│   │   ├── base.ts           # Actor interface, Message/Response types
│   │   ├── registry.ts       # Actor management, routing
│   │   ├── claude.ts         # ClaudeActor (resumable sessions)
│   │   ├── chain.ts          # ChainedActors (pipeline)
│   │   ├── bash.ts           # BashActor (deterministic)
│   │   ├── mock.ts           # MockActor (testing)
│   │   └── actors.test.ts    # Unit tests
│   ├── graph.ts              # Graph store, SEND routing
│   ├── task.ts               # TaskNode, lifecycle
│   ├── knowledge.ts          # KnowledgeNode
│   ├── types.ts              # Shared types
│   └── index.ts              # Exports
├── integration/
│   ├── test-claude-actor.ts  # Real CLI tests
│   └── test-chain.ts         # Pipeline tests
├── demo.ts                   # Task lifecycle demo
└── DESIGN.md                 # This document
```

---

## Running Tests

```bash
# Unit tests (uses mocks, runs anywhere)
bun test

# Integration tests (requires Claude CLI, run outside Claude)
bun run integration/test-claude-actor.ts
bun run integration/test-chain.ts

# Demo
bun run demo.ts
```

---

## Future Work

- [ ] HTTP/Socket server for external access
- [ ] Persistence layer (SQLite)
- [ ] Pattern extraction from completed tasks
- [ ] RecipeActor for multi-step deterministic workflows
- [ ] Uncertainty/escalation protocol
- [ ] Human-in-the-loop approval flow
