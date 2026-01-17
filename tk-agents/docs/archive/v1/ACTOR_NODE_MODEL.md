# Actor-Node-Agent Conceptual Model

## Executive Summary

This document clarifies the relationships between actors, nodes, agents, humans, and programs/tools/APIs within the system. The key insight is that **all nodes ARE actors** (inheritance relationship), while **agents, humans, and external programs USE the system** (external relationship). Actor configurations (message handlers, state) should remain runtime concerns and should NOT be represented in the graph, which focuses on domain relationships and persistent identity.

## Definitions

### Actor

**Definition:** An actor is a computational entity with the following characteristics:
- Has a unique runtime identity (via internal `Symbol` in Address)
- Receives and processes messages asynchronously
- Maintains private state
- Can create other actors
- Foundation-level abstraction (Actor System layer)

**Examples:**
- TaskActor - actor that manages task state and lifecycle
- KnowledgeActor - actor that stores and queries information
- System - actor that provides messaging infrastructure
- AddressProxy - actor that forwards messages based on string ID mapping

**Properties:**
- Runtime-only existence (cannot be serialized)
- Accessed via Address objects
- Registered in Actor System
- Message-based communication only

### Node

**Definition:** A node is an actor that is also registered in the Graph with:
- A human-readable string ID (for serialization, CLI, REST)
- Cached properties (for quick access without messaging)
- Edges to other nodes (typed relationships)
- Coordination-layer abstraction (Graph System layer)

**Examples:**
- Task nodes (type: "task")
- Knowledge nodes (type: "knowledge")
- Artifact nodes (type: "artifact")
- Pattern nodes (type: "pattern")

**Properties:**
- All nodes ARE actors (inheritance)
- Dual identity: string ID (external) + Address (internal)
- Managed by Graph System
- Support edge-based relationships
- Support serialization/persistence

**Relationship to Actor:**
```
Actor (foundation) <--[IS-A]-- Node (coordination)
```

All nodes are actors, but not all actors are nodes. Nodes add:
- String ID for external reference
- Property caching
- Edge management
- Serialization support

### Agent (AI Agent)

**Definition:** An AI agent is an external computational entity that USES the system to accomplish goals. Agents are NOT represented as actors or nodes within the system.

**Examples:**
- ResearchAgent - uses system to coordinate research tasks
- CodeAnalysisAgent - uses system to track code analysis work
- WritingAgent - uses system to manage document creation tasks

**Properties:**
- External to the system (not an actor or node)
- Operates through the Graph/CLI interface
- Creates and manipulates nodes (tasks, knowledge)
- Sends messages to nodes via graph.send()
- May have its own internal state/logic outside the system

**Relationship to System:**
```
Agent (external) --[USES]--> Graph System --[MANAGES]--> Nodes --[ARE]--> Actors
```

Agents are CLIENTS of the system, not COMPONENTS of the system.

### Human

**Definition:** A human user is an external entity that USES the system to organize work and knowledge. Humans are NOT represented as actors or nodes.

**Examples:**
- Developer using Task CLI to manage work
- Team lead reviewing task progress
- Researcher organizing knowledge nodes

**Properties:**
- External to the system
- Operates through CLI or REST API
- Creates and manipulates nodes
- Makes decisions based on system observations
- Provides subjective assessments for criteria

**Relationship to System:**
```
Human (external) --[USES]--> CLI/API --[OPERATES]--> Graph System
```

Humans are USERS of the system, not COMPONENTS of the system.

### Program/Tool/API

**Definition:** An external system or tool that can be:
1. **Referenced** by tasks/knowledge (string identifier, no actor)
2. **Wrapped** as an actor (adapter pattern, becomes callable)
3. **Used** by agents/humans (external integration)

**Examples:**

**Referenced (no actor):**
- Git command in task.toolsAvailable
- External API URL in knowledge.sources
- Shell script path in task properties

**Wrapped as Actor:**
- GitActor - wraps git CLI, responds to messages like { type: "commit", payload: {...} }
- DatabaseActor - wraps database client, responds to queries
- LLMActor - wraps LLM API, responds to inference requests

**Used Externally:**
- npm used by human developer
- Web browser used by agent
- File system used by CLI

**Properties:**
- Default: just string identifiers (no representation)
- Optional: wrapped as actors when message-based interface needed
- Never represented as nodes (unless they produce artifacts)

**Relationship to System:**
```
Tool (string) --[REFERENCED-BY]--> Task/Knowledge properties
Tool (wrapped) --[IS-A]--> Actor (not Node)
Tool (external) --[USED-BY]--> Agent/Human
```

## Relationship Model

```
                    EXTERNAL (Outside System)
                    ========================

                    +--------+        +--------+       +----------+
                    | Human  |        | Agent  |       | Program/ |
                    |        |        | (AI)   |       | Tool/API |
                    +--------+        +--------+       +----------+
                         |                 |                |
                         |                 |                |
                    [USES]            [USES]           [USES]
                         |                 |                |
                         v                 v                v
                    +------------------------------------------+
                    |           CLI / REST API                 |
                    +------------------------------------------+
                                      |
                                      | operates
                                      v
                    +------------------------------------------+
                    |           GRAPH SYSTEM                   |
                    | - String ID <-> Address mapping          |
                    | - Edge management                        |
                    | - Property caching                       |
                    | - Serialization                          |
                    +------------------------------------------+
                                      |
                        manages       |       wraps
                                      |
                         +------------+------------+
                         |                         |
                         v                         v
                    +----------+            +-------------+
                    |  NODES   |            | ACTOR SYSTEM|
                    | (Actors  |            | - Address   |
                    |  with    |            | - Register  |
                    |  string  |            | - Send      |
                    |  IDs)    |            +-------------+
                    +----------+                  |
                         |                        |
                    [IS-A]                   [IS-A]
                         |                        |
                         v                        v
                    +------------------------------------+
                    |          ACTORS                    |
                    | - Message handling                 |
                    | - Private state                    |
                    | - Runtime identity (Symbol)        |
                    +------------------------------------+
                              ^
                              |
                         [IS-A]
                              |
                    +--------------------+
                    | Wrapped Tools      |
                    | (GitActor, etc.)   |
                    | - Adapter pattern  |
                    | - NOT nodes        |
                    +--------------------+


LEGEND:
------
[IS-A]    : Inheritance (all nodes ARE actors)
[USES]    : Usage (agents/humans USE system)
[MANAGES] : Coordination (graph MANAGES nodes)
[WRAPS]   : Encapsulation (actors WRAP tools)
```

## Design Space Exploration

### Question 1: Should Actor Configs Be In Graph?

**The question:** Should actor configuration (message handlers, private state, behavior logic) be represented as nodes in the graph?

#### Option A: Actor Configs as Nodes

**Approach:** Create special "actor_config" nodes that store handler definitions, state schemas, and behavior.

**Pros:**
- Configuration becomes queryable via graph
- Could enable dynamic actor reconfiguration
- Makes behavior "visible" in the graph structure
- Could support graph-based actor composition

**Cons:**
- **Massive conceptual complexity** - conflates runtime behavior with domain structure
- **Serialization nightmare** - functions cannot be serialized, would need eval() or code-as-data
- **Security risk** - storing executable code in data structures
- **Layer violation** - graph layer should not know about actor implementation details
- **Type confusion** - nodes become mix of domain entities and infrastructure config
- **Query pollution** - graph queries would need to filter out config nodes constantly
- **Unclear use case** - no clear requirement for dynamic runtime reconfiguration

**Trade-offs:**
- Gains: dynamic reconfiguration (uncertain value)
- Loses: clear layer separation, type safety, simple serialization, security

#### Option B: Actor Configs Outside Graph (RECOMMENDED)

**Approach:** Actor configurations remain runtime-only concerns:
- Factories define actor behavior (message handlers, state)
- Graph stores only domain properties (goal, status, content, etc.)
- Separation of concerns: behavior (actors) vs. identity/relationships (graph)

**Pros:**
- **Clear layer separation** - actor behavior stays in foundation layer, domain data in graph
- **Simple serialization** - only data serialized, not code
- **Type safety** - nodes are pure data, actors are pure behavior
- **Security** - no executable code in data structures
- **Performance** - no overhead of representing behavior as data
- **Standard pattern** - matches actor model philosophy (behavior is opaque to outside)

**Cons:**
- Actor reconfiguration requires code changes (not runtime changes)
- Behavior not queryable via graph (must examine code)
- Cannot represent actor topology in graph (only node topology)

**Trade-offs:**
- Gains: simplicity, security, performance, clear separation
- Loses: dynamic reconfiguration (but no requirement for this)

#### Option C: Hybrid Approach

**Approach:** Store some configuration metadata in graph (like "actorType" property), but keep behavior in code.

**Pros:**
- Enables queries like "find all task actors"
- Metadata visible without representing full behavior
- Still allows factory-based behavior definition

**Cons:**
- Partial solution that doesn't solve the reconfiguration problem
- Adds complexity without clear benefit
- Type property already serves this purpose

**Trade-offs:**
- Gains: minimal metadata visibility
- Loses: still no dynamic reconfiguration, added complexity

### Question 2: How Do Agents Fit?

**Core insight:** Agents are EXTERNAL to the system, not components of it.

**Relationship:**
- Agents USE the system (via Graph/CLI interface)
- Agents CREATE and MANIPULATE nodes (tasks, knowledge)
- Agents SEND messages to nodes (via graph.send())
- Agents may have their own internal architecture (RAG, planning, tool use)

**Should agents be represented in the graph?**

NO - for these reasons:
- Agents are clients, not managed entities
- Agent state/logic lives outside the system
- No benefit to representing agent metadata
- Agent identity not relevant to task/knowledge relationships

**Alternative:** If agent tracking needed:
- Create a separate AgentRegistry (outside this system)
- Tasks could reference agent IDs in properties (e.g., `assignedAgent: "research_agent_1"`)
- But agent itself is not a node

### Question 3: How Do Humans Fit?

**Core insight:** Humans are USERS of the system, not represented within it.

**Relationship:**
- Humans USE the CLI/API to operate the system
- Humans CREATE tasks/knowledge via commands
- Humans ASSESS subjective criteria for task completion
- Human identity tracked outside system (OS user, auth system)

**Should humans be represented in the graph?**

NO - for these reasons:
- Humans are external users, not managed entities
- Human actions mediated through CLI/API, not direct actor messaging
- No actor behavior to model
- Human identity managed by OS/auth, not our system

**Alternative:** If human tracking needed:
- Tasks could reference human IDs in properties (e.g., `assignedTo: "user123"`)
- Human identity resolved by external auth system
- But human itself is not a node or actor

### Question 4: How Do Programs/Tools/APIs Fit?

**Three patterns depending on integration needs:**

#### Pattern 1: Tool as String Reference (Default)

**Use when:** Tool is just referenced, not invoked by system

```typescript
const task = createTask({
  goal: "Deploy service",
  toolsAvailable: ["kubectl", "helm", "docker"],
  // Just strings - no actor representation
}, graph);
```

**Representation:** String in task properties, no actor, no node

#### Pattern 2: Tool as Wrapped Actor (When Needed)

**Use when:** System needs to send messages to tool

```typescript
// Create actor wrapper
const gitActor = GitActor({ repoPath: "/path", system });
// NOT registered in graph (not a node)

// Task can send to it
await system.send(gitActor, { type: "commit", payload: { message: "..." } });
```

**Representation:** Actor (via wrapper), but NOT a node
- Has Address for messaging
- NOT in graph (no string ID, no edges)
- Adapter pattern around external tool

#### Pattern 3: Tool Used Externally

**Use when:** Tool used by humans/agents, not by system

```typescript
// Agent uses git via shell
exec("git commit -m 'message'");
// Human uses browser to review docs
// No representation in system at all
```

**Representation:** None - purely external

**Should wrapped tools be nodes?**

NO - wrapped tool actors should NOT be nodes because:
- They don't have domain properties to cache
- They don't participate in edge relationships
- They're infrastructure, not domain entities
- Making them nodes would pollute graph with infrastructure

**Exception:** If tool produces an artifact, the artifact can be a node:
```typescript
// Git is not a node
// But the commit it produces could be represented as an artifact node
const artifact = createArtifact({
  type: "artifact",
  title: "Commit abc123",
  content: "...",
  producedBy: "task_42"
}, graph);
```

## Recommendations

### Primary Recommendation: Clear Layer Separation

**Actor configurations should NOT be in the graph.**

**Rationale:**

1. **Layer Separation**: The actor layer and graph layer have distinct responsibilities:
   - Actor layer: Runtime behavior, message handling, private state
   - Graph layer: Persistent identity, relationships, domain properties

2. **Serialization**: Only domain data needs persistence:
   - Task properties: goal, status, criteria, timestamps
   - Knowledge properties: title, content, sources, version
   - NOT: message handlers, state machines, behavior logic

3. **Type Safety**: Nodes are data, actors are behavior:
   - Mixing them creates type confusion
   - Makes reasoning about system harder
   - Breaks clear mental model

4. **Security**: Storing executable code in data structures:
   - Requires eval() or similar (security risk)
   - Makes system vulnerable to code injection
   - Complicates security auditing

5. **No Requirement**: Current use cases don't need dynamic reconfiguration:
   - Actor behavior defined at compile time
   - Changes made via code, not runtime data
   - Standard development workflow (edit, test, deploy)

**Implementation:**
- Keep factory pattern for actor creation
- Graph stores only domain properties
- Factories remain pure functions with explicit dependencies
- Behavior defined in code, data in graph

**Migration Path:**
- No migration needed - this is current design
- Document the decision clearly in specs
- Add examples showing proper layer separation

### Alternative Recommendation: Minimal Metadata (If Needed)

**IF** there's a future need for actor type discovery, add minimal metadata:

```typescript
// In node properties
{
  id: "task_1",
  type: "task",  // Already serves as actor type indicator
  // No need for additional "actorType" field
}
```

**Rationale:**
- The `type` field already indicates what kind of actor it is
- No additional metadata needed
- Keep it simple

## Answer to Core Question

**Question:** Should actor configurations be represented in the graph?

**Answer:** **NO.**

**Why:**

1. **Actor configurations are runtime behavior** - message handlers, state machines, and processing logic belong to the actor/factory implementation, not to the graph representation.

2. **Graph stores domain relationships** - the graph is for tracking persistent identity (string IDs), domain properties (goal, content, status), and semantic relationships (edges). It is not for representing executable behavior.

3. **Clear layer separation** - keeping actor behavior in the foundation layer (Actor System) and domain data in the coordination layer (Graph System) maintains clean architecture.

4. **Serialization boundary** - domain properties serialize to JSON (tasks.json), but actor behavior cannot be serialized. Mixing them creates an inconsistent model.

5. **Type safety and security** - storing executable code in data structures requires eval() or code-as-data patterns that compromise security and type safety.

6. **No current requirement** - there is no identified use case for dynamic actor reconfiguration at runtime. Behavior changes happen via code changes, following standard development workflow.

**Implications:**

- Nodes in the graph represent **what** (tasks, knowledge, artifacts)
- Actors in the system implement **how** (message handling, state management)
- The `type` field in node properties indicates which factory was used
- To understand actor behavior, examine the factory code
- To understand domain structure, query the graph

**Example:**

```typescript
// This goes in the graph (domain data):
{
  id: "task_1",
  type: "task",
  goal: "Implement feature",
  state: "active",
  objectiveSuccessCriteria: [...],
  // ... other domain properties
}

// This stays in code (behavior):
const TaskActor: ActorFactory<TaskActorData> = (data) => {
  // Message handler logic
  const handleMessage = async (msg: Message) => {
    switch (msg.type) {
      case "start": // ...
      case "complete": // ...
      // ... behavior logic
    }
  };

  // Create and register actor
  // ...
};
```

The graph knows "task_1" exists and is active. The code knows how to process messages sent to "task_1". Clear separation.

## Conclusion

The system has a clean architecture with distinct layers:

1. **Foundation Layer (Actors)**: Runtime behavior, message handling, Address-based communication
2. **Coordination Layer (Graph)**: String IDs, edges, property caching, serialization
3. **Domain Layer (Task/Knowledge)**: Specific node types with domain logic
4. **Interface Layer (CLI)**: Human/agent interaction

**Key insights:**

- **All nodes ARE actors** (inheritance) - nodes add string IDs, edges, and caching to actors
- **Agents USE the system** (external) - not represented as actors or nodes
- **Humans USE the system** (external) - not represented as actors or nodes
- **Tools are REFERENCED** (strings), **WRAPPED** (actors), or **EXTERNAL** (no representation)
- **Actor configs stay in code** - graph stores only domain data, not behavior

This model provides:
- Clear separation of concerns
- Simple serialization story
- Type safety and security
- Extensibility (new node types, new edge types)
- Understandable mental model

The decision to keep actor configurations OUT of the graph maintains these properties and aligns with actor model philosophy: behavior is opaque, identity is first-class.
