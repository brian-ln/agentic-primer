# Actor-Node Model V2: Nodes as Executable Work Units

## Executive Summary

This document presents an alternate perspective on the actor-node model where **nodes are executable work units** accessed through **location-transparent addressing**, rather than data entities with external clients.

### V1 vs V2 Perspectives

| Aspect | V1: Nodes as Data | V2: Nodes as Executables |
|--------|-------------------|--------------------------|
| **Core Metaphor** | Nodes are data entities (tasks, knowledge) managed by external clients | Nodes are executable work units with location-transparent addressing |
| **Node Definition** | Actor with string ID, edges, and cached properties | Executable work unit: script, API, agent, tool, or human |
| **Agent Status** | External client that USES the system | Node type: AgentNode defined by markdown specification |
| **Human Status** | External user operating CLI/API | Optional node type: HumanNode with send/receive interface |
| **Tool Status** | Referenced strings, wrapped actors, or external | ToolNode: executable program in the graph |
| **Knowledge** | Data stored in nodes | Can be executable skills/patterns or data |
| **Location** | Implicit (all nodes local in graph) | Explicit: local, remote, in-memory (transparent to caller) |
| **Communication** | Message passing via addresses | Message passing via addresses (same) |
| **Interface** | CLI/API for external clients | All nodes addressable, chatbot = human node interface |

### When to Use Each Perspective

**Use V1 (Nodes as Data) when:**
- Building task management systems
- Managing knowledge bases
- Tracking work items and artifacts
- Clear separation between system and users needed
- Serialization and persistence are primary concerns

**Use V2 (Nodes as Executables) when:**
- Building distributed agent systems
- Implementing location-transparent computation
- Treating agents, tools, and humans uniformly
- Knowledge includes executable skills/patterns
- Scaling across local and remote execution

### Coexistence Strategy

These perspectives are complementary and can coexist:
- V1 provides the **data layer** (tasks, knowledge as entities)
- V2 provides the **execution layer** (agents, tools, humans as work units)
- Both use the same **actor foundation** (addresses, messaging)
- **Type system** distinguishes entity nodes from executable nodes

## Definitions

### Node (V2 Definition)

**Definition:** A node is an executable work unit with:
- A unique address for location-transparent invocation
- A send/receive interface for message-based communication
- An execution type determining how work is performed
- Optional location metadata (local, remote, distributed)
- Optional properties for configuration and state

**Core Principle:** Everything that can do work is a node, regardless of whether it is a program, agent, tool, API, or human.

### Node Execution Types

#### 1. ScriptNode

**Definition:** A node that wraps an executable script (bash, python, ruby, etc.).

**Properties:**
- `executablePath`: Path to script file
- `interpreter`: Optional interpreter (defaults to shebang)
- `environment`: Environment variables for execution
- `workingDirectory`: Execution context

**Invocation:**
```typescript
await system.send(scriptNode, {
  type: "execute",
  payload: {
    args: ["--input", "data.json"],
    stdin: "optional input"
  }
});
```

**Example:**
```typescript
const scriptNode = createScriptNode({
  id: "data_processor",
  executablePath: "/scripts/process_data.py",
  interpreter: "python3",
  environment: { DATA_DIR: "/data" }
}, graph);

// Use it
const result = await graph.send("data_processor", "execute", {
  args: ["--format", "json"]
});
```

#### 2. APINode

**Definition:** A node that wraps an external API endpoint (REST, GraphQL, gRPC).

**Properties:**
- `apiEndpoint`: Base URL or service address
- `protocol`: HTTP, gRPC, WebSocket, etc.
- `authentication`: Auth configuration
- `methods`: Supported operations/methods

**Invocation:**
```typescript
await system.send(apiNode, {
  type: "invoke",
  payload: {
    method: "GET",
    path: "/users/123",
    params: { include: "profile" }
  }
});
```

**Example:**
```typescript
const apiNode = createAPINode({
  id: "github_api",
  apiEndpoint: "https://api.github.com",
  protocol: "https",
  authentication: { type: "token", token: process.env.GITHUB_TOKEN },
  methods: ["get_repo", "list_issues", "create_pr"]
}, graph);

// Use it
const issues = await graph.send("github_api", "invoke", {
  method: "list_issues",
  params: { repo: "owner/repo", state: "open" }
});
```

#### 3. AgentNode

**Definition:** A node that wraps an LLM-based agent defined by a markdown specification.

**Agent Definition (Markdown):** The current Claude Code agent system uses markdown files with YAML frontmatter to define agents. This pattern fits naturally into the V2 model.

**Structure:**
```markdown
---
name: research-agent
model: claude-sonnet-4-5
temperature: 0.7
tools:
  - web_search
  - code_analysis
  - file_system
---

# Research Agent

You are a research specialist that conducts deep investigations into technical topics.

## Goal
[Agent's objective and success criteria]

## Tools Available
[Descriptions of tools this agent can use]

## Interaction Protocol
[How the agent receives tasks and returns results]
```

**Properties:**
- `agentDefinitionPath`: Path to markdown specification
- `model`: LLM model identifier
- `tools`: List of available tools
- `systemPrompt`: Loaded from markdown
- `successCriteria`: From markdown frontmatter

**Invocation:**
```typescript
await system.send(agentNode, {
  type: "execute",
  payload: {
    task: "Research actor model best practices",
    context: { domain: "distributed systems" }
  }
});
```

**Example:**
```typescript
const agentNode = createAgentNode({
  id: "research_agent_1",
  agentDefinitionPath: "/agents/research-agent.md",
  model: "claude-sonnet-4-5",
  tools: ["web_search", "file_system"]
}, graph);

// Use it
const researchResult = await graph.send("research_agent_1", "execute", {
  task: "Research actor model patterns",
  deliverables: ["summary", "code_examples"]
});
```

**Integration with Current System:**
The existing Claude Code agent definitions (markdown files) map directly to AgentNodes:
- Markdown file = agent specification
- Agent execution = LLM in a loop with goal, tools, and success criteria
- Agent instance = node in the graph
- Agent invocation = send message to node address

#### 4. ToolNode

**Definition:** A node that wraps a callable program, function, or system utility.

**Properties:**
- `toolType`: "binary", "library", "function", "system"
- `invocationMethod`: How to call the tool
- `inputSchema`: Expected input format
- `outputSchema`: Output format

**Invocation:**
```typescript
await system.send(toolNode, {
  type: "invoke",
  payload: {
    operation: "transform",
    input: { data: [...] }
  }
});
```

**Example:**
```typescript
const toolNode = createToolNode({
  id: "jq_processor",
  toolType: "binary",
  invocationMethod: "cli",
  executablePath: "/usr/bin/jq",
  inputSchema: { type: "json" },
  outputSchema: { type: "json" }
}, graph);

// Use it
const filtered = await graph.send("jq_processor", "invoke", {
  filter: ".items[] | select(.active)",
  input: jsonData
});
```

#### 5. HumanNode (Optional)

**Definition:** A node that represents a human participant with a send/receive interface.

**Key Insight:** Humans can be modeled as actors that receive messages and send responses. A chatbot interface naturally maps to this pattern:
- User input = message to human node
- Human response = reply from human node
- Conversation = message exchange

**Properties:**
- `humanId`: Human identifier
- `communicationChannel`: "cli", "web", "chat", "email"
- `responseTimeout`: Optional timeout for human responses
- `pendingMessages`: Queue of messages awaiting response

**Invocation:**
```typescript
await system.send(humanNode, {
  type: "request_input",
  payload: {
    prompt: "Should we proceed with deployment?",
    options: ["yes", "no", "wait"],
    timeout: 3600000 // 1 hour
  }
});
```

**Example:**
```typescript
const humanNode = createHumanNode({
  id: "human_reviewer_alice",
  humanId: "alice@example.com",
  communicationChannel: "chat"
}, graph);

// Request human input
const decision = await graph.send("human_reviewer_alice", "request_input", {
  prompt: "Review this code change",
  context: { pr: "PR#123", files: ["auth.ts"] },
  timeout: 7200000 // 2 hours
});

// Human responds via chat interface, which routes back to calling code
```

**Chatbot Interface Mapping:**
```typescript
// Chatbot receives user message
chatbot.on("message", async (userId, messageText) => {
  const humanNodeId = `human_${userId}`;

  // User message treated as human node output
  await graph.send(recipientNode, {
    type: "human_response",
    from: humanNodeId,
    payload: { text: messageText }
  });
});

// System needs human input
async function requestHumanInput(humanNodeId: string, prompt: string) {
  // Send to human node
  await graph.send(humanNodeId, {
    type: "input_request",
    payload: { prompt }
  });

  // Human node routes to chatbot interface
  chatbot.send(humanNodeId, prompt);

  // Wait for response (with timeout)
  return await waitForResponse(humanNodeId);
}
```

**Benefits:**
- Uniform interface: agents, tools, and humans all use send/receive
- Humans can be participants in agent workflows
- Chat interfaces map naturally to message passing
- Enables human-in-the-loop patterns

**Trade-offs:**
- Requires modeling humans as system entities (V1 keeps them external)
- Adds complexity for simple CLI interactions
- Best for systems where human-agent collaboration is central

## Location Transparency

### Core Principle

**Location transparency** means that the caller of a node does not need to know where the node is executing. The same send/receive interface works regardless of whether the node is:
- In the same process (in-memory)
- On the local machine (different process)
- On a remote server (network call)
- Distributed across multiple locations

### Address-Based Routing

All nodes are accessed via addresses. The addressing system handles routing transparently:

```typescript
// Caller doesn't know or care about location
await graph.send("data_processor", "execute", { input: data });

// Routing layer determines:
// - Is "data_processor" local or remote?
// - What protocol to use (in-memory, IPC, HTTP, gRPC)?
// - How to serialize/deserialize the message?
```

### Location Metadata

Nodes can specify location metadata:

```typescript
interface LocationMetadata {
  locationType: "local" | "remote" | "distributed";
  address?: string;           // Remote URL or service address
  protocol?: string;          // "http", "grpc", "ws", etc.
  loadBalancing?: "round_robin" | "random" | "affinity";
}
```

**Example:**
```typescript
// Local script node
const localScript = createScriptNode({
  id: "local_processor",
  executablePath: "/scripts/process.py",
  location: { locationType: "local" }
}, graph);

// Remote API node
const remoteAPI = createAPINode({
  id: "remote_service",
  apiEndpoint: "https://service.example.com",
  location: {
    locationType: "remote",
    address: "https://service.example.com",
    protocol: "https"
  }
}, graph);

// Distributed agent node (load balanced across instances)
const distributedAgent = createAgentNode({
  id: "distributed_research_agent",
  agentDefinitionPath: "/agents/research.md",
  location: {
    locationType: "distributed",
    address: "agent-pool://research-agents",
    loadBalancing: "round_robin"
  }
}, graph);

// All invoked the same way
await graph.send("local_processor", "execute", { input });
await graph.send("remote_service", "invoke", { method: "get_data" });
await graph.send("distributed_research_agent", "execute", { task });
```

### Transparent Dispatch

The graph system routes messages based on location metadata:

```typescript
class Graph {
  async send(nodeId: string, messageType: string, payload: any) {
    const node = this.nodes.get(nodeId);
    const location = node.location;

    switch (location.locationType) {
      case "local":
        // Direct in-memory message passing
        return await this.system.send(node.address, { type: messageType, payload });

      case "remote":
        // HTTP/gRPC call to remote service
        return await this.remoteDispatcher.send(location.address, {
          nodeId,
          type: messageType,
          payload
        });

      case "distributed":
        // Load balancer selects instance
        const instance = await this.loadBalancer.selectInstance(location.address);
        return await this.remoteDispatcher.send(instance, {
          nodeId,
          type: messageType,
          payload
        });
    }
  }
}
```

### Benefits

**For Developers:**
- Write code once, deploy anywhere
- Test locally, run remotely
- Refactor without changing callers

**For Systems:**
- Scale transparently (move nodes to remote servers)
- Optimize placement (co-locate nodes to reduce latency)
- Fault tolerance (redistribute failed nodes)

**For Architecture:**
- Clear separation: addressing vs. location
- Flexible deployment models
- Enable distributed agent systems

## Knowledge as Programs and Skills

### Executable Knowledge

In V2, knowledge can be:
1. **Data** (traditional knowledge nodes): documents, facts, information
2. **Skills** (executable patterns): reusable code patterns, templates
3. **Programs** (executable logic): algorithms, transformations, computations

### Example: Knowledge Node Types

```typescript
// Traditional data knowledge
const dataKnowledge = createKnowledge({
  id: "actor_model_intro",
  type: "knowledge",
  title: "Actor Model Introduction",
  content: "The actor model is a mathematical model...",
  sources: ["https://..."]
}, graph);

// Executable skill knowledge
const skillKnowledge = createSkillNode({
  id: "http_client_pattern",
  type: "skill",
  title: "HTTP Client Pattern",
  pattern: `
    class HTTPClient {
      async get(url: string) { ... }
      async post(url: string, body: any) { ... }
    }
  `,
  applicableContexts: ["api_integration", "web_scraping"],
  executablePath: "/skills/http-client-skill.js"
}, graph);

// Executable program knowledge
const programKnowledge = createProgramNode({
  id: "data_transformer",
  type: "program",
  title: "JSON to CSV Transformer",
  executablePath: "/programs/json-to-csv.py",
  inputSchema: { type: "json" },
  outputSchema: { type: "csv" }
}, graph);
```

### Using Executable Knowledge

```typescript
// Data knowledge: query for information
const info = await graph.query("actor_model_intro", {
  type: "get_content"
});

// Skill knowledge: apply pattern to generate code
const clientCode = await graph.send("http_client_pattern", "generate", {
  context: { baseUrl: "https://api.example.com" }
});

// Program knowledge: execute transformation
const csvData = await graph.send("data_transformer", "execute", {
  input: jsonData
});
```

### Benefits

- Knowledge base becomes active, not passive
- Patterns and skills are first-class entities
- Reusable computational knowledge
- Blurs line between data and code (in a good way)

## Comparison with V1 Model

### Conceptual Trade-offs

| Concern | V1 Approach | V2 Approach | Winner |
|---------|-------------|-------------|--------|
| **Simplicity** | Clear data/client separation | Uniform executable model | V1 |
| **Serialization** | Straightforward (JSON) | Complex (includes executables) | V1 |
| **Extensibility** | Add new node types | Add new execution types | Tie |
| **Distribution** | Implicit local execution | Explicit location transparency | V2 |
| **Agent Integration** | Agents are external | Agents are nodes | V2 |
| **Human Modeling** | Humans are external | Humans optionally nodes | V2 |
| **Tool Integration** | Three patterns (string/wrapped/external) | Uniform ToolNode | V2 |
| **Knowledge Activation** | Passive data | Active skills/programs | V2 |

### Implementation Complexity

**V1 (Lower Complexity):**
- Simpler mental model (data entities)
- Straightforward serialization
- Clear external/internal boundary
- Standard actor + graph pattern

**V2 (Higher Complexity):**
- Requires location routing infrastructure
- Complex serialization (executables + data)
- Requires execution type dispatch
- Requires protocol adapters (HTTP, gRPC, etc.)

### Use Case Fit

**V1 Better For:**
- Task management systems (Jira, Asana style)
- Knowledge management (wiki, note-taking)
- Simple local applications
- Systems with clear human/system boundary

**V2 Better For:**
- Distributed agent systems (multi-agent AI)
- Microservices architectures
- Computational workflows
- Systems where agents/tools/humans collaborate uniformly
- Location-transparent computing

## Migration Path: V1 to V2

### Coexistence Strategy

V1 and V2 can coexist in the same system:

```typescript
// Node type indicates execution model
type NodeType =
  // V1 types (data entities)
  | "task"
  | "knowledge"
  | "artifact"
  | "pattern"
  // V2 types (executable units)
  | "script"
  | "api"
  | "agent"
  | "tool"
  | "human"
  | "skill"
  | "program";

// Type-based dispatch
function sendToNode(nodeId: string, message: Message) {
  const node = graph.getNode(nodeId);

  if (isV1Type(node.type)) {
    // V1: standard actor message passing
    return graph.sendV1(node, message);
  } else {
    // V2: location-transparent execution
    return graph.sendV2(node, message);
  }
}
```

### Gradual Migration

**Phase 1: Add V2 Types**
- Implement execution types (ScriptNode, APINode, etc.)
- Add location metadata to node properties
- Keep V1 types unchanged

**Phase 2: Location Transparency**
- Implement routing layer
- Add remote dispatch support
- Enable transparent local/remote switching

**Phase 3: Unified Interface**
- Expose uniform send/receive for all nodes
- Optimize routing and dispatch
- Document patterns and best practices

**Phase 4: Optional V1 Deprecation**
- Migrate V1 nodes to V2 equivalents (if desired)
- Or keep both models for different use cases

## Implementation Sketch

### Extended Node Interface

```typescript
interface NodeV2 extends NodeV1 {
  executionType?: ExecutionType;
  location?: LocationMetadata;
  executablePath?: string;
  apiEndpoint?: string;
  agentDefinitionPath?: string;
}

type ExecutionType =
  | "script"
  | "api"
  | "agent"
  | "tool"
  | "human"
  | "data";  // V1-style data node

interface LocationMetadata {
  locationType: "local" | "remote" | "distributed";
  address?: string;
  protocol?: string;
  loadBalancing?: "round_robin" | "random" | "affinity";
}
```

### Factory Functions

```typescript
// Script node factory
function createScriptNode(spec: ScriptNodeSpec, graph: Graph): Address {
  const node = {
    ...spec,
    executionType: "script" as const,
    location: spec.location || { locationType: "local" }
  };

  // Create actor with execution handler
  const actor = ScriptActor(node, graph.system);

  // Register in graph with V2 metadata
  return graph.registerNode(spec.id, actor, node);
}

// Agent node factory
function createAgentNode(spec: AgentNodeSpec, graph: Graph): Address {
  // Load markdown definition
  const agentDef = loadAgentDefinition(spec.agentDefinitionPath);

  const node = {
    ...spec,
    executionType: "agent" as const,
    location: spec.location || { locationType: "local" },
    systemPrompt: agentDef.content,
    tools: agentDef.frontmatter.tools,
    model: agentDef.frontmatter.model
  };

  // Create actor with LLM execution handler
  const actor = AgentActor(node, graph.system);

  return graph.registerNode(spec.id, actor, node);
}

// Human node factory
function createHumanNode(spec: HumanNodeSpec, graph: Graph): Address {
  const node = {
    ...spec,
    executionType: "human" as const,
    location: { locationType: "local" },  // Humans are local to chat interface
    pendingMessages: []
  };

  // Create actor with human input handler
  const actor = HumanActor(node, graph.system);

  return graph.registerNode(spec.id, actor, node);
}
```

### Location-Aware Send

```typescript
class GraphV2 extends Graph {
  async send(nodeId: string, messageType: string, payload: any): Promise<any> {
    const node = this.nodes.get(nodeId);

    if (!node) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    // Route based on location
    const location = node.location || { locationType: "local" };

    switch (location.locationType) {
      case "local":
        return this.sendLocal(node, messageType, payload);

      case "remote":
        return this.sendRemote(node, messageType, payload, location);

      case "distributed":
        return this.sendDistributed(node, messageType, payload, location);

      default:
        throw new Error(`Unknown location type: ${location.locationType}`);
    }
  }

  private async sendLocal(node: any, messageType: string, payload: any) {
    // Direct actor message passing
    return await this.system.send(node.address, {
      type: messageType,
      payload
    });
  }

  private async sendRemote(
    node: any,
    messageType: string,
    payload: any,
    location: LocationMetadata
  ) {
    // HTTP/gRPC call to remote service
    const response = await fetch(`${location.address}/invoke`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        nodeId: node.id,
        type: messageType,
        payload
      })
    });

    return await response.json();
  }

  private async sendDistributed(
    node: any,
    messageType: string,
    payload: any,
    location: LocationMetadata
  ) {
    // Load balancer selects instance
    const instance = await this.loadBalancer.select(location.address);

    // Forward to selected instance
    return this.sendRemote(node, messageType, payload, {
      ...location,
      address: instance
    });
  }
}
```

## Examples

### Example 1: Distributed Research System

```typescript
// Local graph coordinator
const graph = new GraphV2();

// Remote API node (GitHub)
const githubAPI = createAPINode({
  id: "github_api",
  apiEndpoint: "https://api.github.com",
  location: {
    locationType: "remote",
    address: "https://api.github.com",
    protocol: "https"
  }
}, graph);

// Distributed research agents
const researchAgent = createAgentNode({
  id: "research_agent_pool",
  agentDefinitionPath: "/agents/research-agent.md",
  location: {
    locationType: "distributed",
    address: "agent-pool://research-agents",
    loadBalancing: "round_robin"
  }
}, graph);

// Local script for data processing
const dataProcessor = createScriptNode({
  id: "data_processor",
  executablePath: "/scripts/process_research.py",
  location: { locationType: "local" }
}, graph);

// Orchestrate research workflow
async function conductResearch(topic: string) {
  // Fetch repos from GitHub (remote)
  const repos = await graph.send("github_api", "invoke", {
    method: "search_repos",
    params: { q: topic, sort: "stars" }
  });

  // Analyze repos with distributed agent pool
  const analysis = await graph.send("research_agent_pool", "execute", {
    task: `Analyze these repositories: ${repos}`,
    deliverables: ["summary", "key_patterns"]
  });

  // Process results locally
  const report = await graph.send("data_processor", "execute", {
    input: analysis,
    format: "markdown"
  });

  return report;
}
```

**Key Points:**
- Three different node types, three different locations
- Uniform send/receive interface
- Location transparent to caller
- Easy to reconfigure (move agent local, move processor remote, etc.)

### Example 2: Human-in-the-Loop Approval Workflow

```typescript
// Create workflow nodes
const codeGenerator = createAgentNode({
  id: "code_generator",
  agentDefinitionPath: "/agents/code-generator.md"
}, graph);

const humanReviewer = createHumanNode({
  id: "human_reviewer_bob",
  humanId: "bob@example.com",
  communicationChannel: "chat"
}, graph);

const deploymentTool = createToolNode({
  id: "deployment_tool",
  toolType: "binary",
  executablePath: "/usr/local/bin/deploy"
}, graph);

// Workflow
async function deployWithReview(feature: string) {
  // Agent generates code
  const code = await graph.send("code_generator", "execute", {
    task: `Implement ${feature}`,
    deliverables: ["code", "tests"]
  });

  // Human reviews
  const approval = await graph.send("human_reviewer_bob", "request_input", {
    prompt: `Review this code for ${feature}`,
    context: { code },
    options: ["approve", "reject", "request_changes"]
  });

  if (approval.response === "approve") {
    // Deploy
    const result = await graph.send("deployment_tool", "invoke", {
      operation: "deploy",
      code,
      environment: "production"
    });

    return result;
  } else {
    return { status: "rejected", reason: approval.reason };
  }
}
```

**Key Points:**
- Human treated as node with send/receive interface
- Human input seamlessly integrated into workflow
- Agent, human, and tool all accessed uniformly

### Example 3: Executable Knowledge Base

```typescript
// Data knowledge
const conceptKnowledge = createKnowledge({
  id: "actor_model_concept",
  title: "Actor Model",
  content: "The actor model is..."
}, graph);

// Skill knowledge (executable pattern)
const httpClientSkill = createSkillNode({
  id: "http_client_pattern",
  title: "HTTP Client Pattern",
  executablePath: "/skills/http-client.js",
  pattern: "class HTTPClient { ... }"
}, graph);

// Program knowledge (executable transformation)
const jsonTransformer = createProgramNode({
  id: "json_to_csv_transformer",
  title: "JSON to CSV",
  executablePath: "/programs/json-to-csv.py"
}, graph);

// Using knowledge
async function generateAPIClient(apiSpec: any) {
  // Query concept (data)
  const concept = await graph.query("actor_model_concept", {
    type: "get_content"
  });

  // Apply skill (executable)
  const clientCode = await graph.send("http_client_pattern", "generate", {
    context: { apiSpec },
    outputLanguage: "typescript"
  });

  // Transform data (executable)
  const csvData = await graph.send("json_to_csv_transformer", "execute", {
    input: apiSpec
  });

  return { code: clientCode, data: csvData };
}
```

**Key Points:**
- Knowledge base includes data, skills, and programs
- Uniform interface for querying and executing
- Knowledge becomes active, not passive

## Conclusion

The V2 perspective treats nodes as **executable work units** with **location-transparent addressing**. This model:

**Strengths:**
- Uniform treatment of agents, tools, and humans
- Location transparency enables distributed systems
- Executable knowledge (skills/programs)
- Natural fit for distributed agent systems
- Flexible deployment (local to remote migration)

**Trade-offs:**
- Higher implementation complexity
- More sophisticated routing infrastructure
- Complex serialization requirements
- Less clear external/internal boundary

**Relationship to V1:**
- V1 and V2 can coexist (type-based dispatch)
- V1 is a special case of V2 (data nodes)
- Both share actor foundation (addresses, messaging)

**Best For:**
- Distributed multi-agent AI systems
- Location-transparent computing
- Systems where agents/tools/humans collaborate uniformly
- Computational workflows with mixed local/remote execution

The V2 model expands the actor-node concept to encompass **all executable entities**, creating a unified framework for distributed, heterogeneous work units.
