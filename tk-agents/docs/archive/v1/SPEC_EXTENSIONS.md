# Specification Extensions: Actor-Node-Agent Model

## Overview

This document provides formal specification extensions to integrate the actor-node-agent conceptual model into the existing system specifications. These extensions clarify the relationships between actors, nodes, agents, humans, and external programs/tools/APIs.

**Key Principles:**
1. All nodes ARE actors (inheritance)
2. Agents and humans USE the system (external clients)
3. Tools/APIs are REFERENCED (strings), WRAPPED (actors), or EXTERNAL (no representation)
4. Actor configurations remain in code, not in graph

## GRAPH_SYSTEM Extensions

### Datalog Predicates (spec.datalog additions)

```datalog
% ============================================================================
% ACTOR-NODE RELATIONSHIP
% ============================================================================

% Core Relationship: All nodes are actors
node_is_actor(GraphID, NodeID) :-
    node_exists(GraphID, NodeID),
    has_address(GraphID, NodeID, AddressID),
    address_exists(AddressID).

% Invariant: Every node must have an actor address
all_nodes_have_actors(GraphID) :-
    forall(
        node_exists(GraphID, NodeID),
        exists(AddressID,
               (has_address(GraphID, NodeID, AddressID),
                address_exists(AddressID)))
    ).

% Actor exists but is not a node (bare actor)
bare_actor(SystemID, AddressID) :-
    address_exists(AddressID),
    actor_registered_in(SystemID, AddressID),
    \+ exists(GraphID, NodeID,
              (graph_exists(GraphID),
               system_exists(GraphID, SystemID),
               has_address(GraphID, NodeID, AddressID))).

% ============================================================================
% NODE TYPE TAXONOMY
% ============================================================================

% Node types with their semantic categories
node_type_category(NodeType, Category) :-
    node_type_mapping(NodeType, Category).

node_type_mapping(task, domain_entity).
node_type_mapping(knowledge, domain_entity).
node_type_mapping(artifact, domain_entity).
node_type_mapping(pattern, domain_entity).

% Node has specific type
node_has_type(GraphID, NodeID, NodeType) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, type, NodeType),
    valid_node_type(NodeType).

% Query nodes by type
nodes_of_type(GraphID, NodeType, Nodes) :-
    findall(NodeID,
            node_has_type(GraphID, NodeID, NodeType),
            Nodes).

% ============================================================================
% EXTERNAL ENTITIES (NOT REPRESENTED AS NODES/ACTORS)
% ============================================================================

% Agents are external clients that USE the system
% Note: Not represented in Datalog facts - exists outside system
external_agent(AgentID) :-
    agent_identifier(AgentID),
    \+ node_exists(_, AgentID),
    \+ actor_exists(AgentID).

% Humans are external users that USE the system
% Note: Not represented in Datalog facts - exists outside system
external_human(UserID) :-
    user_identifier(UserID),
    \+ node_exists(_, UserID),
    \+ actor_exists(UserID).

% Tools/Programs/APIs can be referenced in properties
tool_referenced_by_node(ToolName, NodeID) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_contains(Props, tools_available, ToolName).

% Tools/Programs can be wrapped as actors (but not nodes)
wrapped_tool_actor(ToolName, AddressID) :-
    tool_identifier(ToolName),
    address_exists(AddressID),
    actor_wraps_tool(AddressID, ToolName),
    bare_actor(_, AddressID).

% Invariant: Wrapped tools should not be nodes
no_tool_nodes(SystemID) :-
    forall(
        (wrapped_tool_actor(_, AddressID),
         actor_registered_in(SystemID, AddressID)),
        bare_actor(SystemID, AddressID)
    ).

% ============================================================================
% ACTOR CONFIGURATION (NOT IN GRAPH)
% ============================================================================

% Actor configuration is defined by factory, not stored in graph
actor_defined_by_factory(AddressID, FactoryID) :-
    actor_factory(FactoryID),
    address_exists(AddressID),
    factory_created(FactoryID, AddressID).

% Behavior is in code, not data
behavior_in_code(FactoryID) :-
    actor_factory(FactoryID).

% Graph stores only domain properties, not actor config
graph_stores_domain_properties(GraphID, NodeID) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    forall(
        property_field(Props, Field),
        domain_property(Field)
    ).

% Domain properties vs infrastructure properties
domain_property(id).
domain_property(type).
domain_property(created_at).
domain_property(goal).
domain_property(state).
domain_property(title).
domain_property(content).
domain_property(sources).
domain_property(deliverables).
domain_property(criteria).
% ... all other business domain fields

% Infrastructure properties (should not be in graph)
% Note: These predicates define what should NOT exist
infrastructure_property(message_handler).
infrastructure_property(actor_config).
infrastructure_property(behavior_logic).
infrastructure_property(state_machine).

% Invariant: Graph does not store infrastructure properties
no_infrastructure_in_graph(GraphID) :-
    forall(
        (node_exists(GraphID, NodeID),
         has_properties(GraphID, NodeID, Props)),
        forall(
            property_field(Props, Field),
            \+ infrastructure_property(Field)
        )
    ).

% ============================================================================
% LAYER SEPARATION
% ============================================================================

% Foundation layer: Actors
foundation_layer_entity(AddressID) :-
    address_exists(AddressID).

% Coordination layer: Nodes (actors with graph registration)
coordination_layer_entity(NodeID) :-
    node_exists(_, NodeID).

% Domain layer: Specific node types (task, knowledge)
domain_layer_entity(NodeID) :-
    node_has_type(_, NodeID, Type),
    node_type_category(Type, domain_entity).

% Interface layer: External clients (agents, humans)
interface_layer_entity(ClientID) :-
    external_agent(ClientID);
    external_human(ClientID).

% Layer separation invariant
proper_layer_separation :-
    % Nodes are actors
    all_nodes_have_actors(_),
    % No infrastructure in graph
    no_infrastructure_in_graph(_),
    % Wrapped tools are bare actors, not nodes
    no_tool_nodes(_).

% ============================================================================
% USAGE RELATIONSHIPS
% ============================================================================

% Agents/Humans USE the system (via CLI/API)
% Note: Represented by CLI command invocations, not graph relationships

% Agent/Human creates a node
entity_creates_node(ClientID, NodeID) :-
    (external_agent(ClientID); external_human(ClientID)),
    node_exists(_, NodeID),
    creation_attributed_to(NodeID, ClientID).

% Agent/Human sends message to node
entity_sends_to_node(ClientID, NodeID, MessageType) :-
    (external_agent(ClientID); external_human(ClientID)),
    node_exists(_, NodeID),
    message_sent_by(ClientID, NodeID, MessageType).

% Task references tool
task_references_tool(TaskID, ToolName) :-
    node_has_type(_, TaskID, task),
    tool_referenced_by_node(ToolName, TaskID).

% ============================================================================
% METADATA QUERIES
% ============================================================================

% Find all bare actors (not in graph)
find_bare_actors(SystemID, BareActors) :-
    findall(AddressID,
            bare_actor(SystemID, AddressID),
            BareActors).

% Find all wrapped tool actors
find_wrapped_tools(WrappedTools) :-
    findall((ToolName, AddressID),
            wrapped_tool_actor(ToolName, AddressID),
            WrappedTools).

% Count nodes by type
count_nodes_by_type(GraphID, NodeType, Count) :-
    nodes_of_type(GraphID, NodeType, Nodes),
    length(Nodes, Count).

% Verify layer separation
verify_layer_separation(GraphID) :-
    all_nodes_have_actors(GraphID),
    no_infrastructure_in_graph(GraphID),
    graph_exists(GraphID),
    system_exists(GraphID, SystemID),
    no_tool_nodes(SystemID).

% ============================================================================
% INTEGRATION WITH EXISTING SPECS
% ============================================================================

% Extension to node_exists: ensure actor backing
node_well_formed(GraphID, NodeID) :-
    node_exists(GraphID, NodeID),
    has_string_id(GraphID, NodeID, _),
    has_address(GraphID, NodeID, AddressID),
    address_exists(AddressID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, type, Type),
    valid_node_type(Type).

% Extension to graph_well_formed: include layer separation
graph_fully_well_formed(GraphID) :-
    graph_well_formed(GraphID),
    verify_layer_separation(GraphID).

% ============================================================================
% EXAMPLE FACTS FOR TESTING
% ============================================================================

% Example: Task node (node + actor)
node_exists("test-graph", "task_1").
has_address("test-graph", "task_1", "addr_task_1").
address_exists("addr_task_1").
has_properties("test-graph", "task_1", [type-task, goal-"Implement feature"]).

% Example: Bare actor (actor, not node)
address_exists("addr_git_wrapper").
actor_wraps_tool("addr_git_wrapper", "git").
actor_registered_in("test-system", "addr_git_wrapper").

% Example: External agent (not represented)
agent_identifier("research_agent_1").

% Example: Tool reference (string in properties)
tool_identifier("kubectl").
tool_identifier("git").
tool_identifier("docker").

% ============================================================================
% VERIFICATION QUERIES
% ============================================================================

% Query: Verify all nodes are actors
% ?- all_nodes_have_actors("test-graph").

% Query: Verify no infrastructure properties in graph
% ?- no_infrastructure_in_graph("test-graph").

% Query: Verify wrapped tools are not nodes
% ?- no_tool_nodes("test-system").

% Query: Find all bare actors
% ?- find_bare_actors("test-system", BareActors).

% Query: Verify complete layer separation
% ?- verify_layer_separation("test-graph").

% Query: Check graph is fully well-formed
% ?- graph_fully_well_formed("test-graph").

% ============================================================================
% END DATALOG EXTENSIONS
% ============================================================================
```

### DSL Model Extensions (model.lisp additions)

```lisp
;;;; ============================================================================
;;;; GRAPH_SYSTEM Model Extensions - Actor-Node Relationships
;;;; ============================================================================

;;; Actor-Node Type Hierarchy
(define-type-hierarchy entity-types
  (entity
    (actor
      "Foundation layer: message-passing entity with Address"
      (bare-actor "Actor not registered in graph")
      (node-actor "Actor registered in graph with string ID"))
    (external-client
      "Interface layer: entities that USE the system"
      (agent "AI agent using system for task coordination")
      (human "Human user operating system via CLI/API"))
    (tool-reference
      "External programs/APIs referenced by nodes"
      (string-reference "Tool name as string in properties")
      (wrapped-actor "Tool wrapped as actor (bare-actor)")
      (external-tool "Tool used outside system"))))

;;; Node-Actor Integration Contract
(define-contract node-actor-relationship
  :parties (graph-system actor-system)
  :description "All nodes are actors with bidirectional identity mapping"
  :requires
    ((node-has-address
       "Every node has corresponding actor address"
       (forall node
         (implies (node-exists graph node)
                  (exists address
                    (and (actor-exists system address)
                         (maps-to graph node address))))))
     (node-inherits-actor-capabilities
       "Nodes have all actor capabilities"
       (forall node
         (implies (node-exists graph node)
                  (can-receive-messages node)))))
  :ensures
    ((bidirectional-lookup
       "Can resolve node ID to address and vice versa"
       (forall node address
         (implies (maps-to graph node address)
                  (and (= (graph.get-node node) address)
                       (= (graph.resolve-address address) node)))))))

;;; Layer Separation Contract
(define-contract layer-separation
  :parties (graph-system actor-system)
  :description "Clear separation between foundation and coordination layers"
  :requires
    ((behavior-in-code
       "Actor behavior defined by factories, not stored in graph"
       (forall actor
         (implies (actor-exists system actor)
                  (and (defined-by-factory actor factory)
                       (not (behavior-stored-in-graph actor))))))
     (properties-are-domain-data
       "Graph stores only domain properties, not infrastructure"
       (forall node
         (implies (node-exists graph node)
                  (forall property (properties-of node)
                    (domain-property-p property)))))
     (wrapped-tools-not-nodes
       "Tool actors are bare actors, not graph nodes"
       (forall tool-actor
         (implies (wrapped-tool-actor-p tool-actor)
                  (bare-actor-p tool-actor)))))
  :ensures
    ((clean-serialization
       "Graph dumps contain only domain data"
       (forall dump
         (implies (= dump (graph.dump graph))
                  (forall item (items-in dump)
                    (serializable-p item)))))
     (type-safety
       "Clear distinction between data and behavior"
       (forall node
         (implies (node-exists graph node)
                  (and (data-entity-p (node-properties node))
                       (behavior-entity-p (node-actor node))))))))

;;; External Client Usage Contract
(define-contract external-client-usage
  :parties (agent human cli-api graph-system)
  :description "Agents and humans USE system, are not part of it"
  :protocol
    ((client-authenticates
       "Client identifies itself to CLI/API"
       :external t)
     (client-sends-command
       "Client issues command via CLI/API"
       :validates (command-well-formed-p command)
       :transforms (command -> graph-operation))
     (graph-executes-operation
       "Graph performs operation on behalf of client"
       :result (operation-result))
     (client-receives-response
       "Result returned to client"
       :format (human-readable or structured-data)))
  :invariants
    ((clients-not-represented
       "Clients are not actors or nodes in the system"
       (forall client
         (and (not (actor-exists system client))
              (not (node-exists graph client)))))
     (attribution-in-properties
       "Client identity may be stored in node properties, not as nodes"
       (forall node client
         (implies (created-by node client)
                  (exists property
                    (and (property-of node property)
                         (= (property-name property) "created-by")
                         (= (property-value property) client)))))))))

;;; Tool Integration Contract
(define-contract tool-integration
  :parties (task-system knowledge-system tool)
  :description "Three patterns for tool integration"
  :patterns
    ((string-reference
       :description "Tool referenced by name, not invoked"
       :representation
         (property tools-available (list "git" "kubectl" "docker"))
       :used-when "Tool is context for task, not actively used by system"
       :example "Task lists available tools for human/agent")

     (wrapped-actor
       :description "Tool wrapped as actor for message-based invocation"
       :representation
         (define-actor git-actor
           (handles ((commit message)
                    (checkout branch)
                    (status nil))))
       :used-when "System needs to send messages to tool"
       :example "Task sends message to GitActor to create commit"
       :constraint "Wrapped tool is bare actor, NOT a node")

     (external-usage
       :description "Tool used by agent/human outside system"
       :representation nil
       :used-when "Tool operated by external client, not by system"
       :example "Human runs npm install via shell"
       :constraint "No representation in system at all")))

;;; Property Classification
(define-property-schema domain-properties
  "Properties that belong in graph (domain data)"
  :includes
    ((identity-properties
       :fields (id type created-at)
       :immutable t)
     (task-properties
       :fields (goal state started-at completed-at result
               deliverables objective-success-criteria
               subjective-success-criteria priority labels
               parent-task-id tools-available known-information
               information-gaps)
       :mutable t)
     (knowledge-properties
       :fields (title content sources version)
       :mutable t)))

(define-property-schema infrastructure-properties
  "Properties that should NOT be in graph (actor config)"
  :excludes
    ((actor-configuration
       :fields (message-handler behavior-logic state-machine
               internal-state actor-config)
       :reason "Behavior defined by factory, not stored as data")
     (runtime-internals
       :fields (address symbol actor-reference send-function)
       :reason "Runtime-only, cannot serialize")))

;;; Node Type Definitions (Extended)
(deftype node-type ()
  '(member :task :knowledge :artifact :pattern)
  :documentation "Valid node types - all are domain entities")

(deftype entity-category ()
  '(member :actor :node :agent :human :tool)
  :documentation "Categories of entities in/around the system")

;;; Verification Predicates
(defquery all-nodes-are-actors (graph)
  "Verify every node has a backing actor"
  :check
    (forall node (nodes-of graph)
      (exists address
        (and (actor-p address)
             (= (node-address node) address)))))

(defquery no-infrastructure-in-properties (graph)
  "Verify no infrastructure properties in graph"
  :check
    (forall node (nodes-of graph)
      (forall property (properties-of node)
        (domain-property-p property))))

(defquery wrapped-tools-are-bare-actors (system)
  "Verify wrapped tools are not nodes"
  :check
    (forall actor (actors-of system)
      (implies (wrapped-tool-p actor)
               (and (actor-registered-p actor system)
                    (not (exists graph node
                           (= (node-address node) actor)))))))

(defquery verify-layer-separation (graph)
  "Verify all layer separation invariants"
  :check
    (and (all-nodes-are-actors graph)
         (no-infrastructure-in-properties graph)
         (wrapped-tools-are-bare-actors (graph-system graph))))

;;;; ============================================================================
;;;; END DSL EXTENSIONS
;;;; ============================================================================
```

## COMBINED_SYSTEM Extensions

### Integration Point Clarifications

Add to COMBINED_SYSTEM.spec.md:

```markdown
## Actor-Node-Agent Model Integration

### Layer Responsibilities

**Foundation Layer (Actor System):**
- Runtime behavior and message handling
- Actor registration and address generation
- Message routing by address
- NO knowledge of string IDs or edges

**Coordination Layer (Graph System):**
- String ID <-> Address bidirectional mapping
- Edge management and relationship tracking
- Property caching for fast access
- Serialization/deserialization
- NO knowledge of actor behavior or message handlers

**Domain Layer (Task/Knowledge Systems):**
- Specific node types with domain logic
- Factory functions creating actors and registering as nodes
- Domain-specific message handlers
- Business rules and validation

**Interface Layer (CLI/API):**
- External client interaction
- Command parsing and validation
- Display formatting
- File persistence
- NO direct actor manipulation (all via Graph)

### Entity Classification

**Internal Entities (Part of System):**
- **Actors**: Foundation entities with addresses, message handlers
  - All actors registered in Actor System
  - Some actors also registered as nodes in Graph
- **Nodes**: Actors with string IDs, edges, cached properties
  - All nodes are actors (inheritance)
  - Not all actors are nodes (e.g., wrapped tools)

**External Entities (Use System):**
- **Agents (AI)**: External clients using system for task coordination
  - Create/manipulate nodes via Graph/CLI
  - Send messages via graph.send()
  - NOT represented as nodes or actors
- **Humans**: External users operating system
  - Use CLI/API to interact with system
  - NOT represented as nodes or actors
- **Tools/Programs/APIs**: External systems
  - Referenced as strings in properties
  - Optionally wrapped as bare actors
  - Used by agents/humans externally

### Configuration vs. Data

**Stored in Graph (Domain Data):**
- Node identity: id, type, createdAt
- Task data: goal, state, deliverables, criteria
- Knowledge data: title, content, sources, version
- Relationships: edges with types and properties
- Metadata: labels, priority, timestamps

**Stored in Code (Actor Behavior):**
- Message handler implementations
- State machine logic
- Factory functions
- Business rule validation
- Computation logic

**Not Stored Anywhere (Runtime Only):**
- Addresses (Symbol-based identity)
- Active message handlers
- In-flight messages
- Private actor state (until serialized)

### Invariants

1. **All-Nodes-Are-Actors**: `forall node: exists actor (node.address == actor)`
2. **No-Infrastructure-In-Graph**: `forall property in graph: domain-property(property)`
3. **Wrapped-Tools-Not-Nodes**: `forall tool-actor: not exists node (node.address == tool-actor)`
4. **Clients-External**: `forall agent: not (node-exists(agent) or actor-exists(agent))`
5. **Behavior-In-Code**: `forall node: behavior(node) defined by factory(node.type)`
```

## Migration Guide

### For Existing Code

No breaking changes required. Current implementation already follows this model.

**Verification Checklist:**
- [x] All nodes have actor addresses
- [x] Graph stores only domain properties
- [x] No actor config in graph
- [x] Factories define behavior
- [x] Agents/humans are external

### For New Node Types

When adding new node types:

1. **Create Factory**: Define actor behavior in code
   ```typescript
   const MyNodeActor: ActorFactory<MyNodeData> = (data) => {
     // Message handling logic
     const handleMessage = async (msg: Message) => { ... };

     // Register with system and graph
     const address = system.register({ send: handleMessage });
     graph.registerNode(id, address, properties);

     return address;
   };
   ```

2. **Define Domain Properties**: Only data, no behavior
   ```typescript
   interface MyNodeProperties extends NodeProperties {
     type: "mynode";
     // Domain fields only
     title: string;
     data: SomeData;
     // NO: messageHandler, behavior, config
   }
   ```

3. **Add Type to Valid List**:
   ```typescript
   // In types.ts
   export type NodeType = "task" | "knowledge" | "artifact" | "pattern" | "mynode";
   ```

4. **Update Datalog/Lisp**: Add to valid node types
   ```datalog
   valid_node_type(mynode).
   ```

### For Tool Integration

**Pattern Selection:**

1. **String Reference** (default):
   ```typescript
   const task = createTask({
     toolsAvailable: ["mytool"]  // Just a string
   });
   ```

2. **Wrapped Actor** (if system needs to invoke):
   ```typescript
   const toolActor = MyToolActor({ config, system });
   // DON'T register in graph
   // DO use for messaging
   await system.send(toolActor, { type: "invoke", payload: {...} });
   ```

3. **External** (if only agent/human uses):
   ```typescript
   // No representation needed
   // Agent/human invokes directly
   ```

## Examples

### Example 1: Creating a Task Node (Node + Actor)

```typescript
import { Graph } from "./graph";
import { TaskActor } from "./task";

const graph = new Graph();
const system = graph.getSystem();

// Factory creates actor AND registers as node
const taskAddress = TaskActor({
  goal: "Implement authentication",
  desiredDeliverables: ["Auth module", "Tests"],
  objectiveSuccessCriteria: [
    { criterion: "All tests pass", measure: "boolean", threshold: true }
  ],
  graph  // Factory uses graph to register
});

// Result: task is BOTH an actor (in system) AND a node (in graph)
```

**Datalog Verification:**
```datalog
?- node_is_actor("main-graph", "task_1").
true.

?- node_has_type("main-graph", "task_1", task).
true.
```

### Example 2: Wrapping External Tool as Actor (Actor, NOT Node)

```typescript
import { System } from "./system";

// Create bare actor system (not using graph)
const system = System();

// Wrap git CLI as actor
const GitActor: ActorFactory<GitData> = (data) => {
  const actor = {
    send: async (message: Message) => {
      switch (message.type) {
        case "commit":
          // Execute git commit
          const result = execSync(`git commit -m "${message.payload.message}"`);
          return { success: true, data: result };
        case "status":
          const status = execSync("git status");
          return { success: true, data: status };
        default:
          return { success: false, error: "Unknown command" };
      }
    }
  };

  // Register in system ONLY (not graph)
  return system.register(actor);
};

const gitActor = GitActor({ system });

// Use via messaging
await gitActor.send({ type: "commit", payload: { message: "Update" } });

// Result: gitActor is an actor (has Address) but NOT a node (no string ID, no edges)
```

**Datalog Verification:**
```datalog
?- bare_actor("main-system", "addr_git").
true.

?- node_exists(_, "addr_git").
false.  % Not a node!
```

### Example 3: Agent Using System (External, NOT Actor/Node)

```typescript
// Agent code (external to system)
class ResearchAgent {
  constructor(private graph: Graph) {}

  async conductResearch(topic: string) {
    // Create knowledge node to store findings
    const knowledgeId = await this.createKnowledge(topic);

    // Create task to analyze findings
    const taskId = await this.createTask(
      `Analyze ${topic}`,
      [knowledgeId]  // depends on knowledge
    );

    // Work on task...
    await this.graph.send(taskId, "start", {});

    // Agent uses system but is not represented IN system
  }

  private async createKnowledge(topic: string) {
    const knowledge = createKnowledge({ title: topic, content: "" }, this.graph);
    return knowledge.properties.id;
  }

  private async createTask(goal: string, requiredKnowledge: string[]) {
    const task = TaskActor({ goal, graph: this.graph });
    const taskId = task.properties.id;

    // Link to knowledge
    for (const knId of requiredKnowledge) {
      await this.graph.send(taskId, "link", {
        toId: knId,
        edgeType: "requires_knowledge"
      });
    }

    return taskId;
  }
}

// Usage
const agent = new ResearchAgent(graph);
await agent.conductResearch("Actor Model");

// Result: agent creates nodes, but is NOT itself a node or actor
```

**Datalog Verification:**
```datalog
?- external_agent("research_agent_1").
true.

?- node_exists(_, "research_agent_1").
false.  % Agent not represented as node

?- actor_exists("research_agent_1").
false.  % Agent not represented as actor
```

### Example 4: Verifying Layer Separation

```typescript
import { verifyLayerSeparation } from "./verification";

// Run verification
const results = verifyLayerSeparation(graph);

// Results:
// {
//   allNodesAreActors: true,
//   noInfrastructureInGraph: true,
//   wrappedToolsAreBareActors: true,
//   layerSeparationValid: true
// }
```

**Datalog Verification:**
```datalog
?- verify_layer_separation("main-graph").
true.

?- all_nodes_have_actors("main-graph").
true.

?- no_infrastructure_in_graph("main-graph").
true.

?- no_tool_nodes("main-system").
true.
```

## Appendix: Rejected Alternatives

### Alternative 1: Actor Configs as Nodes

**Approach:** Create "actor_config" nodes storing message handlers and behavior.

**Why Rejected:**
- Requires serializing functions (security risk, complexity)
- Violates layer separation
- No clear use case for dynamic reconfiguration
- Type confusion (data vs. behavior)
- Makes graph queries complex (need to filter config nodes)

**Conclusion:** Keep behavior in code, data in graph.

### Alternative 2: Agents as Actors

**Approach:** Represent AI agents as special actor types in the system.

**Why Rejected:**
- Agents have complex internal state (RAG, planning) not modeled by system
- Agent behavior is external to task/knowledge domain
- No benefit to message-based agent invocation
- Creates unclear boundary between system and clients

**Conclusion:** Agents are external clients that USE the system.

### Alternative 3: Humans as Nodes

**Approach:** Create "human" node type to track users in graph.

**Why Rejected:**
- Human identity managed by OS/auth system, not our concern
- No actor behavior to model
- Human interactions are CLI/API calls, not actor messages
- Would require separate user management system

**Conclusion:** Humans are external users; identity tracked in properties if needed (e.g., `createdBy: "user123"`).

### Alternative 4: All Tools as Actors

**Approach:** Require wrapping every tool reference as an actor.

**Why Rejected:**
- Overhead for tools that are just context (not invoked by system)
- String references sufficient for most cases
- Wrapper pattern available when needed
- No benefit to uniform representation

**Conclusion:** Use three patterns based on integration needs (string, wrapped, external).

## Summary

These specification extensions formalize the actor-node-agent conceptual model:

**Key Extensions:**
1. **Datalog predicates** for actor-node relationships, layer separation, external entities
2. **DSL contracts** for node-actor integration, layer separation, client usage
3. **Property schemas** distinguishing domain vs. infrastructure properties
4. **Verification queries** ensuring invariants hold

**Principles Formalized:**
- All nodes ARE actors (inheritance)
- Actors are foundation, nodes are coordination
- Agents and humans USE system (external)
- Tools: referenced, wrapped, or external
- Behavior in code, data in graph

**Integration:**
- Extends GRAPH_SYSTEM.spec.datalog with relationship predicates
- Extends GRAPH_SYSTEM.model.lisp with contracts and schemas
- Adds clarifications to COMBINED_SYSTEM.spec.md
- Ready for adoption with no breaking changes

These extensions provide formal verification capabilities while maintaining the clean architecture of the existing system.
