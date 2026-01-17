# Specification Extensions V2: Executable Work Units with Location Transparency

## Overview

This document provides formal specification extensions for the V2 actor-node model where nodes are executable work units accessed through location-transparent addressing.

**Key Principles:**
1. Nodes are executable work units (scripts, APIs, agents, tools, humans)
2. Location transparency via address-based routing
3. Uniform send/receive interface for all node types
4. Execution type determines dispatch strategy
5. Knowledge can be executable (skills, programs)

## GRAPH_SYSTEM_V2 Extensions

### Datalog Predicates (spec.datalog additions)

```datalog
% ============================================================================
% NODE EXECUTION TYPES (V2)
% ============================================================================

% Node execution type taxonomy
node_execution_type(NodeID, script) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, execution_type, script).

node_execution_type(NodeID, api_wrapper) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, execution_type, api_wrapper).

node_execution_type(NodeID, agent_program) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, execution_type, agent_program).

node_execution_type(NodeID, tool) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, execution_type, tool).

node_execution_type(NodeID, human) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, execution_type, human).

node_execution_type(NodeID, data) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, execution_type, data).

% Valid execution types
valid_execution_type(script).
valid_execution_type(api_wrapper).
valid_execution_type(agent_program).
valid_execution_type(tool).
valid_execution_type(human).
valid_execution_type(data).

% Query nodes by execution type
nodes_by_execution_type(GraphID, ExecutionType, Nodes) :-
    valid_execution_type(ExecutionType),
    findall(NodeID,
            (node_exists(GraphID, NodeID),
             node_execution_type(NodeID, ExecutionType)),
            Nodes).

% ============================================================================
% LOCATION TRANSPARENCY
% ============================================================================

% Node location type
node_location_type(NodeID, local) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, location_type, local).

node_location_type(NodeID, remote) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, location_type, remote).

node_location_type(NodeID, distributed) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, location_type, distributed).

% Default to local if not specified
node_location_type(NodeID, local) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    \+ property_value(Props, location_type, _).

% Valid location types
valid_location_type(local).
valid_location_type(remote).
valid_location_type(distributed).

% Node address (for remote/distributed)
node_remote_address(NodeID, Address) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, remote_address, Address),
    (node_location_type(NodeID, remote);
     node_location_type(NodeID, distributed)).

% Node protocol (for remote communication)
node_protocol(NodeID, Protocol) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, protocol, Protocol).

% Load balancing strategy (for distributed nodes)
node_load_balancing(NodeID, Strategy) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, load_balancing, Strategy),
    node_location_type(NodeID, distributed).

% Valid load balancing strategies
valid_load_balancing_strategy(round_robin).
valid_load_balancing_strategy(random).
valid_load_balancing_strategy(affinity).

% ============================================================================
% EXECUTION PROPERTIES
% ============================================================================

% Script node properties
node_executable_path(NodeID, Path) :-
    node_execution_type(NodeID, script),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, executable_path, Path).

node_interpreter(NodeID, Interpreter) :-
    node_execution_type(NodeID, script),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, interpreter, Interpreter).

% API node properties
node_api_endpoint(NodeID, Endpoint) :-
    node_execution_type(NodeID, api_wrapper),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, api_endpoint, Endpoint).

node_api_methods(NodeID, Methods) :-
    node_execution_type(NodeID, api_wrapper),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, api_methods, Methods).

% Agent node properties
node_agent_definition_path(NodeID, Path) :-
    node_execution_type(NodeID, agent_program),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, agent_definition_path, Path).

node_agent_model(NodeID, Model) :-
    node_execution_type(NodeID, agent_program),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, model, Model).

node_agent_tools(NodeID, Tools) :-
    node_execution_type(NodeID, agent_program),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, tools, Tools).

% Tool node properties
node_tool_type(NodeID, ToolType) :-
    node_execution_type(NodeID, tool),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, tool_type, ToolType).

% Valid tool types
valid_tool_type(binary).
valid_tool_type(library).
valid_tool_type(function).
valid_tool_type(system).

% Human node properties
node_human_id(NodeID, HumanID) :-
    node_execution_type(NodeID, human),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, human_id, HumanID).

node_communication_channel(NodeID, Channel) :-
    node_execution_type(NodeID, human),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, communication_channel, Channel).

% Valid communication channels
valid_communication_channel(cli).
valid_communication_channel(web).
valid_communication_channel(chat).
valid_communication_channel(email).

% ============================================================================
% EXECUTABLE KNOWLEDGE
% ============================================================================

% Knowledge execution types
knowledge_type(NodeID, data) :-
    node_has_type(GraphID, NodeID, knowledge),
    \+ node_execution_type(NodeID, skill),
    \+ node_execution_type(NodeID, program).

knowledge_type(NodeID, skill) :-
    node_has_type(GraphID, NodeID, knowledge),
    node_execution_type(NodeID, skill),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, pattern, _).

knowledge_type(NodeID, program) :-
    node_has_type(GraphID, NodeID, knowledge),
    node_execution_type(NodeID, program),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, executable_path, _).

% Skill properties
knowledge_pattern(NodeID, Pattern) :-
    knowledge_type(NodeID, skill),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, pattern, Pattern).

knowledge_applicable_contexts(NodeID, Contexts) :-
    knowledge_type(NodeID, skill),
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, applicable_contexts, Contexts).

% ============================================================================
% ROUTING AND DISPATCH
% ============================================================================

% Routing decision based on location and execution type
routing_strategy(NodeID, local_direct) :-
    node_location_type(NodeID, local),
    node_execution_type(NodeID, ExecutionType),
    ExecutionType \= data.

routing_strategy(NodeID, local_data_access) :-
    node_location_type(NodeID, local),
    node_execution_type(NodeID, data).

routing_strategy(NodeID, remote_http) :-
    node_location_type(NodeID, remote),
    node_protocol(NodeID, https).

routing_strategy(NodeID, remote_grpc) :-
    node_location_type(NodeID, remote),
    node_protocol(NodeID, grpc).

routing_strategy(NodeID, distributed_load_balanced) :-
    node_location_type(NodeID, distributed),
    node_load_balancing(NodeID, _).

% Dispatch requirements
requires_serialization(NodeID) :-
    node_location_type(NodeID, remote);
    node_location_type(NodeID, distributed).

requires_local_execution(NodeID) :-
    node_location_type(NodeID, local).

requires_load_balancer(NodeID) :-
    node_location_type(NodeID, distributed).

% ============================================================================
% V1/V2 COEXISTENCE
% ============================================================================

% V1 node types (data entities)
v1_node_type(task).
v1_node_type(knowledge).
v1_node_type(artifact).
v1_node_type(pattern).

% V2 execution types
v2_execution_type(script).
v2_execution_type(api_wrapper).
v2_execution_type(agent_program).
v2_execution_type(tool).
v2_execution_type(human).

% Node is V1-style
is_v1_node(NodeID) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, type, Type),
    v1_node_type(Type),
    \+ node_execution_type(NodeID, _).

% Node is V2-style
is_v2_node(NodeID) :-
    node_exists(GraphID, NodeID),
    node_execution_type(NodeID, ExecutionType),
    v2_execution_type(ExecutionType).

% Hybrid node (has both V1 type and V2 execution type)
is_hybrid_node(NodeID) :-
    node_exists(GraphID, NodeID),
    has_properties(GraphID, NodeID, Props),
    property_value(Props, type, Type),
    v1_node_type(Type),
    node_execution_type(NodeID, ExecutionType),
    v2_execution_type(ExecutionType).

% ============================================================================
% WELL-FORMEDNESS (V2)
% ============================================================================

% Script node is well-formed
script_node_well_formed(NodeID) :-
    node_execution_type(NodeID, script),
    node_executable_path(NodeID, _),
    node_location_type(NodeID, _).

% API node is well-formed
api_node_well_formed(NodeID) :-
    node_execution_type(NodeID, api_wrapper),
    node_api_endpoint(NodeID, _),
    node_location_type(NodeID, _).

% Agent node is well-formed
agent_node_well_formed(NodeID) :-
    node_execution_type(NodeID, agent_program),
    node_agent_definition_path(NodeID, _),
    node_agent_model(NodeID, _),
    node_location_type(NodeID, _).

% Tool node is well-formed
tool_node_well_formed(NodeID) :-
    node_execution_type(NodeID, tool),
    node_tool_type(NodeID, ToolType),
    valid_tool_type(ToolType),
    node_location_type(NodeID, _).

% Human node is well-formed
human_node_well_formed(NodeID) :-
    node_execution_type(NodeID, human),
    node_human_id(NodeID, _),
    node_communication_channel(NodeID, Channel),
    valid_communication_channel(Channel),
    node_location_type(NodeID, local).  % Humans must be local

% V2 node is well-formed
v2_node_well_formed(NodeID) :-
    is_v2_node(NodeID),
    (script_node_well_formed(NodeID);
     api_node_well_formed(NodeID);
     agent_node_well_formed(NodeID);
     tool_node_well_formed(NodeID);
     human_node_well_formed(NodeID)).

% All V2 nodes in graph are well-formed
all_v2_nodes_well_formed(GraphID) :-
    forall(
        (node_exists(GraphID, NodeID),
         is_v2_node(NodeID)),
        v2_node_well_formed(NodeID)
    ).

% ============================================================================
% LOCATION TRANSPARENCY INVARIANTS
% ============================================================================

% Remote nodes must have remote address
remote_nodes_have_address(GraphID) :-
    forall(
        (node_exists(GraphID, NodeID),
         node_location_type(NodeID, remote)),
        node_remote_address(NodeID, _)
    ).

% Distributed nodes must have load balancing strategy
distributed_nodes_have_load_balancing(GraphID) :-
    forall(
        (node_exists(GraphID, NodeID),
         node_location_type(NodeID, distributed)),
        node_load_balancing(NodeID, Strategy)
    ),
    forall(
        node_load_balancing(_, Strategy),
        valid_load_balancing_strategy(Strategy)
    ).

% Human nodes must be local
human_nodes_are_local(GraphID) :-
    forall(
        (node_exists(GraphID, NodeID),
         node_execution_type(NodeID, human)),
        node_location_type(NodeID, local)
    ).

% Location transparency invariants hold
location_transparency_invariants(GraphID) :-
    remote_nodes_have_address(GraphID),
    distributed_nodes_have_load_balancing(GraphID),
    human_nodes_are_local(GraphID).

% ============================================================================
% QUERIES AND VERIFICATION
% ============================================================================

% Find all executable nodes (non-data)
find_executable_nodes(GraphID, ExecutableNodes) :-
    findall(NodeID,
            (node_exists(GraphID, NodeID),
             node_execution_type(NodeID, Type),
             Type \= data),
            ExecutableNodes).

% Find all remote nodes
find_remote_nodes(GraphID, RemoteNodes) :-
    findall(NodeID,
            (node_exists(GraphID, NodeID),
             node_location_type(NodeID, remote)),
            RemoteNodes).

% Find all distributed nodes
find_distributed_nodes(GraphID, DistributedNodes) :-
    findall(NodeID,
            (node_exists(GraphID, NodeID),
             node_location_type(NodeID, distributed)),
            DistributedNodes).

% Find all agent nodes
find_agent_nodes(GraphID, AgentNodes) :-
    findall(NodeID,
            (node_exists(GraphID, NodeID),
             node_execution_type(NodeID, agent_program)),
            AgentNodes).

% Find all human nodes
find_human_nodes(GraphID, HumanNodes) :-
    findall(NodeID,
            (node_exists(GraphID, NodeID),
             node_execution_type(NodeID, human)),
            HumanNodes).

% Count nodes by execution type
count_by_execution_type(GraphID, ExecutionType, Count) :-
    nodes_by_execution_type(GraphID, ExecutionType, Nodes),
    length(Nodes, Count).

% Count nodes by location type
count_by_location_type(GraphID, LocationType, Count) :-
    findall(NodeID,
            (node_exists(GraphID, NodeID),
             node_location_type(NodeID, LocationType)),
            Nodes),
    length(Nodes, Count).

% Verify V2 graph is well-formed
verify_v2_graph(GraphID) :-
    graph_exists(GraphID),
    all_v2_nodes_well_formed(GraphID),
    location_transparency_invariants(GraphID).

% ============================================================================
% EXAMPLE FACTS FOR TESTING
% ============================================================================

% Example: Local script node
node_exists("test-graph-v2", "script_processor").
has_properties("test-graph-v2", "script_processor", [
    execution_type-script,
    executable_path-"/scripts/process_data.py",
    interpreter-python3,
    location_type-local
]).

% Example: Remote API node
node_exists("test-graph-v2", "github_api").
has_properties("test-graph-v2", "github_api", [
    execution_type-api_wrapper,
    api_endpoint-"https://api.github.com",
    protocol-https,
    location_type-remote,
    remote_address-"https://api.github.com",
    api_methods-["get_repo", "list_issues", "create_pr"]
]).

% Example: Agent node (local)
node_exists("test-graph-v2", "research_agent_1").
has_properties("test-graph-v2", "research_agent_1", [
    execution_type-agent_program,
    agent_definition_path-"/agents/research-agent.md",
    model-"claude-sonnet-4-5",
    tools-["web_search", "file_system"],
    location_type-local
]).

% Example: Distributed agent pool
node_exists("test-graph-v2", "agent_pool_research").
has_properties("test-graph-v2", "agent_pool_research", [
    execution_type-agent_program,
    agent_definition_path-"/agents/research-agent.md",
    model-"claude-sonnet-4-5",
    location_type-distributed,
    remote_address-"agent-pool://research-agents",
    load_balancing-round_robin
]).

% Example: Human node
node_exists("test-graph-v2", "human_alice").
has_properties("test-graph-v2", "human_alice", [
    execution_type-human,
    human_id-"alice@example.com",
    communication_channel-chat,
    location_type-local
]).

% Example: Tool node
node_exists("test-graph-v2", "jq_processor").
has_properties("test-graph-v2", "jq_processor", [
    execution_type-tool,
    tool_type-binary,
    executable_path-"/usr/bin/jq",
    location_type-local
]).

% ============================================================================
% VERIFICATION QUERIES (V2)
% ============================================================================

% Query: Verify all V2 nodes are well-formed
% ?- all_v2_nodes_well_formed("test-graph-v2").

% Query: Verify location transparency invariants
% ?- location_transparency_invariants("test-graph-v2").

% Query: Find all executable nodes
% ?- find_executable_nodes("test-graph-v2", Nodes).

% Query: Find all remote nodes
% ?- find_remote_nodes("test-graph-v2", RemoteNodes).

% Query: Find all agent nodes
% ?- find_agent_nodes("test-graph-v2", AgentNodes).

% Query: Count nodes by execution type
% ?- count_by_execution_type("test-graph-v2", agent_program, Count).

% Query: Verify complete V2 graph
% ?- verify_v2_graph("test-graph-v2").

% ============================================================================
% END DATALOG EXTENSIONS (V2)
% ============================================================================
```

### DSL Model Extensions (model.lisp additions)

```lisp
;;;; ============================================================================
;;;; GRAPH_SYSTEM_V2 Model Extensions - Executable Work Units
;;;; ============================================================================

;;; Execution Type Hierarchy
(define-type-hierarchy execution-types
  (execution-unit
    "Executable work unit with location-transparent addressing"
    (script-node
      "Executable script with interpreter"
      :properties (executable-path interpreter environment))
    (api-node
      "External API wrapper"
      :properties (api-endpoint protocol authentication methods))
    (agent-node
      "LLM-based agent from markdown definition"
      :properties (agent-definition-path model tools system-prompt))
    (tool-node
      "Callable program or utility"
      :properties (tool-type invocation-method input-schema output-schema))
    (human-node
      "Human participant with send/receive interface"
      :properties (human-id communication-channel response-timeout))
    (data-node
      "Traditional V1 data entity"
      :properties (content sources version))))

;;; Location Type Hierarchy
(define-type-hierarchy location-types
  (location
    "Execution location metadata"
    (local-location
      "Same process, in-memory execution"
      :dispatch direct-actor-send)
    (remote-location
      "Different machine, network call"
      :dispatch (http grpc websocket)
      :properties (address protocol))
    (distributed-location
      "Load-balanced pool of instances"
      :dispatch load-balanced-remote
      :properties (address load-balancing-strategy))))

;;; Location Transparency Contract
(define-contract location-transparency
  :parties (graph-system routing-layer)
  :description "Uniform send/receive regardless of node location"
  :requires
    ((location-metadata
       "Every node has location metadata"
       (forall node
         (implies (node-exists graph node)
                  (has-location-metadata node))))
     (routing-decision
       "Routing layer determines dispatch based on location"
       (forall node message
         (implies (send-to-node node message)
                  (exists strategy
                    (and (routing-strategy node strategy)
                         (dispatch-via strategy node message))))))
     (transparent-to-caller
       "Caller does not need to know location"
       (forall caller node message
         (implies (caller-sends caller node message)
                  (not (requires-knowledge caller
                                          (location-of node)))))))
  :ensures
    ((same-interface
       "Same send/receive interface for all locations"
       (forall node1 node2 message
         (implies (and (node-exists graph node1)
                      (node-exists graph node2))
                  (= (send-interface node1)
                     (send-interface node2)))))
     (location-refactoring-safe
       "Can move nodes between locations without changing callers"
       (forall node old-location new-location
         (implies (relocate-node node old-location new-location)
                  (forall caller
                    (not (requires-change caller))))))))

;;; Execution Type Contract
(define-contract execution-type-dispatch
  :parties (graph-system execution-runtime)
  :description "Dispatch based on execution type"
  :requires
    ((execution-type-declared
       "Every V2 node declares execution type"
       (forall node
         (implies (v2-node-p node)
                  (has-execution-type node))))
     (execution-handler-registered
       "Handler registered for each execution type"
       (forall execution-type
         (implies (valid-execution-type-p execution-type)
                  (exists handler
                    (handler-for execution-type handler))))))
  :protocol
    ((receive-message
       "Node receives message"
       :validates (message-well-formed-p message))
     (determine-execution-type
       "Runtime determines execution type"
       :result execution-type)
     (select-handler
       "Runtime selects appropriate handler"
       :ensures (handler-matches-type-p handler execution-type))
     (dispatch-to-handler
       "Message dispatched to handler"
       :result execution-result)
     (return-result
       "Result returned to caller"
       :format (success payload) | (error reason))))

;;; Script Node Contract
(define-contract script-node-execution
  :parties (script-node runtime)
  :description "Execute external script with arguments"
  :requires
    ((executable-exists
       "Script file exists and is executable"
       (forall script-node
         (implies (script-node-p script-node)
                  (and (file-exists (executable-path script-node))
                       (executable-p (executable-path script-node))))))
     (interpreter-available
       "Required interpreter is available"
       (forall script-node
         (implies (has-interpreter script-node interpreter)
                  (interpreter-available-p interpreter)))))
  :protocol
    ((receive-execute-message
       "Node receives execute message with args"
       :payload (args stdin environment))
     (resolve-interpreter
       "Determine interpreter (shebang or explicit)")
     (prepare-environment
       "Set environment variables and working directory")
     (execute-script
       "Run script with interpreter and args"
       :captures (stdout stderr exit-code))
     (return-result
       "Return execution result"
       :success (stdout exit-code)
       :error (stderr exit-code))))

;;; API Node Contract
(define-contract api-node-invocation
  :parties (api-node http-client)
  :description "Invoke external API endpoint"
  :requires
    ((api-endpoint-reachable
       "API endpoint is accessible"
       (forall api-node
         (implies (api-node-p api-node)
                  (reachable-p (api-endpoint api-node)))))
     (authentication-valid
       "API credentials are valid"
       (forall api-node
         (implies (requires-auth api-node)
                  (valid-credentials-p (auth-config api-node))))))
  :protocol
    ((receive-invoke-message
       "Node receives invoke message"
       :payload (method path params body))
     (prepare-request
       "Build HTTP request with auth headers"
       :transforms (message -> http-request))
     (send-http-request
       "Execute HTTP call"
       :network-call t)
     (handle-response
       "Process HTTP response"
       :success (status body headers)
       :error (status error-message))
     (return-result
       "Return API result to caller"
       :format (success data) | (error reason))))

;;; Agent Node Contract
(define-contract agent-node-execution
  :parties (agent-node llm-runtime)
  :description "Execute LLM-based agent with goal and tools"
  :requires
    ((agent-definition-loaded
       "Markdown definition parsed and loaded"
       (forall agent-node
         (implies (agent-node-p agent-node)
                  (and (file-exists (agent-definition-path agent-node))
                       (valid-markdown-p (load-file
                                          (agent-definition-path agent-node)))))))
     (model-available
       "LLM model is accessible"
       (forall agent-node
         (implies (agent-node-p agent-node)
                  (model-available-p (model agent-node)))))
     (tools-registered
       "All agent tools are available"
       (forall agent-node
         (forall tool (tools agent-node)
           (tool-registered-p tool)))))
  :protocol
    ((receive-execute-message
       "Agent receives task with goal and context"
       :payload (task deliverables context))
     (load-agent-definition
       "Parse markdown file, extract system prompt and config"
       :result (system-prompt model tools temperature))
     (initialize-llm
       "Create LLM instance with config"
       :creates llm-instance)
     (execute-agentic-loop
       "Run LLM in loop with tools until goal met"
       :loop (think -> act-with-tools -> observe -> repeat)
       :terminates-when success-criteria-met)
     (return-result
       "Return agent output"
       :format (success deliverables metadata) | (error reason))))

;;; Tool Node Contract
(define-contract tool-node-invocation
  :parties (tool-node tool-runtime)
  :description "Invoke callable tool/program"
  :requires
    ((tool-available
       "Tool is installed and accessible"
       (forall tool-node
         (implies (tool-node-p tool-node)
                  (tool-available-p (tool-type tool-node)))))
     (input-schema-valid
       "Input matches tool's expected schema"
       (forall tool-node input
         (implies (invoke-tool tool-node input)
                  (validates input (input-schema tool-node))))))
  :protocol
    ((receive-invoke-message
       "Tool receives invoke message"
       :payload (operation input))
     (validate-input
       "Check input against schema"
       :validates (matches input-schema))
     (invoke-tool
       "Call tool via appropriate method"
       :method (cli | library-call | function-call | system-call))
     (capture-output
       "Capture tool output"
       :validates (matches output-schema))
     (return-result
       "Return tool result"
       :format (success output) | (error reason))))

;;; Human Node Contract
(define-contract human-node-interaction
  :parties (human-node communication-interface)
  :description "Human participant with send/receive messaging"
  :requires
    ((communication-channel-available
       "Human's communication channel is active"
       (forall human-node
         (implies (human-node-p human-node)
                  (channel-active-p (communication-channel human-node)))))
     (human-reachable
       "Human can receive messages"
       (forall human-node
         (implies (human-node-p human-node)
                  (reachable-p (human-id human-node))))))
  :protocol
    ((receive-input-request
       "Human node receives request for input"
       :payload (prompt context options timeout))
     (route-to-interface
       "Forward to human via communication channel"
       :channel (cli | web | chat | email)
       :transforms (message -> human-readable-format))
     (wait-for-human-response
       "Block until human responds or timeout"
       :timeout (timeout-millis)
       :fallback timeout-handler)
     (receive-human-input
       "Human sends response via interface"
       :input (text | selection | structured-data))
     (return-result
       "Return human response to caller"
       :format (success response) | (timeout)))
  :invariants
    ((humans-are-local
       "Human nodes must have local location"
       (forall human-node
         (implies (human-node-p human-node)
                  (= (location-type human-node) local))))
     (async-interaction
       "Human interactions are asynchronous"
       (forall human-node
         (implies (send-to human-node message)
                  (async-p (response-from human-node)))))))

;;; Routing Contract
(define-contract routing-dispatch
  :parties (graph-system routing-layer)
  :description "Route messages based on location metadata"
  :requires
    ((routing-table-initialized
       "Routing layer has mapping of nodes to dispatch strategies"
       (initialized-p routing-table))
     (protocols-available
       "All required protocols are available"
       (forall protocol (used-protocols)
         (protocol-available-p protocol))))
  :dispatch-strategies
    ((local-direct
       :description "Direct actor send (in-memory)"
       :used-when (location-type = local)
       :implementation
         (lambda (node message)
           (actor-send (address node) message)))

     (remote-http
       :description "HTTP POST to remote service"
       :used-when (and (location-type = remote)
                       (protocol = https))
       :implementation
         (lambda (node message)
           (http-post (remote-address node)
                     (serialize-message message))))

     (remote-grpc
       :description "gRPC call to remote service"
       :used-when (and (location-type = remote)
                       (protocol = grpc))
       :implementation
         (lambda (node message)
           (grpc-call (remote-address node)
                      (serialize-message message))))

     (distributed-load-balanced
       :description "Load-balanced dispatch to pool"
       :used-when (location-type = distributed)
       :implementation
         (lambda (node message)
           (let ((instance (select-instance
                            (load-balancing-strategy node)
                            (remote-address node))))
             (http-post instance (serialize-message message)))))))

;;; Executable Knowledge Contract
(define-contract executable-knowledge
  :parties (knowledge-system execution-runtime)
  :description "Knowledge as data, skills, or programs"
  :types
    ((data-knowledge
       :description "Traditional knowledge content"
       :properties (title content sources version)
       :operations (query retrieve search))

     (skill-knowledge
       :description "Executable code pattern"
       :properties (pattern applicable-contexts executable-path)
       :operations (generate apply instantiate))

     (program-knowledge
       :description "Executable transformation"
       :properties (executable-path input-schema output-schema)
       :operations (execute transform)))
  :protocol
    ((query-knowledge
       :for data-knowledge
       :returns content)

     (generate-from-skill
       :for skill-knowledge
       :input (context parameters)
       :returns generated-code)

     (execute-program
       :for program-knowledge
       :input data
       :returns transformed-data)))

;;; V1/V2 Coexistence Contract
(define-contract v1-v2-coexistence
  :parties (graph-system-v1 graph-system-v2)
  :description "V1 and V2 nodes coexist in same graph"
  :requires
    ((type-distinguishable
       "Can distinguish V1 from V2 nodes"
       (forall node
         (xor (v1-node-p node)
              (v2-node-p node)
              (hybrid-node-p node))))
     (dispatch-routing
       "Router selects V1 or V2 dispatch based on type"
       (forall node
         (implies (send-message node message)
                  (if (v1-node-p node)
                      (dispatch-v1 node message)
                      (dispatch-v2 node message))))))
  :ensures
    ((backward-compatible
       "V1 nodes work as before"
       (forall v1-node
         (implies (v1-node-p v1-node)
                  (= (behavior v1-node)
                     (legacy-behavior v1-node)))))
     (forward-compatible
       "V2 nodes add new capabilities"
       (forall v2-node
         (implies (v2-node-p v2-node)
                  (and (supports-location-transparency v2-node)
                       (supports-execution-dispatch v2-node)))))))

;;; Property Schemas (V2)
(define-property-schema v2-execution-properties
  "Properties specific to V2 executable nodes"
  :execution-type-properties
    ((script-properties
       :fields (executable-path interpreter environment working-directory))
     (api-properties
       :fields (api-endpoint protocol authentication methods))
     (agent-properties
       :fields (agent-definition-path model tools system-prompt temperature))
     (tool-properties
       :fields (tool-type invocation-method input-schema output-schema))
     (human-properties
       :fields (human-id communication-channel response-timeout pending-messages)))
  :location-properties
    ((local-properties
       :fields (location-type)
       :values (location-type = local))
     (remote-properties
       :fields (location-type remote-address protocol)
       :values (location-type = remote))
     (distributed-properties
       :fields (location-type remote-address load-balancing)
       :values (location-type = distributed))))

;;; Verification Predicates (V2)
(defquery all-v2-nodes-well-formed (graph)
  "Verify all V2 nodes have required properties"
  :check
    (forall node (v2-nodes-of graph)
      (and (has-execution-type node)
           (has-location-metadata node)
           (execution-type-well-formed node))))

(defquery location-transparency-holds (graph)
  "Verify location transparency invariants"
  :check
    (and (remote-nodes-have-address graph)
         (distributed-nodes-have-load-balancing graph)
         (humans-are-local graph)))

(defquery routing-consistent (graph)
  "Verify routing strategies are consistent"
  :check
    (forall node (nodes-of graph)
      (implies (v2-node-p node)
               (exists strategy
                 (and (routing-strategy-for node strategy)
                      (strategy-matches-location node strategy))))))

(defquery executable-knowledge-valid (graph)
  "Verify executable knowledge nodes are well-formed"
  :check
    (forall node (knowledge-nodes-of graph)
      (implies (executable-knowledge-p node)
               (or (skill-well-formed-p node)
                   (program-well-formed-p node)))))

;;;; ============================================================================
;;;; END DSL EXTENSIONS (V2)
;;;; ============================================================================
```

## Implementation Examples

### Example 1: Mixed Local/Remote Execution

```typescript
// Create graph with mixed node types
const graph = new GraphV2();

// Local script
const localScript = createScriptNode({
  id: "local_data_processor",
  executionType: "script",
  executablePath: "/scripts/process.py",
  interpreter: "python3",
  location: { locationType: "local" }
}, graph);

// Remote API
const remoteAPI = createAPINode({
  id: "github_api",
  executionType: "api_wrapper",
  apiEndpoint: "https://api.github.com",
  protocol: "https",
  location: {
    locationType: "remote",
    address: "https://api.github.com",
    protocol: "https"
  }
}, graph);

// Local agent
const localAgent = createAgentNode({
  id: "research_agent",
  executionType: "agent_program",
  agentDefinitionPath: "/agents/research-agent.md",
  model: "claude-sonnet-4-5",
  location: { locationType: "local" }
}, graph);

// Use uniformly - location transparent to caller
const apiData = await graph.send("github_api", "invoke", {
  method: "list_repos",
  params: { org: "anthropic" }
});

const processed = await graph.send("local_data_processor", "execute", {
  input: apiData
});

const analysis = await graph.send("research_agent", "execute", {
  task: "Analyze these repositories",
  context: { data: processed }
});
```

**Datalog Verification:**
```datalog
?- node_location_type("github_api", remote).
true.

?- node_location_type("local_data_processor", local).
true.

?- routing_strategy("github_api", remote_http).
true.

?- routing_strategy("local_data_processor", local_direct).
true.
```

### Example 2: Distributed Agent Pool

```typescript
// Create distributed agent pool
const agentPool = createAgentNode({
  id: "research_agent_pool",
  executionType: "agent_program",
  agentDefinitionPath: "/agents/research-agent.md",
  model: "claude-sonnet-4-5",
  location: {
    locationType: "distributed",
    address: "agent-pool://research-agents",
    loadBalancing: "round_robin"
  }
}, graph);

// Multiple concurrent requests automatically load balanced
const [result1, result2, result3] = await Promise.all([
  graph.send("research_agent_pool", "execute", {
    task: "Research topic A"
  }),
  graph.send("research_agent_pool", "execute", {
    task: "Research topic B"
  }),
  graph.send("research_agent_pool", "execute", {
    task: "Research topic C"
  })
]);

// Load balancer distributes across pool instances
// Caller doesn't know or care about distribution
```

**Datalog Verification:**
```datalog
?- node_location_type("research_agent_pool", distributed).
true.

?- node_load_balancing("research_agent_pool", round_robin).
true.

?- routing_strategy("research_agent_pool", distributed_load_balanced).
true.
```

### Example 3: Human-in-the-Loop with Chatbot

```typescript
// Create human node
const humanReviewer = createHumanNode({
  id: "human_alice",
  executionType: "human",
  humanId: "alice@example.com",
  communicationChannel: "chat",
  location: { locationType: "local" }  // Humans are always local
}, graph);

// Agent generates code
const codeAgent = createAgentNode({
  id: "code_generator",
  executionType: "agent_program",
  agentDefinitionPath: "/agents/code-generator.md"
}, graph);

// Workflow
async function generateWithReview(feature: string) {
  // Agent generates
  const code = await graph.send("code_generator", "execute", {
    task: `Implement ${feature}`
  });

  // Human reviews (routes to chatbot)
  const review = await graph.send("human_alice", "request_input", {
    prompt: "Review this code",
    context: { code },
    options: ["approve", "reject", "modify"],
    timeout: 3600000  // 1 hour
  });

  return review;
}
```

**Datalog Verification:**
```datalog
?- node_execution_type("human_alice", human).
true.

?- node_location_type("human_alice", local).
true.

?- node_communication_channel("human_alice", chat).
true.

?- human_nodes_are_local("test-graph").
true.
```

## Migration from V1

### Minimal Changes Required

V1 nodes continue to work unchanged:

```typescript
// V1 node (unchanged)
const task = createTask({
  goal: "Implement feature",
  deliverables: ["code", "tests"]
}, graph);

// Graph now supports both V1 and V2
await graph.send(task.id, "start", {});  // V1-style
```

### Adding V2 Capabilities

Add execution metadata to V1 nodes:

```typescript
// Hybrid node (V1 type + V2 execution)
const hybridTask = createTask({
  goal: "Implement feature",
  deliverables: ["code", "tests"],
  // V2 extensions
  executionType: "agent_program",  // Task uses agent to execute
  agentDefinitionPath: "/agents/implementation-agent.md",
  location: { locationType: "distributed" }
}, graph);
```

### Full V2 Migration

Use pure V2 node types:

```typescript
// Pure V2 agent node
const agentNode = createAgentNode({
  id: "implementation_agent",
  executionType: "agent_program",
  agentDefinitionPath: "/agents/implementation-agent.md",
  model: "claude-sonnet-4-5",
  location: { locationType: "local" }
}, graph);
```

## Summary

These V2 specification extensions formalize the executable work units model:

**Key Extensions:**
1. **Execution type taxonomy**: script, api, agent, tool, human, data
2. **Location transparency**: local, remote, distributed with routing
3. **Execution contracts**: per-type execution protocols
4. **Executable knowledge**: skills and programs as knowledge
5. **V1/V2 coexistence**: type-based dispatch

**Principles Formalized:**
- Nodes are executable work units
- Location is transparent to callers
- Uniform send/receive interface
- Routing based on location and execution type
- Agents, tools, and humans are nodes

**Integration:**
- Extends GRAPH_SYSTEM with V2 predicates
- Adds execution and location contracts
- Defines routing and dispatch strategies
- Supports V1/V2 coexistence
- Ready for incremental adoption

These extensions enable distributed, location-transparent agent systems while maintaining backward compatibility with V1 data-centric nodes.
