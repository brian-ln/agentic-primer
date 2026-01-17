;;;; ============================================================================
;;;; COMBINED_SYSTEM - Formal DSL Model
;;;; ============================================================================
;;;; Integration specification for the complete Task/Knowledge Management System
;;;; Defines how Actor, Graph, Task, Knowledge, and CLI systems work together.
;;;; Following System Modeling Protocol.

(defpackage :combined-system-model
  (:use :cl :system-modeling-protocol)
  (:export #:combined-system
           #:integration-contract
           #:cross-system-invariant))

;;;; ============================================================================
;;;; 1. SYSTEM COMPOSITION
;;;; ============================================================================

;;; The Combined System is composed of 5 integrated subsystems
(defsystem 'combined-system
  :intent "Cohesive task and knowledge management platform"
  :category :integration
  :tags '(task-management knowledge-management cli actor-system graph)

  :subsystems '(
    (actor-system
      :role :foundation
      :provides (address-messaging actor-registration message-routing)
      :layer :infrastructure)

    (graph-system
      :role :coordination
      :provides (id-mapping edge-management property-caching serialization)
      :depends-on (actor-system)
      :layer :infrastructure)

    (task-system
      :role :domain
      :provides (task-state-machine hierarchical-tasks success-criteria)
      :depends-on (graph-system actor-system)
      :layer :domain)

    (knowledge-system
      :role :domain
      :provides (knowledge-storage versioning query synthesis)
      :depends-on (graph-system actor-system)
      :layer :domain)

    (task-cli
      :role :interface
      :provides (command-parsing display-formatting persistence)
      :depends-on (graph-system task-system)
      :layer :interface)))

;;;; ============================================================================
;;;; 2. LAYER ARCHITECTURE
;;;; ============================================================================

(deflayer 'interface-layer
  :description "Human interaction layer"
  :components '(task-cli)
  :responsibilities '(
    (command-parsing "Parse CLI arguments into operations")
    (display-formatting "Format output for human consumption")
    (file-persistence "Load/save graph state to tasks.json")
    (user-confirmation "Safety prompts for destructive operations")))

(deflayer 'coordination-layer
  :description "Bridge between external IDs and internal addresses"
  :components '(graph-system)
  :responsibilities '(
    (id-address-mapping "Bidirectional string ID <-> Address mapping")
    (edge-management "Create and query typed relationships")
    (property-caching "Fast access to node properties without messaging")
    (serialization "Dump/restore support for persistence")))

(deflayer 'domain-layer
  :description "Business logic for tasks and knowledge"
  :components '(task-system knowledge-system)
  :responsibilities '(
    (task-lifecycle "State machine for task execution")
    (knowledge-accumulation "Append-only content with versioning")
    (success-evaluation "Objective and subjective criteria")
    (hierarchy-management "Parent-child task relationships")))

(deflayer 'foundation-layer
  :description "Message-passing infrastructure"
  :components '(actor-system)
  :responsibilities '(
    (actor-registration "Register actors, generate addresses")
    (message-routing "Route messages to actors by address")
    (address-identity "Unique, unforgeable actor references")))

;;;; ============================================================================
;;;; 3. INTEGRATION CONTRACTS
;;;; ============================================================================

;;; Contract: Graph owns Actor System
(defcontract 'graph-owns-actor-system
  :parties (graph-system actor-system)
  :description "Graph creates and manages the underlying actor system"
  :invariants '(
    (system-created-at-construction
      :condition "Graph constructor creates System instance"
      :formal (implies (graph-exists g) (system-exists (get-system g))))

    (system-accessible-via-getter
      :condition "System reference exposed via getSystem()"
      :formal (forall g (= (get-system g) (owned-system g))))

    (all-actors-registered-through-system
      :condition "All actor registration goes through owned system"
      :formal (forall addr (implies (registered-in g addr)
                                   (registered-in (get-system g) addr))))))

;;; Contract: Factories perform dual registration
(defcontract 'dual-registration
  :parties (task-actor knowledge-actor graph-system actor-system)
  :description "Domain actors register in both System and Graph"
  :protocol '(
    (step-1 "Factory receives graph reference"
            :formal (factory-has-param :graph g))
    (step-2 "Factory gets system from graph"
            :formal (let ((s (get-system g))) ...))
    (step-3 "Factory registers actor in system"
            :formal (setf addr (system.register s actor)))
    (step-4 "Factory registers in graph with string ID"
            :formal (graph.register-node g id addr props))
    (step-5 "Factory returns address"
            :formal (return addr)))
  :postcondition '(
    (forall node-id
      (implies (graph.has-node g node-id)
               (and (system.has-actor s (graph.get-address g node-id))
                    (graph.has-properties g node-id))))))

;;; Contract: CLI operates through Graph
(defcontract 'cli-graph-interface
  :parties (task-cli graph-system)
  :description "CLI performs all operations through Graph layer"
  :operations '(
    ;; Read operations
    (list-tasks (graph.get-node-ids) (map graph.get-node-properties))
    (show-task (graph.send node-id "get" nil))
    (task-status (graph.send node-id "query_status" nil))
    (eval-task (graph.send node-id "eval" nil))

    ;; Write operations
    (add-task (task-actor graph) (graph.add-edge ...))
    (update-task (graph.send node-id action payload))
    (delete-task (graph.remove-node node-id))

    ;; Persistence
    (load (graph.dump) -> file)
    (save (file) -> factory -> graph.register-node)))

;;; Contract: Message routing equivalence
(defcontract 'message-routing-equivalence
  :parties (graph-system actor-system)
  :description "Graph.send produces same result as Address.send"
  :formal '(
    (forall (node-id msg)
      (let ((addr (graph.get-node graph node-id)))
        (= (graph.send graph node-id msg.type msg.payload)
           (addr.send msg))))))

;;; Contract: Edge semantics
(defcontract 'edge-type-semantics
  :parties (graph-system task-system knowledge-system)
  :description "Edge types have defined meanings across systems"
  :edge-semantics '(
    (spawned-by
      :from :task :to :task
      :meaning "Child task created by parent"
      :enforces (child.parent-task-id = parent.id)
      :affects (child.inherits-tools parent.tools-available))

    (depends-on
      :from :task :to :task
      :meaning "Task requires other task complete first"
      :enforces nil
      :affects (task.is-ready depends on dependency.state))

    (requires-knowledge
      :from :task :to :knowledge
      :meaning "Task needs information from knowledge"
      :enforces nil
      :affects (task.information-gaps))

    (produces
      :from :task :to :artifact
      :meaning "Task creates artifact on completion"
      :enforces nil
      :affects (created on task.complete with artifacts))

    (blocks
      :from :task :to :task
      :meaning "Source is blocking target"
      :enforces nil
      :affects (target.blockers))

    (references
      :from :any :to :any
      :meaning "General reference"
      :enforces nil
      :affects nil)))

;;;; ============================================================================
;;;; 4. DATA FLOW SPECIFICATIONS
;;;; ============================================================================

;;; CLI Load Flow
(defflow 'cli-load-graph
  :description "Load graph from tasks.json file"
  :steps '(
    (1 :read-file "Read tasks.json from file system"
       :input (file-path)
       :output (json-string)
       :error (file-not-found))

    (2 :parse-json "Parse JSON into data structure"
       :input (json-string)
       :output (task-file {:nodes list :edges list})
       :error (parse-error))

    (3 :create-graph "Create fresh Graph instance"
       :input nil
       :output (graph))

    (4 :restore-nodes "Recreate actors from properties"
       :input (task-file.nodes graph)
       :for-each (props)
       :action ((let ((addr (factory props graph)))
                  (graph.register-node props.id addr props)))
       :output (graph-with-nodes))

    (5 :restore-edges "Recreate edges"
       :input (task-file.edges graph)
       :for-each (edge)
       :action ((graph.add-edge edge.from-id edge.to-id edge.type edge.props))
       :output (graph-with-edges))

    (6 :set-counter "Restore edge counter"
       :input (max-edge-id)
       :action ((graph.set-edge-counter max-edge-id))
       :output (restored-graph))))

;;; CLI Save Flow
(defflow 'cli-save-graph
  :description "Persist graph to tasks.json file"
  :steps '(
    (1 :dump-graph "Export current state"
       :input (graph)
       :action ((graph.dump))
       :output ({:nodes list :edges list}))

    (2 :serialize-json "Convert to JSON with date transform"
       :input (dump-result)
       :transforms ((dates -> iso-8601))
       :output (json-string))

    (3 :write-file "Write to file system"
       :input (json-string file-path)
       :action ((fs.write-file-sync file-path json-string))
       :output (success))))

;;; Task Spawn Flow
(defflow 'task-spawn-child
  :description "Create child task from parent"
  :steps '(
    (1 :receive-message "Parent receives create_task message"
       :input (parent-task create-task-payload)
       :validation ((goal not-empty) (deliverables not-empty)))

    (2 :create-child "Call TaskActor factory"
       :input (payload graph)
       :action ((task-actor {:goal payload.goal
                            :deliverables payload.deliverables
                            :criteria payload.criteria
                            :parent-task-id parent.id
                            :tools-available parent.tools-available
                            :graph graph}))
       :output (child-address child-id))

    (3 :create-edge "Link child to parent"
       :input (child-id parent-id graph)
       :action ((graph.add-edge child-id parent-id :spawned-by))
       :output (edge))

    (4 :return-response "Return child ID to caller"
       :output ({:child-task-id child-id :success true}))))

;;; Message Routing Flow
(defflow 'message-routing
  :description "Route message from CLI to actor"
  :steps '(
    (1 :cli-sends "CLI calls graph.send(id, type, payload)"
       :input (node-id message-type payload)
       :output (message))

    (2 :graph-resolves "Graph looks up address"
       :input (node-id)
       :action ((graph.nodes.get node-id))
       :output (address)
       :error (node-not-found "Node not found: {node-id}"))

    (3 :forward-message "Graph calls address.send(message)"
       :input (address message)
       :action ((address.send message))
       :output (response))

    (4 :check-response "Graph checks response.success"
       :input (response)
       :on-failure (throw response.error))

    (5 :return-result "Return response to CLI"
       :output (response))))

;;;; ============================================================================
;;;; 5. CROSS-SYSTEM INVARIANTS
;;;; ============================================================================

;;; Invariant: All nodes registered in Graph
(definvariant 'all-nodes-in-graph
  :scope :combined-system
  :description "Every created node is registered in Graph"
  :formal '(
    (forall node
      (implies (node-created-by-factory node)
               (and (graph.has-node graph node.id)
                    (graph.has-properties graph node.id)
                    (graph.has-address graph node.id))))))

;;; Invariant: Dual registration consistency
(definvariant 'dual-registration-consistency
  :scope :combined-system
  :description "System and Graph registrations are consistent"
  :formal '(
    (forall node-id
      (implies (graph.has-node graph node-id)
               (let ((addr (graph.get-node graph node-id))
                     (sys (graph.get-system graph)))
                 (system.has-actor sys addr))))))

;;; Invariant: Edge references valid nodes (recommended, not enforced)
(definvariant 'edge-reference-validity
  :scope :combined-system
  :description "Edges should reference existing nodes"
  :enforcement :caller-responsibility
  :formal '(
    (forall edge
      (and (graph.has-node graph edge.from-id)
           (graph.has-node graph edge.to-id)))))

;;; Invariant: Task hierarchy consistency
(definvariant 'task-hierarchy-consistency
  :scope :task-system
  :description "Parent-child relationships are consistent"
  :formal '(
    (forall child
      (implies (task.parent-task-id child)
               (exists edge
                 (and (= edge.from-id child.id)
                      (= edge.to-id child.parent-task-id)
                      (= edge.type :spawned-by)))))))

;;; Invariant: CLI preserves graph invariants
(definvariant 'cli-preserves-invariants
  :scope :task-cli
  :description "CLI operations maintain graph consistency"
  :formal '(
    (forall command
      (implies (and (graph-satisfies-invariants graph :before command)
                    (cli-executes command))
               (graph-satisfies-invariants graph :after command)))))

;;; Invariant: Serialization round-trip
(definvariant 'serialization-round-trip
  :scope :combined-system
  :description "Save then load produces equivalent graph"
  :formal '(
    (forall graph
      (let ((dumped (graph.dump graph))
            (restored (restore-graph dumped factory)))
        (and (= (graph.get-node-ids graph)
                (graph.get-node-ids restored))
             (forall id
               (= (graph.get-node-properties graph id)
                  (graph.get-node-properties restored id)))
             (= (graph.get-edges graph)
                (graph.get-edges restored)))))))

;;; Invariant: Message routing equivalence
(definvariant 'routing-equivalence
  :scope :combined-system
  :description "Graph.send equals Address.send"
  :formal '(
    (forall (graph node-id msg)
      (let ((addr (graph.get-node graph node-id)))
        (= (graph.send graph node-id msg.type msg.payload)
           (addr.send msg))))))

;;;; ============================================================================
;;;; 6. STATE SYNCHRONIZATION
;;;; ============================================================================

;;; Graph maintains authoritative ID<->Address mapping
(defsync 'id-address-mapping
  :authoritative :graph
  :description "Graph is source of truth for string ID to Address mapping"
  :updates-on '(register-node remove-node)
  :consumers '(task-cli serialization))

;;; Task actors maintain authoritative state
(defsync 'task-state
  :authoritative :task-actor
  :description "TaskActor is source of truth for task properties"
  :updates-on '(start complete block update)
  :consumers '(graph.node-properties task-cli)
  :note "Graph.nodeProperties is cache, actor has authoritative state")

;;; Knowledge actors maintain authoritative content
(defsync 'knowledge-content
  :authoritative :knowledge-actor
  :description "KnowledgeActor is source of truth for content/version"
  :updates-on '(append update)
  :consumers '(graph.node-properties))

;;; File system is persistence store
(defsync 'persistence
  :authoritative :file-system
  :description "tasks.json is persistent store"
  :updates-on '(cli-save)
  :note "Graph state is ephemeral, file is durable")

;;;; ============================================================================
;;;; 7. ERROR PROPAGATION
;;;; ============================================================================

(deferror-chain 'cli-to-actor
  :description "Error propagation from actor to CLI"
  :chain '(
    (actor-error
      :source :task-actor/:knowledge-actor
      :format {:success false :error message}
      :propagates-to :graph-system)

    (graph-error
      :source :graph-system
      :on (actor-response.success = false)
      :action (throw Error actor-response.error)
      :propagates-to :task-cli)

    (cli-error
      :source :task-cli
      :on (catch error)
      :action ((console.error "Error:" error.message)
               (process.exit 1)))))

;;;; ============================================================================
;;;; 8. LIFECYCLE COORDINATION
;;;; ============================================================================

;;; Task creation lifecycle
(deflifecycle 'task-creation
  :description "Complete lifecycle of task creation"
  :stages '(
    (cli-parse "Parse add command arguments")
    (cli-validate "Validate parent/dependency references")
    (cli-load "Load graph from file")
    (factory-create "TaskActor factory creates actor")
    (system-register "System registers actor, returns Address")
    (graph-register "Graph registers ID -> Address mapping")
    (edge-create "Create spawned_by/depends_on edges")
    (cli-save "Persist graph to file")
    (cli-output "Display success message")))

;;; Task deletion lifecycle
(deflifecycle 'task-deletion
  :description "Complete lifecycle of task deletion"
  :stages '(
    (cli-parse "Parse delete command")
    (cli-validate "Validate task exists")
    (cli-confirm "Prompt for confirmation unless --force")
    (cli-load "Load graph from file")
    (graph-remove "Remove node and cascade edges")
    (cli-save "Persist graph to file")
    (cli-output "Display deletion summary")))

;;; Knowledge synthesis lifecycle
(deflifecycle 'knowledge-synthesis
  :description "Synthesize knowledge from multiple nodes"
  :stages '(
    (receive-message "Knowledge actor receives synthesize message")
    (collect-node-ids "Get list of fromNodes from payload")
    (fetch-content "For each nodeId, get properties via graph")
    (filter-valid "Skip non-existent or non-knowledge nodes")
    (format-content "Format with source headers and dividers")
    (deduplicate-sources "Combine sources with Set for uniqueness")
    (return-synthesis "Return synthesis and sources")))

;;;; ============================================================================
;;;; 9. VERIFICATION HOOKS
;;;; ============================================================================

;;; Query: Verify all integration contracts
(defquery 'verify-integration
  :description "Check all integration contracts hold"
  :check (and
    (contract-holds 'graph-owns-actor-system)
    (contract-holds 'dual-registration)
    (contract-holds 'cli-graph-interface)
    (contract-holds 'message-routing-equivalence)
    (contract-holds 'edge-type-semantics)))

;;; Query: Verify all cross-system invariants
(defquery 'verify-invariants
  :description "Check all cross-system invariants hold"
  :check (and
    (invariant-holds 'all-nodes-in-graph)
    (invariant-holds 'dual-registration-consistency)
    (invariant-holds 'task-hierarchy-consistency)
    (invariant-holds 'cli-preserves-invariants)
    (invariant-holds 'serialization-round-trip)
    (invariant-holds 'routing-equivalence)))

;;; Query: Verify system composition
(defquery 'verify-composition
  :description "Check system is properly composed"
  :check (and
    (subsystem-exists 'actor-system)
    (subsystem-exists 'graph-system)
    (subsystem-exists 'task-system)
    (subsystem-exists 'knowledge-system)
    (subsystem-exists 'task-cli)
    (layer-order-correct '(foundation coordination domain interface))))

;;;; ============================================================================
;;;; 10. EXAMPLES
;;;; ============================================================================

;;; Example: Complete task workflow
(defexample 'complete-task-workflow
  :description "End-to-end task creation and completion"
  :steps '(
    ;; Initialize
    ("task init" creates-file "tasks.json")

    ;; Add parent task
    ("task add 'Build feature' --priority P0"
     creates-node "task_1"
     creates-properties {:goal "Build feature" :priority 0 :state :created})

    ;; Add child task with dependency
    ("task add 'Write tests' --parent task_1 --depends task_1"
     creates-node "task_2"
     creates-edges [("task_2" "task_1" :spawned-by)
                    ("task_2" "task_1" :depends-on)])

    ;; Start parent
    ("task update task_1 start"
     transitions-state (:created -> :active)
     sets-timestamp :started-at)

    ;; Complete parent
    ("task update task_1 complete"
     requires (all-criteria-passed task_1)
     transitions-state (:active -> :completed)
     sets-timestamp :completed-at)

    ;; Now child can start (dependency complete)
    ("task ready"
     includes "task_2"
     because (dependency-complete "task_1"))))

;;; Example: Knowledge linking
(defexample 'knowledge-integration
  :description "Task with knowledge requirements"
  :steps '(
    ;; Create knowledge node
    (create-knowledge {:title "API Docs" :content "..." :sources ["docs.api.com"]})

    ;; Create task requiring knowledge
    (create-task {:goal "Implement endpoint"})

    ;; Link task to knowledge
    ("task link task_1 knowledge_1 requires_knowledge"
     creates-edge ("task_1" "knowledge_1" :requires-knowledge))

    ;; Task can query knowledge
    (send task_1 :query-knowledge {:node-id "knowledge_1" :question "..."})
    returns {:answer "..." :confidence 0.8 :sources [...]}))

;;;; ============================================================================
;;;; END
;;;; ============================================================================
