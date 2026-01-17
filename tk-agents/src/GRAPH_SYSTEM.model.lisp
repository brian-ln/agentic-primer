;;;; ============================================================================
;;;; GRAPH_SYSTEM - Formal DSL Model
;;;; ============================================================================
;;;; The Graph System is a central store and message router bridging string-based
;;;; external interfaces with the actor-based internal messaging system.
;;;; Following System Modeling Protocol.

(defpackage :graph-system-model
  (:use :cl :system-modeling-protocol)
  (:export #:graph-system #:graph-node #:graph-edge))

;;;; ============================================================================
;;;; 1. TYPE DEFINITIONS
;;;; ============================================================================

;;; Node Types - the kinds of nodes that can exist in the graph
(deftype node-type ()
  '(member :task :knowledge :artifact :pattern))

;;; Edge Types - semantic relationships between nodes
(deftype edge-type ()
  '(member :depends-on :requires-knowledge :produces
           :spawned-by :blocks :references))

;;; Task States - lifecycle states for task nodes
(deftype task-state ()
  '(member :created :ready :active :blocked :completed :failed))

;;; String ID - external identifier for serialization
(deftype string-id () 'string)

;;; Address - internal actor reference (opaque)
(deftype address ()
  '(cons (member :address)
         (cons symbol null)))

;;; Edge ID - unique identifier for edges
(deftype edge-id () 'string)

;;;; ============================================================================
;;;; 2. STATE SCHEMAS
;;;; ============================================================================

;;; Node Properties Schema - cached properties for quick access
(defschema node-properties
  "Properties cached for each node"
  (id          :type string-id    :required t :documentation "External identifier")
  (type        :type node-type    :required t :documentation "Node type")
  (created-at  :type timestamp    :required t :documentation "Creation timestamp")
  (extra       :type map          :required nil :documentation "Additional properties"))

;;; Edge Schema - relationship between nodes
(defschema edge
  "Edge connecting two nodes"
  (id          :type edge-id      :required t :documentation "Unique edge identifier")
  (from-id     :type string-id    :required t :documentation "Source node ID")
  (to-id       :type string-id    :required t :documentation "Target node ID")
  (type        :type edge-type    :required t :documentation "Relationship type")
  (properties  :type map          :required nil :documentation "Edge metadata"))

;;; Graph State Schema - internal graph state
(defschema graph-state
  "Internal state of the Graph System"
  (nodes           :type (map string-id address)     :required t :documentation "ID to Address mapping")
  (node-properties :type (map string-id node-properties) :required t :documentation "Cached properties")
  (edges           :type (map edge-id edge)          :required t :documentation "Edge storage")
  (edge-counter    :type integer                     :required t :default 0 :documentation "For edge ID generation")
  (system          :type system-ref                  :required t :documentation "Underlying actor system"))

;;;; ============================================================================
;;;; 3. MESSAGE INTERFACES
;;;; ============================================================================

;;; Graph-level Messages (external API)

(defmessage register-node
  "Register an actor with a string ID"
  :pattern (:id string-id :address address :properties node-properties)
  :intent "Add a node to the graph with bidirectional ID mapping"
  :validation ((unique-id :predicate (not (exists-in-graph id))))
  :effects ((add-to-nodes id address)
            (add-to-properties id properties)))

(defmessage send-to-node
  "Send a message to a node by string ID"
  :pattern (:node-id string-id :message-type string :payload map)
  :intent "Route message through the graph to the target node"
  :validation ((node-exists :predicate (exists-in-graph node-id)))
  :effects ((lookup-address node-id)
            (forward-to-actor address message))
  :returns response)

(defmessage remove-node
  "Remove a node and all its edges"
  :pattern (:node-id string-id)
  :intent "Remove node from graph with cascade edge deletion"
  :validation ((node-exists :predicate (exists-in-graph node-id)))
  :effects ((remove-connected-edges node-id)
            (remove-from-nodes node-id)
            (remove-from-properties node-id)))

(defmessage get-node
  "Get node address by string ID"
  :pattern (:node-id string-id)
  :intent "Retrieve the Address for a registered node"
  :validation ()
  :returns (or address null))

(defmessage get-node-properties
  "Get cached properties for a node"
  :pattern (:node-id string-id)
  :intent "Retrieve cached properties without messaging actor"
  :validation ()
  :returns (or node-properties null))

(defmessage get-node-ids
  "Get all registered node IDs"
  :pattern ()
  :intent "List all nodes in the graph"
  :validation ()
  :returns (list string-id))

;;; Edge Management Messages

(defmessage add-edge
  "Add an edge between nodes"
  :pattern (:from-id string-id :to-id string-id :type edge-type :properties map)
  :intent "Create a typed relationship between two nodes"
  :validation ()
  :effects ((increment-edge-counter)
            (generate-edge-id edge-counter)
            (create-edge id from-id to-id type properties)
            (store-edge id edge))
  :returns edge)

(defmessage remove-edge
  "Remove an edge by ID"
  :pattern (:edge-id edge-id)
  :intent "Delete an edge from the graph"
  :validation ()
  :effects ((delete-from-edges edge-id)))

(defmessage get-edges-from
  "Get all edges from a node"
  :pattern (:node-id string-id)
  :intent "Find outgoing edges from a node"
  :validation ()
  :returns (list edge))

(defmessage get-edges-to
  "Get all edges to a node"
  :pattern (:node-id string-id)
  :intent "Find incoming edges to a node"
  :validation ()
  :returns (list edge))

(defmessage get-all-edges
  "Get all edges connected to a node"
  :pattern (:node-id string-id)
  :intent "Find all edges (incoming and outgoing) for a node"
  :validation ()
  :returns (list edge))

(defmessage get-child-tasks
  "Get child tasks spawned by a task"
  :pattern (:task-id string-id)
  :intent "Find all nodes with spawned-by edge to this task"
  :validation ()
  :returns (list string-id))

;;; Serialization Messages

(defmessage dump
  "Dump graph state for serialization"
  :pattern ()
  :intent "Export current state for persistence"
  :validation ()
  :returns (:nodes (list node-properties) :edges (list edge)))

(defmessage set-edge-counter
  "Set edge counter for restoration"
  :pattern (:counter integer)
  :intent "Restore edge counter after loading from file"
  :validation ((non-negative :predicate (>= counter 0)))
  :effects ((set-edge-counter counter)))

(defmessage get-system
  "Get the underlying actor system"
  :pattern ()
  :intent "Access system for factory creation"
  :validation ()
  :returns system-ref)

;;;; ============================================================================
;;;; 4. BEHAVIOR SPECIFICATION
;;;; ============================================================================

(behavior 'graph-system
  :intent "Central store and message router bridging string IDs with actor Addresses"
  :foundation :coordination-layer
  :category :infrastructure
  :tags '(graph routing serialization coordination)

  :state-schema 'graph-state

  :accepts '((register-node    :transition :ready)
             (send-to-node     :transition :ready)
             (remove-node      :transition :ready)
             (get-node         :transition :ready)
             (get-node-properties :transition :ready)
             (get-node-ids     :transition :ready)
             (add-edge         :transition :ready)
             (remove-edge      :transition :ready)
             (get-edges-from   :transition :ready)
             (get-edges-to     :transition :ready)
             (get-all-edges    :transition :ready)
             (get-child-tasks  :transition :ready)
             (dump             :transition :ready)
             (set-edge-counter :transition :ready)
             (get-system       :transition :ready))

  :fsm (fsm 'graph-lifecycle
    :states '((:created   :intent "Graph instantiated, system created")
              (:ready     :intent "Graph operational, accepting messages"))
    :initial :created
    :transitions '((:created -> :ready :on :initialization-complete))))

;;;; ============================================================================
;;;; 5. INTEGRATION CONTRACTS
;;;; ============================================================================

;;; Contract: Graph owns and wraps Actor System
(defcontract graph-owns-system
  :parties (graph-system actor-system)
  :invariant "Graph creates and manages the underlying actor system"
  :guarantees ((system-created-at-construction)
               (system-accessible-via-get-system)
               (all-messages-routed-through-system)))

;;; Contract: Bidirectional ID Mapping
(defcontract bidirectional-mapping
  :parties (graph-system)
  :invariant "String ID and Address maintain consistent bidirectional mapping"
  :guarantees ((register-creates-both-mappings)
               (remove-deletes-both-mappings)
               (lookup-returns-same-address)))

;;; Contract: Factory Pattern Integration
(defcontract factory-integration
  :parties (graph-system actor-factory)
  :invariant "Factories create actors, graph registers them"
  :protocol ((factory-calls-system-register)
             (factory-returns-address)
             (graph-stores-address-by-id)
             (graph-stores-properties-by-id)))

;;;; ============================================================================
;;;; 6. CONSTRAINTS AND INVARIANTS
;;;; ============================================================================

;;; Invariant: Node ID Uniqueness
(definvariant node-id-uniqueness
  :description "Each node ID maps to exactly one Address"
  :formal "forall id: nodes.count(id) <= 1"
  :enforcement :on-register)

;;; Invariant: Bidirectional Consistency
(definvariant bidirectional-consistency
  :description "nodes and node-properties have identical key sets"
  :formal "nodes.keys() == node-properties.keys()"
  :enforcement :always)

;;; Invariant: Edge ID Uniqueness
(definvariant edge-id-uniqueness
  :description "Each edge ID is unique"
  :formal "forall e1, e2: e1.id == e2.id implies e1 === e2"
  :enforcement :on-add-edge)

;;; Invariant: Edge Counter Monotonicity
(definvariant edge-counter-monotonic
  :description "Edge counter never decreases during normal operation"
  :formal "edge-counter(t+1) >= edge-counter(t)"
  :enforcement :on-add-edge)

;;; Invariant: Cascade Deletion
(definvariant cascade-deletion
  :description "Removing a node removes all its edges"
  :formal "after remove-node(id): not exists edge where fromId=id or toId=id"
  :enforcement :on-remove-node)

;;; Invariant: Message Routing Equivalence
(definvariant routing-equivalence
  :description "Graph.send produces same result as direct Address.send"
  :formal "graph.send(id, type, payload) === graph.getNode(id).send(msg)"
  :enforcement :on-send)

;;;; ============================================================================
;;;; 7. DEPENDENCIES
;;;; ============================================================================

(defdependency graph-system
  :requires ((actor-system :for "Message routing and actor registration")
             (address-type :for "Internal actor references")
             (node-properties :for "Cached property storage")
             (edge-types :for "Relationship semantics"))
  :provides ((string-id-api :for "External serializable interface")
             (edge-management :for "Relationship tracking")
             (property-caching :for "Fast property access")
             (serialization :for "Dump/restore support")))

;;;; ============================================================================
;;;; 8. ERROR CONDITIONS
;;;; ============================================================================

(deferror node-not-found
  :trigger "send-to-node with non-existent node-id"
  :message "Node not found: {node-id}"
  :recovery :throw-exception)

(deferror message-failed
  :trigger "Actor returns response.success = false"
  :message "{response.error}"
  :recovery :throw-exception)

(deferror duplicate-node-id
  :trigger "register-node with existing id (if enforced)"
  :message "Node already exists: {id}"
  :recovery :overwrite-or-reject)

;;;; ============================================================================
;;;; 9. EXAMPLES
;;;; ============================================================================

;;; Example: Basic Task Graph Setup
(defexample basic-task-graph
  :description "Create a graph with parent and child task"
  :setup
  ((create-graph graph)
   (get-system graph system)
   (create-task-actor system "root" root-addr)
   (register-node graph "root" root-addr root-props)
   (create-task-actor system "subtask-1" subtask-addr)
   (register-node graph "subtask-1" subtask-addr subtask-props)
   (add-edge graph "subtask-1" "root" :spawned-by))
  :assertions
  ((= (length (get-node-ids graph)) 2)
   (= (length (get-child-tasks graph "root")) 1)
   (member "subtask-1" (get-child-tasks graph "root"))))

;;; Example: Message Routing
(defexample message-routing
  :description "Send message through graph to actor"
  :setup
  ((create-graph graph)
   (register-task-node graph "task-1" task-addr task-props))
  :action
  (send-to-node graph "task-1" "start" '(:context ()))
  :expected-result
  (:success t))

;;; Example: Persistence Workflow
(defexample persistence-workflow
  :description "Dump and restore graph state"
  :setup
  ((create-populated-graph graph))
  :action
  ((dump graph snapshot)
   (create-new-graph new-graph)
   (restore-from-snapshot new-graph snapshot factory-fn))
  :assertions
  ((= (length (get-node-ids new-graph))
      (length (get-node-ids graph)))))

;;;; ============================================================================
;;;; 10. VERIFICATION HOOKS
;;;; ============================================================================

;;; Query: Verify graph invariants
(defquery verify-graph-invariants
  :description "Check all graph invariants hold"
  :check (and node-id-uniqueness
              bidirectional-consistency
              edge-id-uniqueness))

;;; Query: Find orphaned edges
(defquery find-orphaned-edges
  :description "Find edges referencing non-existent nodes"
  :predicate (lambda (edge)
               (or (not (exists-in-graph (edge-from-id edge)))
                   (not (exists-in-graph (edge-to-id edge))))))

;;; Query: Verify routing equivalence
(defquery verify-routing
  :description "Confirm graph.send equals address.send"
  :test (lambda (graph node-id message)
          (equal (graph-send graph node-id message)
                 (address-send (get-node graph node-id) message))))

;;;; ============================================================================
;;;; END
;;;; ============================================================================
