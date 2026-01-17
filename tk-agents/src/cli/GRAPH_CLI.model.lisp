;;;; GRAPH_CLI - DSL Model
;;;; Low-level graph manipulation interface
;;;; Following System Modeling Protocol

(defpackage :graph-cli-model
  (:use :cl :system-modeling-protocol)
  (:export #:graph-cli))

;;; ============================================================================
;;; COMPONENT DEFINITION
;;; ============================================================================

(behavior 'graph-cli
  :intent "Low-level CLI for direct graph structure manipulation"
  :foundation :command-handler
  :category :user-interface
  :tags '(cli graph-manipulation low-level debugging import-export)

  ;;; ========================================================================
  ;;; STATE SCHEMA
  ;;; ========================================================================

  :state-schema '(
    ;; File system state
    (graph-file :type :string :required t :default "graph.json"
                :description "Path to graph persistence file")
    (file-exists :type :boolean :required nil
                 :description "Whether graph.json exists")

    ;; Runtime state (per-command)
    (graph :type :object :required nil
           :description "In-memory Graph instance")
    (current-command :type :keyword :required nil
                     :description "Command being executed")
    (exit-code :type :number :required nil :default 0
               :description "Process exit code"))

  ;;; ========================================================================
  ;;; COMMAND DEFINITIONS
  ;;; ========================================================================

  :accepts '(
    ;; Initialization
    (init :pattern ()
          :intent "Create new empty graph.json file"
          :validation ((not file-exists))
          :effects ((create-file graph-file)
                    (initialize-empty-graph))
          :output "Created graph.json")

    ;; Node operations
    (create-node :pattern (id &key type data)
                 :intent "Create a new node with specified type and properties"
                 :validation ((file-exists)
                              (not (node-exists id))
                              (valid-node-type type)
                              (type :required t)
                              (valid-json data))
                 :effects ((create-node id type data)
                           (set-created-timestamp id)
                           (register-dummy-actor id)
                           (save-graph))
                 :output "Created node: {id}")

    (delete-node :pattern (id &key force)
                 :intent "Remove node and all connected edges"
                 :validation ((file-exists)
                              (node-exists id))
                 :safety ((unless force (confirm-deletion id)))
                 :effects ((remove-node id)
                           (cascade-remove-edges id)
                           (save-graph))
                 :output "Deleted node {id}")

    ;; Edge operations
    (create-edge :pattern (from to &key type data)
                 :intent "Create edge between two nodes"
                 :validation ((file-exists)
                              (node-exists from)
                              (node-exists to)
                              (valid-edge-type type)
                              (type :required t)
                              (valid-json data))
                 :effects ((create-edge from to type data)
                           (assign-edge-id)
                           (save-graph))
                 :output "Created edge: {edge-id}")

    (delete-edge :pattern (edge-id)
                 :intent "Remove edge from graph"
                 :validation ((file-exists)
                              (edge-exists edge-id))
                 :effects ((remove-edge edge-id)
                           (save-graph))
                 :output "Deleted edge: {edge-id}")

    ;; Listing operations
    (list-nodes :pattern (&key type)
                :intent "List all nodes with optional type filter"
                :validation ((file-exists)
                             (valid-node-type type :allow-nil t))
                :filter-logic :type-match
                :effects ((load-graph)
                          (filter-nodes type)
                          (format-node-list))
                :output "{node-table}")

    (list-edges :pattern (&key from to type)
                :intent "List edges with optional filters"
                :validation ((file-exists)
                             (valid-edge-type type :allow-nil t))
                :filter-logic :and
                :effects ((load-graph)
                          (filter-edges from to type)
                          (format-edge-list))
                :output "{edge-table}")

    ;; Inspection
    (show :pattern (id)
          :intent "Show detailed node information"
          :validation ((file-exists)
                       (node-exists id))
          :effects ((load-graph)
                    (get-node-properties id)
                    (get-incoming-edges id)
                    (get-outgoing-edges id)
                    (format-node-details))
          :output "{node-details}")

    ;; Visualization
    (show-graph :pattern (&key format root)
                :intent "Visualize graph structure"
                :validation ((file-exists)
                             (valid-format format :allow-nil t)
                             (node-exists root :allow-nil t))
                :effects ((load-graph)
                          (traverse-graph root)
                          (detect-cycles)
                          (render-visualization format))
                :output "{graph-visualization}")

    ;; Import/Export
    (export :pattern (&key output)
            :intent "Export graph to JSON"
            :validation ((file-exists))
            :effects ((load-graph)
                      (serialize-json)
                      (write-output output))
            :output "{export-summary}")

    (import :pattern (file)
            :intent "Import nodes and edges from JSON file"
            :validation ((import-file-exists file)
                         (valid-json-structure file))
            :effects ((load-or-create-graph)
                      (merge-nodes file)
                      (merge-edges file)
                      (save-graph))
            :output "Imported graph from {file}"))

  ;;; ========================================================================
  ;;; FINITE STATE MACHINE - CLI EXECUTION
  ;;; ========================================================================

  :fsm (fsm 'cli-execution
    :states '(
      (idle :intent "Awaiting command invocation"
            :accepts ())

      (parsing :intent "Parsing command arguments"
               :accepts (init create-node delete-node create-edge delete-edge
                        list-nodes list-edges show show-graph export import))

      (validating :intent "Validating preconditions"
                  :accepts ())

      (loading :intent "Loading graph from file"
               :accepts ())

      (executing :intent "Executing command logic"
                 :accepts ())

      (saving :intent "Persisting changes to file"
              :accepts ())

      (outputting :intent "Formatting and displaying output"
                  :accepts ())

      (exiting :intent "Process termination"
               :accepts ()))

    :transitions '(
      (idle -> parsing :on command-invoked)
      (parsing -> validating :on args-parsed)
      (parsing -> exiting :on parse-error :exit-code 1)
      (validating -> loading :on validation-passed :condition requires-file)
      (validating -> executing :on validation-passed :condition (not requires-file))
      (validating -> exiting :on validation-failed :exit-code 1)
      (loading -> executing :on graph-loaded)
      (loading -> exiting :on load-error :exit-code 1)
      (executing -> saving :on execution-complete :condition modifies-state)
      (executing -> outputting :on execution-complete :condition (not modifies-state))
      (saving -> outputting :on save-complete)
      (saving -> exiting :on save-error :exit-code 1)
      (outputting -> exiting :on output-complete :exit-code 0))))

;;; ============================================================================
;;; COMMAND INTERFACE DETAILS
;;; ============================================================================

(defcommand 'init
  :synopsis "graph init"
  :description "Create empty graph.json file"
  :options nil
  :preconditions '((not (file-exists-p "graph.json")))
  :postconditions '((file-exists-p "graph.json")
                    (valid-json-p "graph.json")
                    (= (node-count) 0)
                    (= (edge-count) 0)))

(defcommand 'create-node
  :synopsis "graph create-node <id> --type TYPE [--data JSON]"
  :description "Create a new node in the graph"
  :options '(
    (--type :type string :enum (task knowledge artifact pattern) :required t
            :description "Node type")
    (--data :type string :format "JSON" :description "Additional properties"))
  :preconditions '((file-exists-p "graph.json")
                   (not (node-exists id))
                   (valid-node-type type))
  :postconditions '((node-exists id)
                    (= (node-property id 'type) type)
                    (timestamp-set (node-property id 'createdAt))))

(defcommand 'delete-node
  :synopsis "graph delete-node <id> [--force]"
  :description "Remove node and connected edges"
  :options '(
    (--force :type boolean :description "Skip confirmation"))
  :safety '(
    (confirmation :prompt "Delete this node? (yes/no): "
                  :accept "yes"
                  :bypass-with --force))
  :preconditions '((file-exists-p "graph.json")
                   (node-exists id))
  :postconditions '((not (node-exists id))
                    (not (any-edges-involving id))))

(defcommand 'create-edge
  :synopsis "graph create-edge <from> <to> --type TYPE [--data JSON]"
  :description "Create edge between nodes"
  :options '(
    (--type :type string
            :enum (depends_on requires_knowledge produces spawned_by blocks references)
            :required t
            :description "Edge type")
    (--data :type string :format "JSON" :description "Edge properties"))
  :preconditions '((file-exists-p "graph.json")
                   (node-exists from)
                   (node-exists to)
                   (valid-edge-type type))
  :postconditions '((edge-exists new-edge-id)
                    (= (edge-from new-edge-id) from)
                    (= (edge-to new-edge-id) to)
                    (= (edge-type new-edge-id) type)))

(defcommand 'delete-edge
  :synopsis "graph delete-edge <edgeId>"
  :description "Remove edge from graph"
  :preconditions '((file-exists-p "graph.json")
                   (edge-exists edge-id))
  :postconditions '((not (edge-exists edge-id))))

(defcommand 'list-nodes
  :synopsis "graph list-nodes [--type TYPE]"
  :description "List nodes with optional filter"
  :options '(
    (--type :type string :enum (task knowledge artifact pattern)))
  :filter-semantics :type-match
  :display-format '(id type created-timestamp))

(defcommand 'list-edges
  :synopsis "graph list-edges [--from ID] [--to ID] [--type TYPE]"
  :description "List edges with filters"
  :options '(
    (--from :type string :description "Source node ID")
    (--to :type string :description "Target node ID")
    (--type :type string :enum (depends_on requires_knowledge produces spawned_by blocks references)))
  :filter-semantics :conjunction
  :display-format '(edge-id from-id to-id edge-type))

(defcommand 'show
  :synopsis "graph show <id>"
  :description "Show node details"
  :sections '(basic-info additional-properties outgoing-edges incoming-edges))

(defcommand 'show-graph
  :synopsis "graph show-graph [--format ascii|mermaid] [--root ID]"
  :description "Visualize graph structure"
  :options '(
    (--format :type string :enum (ascii mermaid) :default "ascii")
    (--root :type string :description "Root node for traversal"))
  :traversal :depth-first
  :cycle-detection t
  :rendering '((ascii . ascii-tree)
               (mermaid . mermaid-diagram)))

(defcommand 'export
  :synopsis "graph export [--output FILE]"
  :description "Export graph to JSON"
  :options '(
    (--output :type string :description "Output file path"))
  :output-destination '((if output file) (else stdout)))

(defcommand 'import
  :synopsis "graph import <file>"
  :description "Import graph from JSON file"
  :merge-strategy :skip-duplicates
  :edge-id-strategy :regenerate
  :preconditions '((file-exists-p import-file)
                   (valid-json-p import-file)
                   (has-required-keys import-file '(nodes edges))))

;;; ============================================================================
;;; DATA TYPE DEFINITIONS
;;; ============================================================================

(deftype 'node-type
  :enum '(task knowledge artifact pattern)
  :description "Valid node type identifiers")

(deftype 'edge-type
  :enum '(depends_on requires_knowledge produces spawned_by blocks references)
  :description "Valid edge type identifiers"
  :semantics '((depends_on . "B depends on A")
               (requires_knowledge . "Requires knowledge prerequisite")
               (produces . "Creates artifact")
               (spawned_by . "Child-parent relationship")
               (blocks . "Blocks progress")
               (references . "General reference")))

(deftype 'node-id
  :format :free-form
  :uniqueness :global
  :description "User-specified node identifier")

(deftype 'edge-id
  :format "edge_{n}"
  :auto-increment t
  :description "Auto-generated edge identifier")

(deftype 'node-properties
  :schema '((id :type string :required t)
            (type :type node-type :required t)
            (createdAt :type (or date string) :required t)
            (* :type any :description "Additional properties")))

(deftype 'edge
  :schema '((id :type string :required t)
            (fromId :type string :required t)
            (toId :type string :required t)
            (type :type edge-type :required t)
            (properties :type object :required t :default {})))

(deftype 'graph-file
  :schema '((nodes :type (list node-properties) :required t)
            (edges :type (list edge) :required t))
  :serialization :json
  :date-format :iso-8601)

(deftype 'visualization-format
  :enum '(ascii mermaid)
  :renderers '((ascii . render-ascii-tree)
               (mermaid . render-mermaid-diagram)))

;;; ============================================================================
;;; VALIDATION RULES
;;; ============================================================================

(defvalidation 'valid-node-type
  :condition (member type '("task" "knowledge" "artifact" "pattern")
                     :test #'string-equal)
  :error "Invalid type \"{type}\". Valid types: task, knowledge, artifact, pattern")

(defvalidation 'valid-edge-type
  :condition (member type '("depends_on" "requires_knowledge" "produces"
                            "spawned_by" "blocks" "references")
                     :test #'string-equal)
  :error "Invalid edge type \"{type}\". Valid types: depends_on, requires_knowledge, produces, spawned_by, blocks, references")

(defvalidation 'node-exists
  :condition (not (null (get-node-properties graph id)))
  :error "Node not found: {id}")

(defvalidation 'node-not-exists
  :condition (null (get-node-properties graph id))
  :error "Node \"{id}\" already exists")

(defvalidation 'edge-exists
  :condition (find edge-id (get-all-edges graph) :key #'edge-id :test #'string=)
  :error "Edge not found: {edge-id}")

(defvalidation 'file-exists
  :condition (file-exists-p graph-file)
  :error "Graph file not found: {graph-file}. Run 'graph init' first.")

(defvalidation 'file-not-exists
  :condition (not (file-exists-p graph-file))
  :error "{graph-file} already exists")

(defvalidation 'valid-json
  :condition (or (null data)
                 (ignore-errors (parse-json data)))
  :error "Invalid JSON in --data: {parse-error}")

(defvalidation 'valid-format
  :condition (or (null format)
                 (member format '("ascii" "mermaid") :test #'string-equal))
  :error "Unknown format \"{format}\". Use ascii or mermaid.")

(defvalidation 'valid-json-structure
  :condition (and (has-key import-data 'nodes)
                  (has-key import-data 'edges)
                  (arrayp (get-key import-data 'nodes))
                  (arrayp (get-key import-data 'edges)))
  :error "Import file must have 'nodes' and 'edges' arrays")

;;; ============================================================================
;;; FILE I/O OPERATIONS
;;; ============================================================================

(defoperation 'load-graph
  :intent "Load graph from JSON file with dummy actors"
  :steps '(
    (read-file graph-file)
    (parse-json content)
    (create-graph)
    (restore-nodes-with-dummy-actors graph-file.nodes)
    (restore-edges graph-file.edges))
  :actor-creation :dummy
  :error-handling '(
    (file-not-found :throw "Graph file not found")
    (invalid-json :throw "Invalid JSON in graph file")))

(defoperation 'save-graph
  :intent "Persist graph to JSON file"
  :steps '(
    (dump-graph)
    (serialize-json :date-transform iso-8601)
    (write-file graph-file content))
  :atomicity :best-effort)

(defoperation 'merge-nodes
  :intent "Import nodes with duplicate skipping"
  :strategy :skip-duplicates
  :steps '(
    (for-each node import-data.nodes
      (if (not (node-exists node.id))
          (create-node-with-dummy-actor node)
          (skip-node node)))))

(defoperation 'merge-edges
  :intent "Import edges with ID regeneration"
  :strategy :regenerate-ids
  :steps '(
    (for-each edge import-data.edges
      (if (and (node-exists edge.fromId)
               (node-exists edge.toId))
          (create-edge-with-new-id edge)
          (skip-edge edge)))))

;;; ============================================================================
;;; DISPLAY FORMATTING
;;; ============================================================================

(defdisplay 'node-list-row
  :format "{id} {type} {created}"
  :field-widths '((id . 20) (type . 15))
  :header "ID                  Type           Created")

(defdisplay 'edge-list-row
  :format "{id} {from} {to} {type}"
  :field-widths '((id . 12) (from . 20) (to . 20))
  :header "ID          From                To                  Type")

(defdisplay 'node-details
  :format :multi-section
  :sections '(
    (basic-info :format "ID:             {id}\nType:           {type}\nCreated:        {created}")
    (properties :format "\nProperties:\n{key-value-list}")
    (outgoing-edges :format "\nOutgoing Edges ({count}):\n{edge-list}")
    (incoming-edges :format "\nIncoming Edges ({count}):\n{edge-list}")))

(defdisplay 'ascii-tree
  :format :tree
  :node-format "{id} [{type}]"
  :connectors '((branch . "├──")
                (last-branch . "└──")
                (continuation . "│   ")
                (space . "    "))
  :cycle-warning "(cycle: {id})")

(defdisplay 'mermaid-diagram
  :format :mermaid
  :header "graph TD"
  :node-format "{safe-id}[\"{id}<br/>{type}\"]"
  :edge-format "{safe-from} -->|{type}| {safe-to}")

;;; ============================================================================
;;; INTEGRATION CONTRACTS
;;; ============================================================================

(defcontract 'graph-integration
  :partner :graph
  :operations '(
    (register-node :signature (id address properties) :returns void)
    (add-edge :signature (from-id to-id edge-type properties) :returns edge)
    (remove-node :signature (node-id) :returns boolean)
    (remove-edge :signature (edge-id) :returns boolean)
    (get-node-properties :signature (node-id) :returns (or properties null))
    (get-node-ids :signature () :returns (list string))
    (get-edges-from :signature (node-id) :returns (list edge))
    (get-edges-to :signature (node-id) :returns (list edge))
    (get-all-edges :signature (node-id) :returns (list edge))
    (dump :signature () :returns graph-file)))

(defcontract 'dummy-actor
  :intent "Minimal actor stub for graph structure only"
  :messages '(
    (send :payload any :response (success t data {})))
  :note "No behavioral logic - structure storage only")

;;; ============================================================================
;;; ERROR HANDLING
;;; ============================================================================

(deferror-handling 'cli-errors
  :strategy :exit-with-message
  :format "Error: {message}"
  :exit-codes '(
    (success . 0)
    (validation-error . 1)
    (file-not-found . 1)
    (node-not-found . 1)
    (edge-not-found . 1)
    (invalid-argument . 1)
    (operation-failed . 1)))

;;; ============================================================================
;;; GRAPH INVARIANTS
;;; ============================================================================

(assert (forall ((e Edge))
  (=> (edge-exists e)
      (and (node-exists (edge-from e))
           (node-exists (edge-to e)))))
  :name "edge-endpoints-exist"
  :description "All edges must connect existing nodes")

(assert (forall ((n Node))
  (=> (node-exists n)
      (unique-id (node-id n))))
  :name "node-ids-unique"
  :description "All node IDs must be unique")

(assert (forall ((f GraphFile))
  (valid-json f))
  :name "file-valid-json"
  :description "Graph file must be valid JSON")

;;; ============================================================================
;;; END
;;; ============================================================================
