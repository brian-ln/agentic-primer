;;;; KNOWLEDGE_CLI - DSL Model
;;;; Knowledge management interface with versioning and querying
;;;; Following System Modeling Protocol

(defpackage :knowledge-cli-model
  (:use :cl :system-modeling-protocol)
  (:export #:knowledge-cli))

;;; ============================================================================
;;; COMPONENT DEFINITION
;;; ============================================================================

(behavior 'knowledge-cli
  :intent "CLI for managing versioned, queryable knowledge nodes"
  :foundation :command-handler
  :category :user-interface
  :tags '(cli knowledge-management versioning querying synthesis)

  ;;; ========================================================================
  ;;; STATE SCHEMA
  ;;; ========================================================================

  :state-schema '(
    ;; File system state
    (knowledge-file :type :string :required t :default "knowledge.json"
                    :description "Path to knowledge persistence file")
    (file-exists :type :boolean :required nil
                 :description "Whether knowledge.json exists")

    ;; Runtime state (per-command)
    (graph :type :object :required nil
           :description "In-memory Graph instance with KnowledgeActors")
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
          :intent "Create knowledge.json with example node"
          :validation ((not file-exists))
          :effects ((create-file knowledge-file)
                    (create-example-knowledge))
          :output "Created knowledge.json with example knowledge")

    ;; Knowledge creation
    (add :pattern (title &key content sources)
         :intent "Create new knowledge node"
         :validation ((file-exists)
                      (valid-title title))
         :effects ((create-knowledge title content sources)
                   (set-version 1)
                   (save-graph))
         :output "Added knowledge: {knowledge-id}")

    ;; Knowledge inspection
    (get :pattern (id)
          :intent "Show complete knowledge details"
          :validation ((file-exists)
                       (knowledge-exists id))
          :effects ((load-graph)
                    (send-message id 'get nil)
                    (format-knowledge-details))
          :output "{knowledge-details}")

    (list :pattern (&key limit)
          :intent "List all knowledge nodes"
          :validation ((file-exists)
                       (valid-limit limit))
          :effects ((load-graph)
                    (filter-knowledge-nodes)
                    (sort-by-created-desc)
                    (apply-limit limit)
                    (format-knowledge-list))
          :output "{knowledge-table}")

    ;; Knowledge modification
    (append :pattern (id content &key source)
            :intent "Append content to knowledge (version increment)"
            :validation ((file-exists)
                         (knowledge-exists id)
                         (valid-content content))
            :effects ((send-message id 'append (list :data content :source source))
                      (increment-version)
                      (add-source source)
                      (save-graph))
            :output "Appended to {id}")

    (update :pattern (id &key title content)
            :intent "Update knowledge properties (no version change)"
            :validation ((file-exists)
                         (knowledge-exists id)
                         (or title content))
            :effects ((send-message id 'update (list :properties (make-update-props title content)))
                      (save-graph))
            :output "Updated {id}")

    (delete :pattern (id &key force)
            :intent "Remove knowledge node and edges"
            :validation ((file-exists)
                         (knowledge-exists id))
            :safety ((unless force (confirm-deletion id)))
            :effects ((remove-node id)
                      (remove-connected-edges id)
                      (save-graph))
            :output "Deleted {id}")

    ;; Knowledge querying
    (query :pattern (id question)
           :intent "Query knowledge with natural language question"
           :validation ((file-exists)
                        (knowledge-exists id)
                        (valid-question question))
           :effects ((load-graph)
                     (send-message id 'query (list :question question))
                     (format-query-response))
           :output "{query-answer}")

    (search :pattern (query-text)
            :intent "Search all knowledge nodes"
            :validation ((file-exists)
                         (valid-query query-text))
            :effects ((load-graph)
                      (query-all-knowledge query-text)
                      (filter-by-confidence-threshold 0.0)
                      (sort-by-confidence-desc)
                      (format-search-results))
            :output "Found {count} matching knowledge node(s)")

    ;; Knowledge linking
    (link :pattern (knowledge-id node-id &key type)
          :intent "Create edge from knowledge to another node"
          :validation ((file-exists)
                       (knowledge-exists knowledge-id)
                       (valid-edge-type type)
                       (type :required t))
          :effects ((add-edge knowledge-id node-id type)
                    (save-graph))
          :output "Linked {knowledge-id} to {node-id}")

    (unlink :pattern (knowledge-id node-id)
            :intent "Remove edges between knowledge and node"
            :validation ((file-exists)
                         (knowledge-exists knowledge-id)
                         (edges-exist knowledge-id node-id))
            :effects ((remove-edges-between knowledge-id node-id)
                      (save-graph))
            :output "Unlinked {knowledge-id} from {node-id}")

    ;; Knowledge synthesis
    (synthesize :pattern (id &key from)
                :intent "Synthesize knowledge from multiple sources"
                :validation ((file-exists)
                             (knowledge-exists id)
                             (from :required t)
                             (valid-node-list from))
                :effects ((send-message id 'synthesize (list :fromNodes (parse-node-list from)))
                          (merge-sources)
                          (format-synthesis-result))
                :output "Synthesized from {count} source(s)"))

  ;;; ========================================================================
  ;;; FINITE STATE MACHINE - CLI EXECUTION
  ;;; ========================================================================

  :fsm (fsm 'cli-execution
    :states '(
      (idle :intent "Awaiting command invocation"
            :accepts ())

      (parsing :intent "Parsing command arguments"
               :accepts (init add get list append update delete query search link unlink synthesize))

      (validating :intent "Validating preconditions"
                  :accepts ())

      (loading :intent "Loading graph with KnowledgeActors"
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
  :synopsis "knowledge init"
  :description "Create knowledge.json with example knowledge"
  :options nil
  :preconditions '((not (file-exists-p "knowledge.json")))
  :postconditions '((file-exists-p "knowledge.json")
                    (valid-json-p "knowledge.json")
                    (>= (knowledge-count) 1)))

(defcommand 'add
  :synopsis "knowledge add <title> [--content TEXT] [--sources S1,S2]"
  :description "Create new knowledge node"
  :options '(
    (--content :type string :description "Initial content")
    (--sources :type string :format "S1,S2,..." :description "Comma-separated sources"))
  :preconditions '((file-exists-p "knowledge.json"))
  :postconditions '((knowledge-exists new-knowledge-id)
                    (= (knowledge-version new-knowledge-id) 1)))

(defcommand 'get
  :synopsis "knowledge get <id>"
  :description "Show knowledge details"
  :sections '(basic-info content sources connected-nodes))

(defcommand 'list
  :synopsis "knowledge list [--limit N]"
  :description "List knowledge nodes"
  :options '(
    (--limit :type number :description "Limit result count"))
  :sort-order '(created-at :descending)
  :display-format '(id title version created-date))

(defcommand 'append
  :synopsis "knowledge append <id> <content> [--source SRC]"
  :description "Append content to knowledge (increments version)"
  :options '(
    (--source :type string :description "Source attribution"))
  :preconditions '((file-exists-p "knowledge.json")
                   (knowledge-exists id))
  :postconditions '((= (knowledge-version id) (+ old-version 1))
                    (implies source (member source (knowledge-sources id)))))

(defcommand 'update
  :synopsis "knowledge update <id> [--title TEXT] [--content TEXT]"
  :description "Update knowledge properties (version unchanged)"
  :options '(
    (--title :type string :description "New title")
    (--content :type string :description "Replace content"))
  :preconditions '((file-exists-p "knowledge.json")
                   (knowledge-exists id)
                   (or title content))
  :postconditions '((= (knowledge-version id) old-version)
                    (implies title (= (knowledge-title id) title))))

(defcommand 'delete
  :synopsis "knowledge delete <id> [--force]"
  :description "Delete knowledge node"
  :options '(
    (--force :type boolean :description "Skip confirmation"))
  :safety '(
    (confirmation :prompt "Delete this knowledge? (yes/no): "
                  :accept "yes"
                  :bypass-with --force))
  :preconditions '((file-exists-p "knowledge.json")
                   (knowledge-exists id))
  :postconditions '((not (knowledge-exists id))
                    (not (any-edges-involving id))))

(defcommand 'query
  :synopsis "knowledge query <id> <question>"
  :description "Query knowledge with natural language"
  :output-format :formatted-answer
  :fields '(question answer confidence sources))

(defcommand 'search
  :synopsis "knowledge search <query>"
  :description "Search all knowledge nodes"
  :query-strategy :query-all-nodes
  :filter-threshold '(confidence > 0.0)
  :sort-order '(confidence :descending)
  :display-format '(confidence-percent id title answer-snippet))

(defcommand 'link
  :synopsis "knowledge link <kid> <nid> --type TYPE"
  :description "Link knowledge to another node"
  :options '(
    (--type :type string :required t
            :enum (requires_knowledge produces references depends_on spawned_by blocks)))
  :preconditions '((file-exists-p "knowledge.json")
                   (knowledge-exists knowledge-id)
                   (valid-edge-type type))
  :postconditions '((edge-exists new-edge-id)
                    (= (edge-from new-edge-id) knowledge-id)
                    (= (edge-to new-edge-id) node-id)))

(defcommand 'unlink
  :synopsis "knowledge unlink <kid> <nid>"
  :description "Remove edges between nodes"
  :preconditions '((file-exists-p "knowledge.json")
                   (knowledge-exists knowledge-id)
                   (> (count-edges-between knowledge-id node-id) 0))
  :postconditions '((= (count-edges-between knowledge-id node-id) 0)))

(defcommand 'synthesize
  :synopsis "knowledge synthesize <id> --from ID1,ID2"
  :description "Synthesize from multiple sources"
  :options '(
    (--from :type string :required t :format "ID1,ID2,..."
            :description "Comma-separated source node IDs"))
  :preconditions '((file-exists-p "knowledge.json")
                   (knowledge-exists id))
  :postconditions '((sources-merged id from-nodes)))

;;; ============================================================================
;;; DATA TYPE DEFINITIONS
;;; ============================================================================

(deftype 'knowledge-id
  :format "knowledge_{n}"
  :auto-increment t)

(deftype 'knowledge-properties
  :schema '((id :type string :required t)
            (type :type :constant :value "knowledge")
            (title :type string :required t)
            (content :type string :required t)
            (sources :type (list string) :required t :default ())
            (version :type number :required t :default 1)
            (createdAt :type (or date string) :required t)))

(deftype 'query-response
  :schema '((answer :type string :required t)
            (confidence :type number :range (0.0 1.0) :required t)
            (sources :type (list string) :required t)))

(deftype 'synthesis-result
  :schema '((synthesis :type string :required t)
            (sources :type (list string) :required t)))

(deftype 'knowledge-file
  :schema '((nodes :type (list node-properties) :required t)
            (edges :type (list edge) :required t))
  :serialization :json
  :date-format :iso-8601)

(deftype 'confidence-score
  :range '(0.0 1.0)
  :interpretation '((0.8 1.0 . "Highly relevant")
                    (0.5 0.79 . "Moderately relevant")
                    (0.2 0.49 . "Weakly relevant")
                    (0.0 0.19 . "Minimal relevance")))

;;; ============================================================================
;;; VALIDATION RULES
;;; ============================================================================

(defvalidation 'knowledge-exists
  :condition (let ((props (get-node-properties graph id)))
               (and props (string= (getf props :type) "knowledge")))
  :error "Knowledge not found: {id}")

(defvalidation 'valid-title
  :condition (and title (> (length title) 0))
  :error "Title cannot be empty")

(defvalidation 'valid-content
  :condition (not (null content))
  :error "Content cannot be null")

(defvalidation 'valid-question
  :condition (and question (> (length question) 0))
  :error "Question cannot be empty")

(defvalidation 'valid-query
  :condition (and query-text (> (length query-text) 0))
  :error "Query cannot be empty")

(defvalidation 'valid-limit
  :condition (or (null limit) (and (numberp limit) (> limit 0)))
  :error "Limit must be a positive number")

(defvalidation 'valid-edge-type
  :condition (member type '("depends_on" "requires_knowledge" "produces"
                            "spawned_by" "blocks" "references")
                     :test #'string-equal)
  :error "Invalid edge type: {type}")

(defvalidation 'valid-node-list
  :condition (and from (> (length (split-by-comma from)) 0))
  :error "Node list cannot be empty")

(defvalidation 'edges-exist
  :condition (> (count-edges-between graph knowledge-id node-id) 0)
  :error "No edges found between {knowledge-id} and {node-id}")

(defvalidation 'file-exists
  :condition (file-exists-p knowledge-file)
  :error "Knowledge file not found: {knowledge-file}. Run 'knowledge init' first.")

(defvalidation 'file-not-exists
  :condition (not (file-exists-p knowledge-file))
  :error "{knowledge-file} already exists")

(defvalidation 'update-has-fields
  :condition (or title content)
  :error "Provide at least --title or --content")

(defvalidation 'synthesize-has-sources
  :condition (not (null from))
  :error "--from is required (comma-separated node IDs)")

(defvalidation 'link-has-type
  :condition (not (null type))
  :error "--type is required")

;;; ============================================================================
;;; FILE I/O OPERATIONS
;;; ============================================================================

(defoperation 'load-graph
  :intent "Load knowledge graph with active KnowledgeActors"
  :steps '(
    (read-file knowledge-file)
    (parse-json content)
    (create-graph)
    (restore-knowledge-actors graph-file.nodes)
    (restore-edges graph-file.edges))
  :actor-creation :knowledge-actor-factory
  :state-restoration t
  :error-handling '(
    (file-not-found :throw "Knowledge file not found")
    (invalid-json :throw "Invalid JSON in knowledge file")))

(defoperation 'save-graph
  :intent "Persist knowledge graph to JSON file"
  :steps '(
    (dump-graph)
    (serialize-json :date-transform iso-8601)
    (write-file knowledge-file content))
  :atomicity :best-effort)

;;; ============================================================================
;;; DISPLAY FORMATTING
;;; ============================================================================

(defdisplay 'knowledge-list-row
  :format "{id} {title} {version} {created-date}"
  :field-widths '((id . 20) (title . 35) (version . 5))
  :header "ID                  Title                              Ver  Created"
  :truncation '((title . 32)))

(defdisplay 'knowledge-details
  :format :multi-section
  :sections '(
    (basic-info :format "Title:          {title}\nVersion:        {version}\nCreated:        {created}\nContent Length: {content-length} characters\nSources:        {source-count}")
    (content :format "\nContent:\n{content}")
    (sources :format "\nSources:\n{source-list}")
    (connected-nodes :format "\nConnected Nodes ({count}):\n{node-list}")))

(defdisplay 'query-response
  :format "Query: \"{question}\"\nKnowledge: {id}\n{separator}\n\nAnswer:\n{answer}\n\nConfidence: {confidence-percent}%\n\n{sources-section}")

(defdisplay 'search-result
  :format "[{confidence-percent}%] {id}: {title}\n  {snippet}")

(defdisplay 'synthesis-result
  :format "Synthesized from {count} source(s)\nTarget: {id}\nSources: {source-ids}\n{separator}\n\nSynthesis ({length} characters):\n{synthesis}\n\n{separator}\nCombined sources:\n{source-list}")

;;; ============================================================================
;;; INTEGRATION CONTRACTS
;;; ============================================================================

(defcontract 'graph-integration
  :partner :graph
  :operations '(
    (send :signature (node-id message-type payload) :returns response)
    (add-edge :signature (from-id to-id edge-type properties) :returns edge)
    (remove-node :signature (node-id) :returns boolean)
    (remove-edge :signature (edge-id) :returns boolean)
    (get-node-properties :signature (node-id) :returns (or properties null))
    (get-node-ids :signature () :returns (list string))
    (get-all-edges :signature (node-id) :returns (list edge))
    (dump :signature () :returns knowledge-file)))

(defcontract 'knowledge-actor-messages
  :partner :knowledge-actor
  :messages '(
    (get :payload () :response get-response
         :fields (id properties edges))
    (append :payload ((data required) (source optional)) :response append-response
            :fields (success version))
    (update :payload ((properties required)) :response update-response
            :fields (success))
    (query :payload ((question required)) :response query-response
           :fields (answer confidence sources))
    (synthesize :payload ((fromNodes required)) :response synthesis-response
                :fields (synthesis sources))))

;;; ============================================================================
;;; VERSIONING SEMANTICS
;;; ============================================================================

(defsemantics 'versioning
  :rules '(
    (initial-version :value 1 :trigger create)
    (increment-version :trigger append :effect (+ version 1))
    (preserve-version :trigger update :effect version-unchanged)
    (version-monotonic :invariant (forall ((v1 v2 Version))
                                     (=> (before v1 v2)
                                         (<= (version-at v1) (version-at v2)))))))

(defsemantics 'append-vs-update
  :operations '(
    (append :content-effect :concatenate :version-effect :increment
            :use-case "Incremental knowledge growth")
    (update :content-effect :replace :version-effect :preserve
            :use-case "Complete rewrite or title change")))

;;; ============================================================================
;;; QUERY SEMANTICS
;;; ============================================================================

(defsemantics 'query-processing
  :pipeline '(
    (input :type natural-language-question)
    (processing :actor knowledge-actor :method analyze-relevance)
    (output :type query-response :fields (answer confidence sources))))

(defsemantics 'confidence-threshold
  :search-filter '(confidence > 0.0)
  :interpretation '((0.8 1.0 . "Highly relevant")
                    (0.5 0.79 . "Moderately relevant")
                    (0.2 0.49 . "Weakly relevant")
                    (0.0 0.19 . "Minimal relevance")))

(defsemantics 'search-behavior
  :strategy :query-all-nodes
  :filter :confidence-threshold
  :sort :confidence-descending)

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
    (knowledge-not-found . 1)
    (invalid-argument . 1)
    (operation-failed . 1)))

;;; ============================================================================
;;; KNOWLEDGE INVARIANTS
;;; ============================================================================

(assert (forall ((k Knowledge))
  (=> (knowledge-exists k)
      (>= (knowledge-version k) 1)))
  :name "version-starts-at-one"
  :description "All knowledge versions start at 1")

(assert (forall ((k Knowledge))
  (=> (knowledge-exists k)
      (listp (knowledge-sources k))))
  :name "sources-is-list"
  :description "Sources must be a list")

(assert (forall ((k Knowledge))
  (=> (knowledge-exists k)
      (stringp (knowledge-content k))))
  :name "content-is-string"
  :description "Content must be a string")

(assert (forall ((append-op AppendOperation))
  (=> (executed append-op)
      (= (knowledge-version-after append-op)
         (+ (knowledge-version-before append-op) 1))))
  :name "append-increments-version"
  :description "Append always increments version by 1")

(assert (forall ((update-op UpdateOperation))
  (=> (executed update-op)
      (= (knowledge-version-after update-op)
         (knowledge-version-before update-op))))
  :name "update-preserves-version"
  :description "Update never changes version")

;;; ============================================================================
;;; END
;;; ============================================================================
