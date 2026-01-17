;;;; TASK_CLI - DSL Model
;;;; Command-line interface for managing task graphs with persistence
;;;; Following System Modeling Protocol

(defpackage :task-cli-model
  (:use :cl :system-modeling-protocol)
  (:export #:task-cli))

;;; ============================================================================
;;; COMPONENT DEFINITION
;;; ============================================================================

(behavior 'task-cli
  :intent "Command-line interface for managing persistent task graphs"
  :foundation :command-handler
  :category :user-interface
  :tags '(cli task-management persistence file-io graph-visualization)

  ;;; ========================================================================
  ;;; STATE SCHEMA
  ;;; ========================================================================

  :state-schema '(
    ;; File system state
    (tasks-file :type :string :required t :default "tasks.json"
                :description "Path to task persistence file")
    (file-exists :type :boolean :required nil
                 :description "Whether tasks.json exists")

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
          :intent "Create new tasks.json file with example task"
          :validation ((not file-exists))
          :effects ((create-file tasks-file)
                    (create-example-task))
          :output "Created tasks.json with example task")

    ;; Task creation
    (add :pattern (goal &key deliverables criteria depends parent labels priority)
         :intent "Add a new task to the graph"
         :validation ((file-exists)
                      (valid-priority priority)
                      (valid-dependencies depends)
                      (valid-parent parent))
         :effects ((create-task goal deliverables criteria labels priority)
                   (create-dependency-edges depends)
                   (create-parent-edge parent)
                   (save-graph))
         :output "Added task: {task-id}")

    ;; Task modification
    (update :pattern (id action &rest args)
            :intent "Update task state (start, complete, block)"
            :validation ((file-exists)
                         (task-exists id)
                         (valid-action action))
            :transition (case action
                          (start :created->active :ready->active)
                          (complete :active->completed)
                          (block :any->blocked))
            :effects ((send-message id action args)
                      (save-graph))
            :output "{action} task {id}: {result}")

    ;; Task deletion
    (delete :pattern (id &key force)
            :intent "Remove task and connected edges"
            :validation ((file-exists)
                         (task-exists id))
            :safety ((unless force (confirm-deletion id)))
            :effects ((remove-node id)
                      (remove-connected-edges id)
                      (save-graph))
            :output "Deleted task {id}")

    ;; Listing and filtering
    (list :pattern (&key status label priority)
          :intent "List tasks with optional filters"
          :validation ((file-exists)
                       (valid-status status)
                       (valid-priority priority))
          :filter-logic :and
          :effects ((load-graph)
                    (apply-filters status label priority)
                    (format-task-list))
          :output "{task-list}")

    (ready :pattern ()
           :intent "Show tasks with no blockers"
           :validation ((file-exists))
           :effects ((load-graph)
                     (filter-ready-tasks)
                     (sort-by-priority-then-date)
                     (format-ready-list))
           :output "{ready-list}")

    (search :pattern (query)
            :intent "Search tasks by keyword"
            :validation ((file-exists))
            :search-fields (goal deliverables objective-success-criteria)
            :match-type :case-insensitive-substring
            :effects ((load-graph)
                      (search-all-tasks query)
                      (format-search-results))
            :output "Found {count} task(s) matching \"{query}\"")

    ;; Task inspection
    (show :pattern (id)
          :intent "Show detailed task information"
          :validation ((file-exists)
                       (task-exists id))
          :effects ((load-graph)
                    (send-message id 'get nil)
                    (format-task-details))
          :output "{task-details}")

    (status :pattern (id)
            :intent "Show task status with blockers"
            :validation ((file-exists)
                         (task-exists id))
            :effects ((load-graph)
                      (send-message id 'query_status nil)
                      (format-json))
            :output "{status-json}")

    (eval :pattern (id)
          :intent "Evaluate task completion criteria"
          :validation ((file-exists)
                       (task-exists id))
          :effects ((load-graph)
                    (send-message id 'eval nil)
                    (format-json))
          :output "{eval-json}")

    ;; Visualization
    (graph :pattern (id)
           :intent "Show dependency tree for task"
           :validation ((file-exists)
                        (task-exists id))
           :effects ((load-graph)
                     (traverse-dependencies id)
                     (detect-cycles)
                     (render-ascii-tree))
           :output "{dependency-tree}"))

  ;;; ========================================================================
  ;;; FINITE STATE MACHINE - CLI EXECUTION
  ;;; ========================================================================

  :fsm (fsm 'cli-execution
    :states '(
      (idle :intent "Awaiting command invocation"
            :accepts ())

      (parsing :intent "Parsing command arguments"
               :accepts (init add update delete list ready search show status eval graph))

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
  :synopsis "task init"
  :description "Create tasks.json with example task"
  :options nil
  :preconditions '((not (file-exists-p "tasks.json")))
  :postconditions '((file-exists-p "tasks.json")
                    (valid-json-p "tasks.json")
                    (>= (task-count) 1)))

(defcommand 'add
  :synopsis "task add <goal> [options]"
  :description "Add a new task to the graph"
  :options '(
    (--deliverables :type string :format "d1,d2,..." :description "Comma-separated deliverables")
    (--criteria :type string :format "name:measure:threshold" :description "Success criterion")
    (--depends :type string :format "id1,id2,..." :description "Dependency task IDs")
    (--parent :type string :format "id" :description "Parent task ID")
    (--labels :type string :format "tag1,tag2,..." :description "Comma-separated labels")
    (--priority :type string :format "P0|P1|P2|P3|P4" :description "Priority (0 is highest)"))
  :preconditions '((file-exists-p "tasks.json"))
  :postconditions '((task-exists new-task-id)
                    (implies depends (all-edges-exist depends-edges))
                    (implies parent (edge-exists parent-edge))))

(defcommand 'update
  :synopsis "task update <id> <action> [args...]"
  :description "Update task state"
  :actions '(
    (start :description "Transition to active state"
           :from-states (created ready)
           :to-state active)
    (complete :description "Transition to completed state"
              :from-states (active)
              :to-state completed
              :requires (all-criteria-passed))
    (block :description "Transition to blocked state"
           :args ((reason :type string :optional t))
           :from-states (created ready active)
           :to-state blocked))
  :preconditions '((file-exists-p "tasks.json")
                   (task-exists id)
                   (member action '(start complete block))))

(defcommand 'delete
  :synopsis "task delete <id> [--force]"
  :description "Remove task and connected edges"
  :options '(
    (--force :type boolean :description "Skip confirmation"))
  :safety '(
    (confirmation :prompt "Delete this task? (yes/no): "
                  :accept "yes"
                  :bypass-with --force))
  :preconditions '((file-exists-p "tasks.json")
                   (task-exists id))
  :postconditions '((not (task-exists id))
                    (not (any-edges-to-or-from id))))

(defcommand 'list
  :synopsis "task list [options]"
  :description "List tasks with filters"
  :options '(
    (--status :type string :enum (created ready active blocked completed failed))
    (--label :type string :description "Filter by label (case-insensitive)")
    (--priority :type string :format "P0-P4 or 0-4"))
  :filter-semantics :conjunction
  :display-format '(emoji priority-display id state goal-preview labels-display))

(defcommand 'ready
  :synopsis "task ready"
  :description "Show tasks with no blockers"
  :ready-criteria '(
    (state-not-in '(completed failed blocked))
    (all-dependencies-completed))
  :sort-order '(priority :ascending created-at :ascending))

(defcommand 'search
  :synopsis "task search <query>"
  :description "Search tasks by keyword"
  :search-fields '(goal desired-deliverables objective-success-criteria)
  :match-type :case-insensitive
  :display-format '(emoji id state goal-preview matched-fields))

(defcommand 'show
  :synopsis "task show <id>"
  :description "Show task details"
  :sections '(basic-info deliverables success-criteria edges information-gaps))

(defcommand 'status
  :synopsis "task status <id>"
  :description "Show status with blockers"
  :output-format :json
  :fields '(state progress blockers children-status))

(defcommand 'eval
  :synopsis "task eval <id>"
  :description "Evaluate completion criteria"
  :output-format :json
  :fields '(score passed objective-criteria subjective-criteria observations))

(defcommand 'graph
  :synopsis "task graph <id>"
  :description "Show dependency tree"
  :traversal :depth-first
  :edge-type :depends-on
  :rendering :ascii-tree
  :cycle-detection t)

;;; ============================================================================
;;; DATA TYPE DEFINITIONS
;;; ============================================================================

(deftype 'task-state
  :enum '(created ready active blocked completed failed)
  :display-emoji '((created . "o")
                   (ready . "yellow_circle")
                   (active . "arrows_counterclockwise")
                   (blocked . "no_entry_sign")
                   (completed . "white_check_mark")
                   (failed . "x")))

(deftype 'priority
  :range '(0 4)
  :display-format "P{n}"
  :ordering :ascending
  :semantics "Lower number = higher priority")

(deftype 'edge-type
  :enum '(depends_on requires_knowledge produces spawned_by blocks references))

(deftype 'task-id
  :format "task_{n}"
  :auto-increment t)

(deftype 'edge-id
  :format "edge_{n}"
  :auto-increment t)

(deftype 'objective-criterion
  :schema '((criterion :type string :required t)
            (measure :type string :required t)
            (threshold :type (or number boolean) :required t)
            (actual :type (or number boolean) :required nil)
            (passed :type boolean :required nil)))

(deftype 'task-file
  :schema '((nodes :type (list task-properties) :required t)
            (edges :type (list edge) :required t))
  :serialization :json
  :date-format :iso-8601)

;;; ============================================================================
;;; VALIDATION RULES
;;; ============================================================================

(defvalidation 'valid-priority
  :condition (or (null priority)
                 (member priority '(0 1 2 3 4))
                 (member priority '("P0" "P1" "P2" "P3" "P4")
                         :test #'string-equal))
  :error "Invalid priority \"{priority}\". Use P0-P4 or 0-4 (where 0 is highest).")

(defvalidation 'valid-status
  :condition (or (null status)
                 (member status '("created" "ready" "active" "blocked" "completed" "failed")
                         :test #'string-equal))
  :error "Invalid status. Use: created, ready, active, blocked, completed, failed")

(defvalidation 'valid-action
  :condition (member action '("start" "complete" "block") :test #'string-equal)
  :error "Unknown action: {action}. Valid actions: start, complete, block")

(defvalidation 'task-exists
  :condition (not (null (get-node-properties graph id)))
  :error "Task not found: {id}")

(defvalidation 'file-exists
  :condition (file-exists-p tasks-file)
  :error "Task file not found: {tasks-file}. Run 'task init' first.")

(defvalidation 'file-not-exists
  :condition (not (file-exists-p tasks-file))
  :error "{tasks-file} already exists")

(defvalidation 'valid-dependencies
  :condition (every (lambda (dep-id) (task-exists dep-id)) depends)
  :error "Dependency task not found: {missing-dep}")

(defvalidation 'valid-parent
  :condition (or (null parent) (task-exists parent))
  :error "Parent task not found: {parent}")

;;; ============================================================================
;;; FILE I/O OPERATIONS
;;; ============================================================================

(defoperation 'load-graph
  :intent "Load task graph from JSON file"
  :steps '(
    (read-file tasks-file)
    (parse-json content)
    (create-graph)
    (restore-nodes task-file.nodes)
    (restore-edges task-file.edges))
  :error-handling '(
    (file-not-found :throw "Task file not found")
    (invalid-json :throw "Invalid JSON in task file")))

(defoperation 'save-graph
  :intent "Persist task graph to JSON file"
  :steps '(
    (dump-graph)
    (serialize-json :date-transform iso-8601)
    (write-file tasks-file content))
  :atomicity :best-effort)

;;; ============================================================================
;;; DISPLAY FORMATTING
;;; ============================================================================

(defdisplay 'task-list-row
  :format "{emoji} {priority} {id} {state} {goal-preview} {labels}"
  :field-widths '((priority . 3) (id . 15) (state . 10))
  :truncation '((goal-preview . 50)))

(defdisplay 'search-result-row
  :format "{emoji} {id} {state} {goal-preview}\n   [{matched-fields}]")

(defdisplay 'dependency-tree
  :format :ascii-tree
  :node-format "{emoji} {id}: {goal-preview}"
  :connectors '((branch . "|-- ")
                (last-branch . "+-- ")
                (continuation . "|   ")
                (space . "    "))
  :cycle-warning "warning (circular dependency detected)")

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
    (get-edges-from :signature (node-id) :returns (list edge))
    (dump :signature () :returns task-file)))

(defcontract 'task-actor-messages
  :partner :task-actor
  :messages '(
    (get :payload () :response get-response)
    (start :payload ((context optional)) :response state-change-response)
    (complete :payload ((result required)) :response state-change-response)
    (block :payload ((reason required)) :response state-change-response)
    (eval :payload () :response eval-response)
    (query_status :payload () :response status-response)))

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
    (task-not-found . 1)
    (invalid-argument . 1)
    (operation-failed . 1)))

;;; ============================================================================
;;; END
;;; ============================================================================
