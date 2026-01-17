;;;; TASK_SYSTEM.model.lisp - DSL Model
;;;; Task Actor System for hierarchical goal-oriented work management
;;;; Following System Modeling Protocol

(defpackage :task-system-model
  (:use :cl :system-modeling-protocol)
  (:export :task-actor))

;;;; ============================================================================
;;;; CORE BEHAVIOR DEFINITION
;;;; ============================================================================

(behavior 'task-actor
  :intent "Manage goal-oriented work units with success criteria and progress tracking"
  :foundation :state-machine
  :category :task-management
  :tags '(task goal criteria progress hierarchy actor)

  ;;;; STATE SCHEMA
  ;;;; Defines the shape of TaskProperties

  :state-schema '(
    ;; Identity
    (id :type :string :required t :pattern "task_[0-9]+")
    (type :type :keyword :required t :value :task)
    (created-at :type :timestamp :required t :immutable t)

    ;; Goal Definition
    (goal :type :string :required t :min-length 1)
    (desired-deliverables :type :list :required t :item-type :string)

    ;; Success Criteria
    (objective-success-criteria :type :list :required t
      :item-schema (
        (criterion :type :string :required t)
        (measure :type :string :required t)
        (threshold :type (:number :boolean) :required t)
        (actual :type (:number :boolean :nil) :required nil)
        (passed :type :boolean :required nil)))
    (subjective-success-criteria :type :list :required nil
      :item-schema (
        (criterion :type :string :required t)
        (evaluation-guidance :type :string :required t)
        (assessment :type :string :required nil)
        (notes :type :string :required nil)))

    ;; Knowledge Management
    (known-information :type :list :default () :item-type :string)
    (information-gaps :type :list :default () :item-type :string)
    (tools-available :type :list :default () :item-type :string)

    ;; State Management
    (state :type :keyword :required t
      :enum (:created :ready :active :blocked :completed :failed))
    (started-at :type :timestamp :required nil)
    (completed-at :type :timestamp :required nil)
    (result :type :any :required nil)

    ;; Hierarchy
    (parent-task-id :type :string :required nil :pattern "task_[0-9]+")

    ;; Metadata
    (labels :type :list :required nil :item-type :string)
    (priority :type :number :required nil :enum (0 1 2 3 4)))

  ;;;; MESSAGE INTERFACE
  ;;;; All messages the TaskActor accepts

  :accepts '(
    ;; Standard Node Messages (shared with other node types)

    (get
      :pattern ()
      :intent "Return full node state including properties and edges"
      :validation ()
      :response (:id :type :properties :edges))

    (observe
      :pattern ()
      :intent "Return human-readable summary of task status"
      :validation ()
      :response (:state :observations :metadata))

    (update
      :pattern ((properties :partial-task-properties))
      :intent "Modify task properties (except protected fields)"
      :validation (
        (properties-not-include :id :type :created-at))
      :response (:success :updated-properties))

    (link
      :pattern ((to-id :string) (edge-type :edge-type) (properties :map :optional))
      :intent "Create edge from this task to another node"
      :validation (
        (to-id-exists-in-graph t)
        (edge-type-valid t))
      :response (:edge-id :success))

    (unlink
      :pattern ((edge-id :string))
      :intent "Remove an edge by ID"
      :validation (
        (edge-id-exists t))
      :response (:success))

    (delete
      :pattern ()
      :intent "Remove task and all connected edges from graph"
      :validation ()
      :response (:success))

    ;; Task-Specific Messages

    (start
      :pattern ((context :map :optional))
      :intent "Transition task from created/ready to active"
      :validation (
        (state-in (:created :ready)))
      :transition :active
      :response (:success :state))

    (create-task
      :pattern ((goal :string) (deliverables :list) (criteria :list) (context :map :optional))
      :intent "Spawn child task with inheritance from parent"
      :validation (
        (goal-not-empty t)
        (deliverables-not-empty t)
        (criteria-valid t))
      :effects (
        (creates-child-task t)
        (inherits-tools-available t)
        (creates-spawned-by-edge t))
      :response (:child-task-id :success))

    (eval
      :pattern ()
      :intent "Evaluate success criteria and check child completion"
      :validation ()
      :response (:score :passed :objective-criteria :subjective-criteria :observations))

    (complete
      :pattern ((result :any) (artifacts :list :optional))
      :intent "Mark task as completed with result and optional artifacts"
      :validation (
        (eval-passes t))
      :transition :completed
      :effects (
        (sets-completed-at t)
        (stores-result t)
        (creates-produces-edges :if-artifacts))
      :response (:success :final-state))

    (block
      :pattern ((reason :string) (required-knowledge :list :optional))
      :intent "Mark task as blocked with reason"
      :validation (
        (reason-not-empty t))
      :transition :blocked
      :effects (
        (stores-block-reason t)
        (appends-information-gaps :if-required-knowledge))
      :response (:success :state))

    (query-status
      :pattern ()
      :intent "Return comprehensive status including children and blockers"
      :validation ()
      :response (:state :progress :blockers :children-status)))

  ;;;; FINITE STATE MACHINE
  ;;;; Task lifecycle states and transitions

  :fsm (fsm 'task-lifecycle
    :states '(
      (created
        :intent "Task initialized, not yet ready to execute"
        :accepts (get observe update link unlink delete start create-task eval query-status)
        :initial t)

      (ready
        :intent "Prerequisites satisfied, can be started"
        :accepts (get observe update link unlink delete start create-task eval query-status))

      (active
        :intent "Task is being actively worked on"
        :accepts (get observe update link unlink delete create-task eval complete block query-status))

      (blocked
        :intent "Task cannot proceed due to blocker"
        :accepts (get observe update link unlink delete eval query-status)
        :note "Requires unblock message or external resolution")

      (completed
        :intent "Task finished successfully with all criteria met"
        :accepts (get observe query-status)
        :terminal t)

      (failed
        :intent "Task failed and cannot be recovered"
        :accepts (get observe query-status)
        :terminal t))

    :transitions '(
      ;; From created
      (created -> ready :on (dependencies-met)
        :guard (all-depends-on-edges-point-to-completed-tasks))
      (created -> active :on (start)
        :effects (sets-started-at))

      ;; From ready
      (ready -> active :on (start)
        :effects (sets-started-at))

      ;; From active
      (active -> blocked :on (block)
        :effects (stores-block-reason appends-information-gaps))
      (active -> completed :on (complete)
        :guard (eval-passes)
        :effects (sets-completed-at stores-result creates-produces-edges))
      (active -> failed :on (abort)
        :effects (stores-failure-reason))

      ;; From blocked
      (blocked -> ready :on (unblock)
        :guard (blocker-resolved))
      (blocked -> failed :on (abort timeout)
        :effects (stores-failure-reason)))))

;;;; ============================================================================
;;;; EDGE TYPE DEFINITIONS
;;;; ============================================================================

(edge-types 'task-edges
  :types '(
    (spawned-by
      :from :task
      :to :task
      :intent "Child task was spawned by parent task"
      :cardinality :many-to-one)

    (depends-on
      :from :task
      :to :task
      :intent "Task depends on another task to complete first"
      :cardinality :many-to-many)

    (requires-knowledge
      :from :task
      :to :knowledge
      :intent "Task requires this knowledge to proceed"
      :cardinality :many-to-many)

    (produces
      :from :task
      :to :artifact
      :intent "Task produced this artifact as output"
      :cardinality :one-to-many)

    (blocks
      :from :task
      :to :task
      :intent "This task is blocking another task"
      :cardinality :many-to-many)

    (references
      :from :any
      :to :any
      :intent "Generic reference between nodes"
      :cardinality :many-to-many)))

;;;; ============================================================================
;;;; PRIORITY LEVELS
;;;; ============================================================================

(enumeration 'priority-level
  :values '(
    (0 :name :critical :description "Immediate attention required")
    (1 :name :high :description "Should be addressed soon")
    (2 :name :medium :description "Normal priority (default)")
    (3 :name :low :description "Can be deferred")
    (4 :name :backlog :description "Nice to have")))

;;;; ============================================================================
;;;; PROGRESS CALCULATION
;;;; ============================================================================

(derived-property 'progress
  :applies-to :task
  :type :number
  :range (0 1)
  :computation '(
    (cond
      ((eq state :completed) 1)
      ((eq state :created) 0)
      ((null children)
        (case state
          (:active 0.5)
          (otherwise 0.1)))
      (t
        (/ (reduce #'+ (mapcar #'progress children)) (length children))))))

;;;; ============================================================================
;;;; INVARIANTS
;;;; ============================================================================

(invariant 'task-identity
  :for :task
  :property "Task ID is unique and follows pattern"
  :condition '(and
    (matches id "task_[0-9]+")
    (unique id :in graph)))

(invariant 'state-machine
  :for :task
  :property "State is valid and transitions follow FSM"
  :condition '(member state '(:created :ready :active :blocked :completed :failed)))

(invariant 'hierarchy
  :for :task
  :property "If parent-task-id exists, spawned-by edge exists"
  :condition '(implies
    parent-task-id
    (exists-edge id parent-task-id :spawned-by)))

(invariant 'progress-bounds
  :for :task
  :property "Progress is always between 0 and 1"
  :condition '(and
    (>= progress 0)
    (<= progress 1)))

(invariant 'completion
  :for :task
  :property "Completed tasks have completed-at and result"
  :condition '(implies
    (eq state :completed)
    (and completed-at result)))

(invariant 'created-state
  :for :task
  :property "Created tasks have progress 0"
  :condition '(implies
    (eq state :created)
    (= progress 0)))

(invariant 'completed-state
  :for :task
  :property "Completed tasks have progress 1"
  :condition '(implies
    (eq state :completed)
    (= progress 1)))

;;;; ============================================================================
;;;; FACTORY PATTERN
;;;; ============================================================================

(factory 'task-actor-factory
  :creates :task-actor
  :parameters '(
    (goal :required t :type :string)
    (desired-deliverables :required t :type :list)
    (objective-success-criteria :required t :type :list)
    (subjective-success-criteria :required nil :type :list)
    (information-gaps :required nil :type :list :default ())
    (tools-available :required nil :type :list :default ())
    (parent-task-id :required nil :type :string)
    (labels :required nil :type :list)
    (priority :required nil :type :number :default 2)
    (graph :required t :type :graph))
  :process '(
    (let ((id (generate-task-id)))
      (let ((properties (make-task-properties
        :id id
        :type :task
        :created-at (now)
        :state :created
        :goal goal
        :desired-deliverables desired-deliverables
        :objective-success-criteria objective-success-criteria
        :subjective-success-criteria subjective-success-criteria
        :known-information ()
        :information-gaps (or information-gaps ())
        :tools-available (or tools-available ())
        :parent-task-id parent-task-id
        :labels labels
        :priority priority)))
        (let ((actor (make-actor
          :send (lambda (message)
            (handle-message message properties graph)))))
          (let ((address (register-actor actor (get-system graph))))
            (register-node id address properties graph)
            address))))))

;;;; ============================================================================
;;;; MESSAGE HANDLERS
;;;; ============================================================================

(message-handler 'handle-get
  :message :get
  :implementation '(
    (make-get-response
      :id (task-id properties)
      :type (task-type properties)
      :properties properties
      :edges (get-all-edges (task-id properties) graph))))

(message-handler 'handle-observe
  :message :observe
  :implementation '(
    (let ((child-ids (get-child-tasks (task-id properties) graph)))
      (make-observe-response
        :state (task-state properties)
        :observations (generate-observations properties child-ids graph)
        :metadata (make-metadata
          :progress (calculate-progress properties graph)
          :child-count (length child-ids))))))

(message-handler 'handle-start
  :message :start
  :precondition '(member (task-state properties) '(:created :ready))
  :implementation '(
    (setf (task-state properties) :active)
    (setf (task-started-at properties) (now))
    (when context
      (setf (task-context properties) context))
    (make-start-response :success t :state :active)))

(message-handler 'handle-eval
  :message :eval
  :implementation '(
    (let ((criteria-results (evaluate-criteria properties)))
      (let ((score (/ (count-if #'criterion-passed criteria-results)
                      (max 1 (length criteria-results)))))
        (let ((children-complete (all-children-completed properties graph)))
          (make-eval-response
            :score score
            :passed (and (= score 1) children-complete)
            :objective-criteria criteria-results
            :subjective-criteria (task-subjective-criteria properties)
            :observations (generate-eval-observations score children-complete)))))))

(message-handler 'handle-complete
  :message :complete
  :precondition '(eval-passes properties graph)
  :implementation '(
    (setf (task-state properties) :completed)
    (setf (task-completed-at properties) (now))
    (setf (task-result properties) result)
    (when artifacts
      (dolist (artifact-id artifacts)
        (add-edge (task-id properties) artifact-id :produces graph)))
    (make-complete-response :success t :final-state :completed)))

(message-handler 'handle-block
  :message :block
  :implementation '(
    (setf (task-state properties) :blocked)
    (setf (task-block-reason properties) reason)
    (when required-knowledge
      (setf (task-information-gaps properties)
        (append (task-information-gaps properties) required-knowledge)))
    (make-block-response :success t :state :blocked)))

(message-handler 'handle-query-status
  :message :query-status
  :implementation '(
    (let ((child-ids (get-child-tasks (task-id properties) graph)))
      (make-status-response
        :state (task-state properties)
        :progress (calculate-progress properties graph)
        :blockers (detect-blockers properties graph)
        :children-status (map-children-status child-ids graph)))))

;;;; ============================================================================
;;;; INTEGRATION CONTRACTS
;;;; ============================================================================

(integration 'graph-integration
  :requires '(graph)
  :provides '(
    (register-node :signature (id address properties graph) -> void)
    (get-node :signature (id graph) -> address)
    (get-node-properties :signature (id graph) -> properties)
    (add-edge :signature (from-id to-id type graph) -> edge)
    (remove-edge :signature (edge-id graph) -> boolean)
    (get-child-tasks :signature (task-id graph) -> list)
    (get-all-edges :signature (node-id graph) -> list))
  :invariants '(
    (dual-registration "Tasks registered in both System and Graph")
    (id-address-mapping "Graph maintains string ID to Address mapping")))

(integration 'actor-system-integration
  :requires '(system address)
  :provides '(
    (register :signature (actor system) -> address)
    (send :signature (message address) -> response))
  :invariants '(
    (address-send-equivalence "address.send(msg) === system.send(address, msg)")))

;;;; ============================================================================
;;;; END
;;;; ============================================================================
