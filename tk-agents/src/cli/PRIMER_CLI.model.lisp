;;;; PRIMER_CLI - DSL Model
;;;; Unified CLI wrapper with routing to subsystem CLIs
;;;; Following System Modeling Protocol

(defpackage :primer-cli-model
  (:use :cl :system-modeling-protocol)
  (:export #:primer-cli))

;;; ============================================================================
;;; COMPONENT DEFINITION
;;; ============================================================================

(behavior 'primer-cli
  :intent "Unified CLI interface routing to Task, Graph, and Knowledge subsystems"
  :foundation :command-router
  :category :user-interface
  :tags '(cli routing meta-cli unified-interface integration)

  ;;; ========================================================================
  ;;; STATE SCHEMA
  ;;; ========================================================================

  :state-schema '(
    ;; Routing state
    (subsystem :type :keyword :required nil
               :enum (task graph knowledge)
               :description "Target subsystem for routing")
    (command :type :string :required nil
             :description "Command within subsystem")
    (arguments :type (list string) :required nil
               :description "Arguments for subsystem command")

    ;; Global configuration
    (global-flags :type :object :required nil
                  :description "Flags applicable across all subsystems")
    (aliases :type :object :required nil
             :default ((k . knowledge))
             :description "Subsystem aliases")

    ;; Execution state
    (exit-code :type :number :required nil :default 0
               :description "Process exit code"))

  ;;; ========================================================================
  ;;; COMMAND DEFINITIONS
  ;;; ========================================================================

  :accepts '(
    ;; Meta commands
    (help :pattern (&optional subsystem)
          :intent "Show help for Primer or specific subsystem"
          :validation ((valid-subsystem subsystem :allow-nil t))
          :effects ((generate-help subsystem)
                    (display-help))
          :output "{help-text}")

    (version :pattern ()
             :intent "Show version information"
             :effects ((gather-version-info)
                       (format-version-display))
             :output "{version-info}")

    ;; Routing commands
    (route-task :pattern (command &rest args)
                :intent "Route command to Task CLI"
                :subsystem :task
                :validation ((valid-task-command command))
                :effects ((extract-global-flags args)
                          (delegate-to-task-cli command args global-flags))
                :output "{task-cli-output}")

    (route-graph :pattern (command &rest args)
                 :intent "Route command to Graph CLI"
                 :subsystem :graph
                 :validation ((valid-graph-command command))
                 :effects ((extract-global-flags args)
                           (delegate-to-graph-cli command args global-flags))
                 :output "{graph-cli-output}")

    (route-knowledge :pattern (command &rest args)
                     :intent "Route command to Knowledge CLI"
                     :subsystem :knowledge
                     :validation ((valid-knowledge-command command))
                     :effects ((extract-global-flags args)
                               (delegate-to-knowledge-cli command args global-flags))
                     :output "{knowledge-cli-output}"))

  ;;; ========================================================================
  ;;; FINITE STATE MACHINE - ROUTING
  ;;; ========================================================================

  :fsm (fsm 'routing-fsm
    :states '(
      (idle :intent "Awaiting command invocation"
            :accepts ())

      (parsing-subsystem :intent "Parsing subsystem identifier"
                         :accepts (help version task graph knowledge))

      (resolving-alias :intent "Resolving subsystem aliases"
                       :accepts ())

      (parsing-command :intent "Parsing command within subsystem"
                       :accepts ())

      (extracting-flags :intent "Extracting global flags"
                        :accepts ())

      (delegating :intent "Delegating to subsystem CLI"
                  :accepts ())

      (executing :intent "Executing subsystem command"
                 :accepts ())

      (outputting :intent "Displaying results"
                  :accepts ())

      (exiting :intent "Process termination"
               :accepts ()))

    :transitions '(
      (idle -> parsing-subsystem :on command-invoked)
      (parsing-subsystem -> exiting :on no-subsystem :exit-code 1)
      (parsing-subsystem -> resolving-alias :on subsystem-parsed)
      (resolving-alias -> parsing-command :on alias-resolved)
      (parsing-command -> exiting :on no-command :exit-code 1)
      (parsing-command -> extracting-flags :on command-parsed)
      (extracting-flags -> delegating :on flags-extracted)
      (delegating -> executing :on delegation-complete)
      (executing -> outputting :on execution-complete)
      (executing -> exiting :on execution-error :exit-code 1)
      (outputting -> exiting :on output-complete :exit-code 0))))

;;; ============================================================================
;;; COMMAND INTERFACE DETAILS
;;; ============================================================================

(defcommand 'help
  :synopsis "primer help [subsystem]"
  :description "Show help for Primer or specific subsystem"
  :arguments '(
    (subsystem :type string :optional t :enum (task graph knowledge)))
  :sections '(
    (overview :condition (null subsystem) :content primer-overview)
    (subsystems :condition (null subsystem) :content subsystem-list)
    (examples :condition (null subsystem) :content primer-examples)
    (subsystem-help :condition subsystem :content subsystem-specific-help)))

(defcommand 'version
  :synopsis "primer version"
  :description "Show version information"
  :output-format :tree
  :components '(primer-version task-version graph-version knowledge-version
                system-info node-version bun-version))

(defcommand 'route
  :synopsis "primer <subsystem> <command> [args...] [options]"
  :description "Route command to appropriate subsystem CLI"
  :subsystems '(
    (task :cli task-cli :file "task.ts")
    (graph :cli graph-cli :file "graph.ts")
    (knowledge :cli knowledge-cli :file "knowledge.ts" :alias k))
  :delegation-strategy :direct-function-call
  :fallback :process-spawn)

;;; ============================================================================
;;; DATA TYPE DEFINITIONS
;;; ============================================================================

(deftype 'subsystem-identifier
  :enum '(task graph knowledge)
  :aliases '((k . knowledge))
  :description "Valid subsystem identifiers")

(deftype 'global-flags
  :schema '((json :type boolean :default nil
                  :description "Output in JSON format")
            (yes :type boolean :default nil
                 :description "Auto-confirm all prompts"))
  :extraction-pattern '("--json" "--yes"))

(deftype 'routing-result
  :schema '((subsystem :type subsystem-identifier :required t)
            (command :type string :required t)
            (arguments :type (list string) :required t)
            (global-flags :type global-flags :required t)))

(deftype 'delegation-strategy
  :enum '(direct-function-call process-spawn)
  :default 'direct-function-call
  :fallback 'process-spawn)

(deftype 'help-format
  :sections '(
    (overview :type string)
    (usage :type string)
    (subsystems :type (list subsystem-description))
    (global-options :type (list option-description))
    (examples :type (list example))
    (footer :type string)))

(deftype 'version-info
  :schema '((primer-version :type string :required t)
            (task-version :type string :required t)
            (graph-version :type string :required t)
            (knowledge-version :type string :required t)
            (system :type string :required t)
            (runtime :type object :required t)))

;;; ============================================================================
;;; VALIDATION RULES
;;; ============================================================================

(defvalidation 'valid-subsystem
  :condition (or (null subsystem)
                 (member subsystem '("task" "graph" "knowledge" "k")
                         :test #'string-equal))
  :error "Unknown subsystem \"{subsystem}\". Valid subsystems: task, graph, knowledge (k)")

(defvalidation 'subsystem-specified
  :condition (not (null subsystem))
  :error "No subsystem specified\n\nUsage: primer <subsystem> <command> [args...]\n\nUse 'primer help' for more information.")

(defvalidation 'command-specified
  :condition (not (null command))
  :error "No command specified for {subsystem} subsystem\n\nUsage: primer {subsystem} <command> [args...]\n\nUse 'primer help {subsystem}' for available commands.")

(defvalidation 'valid-task-command
  :condition (member command '("init" "add" "update" "delete" "list" "ready"
                              "show" "status" "eval" "graph" "search")
                     :test #'string-equal)
  :error "Unknown task command: {command}\n\nUse 'primer help task' for available commands.")

(defvalidation 'valid-graph-command
  :condition (member command '("init" "create-node" "delete-node" "create-edge"
                              "delete-edge" "list-nodes" "list-edges" "show"
                              "show-graph" "export" "import")
                     :test #'string-equal)
  :error "Unknown graph command: {command}\n\nUse 'primer help graph' for available commands.")

(defvalidation 'valid-knowledge-command
  :condition (member command '("init" "add" "get" "list" "append" "update"
                              "delete" "query" "search" "link" "unlink" "synthesize")
                     :test #'string-equal)
  :error "Unknown knowledge command: {command}\n\nUse 'primer help knowledge' for available commands.")

;;; ============================================================================
;;; ROUTING OPERATIONS
;;; ============================================================================

(defoperation 'parse-arguments
  :intent "Parse command-line arguments into routing structure"
  :steps '(
    (extract-subsystem args.0)
    (resolve-alias subsystem)
    (extract-command args.1)
    (extract-remaining-args args.slice(2))
    (extract-global-flags remaining-args))
  :returns routing-result)

(defoperation 'resolve-alias
  :intent "Convert subsystem aliases to canonical names"
  :mappings '((k . knowledge))
  :steps '(
    (if (has-alias subsystem)
        (return (resolve-alias subsystem))
        (return subsystem))))

(defoperation 'extract-global-flags
  :intent "Extract and remove global flags from arguments"
  :global-flags '(--json --yes)
  :steps '(
    (initialize flags (json nil yes nil))
    (for-each arg arguments
      (cond
        ((string= arg "--json") (setf (getf flags :json) t))
        ((string= arg "--yes") (setf (getf flags :yes) t))))
    (remove-flags-from-arguments arguments global-flags)
    (return flags)))

(defoperation 'delegate-to-subsystem
  :intent "Delegate command execution to subsystem CLI"
  :strategy :direct-function-call
  :steps '(
    (load-subsystem-module subsystem)
    (extract-command-handler subsystem command)
    (apply-global-flags handler global-flags)
    (invoke-handler handler arguments)
    (capture-output)
    (return result)))

;;; ============================================================================
;;; HELP GENERATION
;;; ============================================================================

(defoperation 'generate-help
  :intent "Generate help text for Primer or subsystem"
  :cases '(
    (primer-help :condition (null subsystem)
                 :sections (overview usage subsystems global-options examples footer))
    (subsystem-help :condition subsystem
                    :sections (subsystem-name usage commands global-options reference))))

(defdisplay 'primer-help-overview
  :format "Primer CLI - Unified interface for tk-agents\n\nUsage: primer <subsystem> <command> [args...] [options]")

(defdisplay 'subsystem-list
  :format "Subsystems:\n  {subsystem-name}  {subsystem-description}"
  :subsystems '(
    (task . "Manage tasks with dependencies and priorities")
    (graph . "Low-level graph manipulation")
    (knowledge . "Manage versioned knowledge with querying")))

(defdisplay 'global-options-list
  :format "Global Options:\n  {flag}  {description}"
  :options '(
    (--json . "Output in JSON format (where supported)")
    (--yes . "Auto-confirm all prompts")))

(defdisplay 'primer-examples
  :format "Examples:\n  {example-command}\n  {example-description}"
  :examples '(
    ("primer task add \"My task\" --priority P0" . "Create high-priority task")
    ("primer graph list-nodes --type task" . "List all task nodes")
    ("primer knowledge search \"API design\"" . "Search knowledge")
    ("primer k query knowledge_1 \"What?\"" . "Query using short alias")))

(defdisplay 'subsystem-help
  :format "Primer - {subsystem-title} Subsystem\n\nUsage: primer {subsystem} <command> [args...] [options]\n\nCommands:\n{command-list}\n\nGlobal Options:\n{global-options}\n\nSee full documentation: {spec-file}")

;;; ============================================================================
;;; VERSION DISPLAY
;;; ============================================================================

(defoperation 'gather-version-info
  :intent "Collect version information from all components"
  :steps '(
    (get-primer-version)
    (get-task-version)
    (get-graph-version)
    (get-knowledge-version)
    (get-system-info)
    (get-runtime-info))
  :returns version-info)

(defdisplay 'version-tree
  :format "Primer CLI v{primer-version}\n├─ Task CLI v{task-version}\n├─ Graph CLI v{graph-version}\n└─ Knowledge CLI v{knowledge-version}\n\nSystem: {system}\nNode: v{node-version}\nBun: {bun-version}")

;;; ============================================================================
;;; INTEGRATION CONTRACTS
;;; ============================================================================

(defcontract 'task-cli-integration
  :partner :task-cli
  :delegation-method :direct-function-call
  :interface '(
    (execute-command :signature (command args global-flags) :returns result))
  :commands '(init add update delete list ready show status eval graph search))

(defcontract 'graph-cli-integration
  :partner :graph-cli
  :delegation-method :direct-function-call
  :interface '(
    (execute-command :signature (command args global-flags) :returns result))
  :commands '(init create-node delete-node create-edge delete-edge
              list-nodes list-edges show show-graph export import))

(defcontract 'knowledge-cli-integration
  :partner :knowledge-cli
  :delegation-method :direct-function-call
  :interface '(
    (execute-command :signature (command args global-flags) :returns result))
  :commands '(init add get list append update delete query search
              link unlink synthesize))

;;; ============================================================================
;;; ERROR HANDLING
;;; ============================================================================

(deferror-handling 'routing-errors
  :strategy :exit-with-message
  :format "Error: {message}\n\n{usage-hint}"
  :exit-codes '(
    (success . 0)
    (unknown-subsystem . 1)
    (missing-subsystem . 1)
    (missing-command . 1)
    (delegation-error . 1))
  :usage-hints '(
    (unknown-subsystem . "Use 'primer help' for usage information.")
    (missing-subsystem . "Use 'primer help' for more information.")
    (missing-command . "Use 'primer help {subsystem}' for available commands.")))

(deferror-handling 'subsystem-errors
  :strategy :pass-through
  :description "Errors from subsystem CLIs are passed through transparently")

;;; ============================================================================
;;; ROUTING INVARIANTS
;;; ============================================================================

(assert (forall ((cmd Command))
  (=> (routed cmd)
      (valid-subsystem (command-subsystem cmd))))
  :name "valid-subsystem-routing"
  :description "All routed commands must target valid subsystems")

(assert (forall ((cmd Command))
  (=> (routed cmd)
      (or (meta-command cmd)
          (and (subsystem-specified cmd)
               (command-specified cmd)))))
  :name "complete-routing"
  :description "Non-meta commands must have subsystem and command")

(assert (forall ((alias Alias))
  (=> (defined alias)
      (resolves-to-valid-subsystem alias)))
  :name "valid-aliases"
  :description "All aliases must resolve to valid subsystems")

(assert (forall ((flag GlobalFlag))
  (=> (extracted flag)
      (removed-from-subsystem-args flag)))
  :name "flag-extraction"
  :description "Global flags must be removed before delegation")

;;; ============================================================================
;;; DELEGATION SEMANTICS
;;; ============================================================================

(defsemantics 'delegation-strategy
  :primary :direct-function-call
  :fallback :process-spawn
  :rationale "Direct calls are faster; process spawn for isolation")

(defsemantics 'flag-propagation
  :global-flags '(--json --yes)
  :propagation-method :parameter-passing
  :subsystem-handling :subsystem-specific)

(defsemantics 'output-handling
  :strategy :pass-through
  :format-preservation :maintain-subsystem-format
  :stream :stdout)

(defsemantics 'alias-resolution
  :aliases '((k . knowledge))
  :resolution-time :routing-phase
  :case-sensitivity :case-insensitive)

;;; ============================================================================
;;; PERFORMANCE CHARACTERISTICS
;;; ============================================================================

(defperformance 'routing-overhead
  :startup-time "< 10ms"
  :routing-time "< 1ms"
  :delegation-overhead :minimal
  :memory-usage :subsystem-module-only)

(defperformance 'execution-model
  :process-model :single-process
  :delegation :direct-function-call
  :module-loading :lazy-load-on-route)

;;; ============================================================================
;;; FUTURE EXTENSIONS
;;; ============================================================================

(defextension 'command-aliases
  :intent "Top-level aliases for common commands"
  :examples '(
    ("primer add" . "primer task add")
    ("primer search" . "smart search across subsystems")))

(defextension 'interactive-mode
  :intent "Interactive shell for Primer"
  :trigger "primer shell"
  :features '(persistent-session command-history subsystem-switching))

(defextension 'configuration-file
  :intent "User configuration for Primer"
  :files '(".primerrc" "primer.config.json")
  :options '(default-subsystem aliases global-flags))

(defextension 'pipeline-support
  :intent "Pipe data between subsystems"
  :examples '(
    ("primer task list | primer task update" . "bulk operations")
    ("primer knowledge search | primer task add" . "task from search")))

;;; ============================================================================
;;; END
;;; ============================================================================
