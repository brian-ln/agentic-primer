;; System Lifecycle & Bootstrap Model
;; Defines how the SEAG "Big Bang" occurs

(system SEAG-Lifecycle
  (actors
    ;; RootSupervisor: The "Guardian" actor that restarts failed children.
    (actor RootSupervisor
      (state (children map) (status enum 'booting 'running 'degraded))
      (behavior
        (on start ()
          (log "SEAG Bootstrap Initiated")
          ;; 1. Spawn System Services (Permanent)
          (spawn-permanent 'seag://system/event-log EventLogActor)
          (spawn-permanent 'seag://system/projector GraphProjector)
          (spawn-permanent 'seag://system/parser DocumentParser)
          (spawn-permanent 'seag://system/file-io FileEffectActor)
          
          ;; 2. Spawn Local Agents (Transient)
          (spawn 'seag://local/user-proxy UserProxy)
          (spawn 'seag://system/brain BrainAgent)
          
          (set-status 'running))

        (on child-crashed (child_id reason)
          (log-error child_id reason)
          ;; Restart permanent actors automatically
          (if (is-permanent child_id)
            (spawn-permanent child_id (get-class child_id))))))

    ;; System Kernel: The underlying message dispatcher
    (actor Kernel
      (behavior
        (on dispatch (target msg)
          (if (actor-exists? target)
            (deliver target msg)
            (send 'seag://system/dead-letter-log 'undelivered {target: target, message: msg})))
        
        (on record-event (target msg)
          ;; Every mutator is captured by the EventLog
          (if (is-mutator? msg)
            (send 'seag://system/event-log 'append {source: target, type: msg.type, payload: msg.payload})))))))
