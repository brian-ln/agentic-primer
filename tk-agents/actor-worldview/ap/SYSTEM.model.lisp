;; System Lifecycle & Bootstrap Model
;; Defines how the SEAG "Big Bang" occurs

(system SEAG-Lifecycle
  (actors
    ;; The Root Supervisor: The first actor in any Physical Runtime
    (actor RootSupervisor
      (state (children map) (status enum 'booting 'running 'degraded))
      (behavior
        (on start ()
          (log "SEAG Bootstrap Initiated")
          ;; 1. Spawn the "Truth"
          (let ((log_svc (spawn-permanent EventLogActor)))
            (register 'seag://system/event-log log_svc))
          
          ;; 2. Spawn the "Brain"
          (let ((router (spawn-permanent RouterActor)))
            (register 'seag://system/router router))
          
          ;; 3. Spawn the "Persistence"
          (let ((pm (spawn-permanent PersistenceManager)))
            (register 'seag://system/persistence pm))
          
          (set-status 'running))

        (on child-crashed (child_id reason)
          (log-error child_id reason)
          (match (get-restart-policy child_id)
            ('permanent (spawn-new-instance child_id))
            ('transient (ignore))
            ('virtual (wait-for-next-reference child_id))))))

    ;; Router Actor: The dynamic registry
    (actor RouterActor
      (state (routes map)) ; Logical URI -> Physical Address
      (behavior
        (on resolve (uri)
          (reply (get routes uri)))
        (on update-route (uri address)
          (set routes uri address)
          (emit-signal 'RouteUpdated {uri address}))))))
