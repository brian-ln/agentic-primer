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
          ;; Trigger Autonomous Troubleshooting Loop (Tier 2)
          (let ((diag (spawn DiagnosisAgent child_id reason)))
            (subscribe diag 'resolution-failed 
              (lambda (msg) (escalate-to-human child_id msg)))))))

    ;; DiagnosisAgent: Autonomous Troubleshooter (Tier 2)
    (actor DiagnosisAgent
      (state (target_id address) (error any))
      (behavior
        (on start ()
          (let ((diagnosis (analyze-system-state target_id error)))
            (match diagnosis
              ('recoverable (perform-repair target_id))
              ('irrecoverable (emit-signal 'resolution-failed error)))))))

    ;; Router Actor: The dynamic registry
    (actor RouterActor
      (state (routes map)) ; Logical URI -> Physical Address
      (behavior
        (on resolve (uri)
          (reply (get routes uri)))
        (on update-route (uri address)
          (set routes uri address)
          (emit-signal 'RouteUpdated {uri address}))))))
