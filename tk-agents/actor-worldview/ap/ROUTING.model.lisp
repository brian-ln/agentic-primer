;; Routing & Transport Model

(system Distribution
  (actors
    ;; The Gateway: The boundary of a Physical Runtime
    (actor GatewayRelay
      (state
        (local_registry map) ; UUID -> Local Actor Ref
        (remote_registry map) ; Logical System -> Remote Gateway URI
        (transports map))    ; Protocol -> Transport Actor
      (behavior
        (on send (target_uri message)
          (let ((dest (parse-uri target_uri)))
            (if (is-local? dest)
                (dispatch-local dest message)
                (dispatch-remote dest message))))))

    ;; Transport Actor: Encapsulates the wire protocol (WS, TCP, Unix Socket)
    (actor Transport
      (properties (protocol (enum 'websocket 'grpc 'ipc)))
      (behavior
        (on push (payload)
          (perform-io protocol payload))))

    ;; Entanglement Manager: Manages the "Shadow" state across boundaries
    (actor EntanglementManager
      (state (shadow_actors map))
      (behavior
        (on sync (delta)
          (for-each shadow (get shadow_actors delta.source)
            (send shadow 'apply-delta delta))))))

  (patterns
    ;; Fusion: Optimization to move actors together
    (optimization ActorFusion
      (trigger (latency_above_ms 50))
      (action (migrate_actor source_id destination_runtime)))))
