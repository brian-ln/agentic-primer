;; SEAG Actor Model Definitions
;; Using Lisp-style S-expressions for formal modeling

(system SEAG
  (protocols
    (protocol BaseNode
      (message inspect () (returns NodeSpec))
      (message watch (filter) (returns SubscriptionID))
      (message unwatch (sub-id) (returns Boolean)))

    (protocol DataNode
      (extends BaseNode)
      (message get (keys) (returns ValueMap))
      (message patch (data) (returns EventID)))

    (protocol ExecNode
      (extends BaseNode)
      (message call (args) (returns Result))
      (message status () (returns StateEnum))))

  (actors
    ;; Information Node (Data Centric)
    (actor InformationNode
      (implements DataNode)
      (state (data map) (version int))
      (behavior
        (on patch (new-data)
          (update-state data new-data)
          (emit-event 'NodeUpdated self new-data))))

    ;; Algorithmic Node (Automata)
    (actor AlgorithmicNode
      (implements ExecNode)
      (properties
        (determinism (enum 'deterministic 'non-deterministic))
        (runtime (enum 'in-process 'shell 'remote-api 'inference)))
      (behavior
        (on call (args)
          (execute-internal args))))

    ;; Edge Actor
    (actor GraphEdge
      (implements BaseNode)
      (state
        (from-node address)
        (to-node address)
        (type string))
      (behavior
        (on traverse (direction)
          (if (eq direction 'forward) to-node from-node))))

    ;; The Projector (Maintains the graph view)
    (actor GraphProjector
      (behavior
        (on event (ev)
          (match ev
            ((NodeUpdated id data) (update-index id data))
            ((EdgeCreated id from to type) (create-edge-index id from to type)))))))

  (boundaries
    (boundary Web
      (wrapper RemoteApiNode (url)
        (mapping call (args) (http-post url args))))

    (boundary FileSystem
      (wrapper FileNode (path)
        (mapping get () (file-read path))
        (mapping patch (data) (file-write path))))))
