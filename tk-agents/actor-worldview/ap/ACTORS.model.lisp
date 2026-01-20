;; SEAG Actor Model Definitions
;; Using Lisp-style S-expressions for formal modeling

(defprotocol Inference
  "Interface for Large Language Models."
  (on PROMPT (text params)
    "Params may include 'model' and optional 'provider' (e.g. 'vertex', 'studio')."
    (or
      (one RESPONSE (text))
      (one ERROR (message)))))

(defprotocol Embedding
  "Interface for Vector Embedding models."
  (on EMBED (text)
    (or
      (one VECTOR (floats))
      (one ERROR (message)))))

(defprotocol ModelRegistry
  "Interface for managing and discovering model providers."
  (on REGISTER_PROVIDER (model_id actor_address type)
    (one REGISTER_OK ()))
  (on GET_PROVIDER (model_id)
    (or
      (one PROVIDER_INFO (actor_address type))
      (one ERROR (message))))
  (on LIST_MODELS (type)
    (one MODEL_LIST (models))))

(system SEAG
  (protocols
    (protocol BaseNode
      (properties
        (isolation (enum 'cloned 'shared)) ; 'cloned' implies structuredClone
        (address-family (enum 'local 'remote))
        (max-hops int) ; Defaults to 100 for loop avoidance
        (trace-id uuid)) ; For observability and causality tracking
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

    ;; AI Boundary Actors
    (actor VertexInferenceActor
      (implements Inference)
      (description "Bridges to Google Cloud Vertex AI (Enterprise).")
      (behavior))

    (actor StudioInferenceActor
      (implements Inference)
      (description "Bridges to Google AI Studio (Hobbyist/Dev).")
      (behavior))

    (actor GeminiEmbeddingActor
      (implements Embedding)
      (behavior))

    (actor GeminiDiscoveryActor
      (description "Programmatically retrieves available Gemini model IDs.")
      (behavior
        (on refresh-models () (http-get models))
        (on get-latest-model (type) (find-best-match type))))

    ;; The Router (Virtual Stable Identity)
    (actor InferenceRouter
      (implements Inference)
      (implements ModelRegistry)
      (state (routing_table map))
      (behavior
        (on prompt (text params)
          ;; Routing Logic:
          ;; 1. If params.provider is set, look up by "provider:model"
          ;; 2. Else look up by "model"
          ;; 3. Fallback to default
          (let ((target (get-route (or (concat params.provider ":" params.model) params.model))))
            (delegate target)))
        (on register-provider (model_id addr type)
          (set routing_table model_id addr))))

    ;; The Projector (Maintains the graph view)
    (actor GraphProjector
      (behavior
        (on append (event) (project event))
        (on link-to (payload) (create-edge payload))
        (on create-edge (payload) (create-edge payload))
        (on update-state (payload) (update-node payload))
        (on query (payload) (run-query payload))))
  ) ; End actors

  (boundaries
    (boundary Web
      (wrapper RemoteApiNode (url)
        (mapping call (args) (http-post url args))))

    (boundary FileSystem
      (wrapper FileNode (path)
        (mapping get () (file-read path))
        (mapping patch (data) (file-write path))))))
