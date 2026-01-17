;;; Knowledge System - Formal Model
;;; Type definitions, contracts, and invariants for KnowledgeNode actors
;;; Following System Modeling Protocol

;; ============================================================================
;; Type Definitions
;; ============================================================================

(deftype KnowledgeId String
  "Unique identifier for knowledge nodes (e.g., 'knowledge_1')")

(deftype Version Integer
  "Monotonically increasing version number, starting at 1")

(deftype Source String
  "Attribution for knowledge content (e.g., 'Hewitt 1973')")

(deftype Content String
  "Text content of a knowledge node")

(deftype Keyword String
  "Search keyword extracted from queries (words > 3 chars)")

(deftype Confidence Float
  "Confidence score between 0.0 and 1.0")

(deftype NodeType
  (one-of "task" "knowledge" "artifact" "pattern"))

(deftype EdgeType
  (one-of "depends_on" "requires_knowledge" "produces" "spawned_by" "blocks" "references"))

(deftype KnowledgeProperties
  ((id KnowledgeId)
   (type NodeType)  ;; Always "knowledge"
   (createdAt Timestamp)
   (title String)
   (content Content)
   (sources (List Source))
   (version Version))
  "Properties of a KnowledgeNode")

(deftype Edge
  ((id String)
   (fromId String)
   (toId String)
   (type EdgeType)
   (properties (Map String Any)))
  "Edge connecting two nodes in the graph")

(deftype CreateKnowledgeOptions
  ((title String)
   (content (Optional Content))
   (sources (Optional (List Source))))
  "Options for creating a new KnowledgeNode")

;; ============================================================================
;; Message Types
;; ============================================================================

(deftype AppendPayload
  ((data Content)
   (source (Optional Source)))
  "Payload for append message")

(deftype QueryPayload
  ((question String)
   (context (Optional (Map String Any))))
  "Payload for query message")

(deftype SynthesizePayload
  ((fromNodes (List KnowledgeId)))
  "Payload for synthesize message")

(deftype LinkPayload
  ((toId String)
   (edgeType EdgeType)
   (properties (Optional (Map String Any))))
  "Payload for link message")

(deftype UnlinkPayload
  ((edgeId String))
  "Payload for unlink message")

(deftype UpdatePayload
  ((properties (Partial KnowledgeProperties)))
  "Payload for update message")

(deftype KnowledgeMessage
  (one-of
    ;; Standard messages
    ((type "get") (payload Empty))
    ((type "observe") (payload Empty))
    ((type "update") (payload UpdatePayload))
    ((type "link") (payload LinkPayload))
    ((type "unlink") (payload UnlinkPayload))
    ((type "delete") (payload Empty))
    ;; Knowledge-specific messages
    ((type "append") (payload AppendPayload))
    ((type "query") (payload QueryPayload))
    ((type "synthesize") (payload SynthesizePayload)))
  "All message types accepted by KnowledgeNode")

;; ============================================================================
;; Response Types
;; ============================================================================

(deftype GetResponse
  ((id KnowledgeId)
   (type NodeType)
   (properties KnowledgeProperties)
   (edges (List Edge)))
  "Response to get message")

(deftype ObserveResponse
  ((state String)
   (observations (List String))
   (metadata (Map String Any)))
  "Response to observe message")

(deftype AppendResponse
  ((success Boolean)
   (version Version))
  "Response to append message")

(deftype QueryResponse
  ((answer String)
   (confidence Confidence)
   (sources (List Source)))
  "Response to query message")

(deftype SynthesizeResponse
  ((synthesis String)
   (sources (List Source)))
  "Response to synthesize message")

(deftype LinkResponse
  ((edgeId String)
   (success Boolean))
  "Response to link message")

(deftype UnlinkResponse
  ((success Boolean))
  "Response to unlink/delete message")

(deftype UpdateResponse
  ((success Boolean)
   (updatedProperties (List String)))
  "Response to update message")

;; ============================================================================
;; Data Invariants
;; ============================================================================

(invariant knowledge-type-immutable
  "Knowledge nodes always have type 'knowledge'"
  (forall ((k KnowledgeProperties))
    (= k.type "knowledge")))

(invariant version-positive
  "Version is always positive (>= 1)"
  (forall ((k KnowledgeProperties))
    (>= k.version 1)))

(invariant version-monotonic
  "Version only increases, never decreases"
  (forall ((k KnowledgeProperties) (t1 Time) (t2 Time))
    (implies (< t1 t2)
      (<= (version-at k t1) (version-at k t2)))))

(invariant sources-append-only
  "Sources list only grows, elements are never removed"
  (forall ((k KnowledgeProperties) (t1 Time) (t2 Time))
    (implies (< t1 t2)
      (subset (sources-at k t1) (sources-at k t2)))))

(invariant id-immutable
  "KnowledgeNode ID never changes after creation"
  (forall ((k KnowledgeProperties) (t1 Time) (t2 Time))
    (= (id-at k t1) (id-at k t2))))

(invariant id-format
  "KnowledgeNode IDs follow knowledge_N pattern"
  (forall ((k KnowledgeProperties))
    (matches k.id #/^knowledge_[0-9]+$/)))

(invariant confidence-bounded
  "Query confidence is always between 0 and 1"
  (forall ((r QueryResponse))
    (and (>= r.confidence 0.0) (<= r.confidence 1.0))))

(invariant content-accumulation
  "Append always adds data to content"
  (forall ((k KnowledgeProperties) (append AppendPayload))
    (implies (apply-append k append)
      (contains (content-after k append) append.data))))

;; ============================================================================
;; State Machine
;; ============================================================================

(defstate KnowledgeState
  (one-of :created :active :deleted)
  "Lifecycle states of a KnowledgeNode")

(defstatemachine knowledge-lifecycle
  "KnowledgeNode lifecycle state machine"

  (initial :created)

  (states
    ((:created
      :description "Node instantiated but not yet registered"
      :accepts ())

     (:active
      :description "Node registered in graph, accepting messages"
      :accepts (get observe update link unlink delete append query synthesize))

     (:deleted
      :description "Node removed from graph"
      :accepts ())))

  (transitions
    ((:created -> :active
      :on register
      :action (lambda (node graph) (graph.registerNode node)))

     (:active -> :deleted
      :on delete
      :action (lambda (node graph) (graph.removeNode node.id))))))

;; ============================================================================
;; Operation Contracts
;; ============================================================================

(defcontract create-knowledge
  "Create a new KnowledgeNode"
  (requires
    ((options CreateKnowledgeOptions)
     (graph Graph))
    (and
      (> (length options.title) 0)
      (graph-exists? graph)))
  (returns KnowledgeNode)
  (ensures
    (and
      (= result.properties.type "knowledge")
      (= result.properties.version 1)
      (= result.properties.title options.title)
      (= result.properties.content (or options.content ""))
      (= result.properties.sources (or options.sources []))
      (matches result.properties.id #/^knowledge_[0-9]+$/)
      (registered-in? result graph))))

(defcontract handle-append
  "Append content to knowledge node"
  (requires
    ((node KnowledgeNode)
     (payload AppendPayload)
     (graph Graph))
    (and
      (> (length payload.data) 0)
      (node-active? node graph)))
  (returns AppendResponse)
  (ensures
    (and
      (= response.success true)
      (= response.version (+ node.properties.version 1))
      (contains node.properties.content payload.data)
      (implies (present? payload.source)
        (member payload.source node.properties.sources)))))

(defcontract handle-query
  "Query knowledge for relevant information"
  (requires
    ((node KnowledgeNode)
     (payload QueryPayload)
     (graph Graph))
    (> (length payload.question) 0))
  (returns QueryResponse)
  (ensures
    (and
      (>= response.confidence 0.0)
      (<= response.confidence 1.0)
      (= response.sources node.properties.sources)
      (> (length response.answer) 0))))

(defcontract handle-synthesize
  "Synthesize content from multiple knowledge nodes"
  (requires
    ((node KnowledgeNode)
     (payload SynthesizePayload)
     (graph Graph))
    True)  ;; Empty fromNodes is valid
  (returns SynthesizeResponse)
  (ensures
    (and
      ;; Sources are deduplicated
      (= (length response.sources)
         (length (unique response.sources)))
      ;; This node's sources are included
      (subset node.properties.sources response.sources)
      ;; Valid knowledge nodes' sources are included
      (forall ((nodeId payload.fromNodes))
        (implies (knowledge-node? (graph.getNode nodeId))
          (subset (graph.getNode nodeId).properties.sources response.sources))))))

(defcontract handle-get
  "Get full node state with edges"
  (requires
    ((node KnowledgeNode)
     (graph Graph))
    (node-active? node graph))
  (returns GetResponse)
  (ensures
    (and
      (= response.id node.properties.id)
      (= response.type "knowledge")
      (= response.properties node.properties)
      (= response.edges (graph.getAllEdges node.properties.id)))))

(defcontract handle-observe
  "Get human-readable summary"
  (requires
    ((node KnowledgeNode))
    True)
  (returns ObserveResponse)
  (ensures
    (and
      (= response.state "active")
      (> (length response.observations) 0)
      (= (get response.metadata "version") node.properties.version)
      (= (get response.metadata "contentLength") (length node.properties.content))
      (= (get response.metadata "sourceCount") (length node.properties.sources)))))

(defcontract handle-update
  "Update non-protected properties"
  (requires
    ((node KnowledgeNode)
     (payload UpdatePayload))
    True)
  (returns UpdateResponse)
  (ensures
    (and
      (= response.success true)
      ;; Protected fields not changed
      (= node.properties.id (original-id node))
      (= node.properties.type "knowledge")
      (= node.properties.createdAt (original-createdAt node))
      ;; Only allowed fields updated
      (forall ((field response.updatedProperties))
        (not (member field ["id" "type" "createdAt"]))))))

(defcontract handle-link
  "Create edge to another node"
  (requires
    ((node KnowledgeNode)
     (payload LinkPayload)
     (graph Graph))
    (and
      (node-exists? payload.toId graph)
      (valid-edge-type? payload.edgeType)))
  (returns LinkResponse)
  (ensures
    (and
      (= response.success true)
      (edge-exists? response.edgeId graph)
      (= (edge.from response.edgeId) node.properties.id)
      (= (edge.to response.edgeId) payload.toId))))

(defcontract handle-delete
  "Remove node from graph"
  (requires
    ((node KnowledgeNode)
     (graph Graph))
    (node-active? node graph))
  (returns UnlinkResponse)
  (ensures
    (and
      (= response.success true)
      (not (node-exists? node.properties.id graph)))))

;; ============================================================================
;; Query Functions
;; ============================================================================

(deffunction extract-keywords
  "Extract searchable keywords from question"
  ((question String))
  -> (List Keyword)
  (filter
    (split question #/\s+/)
    (lambda (word) (> (length word) 3))))

(deffunction calculate-confidence
  "Calculate match confidence for query"
  ((keywords (List Keyword))
   (content Content))
  -> Confidence
  (if (= (length keywords) 0)
    0.0
    (let ((lower-content (lowercase content))
          (matches (filter keywords
            (lambda (kw) (contains? lower-content (lowercase kw))))))
      (/ (length matches) (length keywords)))))

(deffunction extract-snippet
  "Extract relevant snippet from content"
  ((keywords (List Keyword))
   (content Content))
  -> String
  (let ((sentences (split content #/[.!?]+/)))
    (or
      (first
        (filter sentences
          (lambda (s)
            (some keywords
              (lambda (kw)
                (contains? (lowercase s) (lowercase kw)))))))
      (if (> (length content) 200)
        (concat (substring content 0 200) "...")
        content)
      "No relevant information found")))

(deffunction format-synthesis
  "Format synthesized content from multiple sources"
  ((contents (List Content)))
  -> String
  (join
    (filter-map contents
      (lambda (c i)
        (if (> (length c) 0)
          (concat "[Source " (+ i 1) "]\n" c)
          nil)))
    "\n\n---\n\n"))

(deffunction deduplicate-sources
  "Remove duplicate sources while preserving order"
  ((sources (List Source)))
  -> (List Source)
  (unique sources))

;; ============================================================================
;; Verification Queries
;; ============================================================================

;; Query: Verify all invariants hold
;; ?- all_invariants_hold.

;; Query: Check version monotonicity
;; ?- version_monotonic.

;; Query: Check confidence bounds
;; ?- forall((r QueryResponse), confidence_bounded(r)).

;; Query: Verify source append-only
;; ?- sources_append_only.

;; Query: Find knowledge nodes
;; ?- knowledge_node(NodeId, Title, Version).

;; ============================================================================
;; Example Facts (for testing)
;; ============================================================================

;; Knowledge nodes
(knowledge-node "knowledge_1" "Actor Model" 1).
(knowledge-node "knowledge_2" "Erlang Processes" 2).
(knowledge-node "knowledge_3" "Message Passing" 1).

;; Sources
(has-source "knowledge_1" "Hewitt 1973").
(has-source "knowledge_2" "Armstrong 2003").
(has-source "knowledge_2" "Virding 1996").

;; Content
(has-content "knowledge_1" "Actors are the fundamental units of computation").
(has-content "knowledge_2" "Erlang processes are lightweight and communicate via message passing").

;; Relationships
(edge "edge_1" "knowledge_1" "knowledge_2" "references").
(edge "edge_2" "knowledge_2" "knowledge_3" "references").

;; ============================================================================
;; Completeness Checks
;; ============================================================================

(deffunction all-invariants-hold
  "Check all knowledge system invariants"
  ()
  -> Boolean
  (and
    (check-invariant knowledge-type-immutable)
    (check-invariant version-positive)
    (check-invariant version-monotonic)
    (check-invariant sources-append-only)
    (check-invariant id-immutable)
    (check-invariant id-format)
    (check-invariant confidence-bounded)
    (check-invariant content-accumulation)))

(deffunction knowledge-node-well-formed
  "Check if a knowledge node is well-formed"
  ((node KnowledgeNode))
  -> Boolean
  (and
    (= node.properties.type "knowledge")
    (>= node.properties.version 1)
    (matches node.properties.id #/^knowledge_[0-9]+$/)
    (>= (length node.properties.title) 1)))

;; ============================================================================
;; End of Model
;; ============================================================================
