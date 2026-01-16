;;; Concept Graph Web Server - Formal Model
;;; Type definitions, contracts, and invariants

;; ============================================================================
;; Type Definitions
;; ============================================================================

(deftype ConceptId String
  "Kebab-case identifier for a concept (e.g., 'actor-model')")

(deftype DomainId String
  "Domain identifier (e.g., 'computer-science')")

(deftype Tag String
  "Tag identifier (e.g., 'concurrency')")

(deftype RelationshipType String
  "Type of relationship (e.g., 'implemented-in', 'uses', 'inspires')")

(deftype Concept
  ((id ConceptId)
   (label String)
   (domains (List DomainId))
   (tags (List Tag))
   (description String)
   (references (List ConceptId)))
  "A concept in the knowledge graph")

(deftype Relationship
  ((from ConceptId)
   (to ConceptId)
   (type RelationshipType)
   (description String))
  "A directed relationship between two concepts")

(deftype Statistics
  ((totalConcepts Integer)
   (totalRelationships Integer)
   (domains Integer)
   (tags Integer)
   (relationshipTypes Integer))
  "Graph statistics")

(deftype RelationshipWithConcept
  ((relationship Relationship)
   (concept Concept))
  "A relationship paired with its target/source concept")

(deftype ConceptRelationships
  ((outgoing (List RelationshipWithConcept))
   (incoming (List RelationshipWithConcept)))
  "All relationships for a concept, separated by direction")

(deftype ConceptDetailsResponse
  ((concept Concept)
   (relationships ConceptRelationships))
  "Response payload for concept detail endpoint")

(deftype SearchOptions
  ((domain (Optional DomainId))
   (tag (Optional Tag))
   (keyword (Optional String))
   (maxResults (Optional Integer)))
  "Search filter options")

(deftype HTTPMethod
  (one-of GET POST PUT DELETE PATCH OPTIONS HEAD))

(deftype HTTPStatus
  (one-of 200 400 404 500))

(deftype HTTPRequest
  ((method HTTPMethod)
   (path String)
   (params (Map String String))
   (query (Map String String))
   (headers (Map String String)))
  "HTTP request representation")

(deftype HTTPResponse
  ((status HTTPStatus)
   (body (one-of String JSONValue))
   (headers (Map String String)))
  "HTTP response representation")

;; ============================================================================
;; Data Invariants
;; ============================================================================

(invariant concept-id-format
  "Concept IDs must be kebab-case (lowercase with hyphens)"
  (forall ((c Concept))
    (matches (concept.id) #/^[a-z0-9]+(-[a-z0-9]+)*$/)))

(invariant non-empty-label
  "Concept labels must be non-empty"
  (forall ((c Concept))
    (> (length (concept.label)) 0)))

(invariant non-empty-domains
  "Every concept must have at least one domain"
  (forall ((c Concept))
    (> (length (concept.domains)) 0)))

(invariant valid-references
  "Concept references must point to existing concept IDs"
  (forall ((c Concept))
    (forall ((ref-id c.references))
      (exists ((other Concept))
        (= other.id ref-id)))))

(invariant valid-relationship-endpoints
  "Relationships must connect existing concepts"
  (forall ((r Relationship))
    (and
      (exists ((from-concept Concept))
        (= from-concept.id r.from))
      (exists ((to-concept Concept))
        (= to-concept.id r.to)))))

(invariant no-self-relationships
  "Concepts cannot have relationships to themselves"
  (forall ((r Relationship))
    (!= r.from r.to)))

;; ============================================================================
;; API Endpoint Contracts
;; ============================================================================

(defcontract GET:/api/concepts
  "Retrieve all concepts"
  (requires True)  ; No preconditions
  (returns HTTPResponse)
  (ensures
    (and
      (= response.status 200)
      (is-array response.body)
      (forall ((item response.body))
        (is-type item Concept)))))

(defcontract GET:/api/relationships
  "Retrieve all relationships"
  (requires True)
  (returns HTTPResponse)
  (ensures
    (and
      (= response.status 200)
      (is-array response.body)
      (forall ((item response.body))
        (is-type item Relationship)))))

(defcontract GET:/api/stats
  "Retrieve graph statistics"
  (requires True)
  (returns HTTPResponse)
  (ensures
    (and
      (= response.status 200)
      (is-type response.body Statistics)
      (= response.body.totalConcepts (count-concepts))
      (= response.body.totalRelationships (count-relationships)))))

(defcontract GET:/api/concept/:id
  "Retrieve concept details with relationships"
  (requires
    ((id ConceptId))
    True)  ; No auth required
  (returns HTTPResponse)
  (ensures
    (or
      ;; Success case
      (and
        (= response.status 200)
        (is-type response.body ConceptDetailsResponse)
        (= response.body.concept.id id))
      ;; Missing ID case
      (and
        (= id "")
        (= response.status 400)
        (= response.body "Missing concept ID"))
      ;; Not found case
      (and
        (!= id "")
        (not (concept-exists? id))
        (= response.status 404)
        (= response.body "Concept not found")))))

(defcontract GET:/api/search
  "Search concepts by keyword, domain, or tag"
  (requires
    ((options SearchOptions))
    (or
      (present? options.keyword)
      (present? options.domain)
      (present? options.tag)))
  (returns HTTPResponse)
  (ensures
    (or
      ;; Success case
      (and
        (= response.status 200)
        (is-array response.body)
        (forall ((concept response.body))
          (and
            (is-type concept Concept)
            (matches-search-criteria? concept options))))
      ;; Missing parameters case
      (and
        (not (or
          (present? options.keyword)
          (present? options.domain)
          (present? options.tag)))
        (= response.status 400)
        (= response.body "Missing search parameters")))))

(defcontract GET:/
  "Serve HTML application"
  (requires True)
  (returns HTTPResponse)
  (ensures
    (and
      (= response.status 200)
      (= (get response.headers "content-type") "text/html"))))

;; ============================================================================
;; Query Functions
;; ============================================================================

(deffunction concept-exists?
  "Check if a concept ID exists in the graph"
  ((id ConceptId))
  -> Boolean
  (exists ((c Concept))
    (= c.id id)))

(deffunction get-concept
  "Retrieve a concept by ID"
  ((id ConceptId))
  -> (Optional Concept)
  (let ((matches (filter concepts (lambda (c) (= c.id id)))))
    (if (= (length matches) 1)
      (first matches)
      None)))

(deffunction get-related-concepts
  "Get all relationships for a concept, grouped by direction"
  ((id ConceptId))
  -> ConceptRelationships
  (let ((outgoing-rels (filter relationships (lambda (r) (= r.from id))))
        (incoming-rels (filter relationships (lambda (r) (= r.to id)))))
    (ConceptRelationships
      (outgoing (map outgoing-rels
        (lambda (rel)
          (RelationshipWithConcept
            rel
            (get-concept rel.to)))))
      (incoming (map incoming-rels
        (lambda (rel)
          (RelationshipWithConcept
            rel
            (get-concept rel.from))))))))

(deffunction search-concepts
  "Search concepts by multiple criteria (AND logic)"
  ((options SearchOptions))
  -> (List Concept)
  (let ((results concepts))
    (when (present? options.domain)
      (setq results
        (filter results
          (lambda (c)
            (contains? c.domains options.domain)))))

    (when (present? options.tag)
      (setq results
        (filter results
          (lambda (c)
            (contains? c.tags options.tag)))))

    (when (present? options.keyword)
      (setq results
        (filter results
          (lambda (c)
            (let ((lower-keyword (lowercase options.keyword)))
              (or
                (contains? (lowercase c.label) lower-keyword)
                (contains? (lowercase c.description) lower-keyword)))))))

    (if (present? options.maxResults)
      (take results options.maxResults)
      results)))

(deffunction matches-search-criteria?
  "Check if a concept matches search criteria"
  ((concept Concept)
   (options SearchOptions))
  -> Boolean
  (and
    (or
      (not (present? options.domain))
      (contains? concept.domains options.domain))
    (or
      (not (present? options.tag))
      (contains? concept.tags options.tag))
    (or
      (not (present? options.keyword))
      (let ((lower-keyword (lowercase options.keyword)))
        (or
          (contains? (lowercase concept.label) lower-keyword)
          (contains? (lowercase concept.description) lower-keyword))))))

(deffunction get-statistics
  "Compute graph statistics"
  ()
  -> Statistics
  (Statistics
    (totalConcepts (count-concepts))
    (totalRelationships (count-relationships))
    (domains (count-unique-domains))
    (tags (count-unique-tags))
    (relationshipTypes (count-unique-relationship-types))))

(deffunction count-concepts
  "Count total concepts"
  ()
  -> Integer
  (length concepts))

(deffunction count-relationships
  "Count total relationships"
  ()
  -> Integer
  (length relationships))

(deffunction count-unique-domains
  "Count unique domains across all concepts"
  ()
  -> Integer
  (length
    (unique
      (flatten
        (map concepts (lambda (c) c.domains))))))

(deffunction count-unique-tags
  "Count unique tags across all concepts"
  ()
  -> Integer
  (length
    (unique
      (flatten
        (map concepts (lambda (c) c.tags))))))

(deffunction count-unique-relationship-types
  "Count unique relationship types"
  ()
  -> Integer
  (length
    (unique
      (map relationships (lambda (r) r.type)))))

;; ============================================================================
;; Server Lifecycle
;; ============================================================================

(defstate ServerState
  ((port Integer)
   (concepts (List Concept))
   (relationships (List Relationship))
   (indexes
     ((conceptById (Map ConceptId Concept))
      (conceptsByDomain (Map DomainId (List Concept)))
      (conceptsByTag (Map Tag (List Concept)))
      (outgoingEdges (Map ConceptId (List Relationship)))
      (incomingEdges (Map ConceptId (List Relationship))))))
  "Server runtime state")

(deflifecycle server-lifecycle
  "Server initialization and shutdown"

  (phase :init
    (load-data-files)
    (build-indexes)
    (validate-invariants))

  (phase :start
    (bind-port)
    (register-routes)
    (enable-hmr)
    (log-startup-info))

  (phase :running
    (handle-requests))

  (phase :shutdown
    (close-connections)
    (cleanup-resources)))

(deffunction load-data-files
  "Load concepts.json and relationships.json"
  ()
  -> (Tuple (List Concept) (List Relationship))
  (let ((concepts-data (read-json-file "concepts.json"))
        (relationships-data (read-json-file "relationships.json")))
    (tuple
      concepts-data.concepts
      relationships-data.relationships)))

(deffunction build-indexes
  "Build in-memory indexes for fast lookup"
  ((concepts (List Concept))
   (relationships (List Relationship)))
  -> Indexes
  (Indexes
    (conceptById (index-by concepts :id))
    (conceptsByDomain (group-by-many concepts :domains))
    (conceptsByTag (group-by-many concepts :tags))
    (outgoingEdges (group-by relationships :from))
    (incomingEdges (group-by relationships :to))))

(deffunction validate-invariants
  "Validate all data invariants on startup"
  ()
  -> (Result Boolean ErrorList)
  (let ((errors []))
    (check-invariant concept-id-format errors)
    (check-invariant non-empty-label errors)
    (check-invariant non-empty-domains errors)
    (check-invariant valid-references errors)
    (check-invariant valid-relationship-endpoints errors)
    (check-invariant no-self-relationships errors)

    (if (empty? errors)
      (Success True)
      (Failure errors))))

;; ============================================================================
;; Error Handling
;; ============================================================================

(deffunction handle-error
  "Global error handler for unhandled exceptions"
  ((error Error))
  -> HTTPResponse
  (do
    (log-error error)
    (HTTPResponse
      (status 500)
      (body "Internal Server Error")
      (headers {}))))

;; ============================================================================
;; Non-Functional Properties
;; ============================================================================

(property response-time
  "API endpoints respond within 50ms"
  (forall ((endpoint APIEndpoint))
    (< (measure-latency endpoint) 50ms)))

(property concurrent-capacity
  "Server handles at least 100 concurrent connections"
  (>= (max-concurrent-connections) 100))

(property deterministic-output
  "Same request always produces same response"
  (forall ((req HTTPRequest))
    (= (handle-request req)
       (handle-request req))))

(property memory-safety
  "No memory leaks during HMR cycles"
  (forall ((hmr-cycle HMRCycle))
    (= (memory-after hmr-cycle)
       (memory-before hmr-cycle))))

;; ============================================================================
;; End of Model
;; ============================================================================
