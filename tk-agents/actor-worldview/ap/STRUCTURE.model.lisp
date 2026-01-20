;; Structural Destructuring Model
;; How we turn "Blobs" into "Graphs"

(defprotocol Persistence
  "Interface for actor state durability."
  (on SNAPSHOT (target_uri)
    (or
      (one SNAPSHOT_OK (uri version))
      (one ERROR (message))))
  (on RESTORE (source_uri)
    (or
      (one RESTORE_OK (version))
      (one ERROR (message)))))

(system StructuralDestructuring
  (actors
    ;; The Parser Actor: Observes a "Blob" and spawns "Fragment" actors
    (actor DocumentParser
      (properties
        (format (enum 'markdown 'html 'json 'typescript)))
      (behavior
        (on shred (blob_node_id)
          (let ((fragments (call-external-parser format blob_node_id)))
            (for-each f fragments
              (spawn FragmentNode f)
              (link blob_node_id f 'contains))))))

    ;; Document Actor: Coordinator for fragments
    (actor DocumentActor
      (state
        (path string)
        (format enum)
        (fragments map)) ; frag_id -> content
      (behavior
        (on fragment-registered (id content)
          (set fragments id content))
        (on fragment-updated (id content)
          (set fragments id content)
          (send self 'fold-and-persist))))

    ;; Fragment Node: A piece of a larger document
    (actor FragmentNode
      (implements DataNode)
      (state
        (parent_doc_id address)
        (content any))
      (behavior
        (on patch (update)
          (if (and (is-object? content) (is-object? update))
            (set content (merge content update))
            (set content update))
          ;; Signal the parent that a fragment changed for re-assembly
          (send parent_doc_id 'fragment-updated content))))

    ;; Persistence Boundary: The "Spoon" manager
    (actor PersistenceManager
      (implements Persistence)
      (state (registry map)) ; address -> path
      (behavior
        (on snapshot (target_uri)
          (send target_uri 'get) ; Pull current state
          (on state (data) 
            (call-file-effect 'write target_uri data)))
        (on restore (source_uri)
          (let ((data (call-file-effect 'read source_uri)))
            (send source_uri 'patch data)))
        (on commit-log (event)
          (send 'seag://system/event-log 'append event)))))) ; Closing PersistenceManager, actors list, and system
