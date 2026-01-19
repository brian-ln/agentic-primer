;; Structural Destructuring Model
;; How we turn "Blobs" into "Graphs"

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
      (state (registry map)) ; UUID -> URI
      (behavior
        (on resolve (uuid)
          (get registry uuid))
        (on sync (uuid event)
          (let ((uri (resolve uuid)))
            (match uri
              ((fs-path p) (send FileActor 'write p event))
              ((web-url u) (send WebActor 'post u event)))))))))
