;; Structural Destructuring Model
;; How we turn "Blobs" into "Graphs"

(system StructuralDestructuring
  (actors
    ;; The Parser Actor: Observes a "Blob" and spawns "Fragment" actors
    (actor DocumentParser
      (properties
        (format (enum 'markdown 'html 'json 'typescript)))
      (behavior
        (on parse (blob_node_id)
          (let ((fragments (call-external-parser format blob_node_id)))
            (for-each f fragments
              (spawn FragmentNode f)
              (link blob_node_id f 'contains))))))

    ;; Fragment Node: A piece of a larger document
    (actor FragmentNode
      (implements DataNode)
      (state
        (parent_doc_id address)
        (fragment_type string) ; e.g., 'h1', 'function_body'
        (content any)
        (extent (tuple offset int length int))) ; Optional: for random-access I/O
      (behavior
        (on patch (new_content)
          (update-state content new_content)
          ;; Signal the parent that a fragment changed
          (send parent_doc_id 'fragment_updated self))))

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
