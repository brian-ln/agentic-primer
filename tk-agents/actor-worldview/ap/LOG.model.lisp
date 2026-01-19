;; Log Actor & Projection Model

(system LogSystem
  (actors
    ;; The Log Actor: Manages the immutable sequence
    (actor LogNode
      (implements BaseNode)
      (state
        (entries list)
        (cursor int)
        (persistence_ref uri))
      (behavior
        (on append (payload)
          (let ((entry (create-entry payload cursor)))
            (push entries entry)
            (increment cursor)
            (emit-signal 'EntryAppended entry)
            (send PersistenceManager 'sync self entry)))
        (on replay (start_id end_id)
          (reply (slice entries start_id end_id)))))

    ;; The Folder (Projector): Aggregates a log into a state
    (actor LogFolder
      (state
        (target_node_id address)
        (last_processed_id int))
      (behavior
        (on 'EntryAppended (entry)
          (let ((new_state (apply-logic-to-state target_node_id entry)))
            (send target_node_id 'update_from_log new_state)
            (update-state last_processed_id entry.id)))))

    ;; The Synthesis Actor: Derives new logs from existing ones
    (actor SynthesisAgent
      (behavior
        (on 'EntryAppended (entry)
          (let ((insight (derive-insight entry)))
            (if insight
                (send target_summary_log 'append insight))))))))
