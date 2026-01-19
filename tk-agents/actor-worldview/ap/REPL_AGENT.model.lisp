;; Agentic REPL Model

(system AgenticREPL
  (actors
    ;; The User Proxy: Represents the human in the graph
    (actor UserProxy
      (state (session_id uuid) (last_seen timestamp))
      (behavior
        (on input (text)
          (let ((log_entry (send InteractionLog 'append {from: 'user, content: text})))
            (send BrainAgent 'think {context_id: log_entry})))))

    ;; The Brain: The Inference & Execution coordinator
    (actor BrainAgent
      (behavior
        (on think (input)
          (match (parse-intent input)
            (('mount path) (send self 'handle-mount path))
            (('explore addr) (send self 'handle-explore addr))
            (('set addr val) (send self 'handle-set addr val))
            (_ (send self 'handle-heuristic input))))
        
        (on handle-mount (path)
          (send FileIO 'read-file path)
          (send self 'signal 'thinking "Mounting file..."))
        
        (on handle-explore (addr)
          (send GraphProjector 'query 'reachable addr))
        
        (on handle-set (addr val)
          (send addr 'patch val))))

    ;; The WebUI Actor (Shadow Actor in the browser)
    (actor WebUIActor
      (behavior
        (on output (content)
          (dom-update "#chat-log" content))
        (on signal (type)
          (dom-update "#status-indicator" type))))))
