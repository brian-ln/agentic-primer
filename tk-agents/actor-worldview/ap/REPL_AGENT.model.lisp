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
        (on think (task)
          (send self 'signal 'thinking)
          (let ((history (send InteractionLog 'replay -10)) ; Get last 10 messages
                (graph_context (send Graph 'query "find relevant knowledge")))
            (let ((plan (call-inference history graph_context)))
              (execute-plan plan))))
        (on execute-plan (plan)
          (for-each step plan
            (match step
              ((search query) (send WebActor 'search query))
              ((run command) (send ShellActor 'call command))
              ((reply text) (send UserProxy 'output text)))))))

    ;; The WebUI Actor (Shadow Actor in the browser)
    (actor WebUIActor
      (behavior
        (on output (content)
          (dom-update "#chat-log" content))
        (on signal (type)
          (dom-update "#status-indicator" type))))))
