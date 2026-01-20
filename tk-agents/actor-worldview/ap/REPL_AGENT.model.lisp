;; Agentic REPL Model

(system AgenticREPL
  (actors
    ;; The User Proxy: Represents the human in the graph
    (actor UserProxy
      (state (session_id uuid) (last_seen timestamp))
      (behavior
        (on input (text)
          ;; 1. Record the interaction
          (send InteractionLog 'append {from: 'user, content: text})
          ;; 2. Trigger Brain
          (send BrainAgent 'think {input: text}))

        (on output (content)
          ;; Forward result to the user (via Gateway relay)
          (send GatewayRelay 'output {content: content}))

        (on signal (detail)
          ;; Forward status updates
          (send GatewayRelay 'signal {detail: detail}))))

    ;; The Brain: The Inference & Execution coordinator
    (actor BrainAgent
      (behavior
        (on think (input)
          (match (parse-intent input)
            (('mount path) (send self 'handle-mount path))
            (('explore addr) (send self 'handle-explore addr))
            (('watch path) (send self 'handle-watch path))
            (('get addr) (send self 'handle-get addr))
            (('set addr val) (send self 'handle-set addr val))
            (('help) (send self 'handle-help))
            (('help query...) (send InferenceAgent 'prompt {text: query}))
            (_ (send self 'handle-unknown input))))
        
        (on handle-mount (path)
          (send self 'signal "Mounting " path)
          (send FileIO 'read-file path))
        
        (on handle-watch (path)
          (send self 'signal "Watching " path)
          (send FileIO 'watch-file path))

        (on handle-explore (addr)
          (send GraphProjector 'query 'reachable addr))
        
        (on handle-get (addr)
          (send addr 'get_state))

        (on handle-set (addr val)
          (send addr 'patch val))))

    ;; The Gateway Relay: Bridges to the WebSocket
    (actor GatewayRelay
      (behavior
        (on output (content) (websocket-send {type: 'OUTPUT, payload: {content: content}}))
        (on signal (detail) (websocket-send {type: 'SIGNAL, payload: {detail: detail}}))))))
