;; Messaging Actors Model

(system Messaging
  (actors
    ;; Topic Actor: The Pub/Sub Hub
    (actor TopicNode
      (state (subscribers set))
      (behavior
        (on subscribe (consumer_id filter)
          (add subscribers (tuple consumer_id filter)))
        (on publish (message)
          (for-each (id filter) subscribers
            (if (match-filter? filter message)
                (send id 'notify message))))))

    ;; Queue Actor: The Work Distributor
    (actor QueueNode
      (state 
        (backlog list)
        (pending map) ; id -> {worker, timestamp}
        (workers set))
      (behavior
        (on enqueue (payload)
          (push backlog payload)
          (trigger-distribution))
        (on register-worker (worker_id)
          (add workers worker_id))
        (on ack (msg_id)
          (remove pending msg_id))
        (on nack (msg_id)
          (let ((msg (get pending msg_id)))
            (push backlog msg)
            (remove pending msg_id)
            (trigger-distribution)))
        (on distribute ()
          (while (and backlog workers)
            (let ((msg (pop backlog))
                  (worker (pop workers)))
              (send worker 'do-work msg)
              (set pending msg.id {worker, now}))))))))
