;; SEAG Protocol Definitions
;; Defines the behavioral contracts between actors.

(defprotocol FileIO
  "Interface for file system operations."
  
  (on READ_FILE (path)
    (yields FILE_CONTENT (path data))
    (yields ERROR (message)))

  (on WRITE_FILE (path data)
    (yields WRITE_OK (path))
    (yields ERROR (message)))

  (on WATCH_FILE (path)
    (yields WATCH_OK (path))
    (yields FILE_CHANGED (path content))
    (yields ERROR (message))))

(defprotocol Observable
  "Interface for actors that emit state changes."
  (on WATCH (filter)
    (yields WATCH_OK (sub_id))
    (yields CHANGED (state)))
  (on UNWATCH (sub_id)
    (yields UNWATCH_OK ())))

(defprotocol Inference
  "Interface for Large Language Models."
  (on PROMPT (text params)
    (yields RESPONSE (text))
    (yields ERROR (message))))

(defprotocol Embedding
  "Interface for Vector Embedding models."
  (on EMBED (text)
    (yields VECTOR (floats))
    (yields ERROR (message))))
