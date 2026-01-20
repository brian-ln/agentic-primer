;; SEAG Protocol Definitions
;; Defines the behavioral contracts (Session Types) between actors.
;;
;; Quantifiers:
;; (one MSG)  -> Exactly one message
;; (opt MSG)  -> Zero or one (Optional)
;; (any MSG)  -> Zero or more (Star *)
;; (some MSG) -> One or more (Plus +)
;; (seq A B)  -> Sequence (A then B)
;; (or A B)   -> Choice (A or B)

(defprotocol FileIO
  "Interface for file system operations."
  
  (on READ_FILE (path)
    (or 
      (one FILE_CONTENT (path data))
      (one ERROR (message))))

  (on WRITE_FILE (path data)
    (or
      (one WRITE_OK (path))
      (one ERROR (message))))

  (on WATCH_FILE (path)
    (seq
      (one WATCH_OK (path))
      (any FILE_CHANGED (path content)))))

(defprotocol Observable
  "Interface for actors that emit state changes."
  (on WATCH (filter)
    (seq
      (one WATCH_OK (sub_id))
      (any CHANGED (state))))
  
  (on UNWATCH (sub_id)
    (one UNWATCH_OK ())))

(defprotocol Inference
  "Interface for Large Language Models."
  (on PROMPT (text params)
    (or
      (one RESPONSE (text))
      (one ERROR (message)))))

(defprotocol Embedding
  "Interface for Vector Embedding models."
  (on EMBED (text)
    (or
      (one VECTOR (floats))
      (one ERROR (message)))))