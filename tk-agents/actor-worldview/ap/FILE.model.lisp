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

(actor FileEffectActor
  (implements FileIO)
  (state (watches map))
  (behavior))
