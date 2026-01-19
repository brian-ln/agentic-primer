;; External Interface Actor Models

(system ExternalInterfaces
  (actors
    ;; Shell Actor: Execute commands and stream output
    (actor ShellNode
      (implements ExecNode)
      (state
        (process_id int)
        (working_dir path)
        (output_log_id address))
      (behavior
        (on call (command args)
          (let ((proc (spawn-process command args)))
            (subscribe proc 'stdout (lambda (line) (send output_log_id 'append line)))
            (wait proc 'exit (lambda (code) (emit-signal 'Exit code)))))))

    ;; Browser Actor: Manage a web session
    (actor BrowserNode
      (state
        (session_id string)
        (current_url string)
        (dom_root_node address))
      (behavior
        (on navigate (url)
          (perform-navigation url)
          (update-dom-graph dom_root_node))
        (on click (selector)
          (perform-click selector))
        (on query-dom (selector)
          (reply (search-graph dom_root_node selector)))))

    ;; Terminal Actor: Interactive TTY
    (actor TerminalNode
      (state
        (buffer string)
        (is_attached bool))
      (behavior
        (on input (chars)
          (write-to-tty chars))
        (on read-screen ()
          (reply buffer))))))
