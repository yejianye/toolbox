(defun ry/sql-connect-and-bind (connection &optional new-name)
  (interactive
   (if sql-connection-alist
       (list (sql-read-connection "Connection: " nil '(nil))
             current-prefix-arg)
     (user-error "No SQL Connections defined")))
  (setq sql-product
        (-last-item (ry//sql-connection-prop 'sql-product connection))
        )
  (let* ((cur-buf (current-buffer))
         (sqli-buf (sql-connect connection))
         (sql-startup (ry//sql-connection-prop 'sql-startup connection))
         )
    (with-current-buffer sqli-buf
      (sql-rename-buffer connection)
      (setq sql-buffer (buffer-name sqli-buf))
      )
    (with-current-buffer cur-buf
      (setq-local sql-buffer (buffer-name sqli-buf))
      (setq-local sql-connection-name connection)
      (run-hooks 'sql-set-sqli-hook)
      (sql-send-string sql-startup)
      )
    ))

(defun ry/sql-list-clear-cache ()
  (interactive)
  (with-current-buffer sql-buffer
    (setq sql-completion-object nil)
    ))

(defun ry/sql-kill-buffer ()
  (interactive)
  (let* ((sqli-buf (get-buffer sql-buffer))
         (sqli-process (get-buffer-process sqli-buf)))
    (set-process-query-on-exit-flag (get-buffer-process sqli-buf) nil)
    (sql-stop sqli-process "finished")
    (set-process-sentinel sqli-process nil)
    (kill-buffer sqli-buf)
    (setq-local sql-buffer nil)
    ))

(defun ry/sql-reconnect ()
  (interactive)
  (when sql-buffer
    (ry/sql-kill-buffer))
  (if (and (boundp 'sql-connection-name) sql-connection-name)
      (ry/sql-connect-and-bind sql-connection-name)
    (call-interactively 'ry/sql-connect-and-bind))
  )

(defun ry//sql-connection-prop (name &optional connection)
  (let* ((connection-name (or connection sql-connection-name))
         (connect-props (cdr (assoc connection-name sql-connection-alist))))
    (car (alist-get name connect-props))))

(defun ry//sql-viz-image-path ()
  (let ((viz-image-dir "/tmp/sql_viz_image")
        (filename (format-time-string "%Y-%m-%d-%H%M%S.svg")))
    (unless (file-directory-p viz-image-dir)
      (make-directory viz-image-dir))
    (format "%s/%s" viz-image-dir filename)))

(defun ry/sql-show-viz ()
  (interactive)
  (let* ((start (save-excursion
                  (backward-paragraph)
                  (point)))
         (end (save-excursion
                (forward-paragraph)
                (point)))
         (image-path (ry//sql-viz-image-path))
         (retcode (shell-command-on-region
                   start end
                   (format (concat "sqlviz --host=%s --port=%s --user=%s "
                                   "--save-image=%s --prefix-sql=\"%s\" %s")
                           (ry//sql-connection-prop 'sql-server)
                           (ry//sql-connection-prop 'sql-port)
                           (ry//sql-connection-prop 'sql-user)
                           image-path
                           (ry//sql-connection-prop 'sql-startup)
                           (ry//sql-connection-prop 'sql-database))))
         )
    (when (= retcode 0)
      (find-file-other-window image-path))))

(defun ry/sql-set-sqli-hook ()
  (setq sql-connection-name (nth 1 (s-match "^\\*SQL: \\(.*\\)\\*$" sql-buffer))))

(provide 'ry-sql)
