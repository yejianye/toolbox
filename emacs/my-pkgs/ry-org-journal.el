(defun ry/org-goto-journal()
  "Goto journal org file, and create headings for today if not exists"
  (interactive)
  (let ((journal-file (ry/journal-org-current-month))
        (today-heading (format "* %s" (ry/today-string)))
        (title (format-time-string "#+title: Journal - %b, %Y\n"))
        (todo "#+TODO: NEW(n) | REVIEWED(r)\n")
        (settings "# -*- eval: (toggle-word-wrap -1); -*-\n")
        )
    (find-file journal-file)
    (unless (file-exists-p journal-file)
      (insert (concat settings title todo))
      (save-buffer))
    (unless (ry//string-in-buffer-p today-heading)
      (goto-char (point-max))
      (insert (format "\n%s\n** Todo\n%s"
                      today-heading
                      (ry//org-todos-from-previous-day)))))
  )

(defun ry/org-new-today-todo()
  "Quick shortcut to add todo item in Today's journal"
  (interactive)
  (ry//org-insert-today-todo (read-string "Todo:"))
  )

(defun ry//org-todos-from-previous-day ()
  (let ((content (thread-first (ry/orgapi-get-root)
                   (ry/orgapi-last-child)
                   (ry/orgapi-first-child :title "Todo")
                   (ry/orgapi-get-contents)))
        )
    (thread-last content
      (s-split "\n")
      (--filter (not (s-starts-with? "- [X]" it)))
      (s-join "\n")
      )))

(defun ry//org-insert-today-todo(text)
  "Insert a todo item in Today's journal"
  (ry/org-goto-journal)
  (let* ((today-heading (format-time-string "* %Y-%m-%d %A"))
        (todo-heading "** Todo")
        (todo-desc (s-capitalize text))
        )
    (goto-char (point-min))
    (search-forward today-heading)
    (if (ry//string-in-buffer-p todo-heading (point))
      (progn
        (search-forward todo-heading)
        (insert (format "\n- [ ] %s" todo-desc))
        )
      (progn
       (insert (format "\n%s" todo-heading))
       (insert (format "\n- [ ] %s" todo-desc)))
      )
    )
  )

(defun ry//today-heading-string ()
  (format-time-string "%Y-%m-%d %A"))

(defun ry//today-journal-node ()
  "Get Today's Journal Org Node"
  (ry/orgapi-first-child (ry/orgapi-get-root) :title (ry//today-heading-string)))

;; (defun ry//org-insert-instant-note(text)
;;   "Insert an instant note in Today's journal"
;;   (ry/org-goto-journal)
;;   (let* ((today-node (ry//today-journal-node))
;;          (inst-node (ry/orgapi-tset-child today-node "Instant Notes")))
;;     (ry/orgapi-append-contents inst-node text))
;;   )

(defun ry//org-insert-instant-note(text)
  "Insert an instant note in Today's journal"
  (ry/org-goto-journal)
  (let* ((today-node (ry//today-journal-node))
         (inst-node (ry/orgapi-first-child today-node :title "Instant Notes")))
    (if inst-node
        (ry/orgapi-append-contents inst-node text)
      (ry/orgapi-insert-child today-node "Instant Notes" text)))
  )

(provide 'ry-org-journal)
