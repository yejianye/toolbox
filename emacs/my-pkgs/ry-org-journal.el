(defun ry/journal-org-current-month ()
  (concat ry-org-journal-dir (format-time-string "%Y-%m.org")))

(defun ry/journal-org-last-month ()
  (let* ((days (+ (string-to-number (format-time-string "%d")) 1))
         (date (seconds-to-time (- (float-time) (* days 86400)))))
    (concat ry-org-journal-dir (format-time-string "%Y-%m.org" date))))

(defun ry/org-goto-journal()
  "Goto journal org file, and create headings for today if not exists"
  (interactive)
  (let ((journal-file (ry/journal-org-current-month))
        (today-heading (format "* %s" (ry/today-string)))
        (title (format-time-string "#+title: Journal - %b, %Y\n"))
        (property "#+TODO: NEW(n) | REVIEWED(r)\n#+INDEX-ENTRIES: level:2 !heading:Todo\n")
        (settings "# -*- eval: (toggle-word-wrap -1); -*-\n"))
    (find-file journal-file)
    (unless (file-exists-p journal-file)
      (insert (concat settings title property))
      (save-buffer))
    (unless (ryc//string-in-buffer-p today-heading)
      (goto-char (point-max))
      (insert (format "\n%s\n** Todo\n%s"
                      today-heading
                      (ry//org-todos-from-previous-day))))))

(defun ry/org-new-today-todo()
  "Quick shortcut to add todo item in Today's journal"
  (interactive)
  (ry//org-insert-today-todo (read-string "Todo:")))

(defun ry/move-to-someday-todo(&optional copy)
  (interactive)
  (let ((todo (ry//get-current-todo))
        (old-buffer (current-buffer)))
    (unless copy
      (kill-whole-line))
    (ry//org-insert-todo-someday todo)
    (switch-to-buffer old-buffer)))

(defun ry/copy-to-someday-todo()
  (interactive)
  (ry/move-to-someday-todo t))

(defun ry/move-to-today-todo(&optional copy)
  (interactive)
  (let ((todo (s-replace-regexp "^\\[[0-9-]+\\] *" "" (ry//get-current-todo)))
        (old-buffer (current-buffer)))
    (unless copy
      (kill-whole-line))
    (ry//org-insert-today-todo todo)
    (switch-to-buffer old-buffer)))

(defun ry/copy-to-today-todo()
  (interactive)
  (ry/move-to-today-todo t))

(defun ry//get-current-todo()
  (s-replace-regexp "- \\(\\[ \\]\\)? *" "" (ryc/current-line-content)))

(defun ry//org-insert-todo-someday (todo)
  "Add todo item to someday list"
  (find-file "~/org/tasks.org")
  (let ((content (format "- [ ] [%s] %s" (format-time-string "%Y-%m-%d") todo)))
    (ry/orgapi-append-contents
     (ry/orgapi-get-node-by-heading "Todo - Some day")
     content)))

(defun ry//org-todos-from-previous-day ()
  (let ((content (thread-first (ry/orgapi-get-root)
                   (ry/orgapi-last-child)
                   (ry/orgapi-first-child :title "Todo")
                   (ry/orgapi-get-contents))))
    (thread-last content
      (s-split "\n")
      (--filter (not (s-starts-with? "- [X]" it)))
      (s-join "\n"))))

(defun ry//org-insert-today-todo(text)
  "Insert a todo item in Today's journal"
  (ry/org-goto-journal)
  (let* ((today-node (ry//today-journal-node))
         (todo-node (ry/orgapi-tset-child today-node "Todo")))
    (ry/orgapi-append-contents todo-node
                               (format "- [ ] %s" (s-capitalize text)))))

(defun ry//today-heading-string ()
  (format-time-string "%Y-%m-%d %A"))

(defun ry//today-journal-node ()
  "Get Today's Journal Org Node"
  (ry/orgapi-first-child (ry/orgapi-get-root) :title (ry//today-heading-string)))

(defun ry//org-insert-instant-note(text)
  "Insert an instant note in Today's journal"
  (ry/org-goto-journal)
  (let* ((today-node (ry//today-journal-node))
         (inst-node (ry/orgapi-tset-child today-node "Instant Notes")))
    (ry/orgapi-append-contents inst-node text)))

(provide 'ry-org-journal)
