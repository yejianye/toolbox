(defvar ry/org-journal-add-todo nil
  "Add Todo section in daily journal")

(defvar ry/reminder-lists '("Inbox" "work" "life" "prod"))
(defvar ry/reminder-due-dates '("N/A" "today" "tomorrow" "this weekend"))
(defvar ry/reminder-default-list "Inbox")

;; Utility

(defun ry//today-heading-string ()
  (format-time-string "%Y-%m-%d %A"))

;; Journal

(defun ry//today-journal-node ()
  "Get Today's Journal Org Node"
  (ry/orgapi-first-child (ry/orgapi-get-root) :title (ry//today-heading-string)))

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
      (insert (format "\n%s\n" today-heading))
      (when ry/org-journal-add-todo
        (insert (format "** Todo\n%s" (ry//org-todos-from-previous-day)))))))

;; Todo

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

(defun ry//send-todo-to-reminder-app (content todo-list due-date)
  "Send todo to Reminder.app"
  (->
    (ry/http-post "http://localhost:3000/reminder/add"
                  (list :todo content
                        :list todo-list
                        :due-date due-date))
    (plist-get :data)
    (plist-get :message)
    (message)))

(defun ry/send-current-line-to-reminder-app ()
  (interactive)
  (let* ((todo-content (ry//get-current-todo))
         (todo-list (completing-read "Choose list:" ry/reminder-lists))
         (due-date (completing-read "Choose due date:" ry/reminder-due-dates))
         (due-date (if (string= due-date "N/A") nil due-date)))
    (ry//send-todo-to-reminder-app todo-content todo-list due-date)))

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

;; Notes

(defun ry//org-insert-instant-note(text)
  "Insert an instant note in Today's journal"
  (ry/org-goto-journal)
  (let* ((today-node (ry//today-journal-node))
         (inst-node (ry/orgapi-tset-child today-node "Instant Notes")))
    (ry/orgapi-append-contents inst-node text)))

;; Meeting Notes

(defvar ry/meeting-note-map
  '(("#* *1-1 \\(.*\\)" ry/insert-one-on-one-note)
    ("#* *\\(.*weekly.*\\)" ry/insert-regular-meeting)
    ("#* *\\(.*monthly.*\\)" ry/insert-regular-meeting)
    ("#* *\\(.*\\)" ry/insert-adhoc-meeting-note)))

(defun ry/insert-adhoc-meeting-note (title content)
  (let* ((meeting-root (ry/orgx-select-one '(heading "Adhoc Meetings")
                                           "~/org/bytedance/regular-meetings.org"))
         (title (format "%s %s" (format-time-string "%Y-%m-%d") title)))
    (ry/orgx-child-prepend meeting-root title :content content)
    title))

(defun ry/insert-regular-meeting (title content)
  (let* ((root (ry/orgx-select-one '(heading "Regular Meetings")
                                   "~/org/bytedance/regular-meetings.org"))
         (meeting (ry/orgx-child-prepend root title :tset t)))
    (ry/orgx-child-prepend meeting (ry/today-string) :content content)
    (plist-get meeting :title)))

(defun ry/insert-one-on-one-note (title content)
  (let ((node (ry/orgx-select-one `(heading ,title) "~/org/bytedance/one-on-one/others.org")))
    (-> (ry/orgx-child-prepend node "To be discussed" :tset t)
        (ry/orgx-sibling-append (ry/today-string) :content content))
    (plist-get node :title)))

(defun ry//find-meeting-note-func (title)
  (let* ((funcs (--map (list :matched_title (-> (s-match (-first-item it) title)
                                                (-last-item))
                             :func (-last-item it))
                       ry/meeting-note-map)))
    (-> (--filter (plist-get it :matched_title) funcs)
        (-first-item))))

(defun ry/insert-meeting-notes-from-clipboard ()
  "Paste meeting notes from clipboard. First line as node title.
   Rest content as node body."
  (interactive)
  (let* ((lines (s-split "\n" (ry//clipboard-content)))
         (title (-first-item lines))
         (content (concat (s-join "\n" (-drop 1 lines)) "\n"))
         (matched (ry//find-meeting-note-func title)))
    (funcall (plist-get matched :func) (plist-get matched :matched_title) content)))

(provide 'ry-org-journal)
