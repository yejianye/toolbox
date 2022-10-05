(setq ry/alfred-cmd-map
  '(:one-on-one (:func ry/alfred-one-on-one
                 :params "\\(.*\\) @\\(.*\\)")
    :todo (:func ry//org-insert-today-todo)
    :meeting-note (:func ry/alfred-meeting-notes)))

(defvar ry/alfred-msg-file "~/ry-alfred.msg")

(defun ry/alfred-command (cmd params)
  "Parse alfred PARAMS string and then dispatch CMD to corresponding function"
  (ry/alfred-init)
  (let* ((cmd-map (plist-get ry/alfred-cmd-map cmd))
         (params-regex (or (plist-get cmd-map :params) "\\(.*\\)"))
         (params (s-match params-regex params))
         (func (plist-get cmd-map :func)))
    (apply func (-drop 1 params))))

(defun ry/alfred-one-on-one (topic name)
   (org-ql-select "~/org/bytedance/one-on-one/others.org"
     (list 'heading name)
     :action (lambda ()
               (let ((heading (org-get-heading)))
                 (-> (ry/orgapi-current-heading-node)
                     (ry/orgapi-tset-child "To be discussed" nil t)
                     (ry/orgapi-prepend-contents (format "- [ ] %s\n" topic)))
                 (ry/alfred-message "Topic '%s' added to '%s'\n" topic heading)))))

(defun ry/alfred-meeting-notes (_)
  (let ((title (ry/insert-meeting-notes-from-clipboard)))
    (ry/alfred-message "Meeting Notes Added: %s" title)))


(defun ry/alfred-message (fmt &rest args)
  (f-append-text (apply 'format fmt args)
                 'utf-8 ry/alfred-msg-file))

(defun ry/alfred-init ()
  (when (f-exists? ry/alfred-msg-file)
    (f-delete ry/alfred-msg-file))
  (f-touch ry/alfred-msg-file))

;; (ry/alfred-command :one-on-one "this is an alfred test @shaochi")

(provide 'ry-alfred)
