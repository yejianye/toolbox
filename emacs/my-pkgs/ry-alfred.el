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

;; (defun ry/alfred-one-on-one (topic name)
;;   (let ((node (ry/orgx-select-one (list 'heading name) "~/org/bytedance/one-on-one/others.org")))
;;     (-> (ry/orgx-child-prepend node "To be discussed" :tset t)
;;         (ry/orgx-content-prepend (format "- [ ] %s\n" topic)))
;;     (ry/alfred-message "Topic '%s' added to '%s'\n" topic (plist-get node :title))))

(defun ry/alfred-one-on-one (topic name)
  (let ((node (-> (ry/search-note name nil "one-on-one")
                  (first)
                  (plist-get :id)
                  (ry/orgx-select-by-id))))
    (-> (ry/orgx-child-prepend node "To be discussed" :content "\n" :tset t)
        (ry/orgx-content-prepend (format "- [ ] %s\n" topic)))
    (ry/alfred-message "Topic '%s' added to '%s'\n" topic (plist-get node :title))))

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

(defun ry/alfred-test-cases ()
  (ry/alfred-one-on-one-v2 "this is an alfred test" "zhangjie"))

(provide 'ry-alfred)
