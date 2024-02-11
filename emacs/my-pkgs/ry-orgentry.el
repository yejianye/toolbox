(defun ry//orgentry-get-hash ()
  (thread-last (ry/org-get-content)
    (s-replace "\n" "")
    (s-replace-regexp ":PROPERTIES:.*?:END:" "")
    (s-trim)
    (md5)))

(defun ry//orgentry-query-sexp ()
  (let ((index-entries (ry/org-get-file-property "INDEX-ENTRIES")))
    (when index-entries
      (org-ql--query-string-to-sexp index-entries))))

(defun ry/orgentry-index-entry-default ()
  (org-id-get-create)
  (let ((time-created (org-entry-get nil "CREATED"))
        (prev-hash (org-entry-get nil "HASH"))
        (cur-hash (ry//orgentry-get-hash))
        (now (format-time-string "%Y-%m-%d %H:%M:%S")))
    (when (not time-created)
      (org-set-property "CREATED" now))
    (unless (and prev-hash (string= prev-hash cur-hash))
      (org-set-property "MODIFIED" now)
      (org-set-property "HASH" cur-hash))))

(defun ry/orgentry-index-entry-root ()
  (let ((now (format-time-string "%Y-%m-%d %H:%M:%S"))
        (time-created (thread-first
                        (s-replace "{}" buffer-file-name "stat -f '%SB' -t '%Y-%m-%d %H:%M:%S' '{}'")
                        (shell-command-to-string)
                        (s-trim))))
      (org-set-property "CREATED" time-created)
      (org-set-property "MODIFIED" now)))

(defun ry/orgentry-index-entry ()
  "Index current entry"
  (interactive)
  (if (org-entry-get nil "ROOT")
      (ry/orgentry-index-entry-root)
    (ry/orgentry-index-entry-default)))

(defun ry/orgentry-ignore-buffer (&optional org-file)
  (or (s-contains-p "note-template" (or org-file buffer-file-name))))

(defun ry/orgentry-index-buffer ()
  "Index current buffer"
  (interactive)
  (unless (ry/orgentry-ignore-buffer)
    (let ((query-sexp (ry//orgentry-query-sexp)))
      (org-ql-select (current-buffer)
        (list 'or '(property "ID") query-sexp)
        :action 'ry/orgentry-index-entry))))

(defun ry/orgentry-db-sync (&optional org-file)
  "Sync index entries with local DB"
  (interactive)
  (unless (ry/orgentry-ignore-buffer org-file)
    (let* ((real-path (file-truename (or org-file buffer-file-name)))
           (working-dir (file-truename org-directory))
           (relative-path (file-relative-name real-path working-dir))
           (result (ry/pyfunc "rypy.org-entry" "index" relative-path working-dir))
           (count (gethash "count" result)))
      (when (> count 0)
        (message "%s entries synced with DB" count))
      count)))

(defun ry/orgentry-prettify-buffer-name ()
  "Use Note Title instead of Filename"
  (save-excursion
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (when (org-entry-get nil "ROOT")
      (rename-buffer (substring-no-properties (org-get-heading))))))

;; WARNING: This function doesn't work, need debugging later
(defun ry/orgentry-db-sync-in-background ()
  "Sync index entries with local DB in background"
  (async-start 'ry/orgentry-db-sync))

(defun ry/orgentry-select (where sortedby &optional ascend)
  (ry/pyfunc "rypy.org-entry" "select" where sortedby ascend))

(provide 'ry-orgentry)
