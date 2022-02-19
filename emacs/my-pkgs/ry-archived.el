;; Code snippets
(cl-defun ry/helm-source-code-snippets (filenames)
  (helm-build-sync-source "Code snippets"
    :candidates (helm-org-get-candidates filenames)
    :action '(("View code snippet" . ry/helm-org-view-code-snippet))))

(defun ry/helm-org-view-code-snippet (marker)
  (switch-to-buffer-other-window (marker-buffer marker))
  (hide-sublevels 1)
  (goto-char (marker-position marker))
  (org-show-context)
  (org-show-entry))

(defun ry/helm-code-snippets ()
  (interactive)
  (let ((helm-org-headings--nofilename t))
    (helm :sources (ry/helm-source-code-snippets (list ry-org-code-snippet-file))
          :candidate-number-limit 99999
          :buffer "*helm code snippets*")))

(defun ry/rsync-project-to-remote ()
  (interactive)
  (if (boundp 'sync-remote-dir)
      (save-window-excursion
        (message (format "Sync project with %s!" sync-remote-dir))
        (async-shell-command (format "rsync -av --delete --exclude='terraform.tfstate' --exclude='/.git' --filter=':- .gitignore' '%s' '%s'" (projectile-project-root) sync-remote-dir)))
    (message "sync-remote-dir undefined!")))

(defun ry/yamlsql-show-sql ()
  (interactive)
  (let ((query-name (substring-no-properties (thing-at-point 'word))))
    (shell-command (format "yaml2sql %s --query %s"
                           (buffer-file-name) query-name)
                   "*yaml2sql-output*")))

(defun ry/yamlsql-show-sql-all ()
  (interactive)
  (shell-command (format "yaml2sql %s" (buffer-file-name))
                 "*yaml2sql-output*"))

(defun ry/yamlsql-run-sql ()
  (interactive)
  (let ((query-name (substring-no-properties (thing-at-point 'word))))
    (shell-command (format "yaml2sql %s --query %s --run-sql"
                           (buffer-file-name) query-name)
                   "*yaml2sql-output*")))

(defun ry/yamlsql-run-sql-all ()
  (interactive)
  (shell-command (format "yaml2sql %s --run-sql" (buffer-file-name))
                 "*yaml2sql-output*"))

(provide 'ry-archived)
