(defmacro ry/set-key-for-file (key filename)
  `(evil-leader/set-key ,key (lambda () (interactive)(find-file ,filename)))
  )

(defun ry/insert-today-date()
  (interactive)
  (insert (calendar-date-string (calendar-current-date) nil
                                nil)))

(defun ry/backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

(defun ry/insert-yas ()
  (interactive)
  (evil-insert-state)
  (helm-yas-complete))

(defun ry/read-file-content (filename)
  (when (file-exists-p filename)
    (with-temp-buffer
        (insert-file-contents filename)
        (buffer-string)))
  )

(defun ry/org-copy-to-clipboard ()
  "Copy current region as rich text to clipboard.
This function makes use of command-line utility: pbcopy, textutil
and only works on MacOS."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (content (org-export-string-as (buffer-substring beg end) 'html t))
         (fname (make-temp-file "org-clipboard"))
         (style (ry/read-file-content "~/.org-clipboard.css"))
         (content-with-style (format "<style>%s</style>%s" style content)))
    (write-region content-with-style nil fname)
    (shell-command (format "cat %s | textutil -stdin -format html -convert rtf -stdout | pbcopy" fname))
    (delete-file fname))
  )

(defun ry/rsync-project-to-remote ()
  (interactive)
  (if (boundp 'sync-remote-dir)
      (save-window-excursion
        (message (format "Sync project with %s!" sync-remote-dir))
        (async-shell-command (format "rsync -av --delete --exclude='/.git' --filter=':- .gitignore' '%s' '%s'" (projectile-project-root) sync-remote-dir))
        )
    (message "sync-remote-dir undefined!")
    )
  )

;; The following snippets for reloading dir locals are copied from stackoverflow
;; link: http://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun ry/reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun ry/reload-dir-locals-for-all-buffer-in-this-project ()
  "For every buffer with the same `default-directory` as the 
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (string-prefix-p dir default-directory))
        (ry/reload-dir-locals-for-current-buffer)))))

(defun ry/auto-reload-dir-locals-hook ()
  (when (and (buffer-file-name)
             (equal dir-locals-file
                    (file-name-nondirectory (buffer-file-name))))
    (add-hook (make-variable-buffer-local 'after-save-hook)
              'ry/reload-dir-locals-for-all-buffer-in-this-project)))

;; Code snippets
(cl-defun ry/helm-source-code-snippets (filenames)
  (helm-build-sync-source "Code snippets"
    :candidates (helm-org-get-candidates filenames 1 2)
    :action '(("View code snippet" . ry/helm-org-view-code-snippet)
              )))

(defun ry/helm-org-view-code-snippet (marker)
  (switch-to-buffer-other-window (marker-buffer marker))
  (hide-sublevels 1)
  (goto-char (marker-position marker))
  (org-show-context)
  (org-show-entry))

(defun ry/helm-code-snippets ()
  (interactive)
  (require 'helm-org)
  (let ((helm-org-headings--nofilename t)
        (code-snippets-org-file "~/baidu/org/code-snippets.org")
        )
    (helm :sources (ry/helm-source-code-snippets (list code-snippets-org-file))
          :candidate-number-limit 99999
          :buffer "*helm code snippets*")))

(defun ry/osx-copy()
  "Copy region to OS X system pasteboard."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "pbcopy")
  (evil-normal-state)
  )

(defun ry/osx-paste()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (shell-command-on-region
   (point) (if mark-active (mark) (point)) "pbpaste" nil t))

