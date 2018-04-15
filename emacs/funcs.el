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

(defun ry/org-copy-as-rtf ()
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
    (shell-command (format "LANG=en_US.UTF-8 cat %s | textutil -stdin -format html -convert rtf -stdout | pbcopy" fname))
    (delete-file fname))
  )

(defun ry/org-copy-as-markdown ()
  "Copy current region as markdown to clipboard.
It only works in Mac OS "
  (interactive)
  (require 'ox-md)
  (let* ((beg (region-beginning))
         (end (region-end))
         (region-string (format "#+OPTIONS: toc:nil\n%s" (buffer-substring beg end)))
         (content (org-export-string-as region-string 'md t))
         (fname (make-temp-file "org-clipboard")))
    (write-region content nil fname)
    (shell-command (format "LANG=en_US.UTF-8 cat %s | pbcopy" fname))
    (delete-file fname))
  )

(defun ry/rsync-project-to-remote ()
  (interactive)
  (if (boundp 'sync-remote-dir)
      (save-window-excursion
        (message (format "Sync project with %s!" sync-remote-dir))
        (async-shell-command (format "rsync -av --delete --exclude='terraform.tfstate' --exclude='/.git' --filter=':- .gitignore' '%s' '%s'" (projectile-project-root) sync-remote-dir))
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
    :candidates (helm-org-get-candidates filenames)
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
        )
    (helm :sources (ry/helm-source-code-snippets (list ry-org-code-snippet-file))
          :candidate-number-limit 99999
          :buffer "*helm code snippets*")))

(defun ry/org-filter-todo-keyword ()
  (interactive)
  (require 'helm-org)
  (org-show-todo-tree '(4))
  )

(defun ry//transform-evernote-link(note-link)
  (require 's)
  (s-replace "www.evernote.com" "app.yinxiang.com" note-link)
  )

(defun ry/paste-evernote-link()
  (interactive)
  (let ((note-link (shell-command-to-string "LANG=en_US.UTF-8 pbpaste"))
        )
    (insert (ry//transform-evernote-link note-link))
  ))


(defun ry/osx-copy()
  "Copy region to OS X system pasteboard."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "LANG=en_US.UTF-8 pbcopy")
  (evil-normal-state)
  )

(defun ry/osx-paste()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (let ((clipboard (shell-command-to-string "LANG=en_US.UTF-8 pbpaste"))
        )
    (when (eq evil-state 'visual)
      (kill-region (region-beginning) (region-end))
      (evil-normal-state)
      )
    (insert clipboard)))

(defun ry/org-paste-image()
  (interactive)
  (let* ((is-retina (y-or-n-p "Is the screenshot taken from a retina display?"))
         (date-string (format-time-string "%Y-%m-%d-%H-%M"))
         (filename (format "%s-%04x.png" date-string (random (expt 16 4))))
         (fullpath (expand-file-name (concat ry-org-images-dir filename))))
    (if is-retina
        (shell-command (format "~/utils/save_screen.py --scale=0.5 --filename=%s" fullpath))
      (shell-command (format "~/utils/save_screen.py --filename=%s" fullpath)))
    (insert (format "[[file:%s]]" fullpath))
    (org-redisplay-inline-images)
    )
  )

(defun ry/org-cliplink-osx ()
  "Takes a URL from the OSX clipboard and inserts an org-mode link
with the title of a page found by the URL into the current
buffer"
  (interactive)
  (org-cliplink-insert-transformed-title (shell-command-to-string "LANG=en_US.UTF-8 pbpaste")
                                         'org-cliplink-org-mode-link-transformer))

(defun ry/markdown-cleanup-org-tables()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^|-" nil t)
      (replace-string "-+-" "-|-" nil (point) (line-end-position)))
    ))

(defun ry/markdown-convert-to-org-tables()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^|-" nil t)
      (replace-string "-|-" "-+-" nil (point) (line-end-position)))
    ))

(defun ry/setup-markdown-mode()
  (ry/markdown-orgtbl-mode)
  )

(defun ry/markdown-orgtbl-mode()
  (require 'org)
  (orgtbl-mode)
  (add-hook (make-variable-buffer-local 'first-change-hook) 'ry/markdown-convert-to-org-tables)
  (add-hook (make-variable-buffer-local 'before-save-hook) 'ry/markdown-cleanup-org-tables)
  )

(defun ry//string-in-buffer-p(str &optional start)
  (save-excursion
    (widen)
    (goto-char (or start (point-min)))
    (search-forward str nil t))
  )

;; Journal
(defun ry/org-goto-journal()
  "Goto journal org file, and create headings for today if not exists"
  (interactive)
  (let ((journal-file (concat ry-org-journal-dir (format-time-string "%Y-%m.org")))
        (today (format-time-string "* %Y-%m-%d %A"))
        (title (format-time-string "#+title: Journal - %b, %Y\n"))
        (todo "#+TODO: NEW(n) | REVIEWED(r)\n"))
    (find-file journal-file)
    (unless (file-exists-p journal-file)
      (insert (concat title todo))
      (save-buffer))
    (unless (ry//string-in-buffer-p today)
      (goto-char (point-max))
      (insert (format "\n%s" today))))
  )

(defun ry//org-insert-today-todo(text)
  "Insert a todo item in Today's journal"
  (ry/org-goto-journal)
  (let* ((today-heading (format-time-string "* %Y-%m-%d %A"))
        (todo-heading "** Todo")
        (todo-words (split-string text))
        (first-word (capitalize (car todo-words)))
        (remaining-words (cdr todo-words))
        (todo-desc (s-join " " (cons first-word remaining-words)))
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

(defun ry/org-new-today-todo()
  "Quick shortcut to add todo item in Today's journal"
  (interactive)
  (ry//org-insert-today-todo (read-string "Todo:"))
  )

(defun ry/org-cycle()
  "Cycle visibility at anywhere inside a subtree"
  (interactive)
  (org-back-to-heading)
  (org-cycle)
  )

(defun ry/helm-org-journal ()
  (interactive)
  (require 'helm-org)
  (let ((journal-files (file-expand-wildcards (format "%s/*.org" ry-org-journal-dir)))
        )
    (helm :sources (helm-source-org-headings-for-files journal-files)
          :candidate-number-limit 99999
          :buffer "*helm journal*"))
  )

;; Diary
(defun ry/org-goto-diary()
  "Goto Diary org file, and create headings for today if not exists"
  (interactive)
  (let* ((diary-file (concat ry-org-root-dir "diary.org"))
        (today (format-time-string "* %Y-%m-%d %A"))
        )
    (find-file diary-file)
    (widen)
    (goto-char (point-max))
    (unless (ry//string-in-buffer-p today)
      (insert (format "\n%s" today)))
    )
  )

;; Syntax table
(defun ry//underscore-as-word()
  (modify-syntax-entry ?_ "w")
  )

(defun ry/add-hook-underscore-as-word (hook-list)
  (mapcar (lambda (mode-hook)
            (add-hook mode-hook 'ry//underscore-as-word))
          hook-list)
  )

;; Projectile
(defun ry/projectile-add-new-project (project-root)
  (interactive (list (read-directory-name "Add a new project: " "/Users/ryan/source_code/")))
  (projectile-add-known-project project-root)
  (projectile-switch-project-by-name project-root)
  )

;; Python
;; This is copied from https://github.com/syl20bnr/spacemacs/pull/7070
(defun ry/python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             ;; `run-python' has different return values and different
             ;; errors in different emacs versions. In 24.4, it throws an
             ;; error when the process didn't start, but in 25.1 it
             ;; doesn't throw an error, so we demote errors here and
             ;; check the process later
             (with-demoted-errors "Error: %S"
               ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
               ;; shell process
               (call-interactively #'run-python)
               (python-shell-get-process)))))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))

(defun ry/treemacs-ignore-file-predicate (relpath abspath)
  "Custom file ignores for treemacs"
  (s-matches? "^.*\.pyc$" relpath)
  )

(defun ry/mdmail-send-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "mdmail"))

(defun ry/writeroom-font-size ()
  (if (bound-and-true-p writeroom-mode)
      (cnfonts-increase-fontsize)
    (cnfonts-decrease-fontsize)
    )
  )

(defun ry/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (progn
          (kill-new file-name)
          (message file-name)
          )
      (error "Buffer not visiting a file"))))

(defun ry/sql-mode-hook ()
  (sqlind-minor-mode 1)
  (setq-local outline-regexp "-- [*\f]+")
  (outline-minor-mode 1)
  )

(defun ry/elisp-add-to-watch (&optional region-start region-end)
  "Add the current variable to the *EDebug* window"
  (interactive "r")
  (require 'eldoc)
  (let ((statement
         (if (and region-start region-end (use-region-p))
             (buffer-substring region-start region-end)
           (symbol-name (eldoc-current-symbol)))))
    ;; open eval buffer
    (edebug-visit-eval-list)
    ;; jump to the end of it and add a newline
    (goto-char (point-max))
    (newline)
    ;; insert the variable
    (insert statement)
    ;; update the list
    (edebug-update-eval-list)
    ;; jump back to where we were
    (edebug-where)))

(defun ry/sql-connect-and-bind (connection &optional new-name)
  (interactive
   (if sql-connection-alist
       (list (sql-read-connection "Connection: " nil '(nil))
             current-prefix-arg)
     (user-error "No SQL Connections defined")))
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
                   (format "sqlviz --host=%s --port=%s --user=%s --save-image=%s %s"
                           (ry//sql-connection-prop 'sql-server)
                           (ry//sql-connection-prop 'sql-port)
                           (ry//sql-connection-prop 'sql-user)
                           image-path
                           (ry//sql-connection-prop 'sql-database))))
         )
    (when (= retcode 0)
      (find-file-other-window image-path))))
  )

(defun ry/sql-set-sqli-hook ()
  (setq sql-connection-name (nth 1 (s-match "^\\*SQL: \\(.*\\)\\*$" sql-buffer))))

(defun ry/git-diff ()
  (interactive)
  (magit-diff "HEAD"))
