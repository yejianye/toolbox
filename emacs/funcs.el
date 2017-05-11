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
  (insert (shell-command-to-string "LANG=en_US.UTF-8 pbpaste")))

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
  (let ((today-heading (format-time-string "* %Y-%m-%d %A"))
        (todo-heading "** Todo"))
    (goto-char (point-min))
    (search-forward today-heading)
    (if (ry//string-in-buffer-p todo-heading (point))
      (progn
        (search-forward todo-heading)
        (insert (format "\n- [ ] %s" text))
        )
      (progn
       (insert (format "\n%s" todo-heading))
       (insert (format "\n- [ ] %s" text)))
      )
    )
  )

(defun ry/org-new-today-todo()
  "Quick shortcut to add todo item in Today's journal"
  (interactive)
  (ry//org-insert-today-todo (read-string "Todo:"))
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
    (org-narrow-to-subtree)
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

