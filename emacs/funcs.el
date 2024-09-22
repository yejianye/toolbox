;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/toolbox/emacs/my-pkgs/")

(defun ry/remove-menu-from-keymap (keymap)
  (define-key keymap [menu-bar] nil))

(defun ry/show-parens-mode ()
  "Enable show-smartparens-mode while disable smartparanes-mode"
  ;; We'd like to turn on show-smartparens-mode but turn off smartparens-mode
  ;; show-smartparens-mode won't take effect if smartparens-mode is off when changing the setting
  (interactive)
  (smartparens-mode 1)
  (show-smartparens-mode 1)
  (smartparens-mode -1))

(defun ry//set-key-for-file (key filename)
  (evil-leader/set-key key (lambda () (interactive)(find-file filename))))

(defun ry/set-key-for-file (key filename &rest shortcuts)
  (while key
    (ry//set-key-for-file key filename)
    (setq key (pop shortcuts) filename (pop shortcuts))))

(defun ry/insert-today-date()
  (interactive)
  (insert (ry/today-string-short)))

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

(defun ry/org-filter-todo-keyword ()
  (interactive)
  (org-show-todo-tree '(4)))

(defun ry/org-paste-image()
  (interactive)
  (let* ((is-retina (y-or-n-p "Is the screenshot taken from a retina display?"))
         (width (string-to-number (read-string "Resize to width (empty for original size): ")))
         (date-string (format-time-string "%Y-%m-%d-%H-%M"))
         (random-id (random (expt 16 4)))
         (filename (format "%s-%04x.png" date-string random-id))
         (fullpath (expand-file-name (concat ry-org-images-dir filename))))
    (if (not is-retina)
        (shell-command (format "~/utils/save_screen.py --filename='%s'" fullpath))
      (shell-command (format "~/utils/save_screen.py --scale=0.5 --filename='%s'" fullpath))
      (shell-command (format "~/utils/save_screen.py --filename='%s'"
                             (s-replace ".png" "@2x.png" fullpath))))
    (if (> width 0)
        (insert (format "#+ATTR_ORG: :width %d\n" width)))
    (insert (format "[[file:%s]]\n" fullpath))
    (org-redisplay-inline-images)))

(defun ry/org-hide-other-subtrees ()
  "Show next entry, keeping other entries closed.
   Source: https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
  "
  (interactive)
  (let ((buf-pos (point)))
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (show-children))
      (outline-back-to-heading)
      (unless (and (bolp) (org-on-heading-p))
        (org-up-heading-safe)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)
      (goto-char buf-pos))))

(defun ry/markdown-cleanup-org-tables()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^|-" nil t)
      (replace-string "-+-" "-|-" nil (point) (line-end-position)))))

(defun ry/markdown-convert-to-org-tables()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^|-" nil t)
      (replace-string "-|-" "-+-" nil (point) (line-end-position)))))

(defun ry/setup-markdown-mode()
  (ry/markdown-orgtbl-mode))

(defun ry/markdown-orgtbl-mode()
  (require 'org)
  (orgtbl-mode)
  (add-hook (make-variable-buffer-local 'first-change-hook) 'ry/markdown-convert-to-org-tables)
  (add-hook (make-variable-buffer-local 'before-save-hook) 'ry/markdown-cleanup-org-tables))

(defun ry/org-agenda-column-view()
  "View agenda column view"
  (interactive)
  (org-agenda-list)
  (org-agenda-columns))

(defun ry/org-clock-in-history()
  "Show recent clocking history and select a task to clock in"
  (interactive)
  (org-clock-in '(4)))

(defun ry/org-cycle()
  "Cycle visibility at anywhere inside a subtree"
  (interactive)
  (org-back-to-heading)
  (org-cycle))

(defun ry/org-find-backlink-action (marker)
  (interactive)
  (pop-to-buffer (marker-buffer marker) t)
  (goto-char marker)
  (org-show-entry))

(defun ry/org-find-backlinks ()
  "Find backlinks of current heading"
  (interactive)
  (let* ((note-id (org-entry-get-with-inheritance "ID"))
         (id-link (format "id:%s" note-id))
         (matched-files
          (ry/grep-files-in-directory id-link org-directory "*.org" t)))
      (if matched-files
          (let ((helm-org-ql-actions (list (cons "Show heading in a new window" 'ry/org-find-backlink-action))))
            (helm :prompt "Link ID: "
                  :sources (helm-org-ql-source matched-files :name "Back Links")
                  :input (format "link:%s" note-id)))
          ;; (org-ql-search matched-files (list 'link id-link))
        (message "No back links found"))))

;; Syntax table
(defun ry//underscore-as-word()
  (modify-syntax-entry ?_ "w"))

(defun ry/add-hook-underscore-as-word (hook-list)
  (mapcar (lambda (mode-hook)
            (add-hook mode-hook 'ry//underscore-as-word))
          hook-list))

;; Projectile
(defun ry/projectile-add-new-project (project-root)
  (interactive (list (read-directory-name "Add a new project: " "/Users/ryan/repos/")))
  (projectile-add-known-project project-root)
  (projectile-switch-project-by-name project-root))

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
  (s-matches? "^.*\.pyc$" relpath))

(defun ry/writeroom-font-size ()
  (if (bound-and-true-p writeroom-mode)
      (cnfonts-increase-fontsize)
    (cnfonts-decrease-fontsize)))

(defun ry/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (progn
          (kill-new file-name)
          (message file-name))
      (error "Buffer not visiting a file"))))

(defun ry/sql-mode-hook ()
  (sqlind-minor-mode 1)
  (setq-local outline-regexp "-- [*\f]+"))
  ;; It's unclear why enabling outline-minor-mode doesn't update the heading fonts
  ;; (outline-minor-mode 1)


(defun ry/git-diff ()
  (interactive)
  (magit-diff "HEAD"))

(defun ry/github-branch-diff ()
  (interactive)
  (let* ((url (magit-get "remote" (magit-upstream-repository) "url"))
         (url (thread-last url
                (s-replace "git@github.com:" "https://github.com/")
                (s-replace-regexp "\\.git$" "")))
         (branch (magit-get-current-branch)))
    (browse-url-generic (format "%s/compare/%s" url branch))))

(defun ry/switch-to-prev-buffer-in-other-window ()
  (interactive)
  (switch-to-buffer-other-window  (other-buffer (current-buffer) 1)))

(defun ry/log-buffer()
  (get-buffer-create "*app-log*"))

(defun ry/log (format-string &rest args)
  (let ((msg (apply 'format (cons format-string args))))
    (with-current-buffer (ry/log-buffer)
      (goto-char (point-max))
      (insert (format "[%s] %s\n" (current-time-string) msg)))))

(defun ry/show-log-buffer ()
  (interactive)
  (switch-to-buffer (ry/log-buffer)))

(defun ry/insert-page-breaker ()
  (interactive)
  (insert 12))

(defun ry/org-insert-sub-heading ()
  (interactive)
  (org-insert-heading)
  (org-demote))

(defun ry/projectile-switch-and-search (project)
  (let ((projectile-switch-project-action 'spacemacs/helm-project-smart-do-search))
    (projectile-switch-project-by-name project)))

(defun ry/org-schedule-tomorrow ()
  (interactive)
  (org-schedule nil "+1d"))

(defun ry/org-schedule-next-week ()
  (interactive)
  (org-schedule nil "+7d"))

(defun ry/today-string ()
  (format-time-string "%Y-%m-%d %A"))

(defun ry/today-string-short ()
  (format-time-string "%Y-%m-%d"))

(defun ry/month-string ()
  (format-time-string "%Y-%m"))

(defun ry/sum-on-region ()
  (interactive)
  (message (format "%s" (ry/pyfunc-on-region "rypy.elfunc" "sum"))))

(defun ry/org-new-interview ()
  "Create an interview template via current interview page in Chrome"
  (interactive)
  (insert (format "* %s - %s\nLink: %s\n\n" (ry/today-string)
                  (-first-item (s-split " - " (ry/osx-browser-title)))
                  (ry/osx-browser-url)))
  (yas-insert-snippet))

(defun ry/org-files (dir)
  (append
    (ry/pyfunc "rypy.elfunc" "filter_files" (expand-file-name dir) '("org"))
    nil))

(cl-defun ry/grep-files-in-directory (pattern directory &optional
                                              (filename-pattern "*")
                                              (fixed-string nil))
  "Find files that matches PATTERN in DIRECTORY"
  (let* ((grep-command (format "find -L %s -name '%s' | xargs grep -l%s '%s'"
                               directory filename-pattern
                               (or (and fixed-string "F") "") pattern))
         (grep-result (s-trim (shell-command-to-string grep-command))))
    (if (s-blank? grep-result)
        nil
      (s-split "\n" grep-result))))
;; Test Case
;; (ry/grep-files-in-directory "index.org::*Organization structure]"
;;                             org-directory "*.org" t)

(defun ry/table-count-unplanned-tasks (l)
  (length (-filter (apply-partially 's-ends-with-p "*") l)))

(defun ry/table-count-unplanned-hours (tname thour)
  "Count number of hours spending on unplanned tasks"
  (let* ((unplanned (-map (lambda (x) (if (s-ends-with? "*" x) 1 0)) tname))
         (thour (-map 'string-to-number thour))
         (unplanned-hours (mapcar* '* unplanned thour)))
    (apply '+ unplanned-hours)))

(defun ry/find-checkbox-in-current-buffer ()
  "Find unticked checkbox in current buffer"
  (interactive)
  (org-occur "- \\[ \\]"))

(defun ry/reload-prettify-symbols ()
  (interactive)
  (setq prettify-symbols-alist
        '(("[ ]" . "☐")
          ("[X]" . "☑")
          ;; ("[-]" . "⊟")
          ("=>" .  "➔")
          ("(+)" .  "✔")
          ("(-)" .  "✘")))
  (prettify-symbols-mode 1))

(defun ry/declare-prefix-for-mode (mode key name &rest more)
  "Declare a list of prefix PREFIX-LIST for major mode MODE. PREFIX-LIST is a list with each element containing a key sequence and a name"
  (while key
    (spacemacs/declare-prefix-for-mode mode key name)
    (setq key (pop more) name (pop more))))

(defun ry/declare-prefix-for-minor-mode (mode key name &rest more)
  "Declare a list of prefix PREFIX-LIST for minor mode MODE. PREFIX-LIST is a list with each element containing a key sequence and a name"
  (while key
    (spacemacs/declare-prefix-for-minor-mode mode key name)
    (setq key (pop more) name (pop more))))

(defun ry/org-add-link-on-region ()
  (interactive)
  (let ((desc (buffer-substring (region-beginning) (region-end)))
        (link (read-string "Link:")))
    (delete-region (region-beginning) (region-end))
    (insert (format "[[%s][%s]]" link desc))))

(defun ry//org-desc-from-filepath (fpath)
  "Generate link description from filepath"
  (-last-item (s-split "/" fpath)))

(defun ry/org-insert-file-link ()
  "Insert a file link with description as filename"
  (interactive)
  (let* ((fpath (org-link-complete-file))
         (fname (ry//org-desc-from-filepath fpath)))
    (insert (format "[[%s][%s]]" fpath fname))))

(defun ry/org-capture-webpage-template ()
  "Content template for taking notes for a specific web page"
  (format "** %s\nLink: %s\n%s" (ry/osx-browser-title) (ry/osx-org-link-from-current-webpage) "%?"))

(defun ry/delete-whitespace-lines ()
  "Remove lines that only contains whitespace"
  (interactive)
  (let ((evil-ex-current-buffer (current-buffer)))
    (save-excursion
      (evil-ex-execute "g/^ +$/d"))))

(defun ry/selected-text-to-bullet-list (beg end)
  "Remove all blank lines in the region and convert the text to an ordered list in org-mode."
  (interactive "r")
  (save-excursion
    (let ((content (->> (buffer-substring beg end)
                       (s-split "\n")
                       (--filter (not (s-matches? "^ *$" it)))
                       (--map (format "- %s" it))
                       (s-join "\n"))))
      (kill-region beg end)
      (insert (concat content "\n")))))

(defun ry/word-translate-clipboard (&optional stdout)
  "Translate selected words and save the result to an Excel sheet"
  (let ((output-buffer (get-buffer-create "*word_translate*"))
        (command (format "pbpaste | word_translate.py %s"
                         (if stdout "--stdout" ""))))
    (with-current-buffer output-buffer
      (erase-buffer))
    (async-shell-command command output-buffer output-buffer)))

(defun ry/word-translate-to-excel ()
  (interactive)
  (ry/osx-copy)
  (ry/word-translate-clipboard))

(defun ry/word-translate-to-buffer ()
  (interactive)
  (ry/osx-copy)
  (ry/word-translate-clipboard t))


(defun ry/chatgpt ()
  (interactive)
  (let ((chatgpt-buffer
         (make-comint-in-buffer "chatgpt" "*chatgpt*" "/bin/zsh" nil "-c" "chatgpt")))
    (pop-to-buffer chatgpt-buffer)))

(defun string-equal-ignore-case (str1 str2)
  "FIX for org-ai. Compare two strings for equality, ignoring case."
  (string-equal (downcase str1) (downcase str2)))

(require 'helm-org)
(require 'org-tempo)
(require 'org-ql)
(require 'org-ql-search)
(require 'ry-core)
(require 'ry-org)
(require 'ry-orgx)
(require 'ry-orgentry)
(require 'ry-orgtable)
(require 'ry-orgtext)
(require 'ry-orglark)
(require 'ry-org-journal)
(require 'ry-osx)
(require 'ry-timesheet)
(require 'ry-pyfunc)
(require 'ry-cnfonts)
(require 'ry-search)
(require 'ry-sql)
(require 'ry-debug)
(require 'ry-abbrev)
(require 'ry-http)
(require 'ry-clj)
(require 'ry-alfred)
(require 'ry-pkm)
(require 'ry-tablex)
(require 'ry-writing)
;; (require 'ry-archived)
;; (require 'helm-org-ql)
