;;; ry-tablex.el --- Enhanced table with multi-line cell support  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ryan Ye

;; Create
(defun ry/org-tablex-create-interactively ()
  (interactive)
  (let* ((column-names (thread-last (read-string "Enter column names separated by comma: ")
                          (s-split ",")
                          (--map (s-trim it))
                          (ryc/list-to-vector)))
         (table-id (ry/tablex-create column-names))
         (table-block (format "#+begin: tablex :id %s\n#+end:" table-id)))
    (insert table-block)
    (org-ctrl-c-ctrl-c)))

;; Full Table Edit
(defun ry/org-tablex-edit-full-table ()
  (interactive)
  (let* ((table-id (ry/tablex-get-table-id))
         (src-buffer (current-buffer))
         (buffer (generate-new-buffer (format "*Table %s*" table-id)))
         (table-content (ry/tablex-get-table table-id t)))
    (pop-to-buffer buffer)
    (insert table-content)
    (yaml-mode)
    (tablex-edit-mode)
    (evil-normal-state)
    (setq-local ry/tablex-source-table-id table-id)
    (setq-local ry/tablex-source-buffer src-buffer)))

(define-minor-mode tablex-edit-mode
  "A minor mode to edit tablex"
  :lighter "tablex"
  :keymap (make-sparse-keymap)
  (if tablex-edit-mode
      (message "tablex-edit-mode enabled")
    (message "tablex-edit-mode disabled")))

(defun ry/tablex-commit-changes ()
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (buffer (current-buffer))
         (src-buffer ry/tablex-source-buffer)
         (table-id ry/tablex-source-table-id))
    (ry/tablex-save-raw table-id content)
    (delete-window)
    (kill-buffer buffer)
    (display-buffer src-buffer '(display-buffer-reuse-window))
    (goto-char (point-min))
    (re-search-forward (format ":id %s" table-id))
    (org-ctrl-c-ctrl-c)))

(defun ry/tablex-abort-changes ()
  (interactive)
  (let ((buffer (current-buffer))
        (src-buffer ry/tablex-source-buffer))
    (when (yes-or-no-p "Are you sure to abort the changes?")
        (delete-window)
        (kill-buffer buffer)
        (display-buffer src-buffer '(display-buffer-reuse-window)))))

(defun org-dblock-write:tablex (params)
  (let* ((table-id (plist-get params :id))
         (output (gethash "display" (ry/tablex-render table-id)))
         (prop-output (propertize output 'line-spacing 0)))
    (insert prop-output)))

;; Tablex Core Wrapper
(defun ry/tablex-render (table-id)
  (ry/pyfunc "rypy.tablex" "table_render" table-id))

(defun ry/tablex-get-table (table-id yaml-str)
  (ry/pyfunc "rypy.tablex" "table_get" table-id yaml-str))

(defun ry/tablex-create (column-names)
  (ry/pyfunc "rypy.tablex" "table_create" column-names))

(defun ry/tablex-save-raw (table-id content)
  (ry/pyfunc "rypy.tablex" "table_save_raw" table-id content))

;; Look & Feel
(defun ry/tablex-register-font-face ()
  (font-lock-add-keywords
    'org-mode
    `(("^[ \t]*\\([┌│├└].*\\S-\\)"
        1 'org-table prepend))
    'append))

;; Help function
(defun ry/tablex-get-table-id ()
  (save-excursion
    (let ((case-fold-search t)
          (id-pattern ":id \\([0-9-]+\\)"))
      (move-end-of-line nil)
      (re-search-backward "^#\\+begin: tablex")
      (if (re-search-forward id-pattern nil t)
          (substring-no-properties (match-string 1))
        nil))))

(defvar ry/tablex-init-completed nil)
(defun ry/tablex-init ()
  (unless ry/tablex-init-completed
    (message "Initialize ry/tablex module")
    (evil-define-key 'normal tablex-edit-mode-map
      ",cc" 'ry/tablex-commit-changes
      ",ck" 'ry/tablex-abort-changes)
    (ry/tablex-register-font-face)
    (setq ry/tablex-init-completed t)))

(ry/tablex-init)
(provide 'ry-tablex)