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

;; Resize columns
(defun ry/org-tablex-column-current-index ()
  (let* ((table-id (ry/tablex-get-table-id))
         (col-pos (thread-last (ry/tablex-render table-id)
                               (gethash "col_pos")
                               (ryc/vector-to-list)))
         (cur-col (current-column)))
    (thread-last col-pos
                  (--filter (<= it cur-col))
                  (length)
                  (1-))))

(defun ry/org-tablex-column-width-inc (&optional val)
  (interactive)
  (let* ((step (or val 5))
         (table-id (ry/tablex-get-table-id))
         (col-idx (ry/org-tablex-column-current-index)))
    (ry/tablex-column-width-inc table-id col-idx step)
    (ry/org-tablex-redisplay)))

(defun ry/org-tablex-column-width-dec ()
  (interactive)
  (ry/org-tablex-column-width-inc -5))

(defun ry/org-tablex-column-insert-after ()
  (interactive)
  (ry/org-tablex-column-insert t))

(defun ry/org-tablex-column-insert-before ()
  (interactive)
  (ry/org-tablex-column-insert nil))

(defun ry/org-tablex-column-insert (after)
  (let* ((column-name (read-string "Enter column name: "))
         (table-id (ry/tablex-get-table-id))
         (col-idx (ry/org-tablex-column-current-index)))
    (ry/tablex-column-insert table-id (if after (1+ col-idx) col-idx) column-name)
    (ry/org-tablex-redisplay)))

(defun ry/org-tablex-column-remove ()
  (interactive)
  (let* ((table-id (ry/tablex-get-table-id))
         (col-idx (ry/org-tablex-column-current-index)))
    (ry/tablex-column-remove table-id col-idx)
    (ry/org-tablex-redisplay)))

;; Refresh table
(defun ry/org-tablex-redisplay ()
  "Re-render the tablex at point"
  (interactive)
  (let ((line-pos (line-number-at-pos))
        (col-pos (current-column))
        (case-fold-search t))
      (move-end-of-line nil)
      (re-search-backward "^#\\+begin: tablex")
      (org-ctrl-c-ctrl-c)
      (goto-char (point-min))
      (forward-line (1- line-pos))
      (move-to-column col-pos)))

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

(defun ry/tablex-get-table (table-id &optional yaml-str)
  (ry/pyfunc "rypy.tablex" "table_get" table-id yaml-str))

(defun ry/tablex-create (column-names)
  (ry/pyfunc "rypy.tablex" "table_create" column-names))

(defun ry/tablex-save-raw (table-id content)
  (ry/pyfunc "rypy.tablex" "table_save_raw" table-id content))

(defun ry/tablex-column-width-inc (table-id column-index val)
  (ry/pyfunc "rypy.tablex" "table_column_width_inc" table-id column-index val))

(defun ry/tablex-column-insert (table-id column-index column-name)
  (ry/pyfunc "rypy.tablex" "table_column_insert" table-id column-index column-name))

(defun ry/tablex-column-remove (table-id column-index)
  (ry/pyfunc "rypy.tablex" "table_column_remove" table-id column-index))

;; Look & Feel
(defun ry/tablex-register-font-face ()
  (font-lock-add-keywords
    'org-mode
    `(("^[ \t]*\\([┌│├└].*\\S-\\)"
        1 'org-table prepend))
    'append))

;; Hydra
(defhydra ry/hydra-org-tablex (:color red :hint nil)
  "Tablex"
  ("t" ry/org-tablex-create-interactively "Create Table"))

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
