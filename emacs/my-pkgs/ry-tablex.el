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
(defun ry/org-tablex-edit-full-table
    (interactive)
  (let ((table-id (ry/tablex-get-table-id)))
    (find-file-other-window)))

(defun org-dblock-write:tablex (params)
  (let* ((table-id (plist-get params :id))
         (output (gethash "display" (ry/tablex-render table-id)))
         (prop-output (propertize output 'line-spacing 0)))
    (insert prop-output)))

;; Tablex Core Wrapper
(defun ry/tablex-render (table-id)
  (ry/pyfunc "rypy.tablex" "table_render" table-id))

(defun ry/tablex-get-table (table-id)
  (ry/pyfunc "rypy.tablex" "table_get" table-id))

(defun ry/tablex-create (column-names)
  (ry/pyfunc "rypy.tablex" "table_create" column-names))

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
    (ry/tablex-register-font-face)
    (setq ry/tablex-init-completed t)))

(ry/tablex-init)
(provide 'ry-tablex)
