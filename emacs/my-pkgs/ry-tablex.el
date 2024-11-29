;;; ry-tablex.el --- Enhanced table with multi-line cell support  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ryan Ye
(defvar ry/tablex-yanked-table-id nil)

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
    (org-ctrl-c-ctrl-c)
    (ry/org-tablex-goto-cell 0 0)))

(defun ry/org-tablex-yank ()
  (interactive)
  (let ((table-id (ry/tablex-get-table-id)))
    (setq ry/tablex-yanked-table-id table-id)
    (message (format "Table %s yanked" table-id))))

(defun ry/org-tablex-paste ()
  (interactive)
  (if ry/tablex-yanked-table-id
      (let* ((table-id (ry/tablex-duplicate ry/tablex-yanked-table-id))
             (tablex-block (format "\n#+begin: tablex :id %s\n#+end:" table-id)))
        (insert tablex-block)
        (ry/org-tablex-goto-beginning)
        (org-ctrl-c-ctrl-c)
        (ry/org-tablex-goto-cell 0 0))))

;; Full Table Edit
(defun ry/org-tablex-edit-full-table ()
  (interactive)
  (let* ((table-id (ry/tablex-get-table-id))
         (src-buffer (current-buffer))
         (buffer (generate-new-buffer (format "*Table %s*" table-id)))
         (table-content (ry/tablex-get-table-yaml table-id)))
    (pop-to-buffer buffer)
    (insert table-content)
    (yaml-mode)
    (tablex-edit-mode)
    (evil-normal-state)
    (setq-local ry/tablex-source-table-id table-id)
    (setq-local ry/tablex-source-buffer src-buffer)))

;; Movement
(defun ry/tablex-find-pos (array val)
  "Find the index of a sorted ARRAY where the indexed element is smaller or equal to VAL"
  (thread-last array
               (--filter (<= it val))
               (length)
               (1-)))

(defun ry/org-tablex-pos-map ()
  (let* ((table-id (ry/tablex-get-table-id))
         (render-data (ry/tablex-render table-id)))
    (list :rows (ryc/vector-to-list (gethash "row_pos" render-data))
          :cols (ryc/vector-to-list (gethash "col_pos" render-data)))))

(defun ry/org-tablex-goto-beginning ()
  (interactive)
  (let ((case-fold-search t))
    (move-end-of-line nil)
    (re-search-backward "^#\\+begin: tablex")))

(defun ry/org-tablex-goto-cell (row col)
  "Goto specific row and column of current tablex"
  (let* ((pos-map (ry/org-tablex-pos-map))
         ;; (margin-left 10)
         ;; (margin-right 15)
         (table-id (ry/tablex-get-table-id))
         (display-width (ry/tablex-max-display-width))
         (hscroll (ry/org-tablex-hscroll-get table-id))
         (row-offset (nth row (plist-get pos-map :rows)))
         (col-offset (- (nth col (plist-get pos-map :cols)) hscroll)))
    ;; (if (< col-offset 0)
    ;;     (ry/org-tablex-hscroll-left (+ (- col-offset) margin-left))
    ;;   (setq col-offset margin-left))
    ;; (if (> col-offset display-width)
    ;;     (ry/org-tablex-hscroll-right (+ margin-right (- col-offset display-width)))
    ;;   (setq col-offset (- display-width margin-right)))
    (ry/org-tablex-goto-beginning)
    (forward-line (1+ row-offset))
    (move-to-column (1+ col-offset))))

(defun ry/org-tablex-column-current-index ()
  (let* ((col-pos (plist-get (ry/org-tablex-pos-map) :cols))
         (cur-col (+ (current-column) (ry/org-tablex-hscroll-get))))
    (ry/tablex-find-pos col-pos cur-col)))

(defun ry/org-tablex-row-current-index ()
  (let* ((row-pos (plist-get (ry/org-tablex-pos-map) :rows))
         (start-row (save-excursion
                      (ry/org-tablex-goto-beginning)
                      (line-number-at-pos)))
         (cur-row (- (line-number-at-pos) (1+ start-row))))
    (ry/tablex-find-pos row-pos cur-row)))

(defun ry/org-tablex-next-column ()
  (interactive)
  (let* ((cur-col (ry/org-tablex-column-current-index))
         (cur-row (ry/org-tablex-row-current-index))
         (total-cols (length (plist-get (ry/org-tablex-pos-map) :cols))))
    (unless (= cur-col (1- total-cols))
      (ry/org-tablex-goto-cell cur-row (1+ cur-col)))))

(defun ry/org-tablex-prev-column ()
  (interactive)
  (let* ((cur-col (ry/org-tablex-column-current-index))
         (cur-row (ry/org-tablex-row-current-index)))
    (unless (= cur-col 0)
      (ry/org-tablex-goto-cell cur-row (1- cur-col)))))

(defun ry/org-tablex-next-row ()
  (interactive)
  (let* ((cur-col (ry/org-tablex-column-current-index))
         (cur-row (ry/org-tablex-row-current-index))
         (total-rows (length (plist-get (ry/org-tablex-pos-map) :rows))))
    (unless (= cur-row (1- total-rows))
      (ry/org-tablex-goto-cell (1+ cur-row) cur-col))))

(defun ry/org-tablex-prev-row ()
  (interactive)
  (let* ((cur-col (ry/org-tablex-column-current-index))
         (cur-row (ry/org-tablex-row-current-index)))
    (unless (= cur-row 0)
      (ry/org-tablex-goto-cell (1- cur-row) cur-col))))

;; Resize columns
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

;; Insert / Remove Column
(defun ry/org-tablex-column-insert-after ()
  (interactive)
  (ry/org-tablex-column-insert t))

(defun ry/org-tablex-column-insert-before ()
  (interactive)
  (ry/org-tablex-column-insert nil))

(defun ry/org-tablex-column-insert (after)
  (let* ((column-name (read-string "Enter column name: "))
         (table-id (ry/tablex-get-table-id))
         (col-idx (ry/org-tablex-column-current-index))
         (row-idx (ry/org-tablex-row-current-index)))
    (ry/tablex-column-insert table-id (if after (1+ col-idx) col-idx) column-name)
    (ry/org-tablex-redisplay)
    (if after
        (ry/org-tablex-goto-cell row-idx (1+ col-idx))
      (ry/org-tablex-goto-cell row-idx col-idx))))

(defun ry/org-tablex-column-remove ()
  (interactive)
  (let* ((table-id (ry/tablex-get-table-id))
         (col-idx (ry/org-tablex-column-current-index))
         (row-idx (ry/org-tablex-row-current-index))
         (total-cols (length (plist-get (ry/org-tablex-pos-map) :cols))))
    (unless (= total-cols 1)
      (ry/tablex-column-remove table-id col-idx)
      (ry/org-tablex-redisplay)
      (if (> col-idx 0)
          (ry/org-tablex-goto-cell row-idx (1- col-idx))
        (ry/org-tablex-goto-cell row-idx 0)))))

(defun ry/org-tablex-column-rename ()
  (interactive)
  (let* ((table-id (ry/tablex-get-table-id))
         (col-idx (ry/org-tablex-column-current-index))
         (columns (gethash "columns" (ry/tablex-get-table table-id)))
         (old-name (gethash "name" (aref columns col-idx)))
         (new-name (read-string "Enter new column name: " old-name)))
    (unless (string= new-name old-name)
      (ry/tablex-column-rename table-id col-idx new-name)
      (ry/org-tablex-redisplay))))

;; Insert / Remove Row
(defun ry/org-tablex-row-insert-after ()
  (interactive)
  (ry/org-tablex-row-insert t))

(defun ry/org-tablex-row-insert-before ()
  (interactive)
  (ry/org-tablex-row-insert nil))

(defun ry/org-tablex-row-insert (after)
  (let* ((table-id (ry/tablex-get-table-id))
         (row-idx (ry/org-tablex-row-current-index))
         (col-idx (ry/org-tablex-column-current-index)))
    (ry/tablex-row-insert table-id (if after (1+ row-idx) row-idx))
    (ry/org-tablex-redisplay)
    (if after
        (ry/org-tablex-goto-cell (1+ row-idx) col-idx)
      (ry/org-tablex-goto-cell row-idx col-idx))))

(defun ry/org-tablex-row-remove ()
  (interactive)
  (let* ((table-id (ry/tablex-get-table-id))
         (row-idx (ry/org-tablex-row-current-index))
         (col-idx (ry/org-tablex-column-current-index))
         (total-rows (length (plist-get (ry/org-tablex-pos-map) :rows))))
    (ry/tablex-row-remove table-id row-idx)
    (if (= total-rows 1)
      (ry/tablex-row-insert table-id 0))
    (ry/org-tablex-redisplay)
    (if (> row-idx 0)
        (ry/org-tablex-goto-cell (1- row-idx) col-idx)
      (ry/org-tablex-goto-cell 0 col-idx))))

;; Edit Cell
(defun ry/org-tablex-cell-paste ()
  (interactive)
  (let* ((table-id (ry/tablex-get-table-id))
         (cur-row (ry/org-tablex-row-current-index))
         (cur-col (ry/org-tablex-column-current-index))
         (text (current-kill 0 t)))
    (ry/tablex-cell-update table-id cur-row cur-col text)
    (ry/org-tablex-redisplay)
    (ry/org-tablex-goto-cell cur-row cur-col)))

(defun ry/org-tablex-cell-edit ()
  (interactive)
  (let* ((table-id (ry/tablex-get-table-id))
         (cur-row (ry/org-tablex-row-current-index))
         (cur-col (ry/org-tablex-column-current-index))
         (src-buffer (current-buffer))
         (buffer (generate-new-buffer (format "*Edit Tablex %s (%d, %d)*"
                                              table-id cur-row cur-col)))
         (cell-content (ry/tablex-cell-get table-id cur-row cur-col)))
    (pop-to-buffer buffer '((display-buffer-below-selected) (window-height .10)))
    (insert cell-content)
    (local-set-key (kbd "C-c C-c") 'ry/org-tablex-cell-edit-commit)
    (local-set-key (kbd "C-c C-k") 'ry/org-tablex-cell-edit-abort)
    (undo-tree-mode 1)
    (setq-local ry/tablex-source-table-id table-id)
    (setq-local ry/tablex-source-buffer src-buffer)
    (setq-local ry/tablex-source-row cur-row)
    (setq-local ry/tablex-source-col cur-col)))

(defun ry/org-tablex-cell-edit-inline ()
  (interactive)
  (let* ((table-id (ry/tablex-get-table-id))
         (cur-row (ry/org-tablex-row-current-index))
         (cur-col (ry/org-tablex-column-current-index))
         (cell-content (ry/tablex-cell-get table-id cur-row cur-col))
         (new-content (read-string "Edit: " cell-content)))
    (ry/tablex-cell-update table-id cur-row cur-col new-content)
    (ry/org-tablex-redisplay)))

(defun ry/org-tablex-cell-edit-commit ()
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (buffer (current-buffer))
         (src-buffer ry/tablex-source-buffer)
         (table-id ry/tablex-source-table-id)
         (row ry/tablex-source-row)
         (col ry/tablex-source-col))
    (ry/tablex-cell-update table-id row col content)
    (delete-window)
    (kill-buffer buffer)
    (display-buffer src-buffer '(display-buffer-reuse-window))
    (ry/org-tablex-goto-beginning)
    (org-ctrl-c-ctrl-c)
    (ry/org-tablex-goto-cell row col)
    (ry/hydra-org-tablex/body)))

(defun ry/org-tablex-cell-edit-abort ()
  (interactive)
  (let ((buffer (current-buffer))
        (src-buffer ry/tablex-source-buffer))
    (delete-window)
    (kill-buffer buffer)
    (display-buffer src-buffer '(display-buffer-reuse-window))
    (ry/hydra-org-tablex/body)))

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

(defun ry/org-tablex-redisplay-all ()
  "Re-render all tablex in current buffer"
  (interactive)
  (message "ry/org-tablex-redisplay-all called")
  (let ((line-pos (line-number-at-pos))
        (col-pos (current-column))
        (case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward "^#\\+begin: tablex" nil t)
      (org-ctrl-c-ctrl-c))
    (goto-char (point-min))
    (forward-line (1- line-pos))
    (move-to-column col-pos)))

;; Horizontal Scrolling
(defun ry/org-tablex-hscroll-get (&optional table-id)
  (unless (local-variable-p 'ry/org-tablex-hscroll-map)
    (setq-local ry/org-tablex-hscroll-map (make-hash-table :test 'equal)))
  (let ((table-id (or table-id (ry/tablex-get-table-id))))
    (or (gethash table-id ry/org-tablex-hscroll-map)
        (puthash table-id 0 ry/org-tablex-hscroll-map))))

(defun ry/org-tablex-hscroll-left (&optional table-id offset)
  (interactive)
  (let ((table-id (or table-id (ry/tablex-get-table-id)))
        (offset (or offset 5)))
    (ry/org-tablex-hscroll-right table-id (- offset))))

(defun ry/org-tablex-hscroll-right (&optional table-id offset)
  (interactive)
  (let* ((table-id (or table-id (ry/tablex-get-table-id)))
         (cur-scroll (ry/org-tablex-hscroll-get table-id))
         (col-idx (ry/org-tablex-column-current-index))
         (row-idx (ry/org-tablex-row-current-index))
         (offset (or offset 5))
         (new-scroll (+ cur-scroll offset)))
    (if (< new-scroll 0)
        (puthash table-id 0 ry/org-tablex-hscroll-map)
      (puthash table-id (+ cur-scroll offset) ry/org-tablex-hscroll-map))
    (ry/org-tablex-redisplay)
    (ry/org-tablex-goto-cell row-idx col-idx)))

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

;; Dynamic Block
(defun ry/tablex-max-display-width ()
  (- (window-width) (* (org-current-level) org-indent-indentation-per-level)))

(defun org-dblock-write:tablex (params)
  (let* ((table-id (format "%s" (plist-get params :id)))
         (display-width (ry/tablex-max-display-width))
         (hscroll (ry/org-tablex-hscroll-get table-id))
         (output (thread-last (gethash "display" (ry/tablex-render table-id))
                              (s-split "\n")
                              (--map (substring-by-display-width it hscroll (1- display-width)))
                              (--map (concat "\u200B" it))
                              (s-join "\n")))
         (prop-output (propertize output 'line-spacing 0)))
    (insert prop-output)
    (setq-local ry/org-tablex-exists t)))

;; Hooks
(defun ry/org-tablex-on-load ()
  (ry/org-tablex-redisplay-all))

(defun ry/org-tablex-on-window-resize (window)
  (let ((prev-window-width (if (local-variable-p 'ry/org-tablex-window-width)
                               ry/org-tablex-window-width 0)))
    (when (and (local-variable-p 'ry/org-tablex-exists)
              (/= prev-window-width (window-width)))
      (ry/org-tablex-redisplay-all)
      (setq-local ry/org-tablex-window-width (window-width)))))

;; Tablex Core Wrapper
(defun ry/tablex-render (table-id)
  (ryc/with-cache table-id "tablex-render"
    (ry/pyfunc "rypy.tablex" "table_render" table-id)))

(defun ry/tablex-get-table (table-id)
  (ryc/with-cache table-id "tablex-get-table"
    (ry/pyfunc "rypy.tablex" "table_get" table-id)))

(defun ry/tablex-get-table-yaml (table-id)
    (ry/pyfunc "rypy.tablex" "table_get" table-id t))

(defun ry/tablex-create (column-names)
  (ry/pyfunc "rypy.tablex" "table_create" column-names))

(defun ry/tablex-duplicate (table-id)
  (ry/pyfunc "rypy.tablex" "table_duplicate" table-id))

(defun ry/tablex-save-raw (table-id content)
  (ryc/cache-invalidate table-id)
  (ry/pyfunc "rypy.tablex" "table_save_raw" table-id content))

(defun ry/tablex-column-width-inc (table-id column-index val)
  (ryc/cache-invalidate table-id)
  (ry/pyfunc "rypy.tablex" "table_column_width_inc" table-id column-index val))

(defun ry/tablex-column-insert (table-id column-index column-name)
  (ryc/cache-invalidate table-id)
  (ry/pyfunc "rypy.tablex" "table_column_insert" table-id column-index column-name))

(defun ry/tablex-column-rename (table-id column-index column-name)
  (ryc/cache-invalidate table-id)
  (ry/pyfunc "rypy.tablex" "table_column_rename" table-id column-index column-name))

(defun ry/tablex-column-remove (table-id column-index)
  (ryc/cache-invalidate table-id)
  (ry/pyfunc "rypy.tablex" "table_column_remove" table-id column-index))

(defun ry/tablex-row-insert (table-id row-index)
  (ryc/cache-invalidate table-id)
  (ry/pyfunc "rypy.tablex" "table_row_insert" table-id row-index))

(defun ry/tablex-row-remove (table-id row-index)
  (ryc/cache-invalidate table-id)
  (ry/pyfunc "rypy.tablex" "table_row_remove" table-id row-index))

(defun ry/tablex-cell-get (table-id row-index col-index)
  (ry/pyfunc "rypy.tablex" "table_cell_get" table-id row-index col-index))

(defun ry/tablex-cell-update (table-id row-index col-index content)
  (ryc/cache-invalidate table-id)
  (ry/pyfunc "rypy.tablex" "table_cell_update" table-id row-index col-index content))

;; Look & Feel
(defun ry/tablex-register-font-face ()
  (font-lock-add-keywords
    'org-mode
    `(("^[ \t]*[\u2000-\u200D]\\(.*\\S-\\)"
        1 'org-table prepend))
    'append))

;; (defun ry/tablex-register-font-face ()
;;   (font-lock-add-keywords
;;    'org-mode
;;    `(("^[ \t]*\\([┌│├└].*\\S-\\)"
;;       1 'org-table prepend))
;;    'append))

;; Hydra
(defhydra ry/hydra-org-tablex (:color red :hint nil)
  "Tablex
------------------------------------------------------------------------------
[t] Create Table         [e/c] Edit Cell (inline)    [r] Rename Column
[h] Move to Prev Column  [H] Insert Column to Left   [+] Increase Column Width
[l] Move to Next Column. [L] Insert Column to Right  [-] Decrease Column Width
[k] Move to Prev Row.    [K] Insert Row Above        [D] Delete Column
[j] Move to Next Row.    [J] INsert Row Below        [d] Delete Row
[E] Edit Raw Table       [p] Paste Text to Cell.     [,] Scroll Left
[Y] Yank Table           [P] Paste Table             [.] Scroll Right
[q or any other key to exit]
"
  ("t" ry/org-tablex-create-interactively)
  ("Y" ry/org-tablex-yank :exit t)
  ("P" ry/org-tablex-paste)
  ("h" ry/org-tablex-prev-column)
  ("l" ry/org-tablex-next-column)
  ("k" ry/org-tablex-prev-row)
  ("j" ry/org-tablex-next-row)
  ("L" ry/org-tablex-column-insert-after)
  ("H" ry/org-tablex-column-insert-before)
  ("+" ry/org-tablex-column-width-inc)
  ("-" ry/org-tablex-column-width-dec)
  ("D" ry/org-tablex-column-remove)
  ("r" ry/org-tablex-column-rename)
  ("p" ry/org-tablex-cell-paste)
  ("c" ry/org-tablex-cell-edit-inline)
  ("e" ry/org-tablex-cell-edit :exit t)
  ("E" ry/org-tablex-edit-full-table :exit t)
  ("J" ry/org-tablex-row-insert-after)
  ("K" ry/org-tablex-row-insert-before)
  ("d" ry/org-tablex-row-remove)
  ("," ry/org-tablex-hscroll-left)
  ("." ry/org-tablex-hscroll-right))

;; Help function
(defun substring-by-display-width (str start width)
  "Return a substring of STR starting at display width START with a total display width of WIDTH."
  (let ((current-width -1)
        (result ""))
    (dolist (char (string-to-list str))
      (let ((char-width (string-width (string char))))
        (setq current-width (+ current-width char-width))
        (cond
         ((and (>= current-width start)
               (< current-width (+ start width)))
          (setq result (concat result (string char))))
         ((>= current-width (+ start width))
          (cl-return result)))))
    result))

(defun ry/tablex-get-table-id ()
  (save-excursion
    (let ((case-fold-search t)
          (id-pattern ":id \\([0-9-]+\\)"))
      (ry/org-tablex-goto-beginning)
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
    (add-hook 'window-size-change-functions 'ry/org-tablex-on-window-resize)
    (setq ry/tablex-init-completed t)))

(ry/tablex-init)
(provide 'ry-tablex)
