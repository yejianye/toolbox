(defun ry/orgtable-insert-row-after ()
  "Insert row after current line"
  (interactive)
  (org-table-insert-row 1))

(provide 'ry-orgtable)