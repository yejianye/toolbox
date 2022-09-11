(defun ry//helm-abbrev-candidates ()
  (--map (cons (format "[%s] %s" (car it) (cdr it)) (cdr it)) ry-abbrev-table))

(defun ry//helm-abbrev-action (candidate)
  (insert candidate))

(defun ry/helm-abbrev ()
  "Insert emoji via helm completion"
  (interactive)
  (helm :sources
        (helm-build-sync-source "Abbrev"
          :candidates 'ry//helm-abbrev-candidates
          :action 'ry//helm-abbrev-action)
        :buffer "*helm abbrev*"))

(provide 'ry-abbrev)
