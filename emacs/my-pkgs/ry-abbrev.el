(setq ry-abbrev-table
      '(("zyes" "✔")
        ("zno" "✘")
        ("zstar" "★")))

(defun ry//helm-abbrev-candidates ()
  (--map (cons (format "[%s] %s" (nth 0 it) (nth 1 it)) (nth 1 it)) ry-abbrev-table))

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
