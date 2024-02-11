(setq ry-abbrev-table
      '(("zyes" "✔")
        ("zno" "✘")
        ("zstar" "★")
        ("zarrow" "➔")
        ("zidx" "#+INDEX-ENTRIES: level:2")))

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

(defun ry/abbrev-init ()
  (define-abbrev-table 'global-abbrev-table ry-abbrev-table))

(ry/abbrev-init)
(provide 'ry-abbrev)
