(setq emoji-table
      '(("yes" . "✔")
        ("no" . "✘")
        ("star" . "★")
        ("right-arrow" . "➔")))

(defun ry//helm-emoji-candidates ()
  (--map (cons (format "[%s] %s" (car it) (cdr it)) (cdr it)) emoji-table))

(defun ry//helm-emoji-action (candidate)
  (insert candidate))

(defun ry/helm-emoji ()
  "Insert emoji via helm completion"
  (interactive)
  (helm :sources
        (helm-build-sync-source "Emoji"
          :candidates 'ry//helm-emoji-candidates
          :action 'ry//helm-emoji-action)
        :buffer "*helm emoji*"))

(provide 'ry-emoji)
