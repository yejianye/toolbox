(setq ry/org-archive-file-suffix "_archive")

(defun ry/org-smart-archive ()
  "Archive current heading to specified archive file =ry/org-smart-archive-file=
if the current heading is level-1, then move it to root in archived file.
Otherwise, move it to the same topic-level heading in archvied file.

For example, a heading with path `Heading 1/Sub-heading 2` will move to `Heading 1` in archvied file"
  (interactive)
  (let* ((org-archive-location (if (> (org-current-level) 1)
                                   (format "%%s%s::* %s" ry/org-archive-file-suffix (first (org-get-outline-path)))
                                 (format "%s::" ry/org-smart-archive-file))))
    (org-archive-subtree)))

(provide 'ry-org)