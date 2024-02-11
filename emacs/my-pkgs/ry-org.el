(setq ry/org-archive-file-suffix "_archive")

(defun ry/org-smart-archive ()
  "Archive current heading to specified archive file =ry/org-smart-archive-file=
if the current heading is level-1, then move it to root in archived file.
Otherwise, move it to the same topic-level heading in archvied file.

For example, a heading with path `Heading 1/Sub-heading 2` will move to `Heading 1` in archvied file"
  (interactive)
  (let* ((archive-file (format "%%s%s" ry/org-archive-file-suffix))
         (org-archive-location (if (> (org-current-level) 1)
                                   (format "%s::* %s" archive-file (first (org-get-outline-path)))
                                 (format "%s::" archive-file))))
    (org-archive-subtree)))

(defun ry/org-id-update-id-locations ()
  "Update org-id locations for all org and archive files"
  (interactive)
  (org-id-update-id-locations
   (append
     (directory-files-recursively org-directory "\\.org\\'")
     (directory-files-recursively org-directory "\\.org_archive\\'"))
   t))

(defun ry/org-heading-to-indirect-buffer ()
  "Similar to =org-tree-to-indirect-buffer= but this function is
able to create a unique indirect buffer for each individual heading."
  (interactive)
  (let* ((bname (format "%s-%s-1" (buffer-name) (org-get-heading 'no-tags)))
         (buf (get-buffer bname)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (org-tree-to-indirect-buffer t))))

(defun ry/org-heading-to-indirect-buffer-with-log ()
  "Call =ry/org-heading-to-indirect-buffer= and at the same time
emit heading_click event if the heading is an indexed entry
(i.e. having org-id)"
  (interactive)
  (save-buffer)
  (ry/org-heading-to-indirect-buffer)
  (when-let ((id (org-id-get)))
    (ry/log-heading-click (list :id id))))

(defun ry/org-capture (template-key note)
  "Create a note based via org-capture based on TEMPLATE-KEY with content NOTE"
  (org-capture nil template-key)
  (insert note)
  (org-capture-finalize))

(defun ry/org-bold-lines-to-headings ()
  (interactive)
  (save-restriction
    (widen)
    (org-previous-visible-heading 1)
    (let* ((level (org-current-level))
           (stars (s-repeat (+ level 1) "*")))
      (->> (ry/org-get-content)
           (s-split "\n")
           (--map (s-replace-regexp
                    "^\\*\\(.*\\)\\*$"
                    (format "%s \\1" stars)
                    it))
           (s-join "\n")
           (ry/org-set-current-content)))))

(provide 'ry-org)
