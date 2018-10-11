(defun ry/orgapi-get-root ()
  "Return the root element of an Org document"
  (org-element-parse-buffer 'headline)
  )

(defun ry/orgapi-get-children (parent &optional prop-name pattern)
  "Find children whose PROP-NAME matches PATTERN (regex). PATTERN could also be a predicate function."
  (if (and prop-name pattern)
      (if (stringp pattern)
          (--filter (s-matches? pattern (org-element-property prop-name it))
                    (org-element-contents parent))
        (--filter (funcall pattern (org-element-property prop-name it))
                  (org-element-contents parent)))
    (org-element-contents parent)
  ))

(defun ry/orgapi-first-child (parent &optional prop-name pattern)
  (-first-item (ry/orgapi-get-children parent prop-name pattern))
  )

(defun ry/orgapi-last-child (parent &optional prop-name pattern)
  (-last-item (ry/orgapi-get-children parent prop-name pattern))
  )

(defun ry/orgapi-get-contents (item)
  "Get contents of ITEM as string (with no properties)"
  (let ((begin (org-element-property :contents-begin item))
        (end (org-element-property :contents-end item)))
    (buffer-substring-no-properties begin end)))

(defun ry/orgapi-set-contents (item contents)
  "Set CONTENTS as content string of ITEM"
  (let ((begin (org-element-property :contents-begin item))
        (end (org-element-property :contents-end item)))
    (delete-region begin end)
    (goto-char begin)
    (insert contents)))

(defun ry//orgapi-example ()
  (with-temp-buffer
    (insert-file-contents "orgapi-test.org")
    (pp (thread-first (ry/orgapi-get-root)
          (ry/orgapi-first-child)
          (ry/orgapi-first-child :title "Sub-heading-1-1")
          ;; (ry/orgapi-first-child :title
          ;;                        (lambda (x)
          ;;                          (string> x "Sub-heading")))
          (ry/orgapi-get-contents)
          ;; (ry/orgapi-set-contents "Good day!")
          )))
  )

(provide 'ry-orgapi)
