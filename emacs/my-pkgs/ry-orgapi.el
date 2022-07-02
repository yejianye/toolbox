(defun ry/org-get-file-property (keyword &optional multi)
  "Get file property KEYWORD of current buffer"
  (let ((result
         (thread-last (list keyword)
           (org-collect-keywords)
           (assoc keyword))))
    (if multi
        (cdr result)
      (cadr result))))

(defun ry/org-get-content ()
  "Get body content of current entry"
  (let* ((ele (org-element-at-point))
         (begin (org-element-property :contents-begin ele))
         (end (org-element-property :contents-end ele)))
    (buffer-substring-no-properties begin end)))

(defun ry/org-set-current-content (contents)
  "Set body content of current entry"
  (ry/orgapi-set-contents (org-element-at-point) contents))


;; Low-level API
;; Operating org nodes within an org-mode file
(defun ry/orgapi-get-root ()
  "Return the root element of an Org document"
  (org-element-parse-buffer 'headline))
  

(defun ry/orgapi-get-children (parent &optional prop-name pattern)
  "Find children whose PROP-NAME matches PATTERN (regex). PATTERN could also be a predicate function."
  (if (and prop-name pattern)
      (if (stringp pattern)
          (--filter (s-matches? pattern (org-element-property prop-name it))
                    (org-element-contents parent))
        (--filter (funcall pattern (org-element-property prop-name it))
                  (org-element-contents parent)))
    (org-element-contents parent)))
  

(defun ry/orgapi-first-child (parent &optional prop-name pattern)
  (-first-item (ry/orgapi-get-children parent prop-name pattern)))
  

(defun ry/orgapi-last-child (parent &optional prop-name pattern)
  (-last-item (ry/orgapi-get-children parent prop-name pattern)))
  

(defun ry/orgapi-get-node-by-heading (heading &rest more-headings)
  "Get node by heading. You can also specify a list of headings"
  (ry/orgapi-get-node-from-path (-insert-at 0 heading more-headings)))
  

(defun ry/orgapi-get-node-from-path (headings)
  "Get node by a list of headings"
  (--reduce-from (ry/orgapi-first-child acc :title it)
                 (ry/orgapi-get-root)
                 headings))
  

(defun ry/orgapi-get-contents (item)
  "Get contents of ITEM as string (with no properties)"
  (let ((begin (org-element-property :contents-begin item))
        (end (org-element-property :contents-end item)))
    (if begin
        (buffer-substring-no-properties begin end)
      "")))

(defun ry/orgapi-set-contents (item contents &optional refresh)
  "Set CONTENTS as content string of ITEM"
  (let* ( ;; if contents is empty, :contents-begin would be nil.
        ;; In that case, :end is the end of headline which should be the beginning of contents
         (begin (or (org-element-property :contents-begin item)
                    (org-element-property :end item)))
         (end (or (org-element-property :contents-end item) begin)))
    (when (< begin end)
      (delete-region begin end))
    (goto-char begin)
    (insert (if (s-ends-with? "\n" contents)
                contents
              (format "%s\n" contents)))
    (when refresh
      (ry//orgapi-refresh-item item))))
    

(defun ry/orgapi-prepend-contents (item contents &optional refresh)
  "Prepend CONTENTS to ITEM"
  (let ((current-contents (ry/orgapi-get-contents item)))
        
    (ry/orgapi-set-contents item
                            (format "%s\n%s" contents current-contents)
                            refresh)))
  

(defun ry/orgapi-append-contents (item contents &optional refresh)
  "Append CONTENTS to ITEM"
  (let ((current-contents (ry/orgapi-get-contents item)))
    (ry/orgapi-set-contents item
                            (format "%s%s" current-contents contents)
                            refresh)))

(defun ry/orgapi-prepend-contents (item contents &optional refresh)
  "Append CONTENTS to ITEM"
  (let ((current-contents (ry/orgapi-get-contents item)))
    (ry/orgapi-set-contents item
                            (format "%s%s" contents current-contents)
                            refresh)))

(defun ry/orgapi-insert-child (item heading &optional contents prepend)
  "Insert a child under ITEM with HEADING and CONTENTS.
Return newly created child node.
"
  (let* ((level (org-element-property :level item))
         (child-contents (format "%s %s\n%s"
                                 (s-repeat (1+ level) "*")
                                 heading
                                 (or contents "")))
         (item (if prepend
                   (ry/orgapi-prepend-contents item child-contents t)
                 (ry/orgapi-append-contents item child-contents t))))
    (ry/orgapi-first-child item :title heading)))

(defun ry/orgapi-tset-child (item heading &optional contents)
  "Insert a child under ITEM with HEADING if no such child exists.
Return the newly created child or existing child with HEADING."
  (let ((child (ry/orgapi-first-child item :title heading)))
        
    (or child
        (ry/orgapi-insert-child item heading contents))))
    

(defun ry//orgapi-refresh-item (item)
  (let ((headline-path
         (cl-loop for cur = item then parent
                  for parent = (org-element-property :parent cur)
                  while (eq (org-element-type cur) 'headline)
                  collect (org-element-property :title cur))))
                  
    (--reduce-r-from (ry/orgapi-first-child acc :title it)
                     (ry/orgapi-get-root)
                     headline-path)))
    


(defun ry//orgapi-example ()
  (with-temp-buffer
    (insert-file-contents "orgapi-test.org")
    (ry/orgapi-get-node-by-heading "Heading1" "Sub-heading-1-1")))
    ;; (thread-first (ry/orgapi-get-root)
    ;;   (ry/orgapi-first-child)
    ;;   (ry/orgapi-first-child :title "Sub-heading-1-1")
    ;;   ;; (org-element-put-property :title "Good day")
    ;;   ;; (org-element-interpret-data)
    ;;   ;; (ry/orgapi-tset-child "Good day")
    ;;   ;; (ry/orgapi-first-child :title
    ;;   ;;                        (lambda (x)
    ;;   ;;                          (string> x "Sub-heading")))
    ;;   ;; (ry/orgapi-get-contents)
    ;;   ;; (ry/orgapi-prepend-contents "Good day!")
    ;;   (pp)
    ;;   )
    ;; (buffer-string)
    
  
;; (ry//orgapi-example)

(provide 'ry-orgapi)
