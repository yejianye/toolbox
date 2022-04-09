;; Core and Fudamental functions that makes elisp programming easier

;; Alist
(defun ryc/al-get (key alist)
  "Get value for KEY in ALIST"
  (cdr (assoc key alist)))

;; list
(defun ryc/vector-to-list (vec)
  "Convert vector to list"
  (append vec nil))

;; Buffers and Texts
(defun ryc/read-file-content (filename)
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

(defun ryc//string-in-buffer-p(str &optional start)
  "Check whether a string exists in the buffer"
  (save-excursion
    (widen)
    (goto-char (or start (point-min)))
    (search-forward str nil t)))

(defun ryc/line-begin-pos (&optional lineno)
  "Get point of beginning of the line"
  (save-excursion
    (when lineno
      (goto-line lineno))
    (beginning-of-line)
    (point)))

(defun ryc/line-end-pos (&optional lineno)
  "Get point of end of the line"
  (save-excursion
    (when lineno
      (goto-line lineno))
    (end-of-line)
    (point)))

(defun ryc/current-line-content ()
  "Get content from content line"
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun ryc/replace-at-line (lineno old new)
  "Replace OLD to NEW at line LINENO"
  (let ((start (ryc/line-begin-pos lineno))
        (end (ryc/line-end-pos lineno)))
    (save-excursion
      (goto-line lineno)
      (beginning-of-line)
      (replace-string old new t start end))))

;; Alias name that can be remembered more easily
(defalias 's-replace-regexp 'replace-regexp-in-string)
(defalias '-> 'thread-first)
(defalias '->> 'thread-last)
(defalias 'fn 'lambda)

;; Macros
(defmacro make-cmd (&rest body)
  "create a lambda function for a command"
  `(lambda () (interactive)
     ,@body))

(provide 'ry-core)

