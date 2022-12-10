;; -*- lexical-binding: t -*-
;; Core and Fudamental functions that makes elisp programming easier

;; Alist
(defun ryc/al-get (key alist)
  "Get value for KEY in ALIST"
  (cdr (assoc key alist)))

;; Plist
(defun ryc/plist-path (plist path)
  "Like JSON path. Get value from PLIST following a list of keys
E.g. (ryc/plist-path '(:a (:b 1) :c 2) '(:a :b)) => 1"
  (--reduce-from (plist-get acc it) plist path))

(defalias 'ryc/plist-merge 'org-combine-plists)

;; list conversion
(defun ryc/vector-to-list (vec)
  "Convert vector to list"
  (append vec nil))

(defun ryc/plist-to-alist (plist)
  "Convert plist to alist"
  (let ((plist (-clone plist))
        (alist '())
        key val)
    (while plist
      (setq key (pop plist)
            val (pop plist))
      (when (keywordp key)
        (setq key (substring (symbol-name key) 1)))
      (push (cons key val) alist))
    alist))

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

;; This should have already been included in dash.el
;; (defalias '-> 'thread-first)
;; (defalias '->> 'thread-last)
(defalias 'fn 'lambda)

;; Macros
(defmacro make-cmd (&rest body)
  "create a lambda function for a command"
  `(lambda () (interactive)
     ,@body))

(defmacro with-hook (mode-hook func-name &rest body)
  `(progn
    (defalias ,func-name
      (function
        (lambda () ,@body)))
    (add-hook ,mode-hook ,func-name)))

(defmacro with-temp-file-buffer (filepath &rest body)
  "Create a temp buffer with the content of FILEPATH
   Change content of buffer has no impact to the original file."
  `(with-temp-buffer
     (insert-file-contents ,filepath)
     ,@body))

(defmacro with-file-buffer (filepath &rest body)
  "Switch to a buffer visiting FILEPATH, create one if the buffer doesn't exist)
And then execute BODY in that buffer"
  `(with-current-buffer (find-file-noselect ,filepath)
     ,@body))


(provide 'ry-core)

