;; -*- lexical-binding: t -*-
;; Core and Fudamental functions that makes elisp programming easier

;; Alist
(defun ryc/al-get (key alist)
  "Get value for KEY in ALIST"
  (cdr (assoc key alist)))

;; Plist
(defun ryc/plist-keys (plist)
  "Get a list of all the keys in a plist."
  (let ((result '()))
    (while plist
      (setq result (cons (car plist) result))
      (setq plist (cddr plist)))
    (reverse result)))

(defun ryc/plist-values (plist)
  "Get a list of all the values in a plist."
  (let ((keys (plist-keys plist)))
    (mapcar (lambda (key) (plist-get plist key))
            keys)))

(defun ryc/plist-to-alist (plist)
  "Convert a plist to an alist."
  (let ((result '()))
    (while plist
      (setq result (cons (cons (car plist) (cadr plist)) result))
      (setq plist (cddr plist)))
    (reverse result)))

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

;; file
(defun ryc/spit (filename content)
  "Write CONTENT to FILENAME, similar to spit function in Clojure"
  (with-temp-buffer
    (insert content)
    (when (file-writable-p filename)
      (write-region (point-min) (point-max) filename))))

(defun ryc/slurp (filename)
  "Read content of filename into a string"
  (if (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (buffer-string))))

(defun ryc/var-store (exp filename)
  "Store the value of EXP into file FILENAME"
  (with-temp-file filename
    (prin1 exp (current-buffer))))

(defun ryc/var-load (filename)
  "Read sexp from filename and return a lisp object"
  (with-temp-buffer
      (insert-file-contents filename)
      (read (current-buffer))))

;; date time
(defun ryc/string-to-timestamp (time-string time-format)
  "Given a time string return unix timestamp"
  (thread-last "date -j -f '{time-format}' '{time-string}' '+%s'"
               (s-replace "{time-format}" time-format)
               (s-replace "{time-string}" time-string)
               (shell-command-to-string)
               (string-to-number)))

;; Alias name that can be remembered more easily
(defalias 's-replace-regexp 'replace-regexp-in-string)
(defalias 'range 'number-sequence)

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

