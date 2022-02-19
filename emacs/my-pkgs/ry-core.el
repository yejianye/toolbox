;; Core and Fudamental functions that makes elisp programming easier

;; Buffers and Texts
(defun ry/read-file-content (filename)
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

(defun ry//string-in-buffer-p(str &optional start)
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

(defun ryc/replace-at-line (lineno old new)
  "Replace OLD to NEW at line LINENO"
  (let ((start (ryc/line-begin-pos lineno))
        (end (ryc/line-end-pos lineno)))
    (save-excursion
      (goto-line lineno)
      (beginning-of-line)
      (replace-string old new t start end))))

(setq s-replace-regexp 'replace-regexp-in-string)

(provide 'ry-core)
