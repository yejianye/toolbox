(defvar ry/python-executable "python"
  "Python executable used in ry-pyfunc")

(defun ry/pyfunc (module func-name &rest args)
    (with-temp-buffer
      (if args
        (insert (json-encode args))
        (insert "[]"))
      (ry/log-time "[pyfunc] Time spent in Python Process"
        (call-process-region (point-min) (point-max)
                            ry/python-executable
                            t (list (current-buffer) nil) nil
                            "-mrypy.emacs" module func-name))
      (let* ((json-object-type 'hash-table)
             (result (ry/log-time "[pyfunc] Time spent in JSON Parsing"
                                  (json-read-from-string (buffer-string)))))
        (if (= (gethash "rc" result) 0)
            (progn
              (message "[pyfunc] Time spent in actual python function: %dms"
                       (->> result
                            (gethash "debug")
                            (gethash "time_spent")))
              (gethash "data" result))
          (progn
            (message (gethash "data" result))
            (error (format "Failed to call Python function %s.%s" module func-name)))))))

(defun ry//use-rect-p ()
  (let ((coord (-first-item (extract-rectangle-bounds (region-beginning) (region-end)))))
    (< (car coord) (cdr coord))))

(defun ry/pyfunc-on-region (module func-name &rest args)
  (let* ((text (if (ry//use-rect-p)
                   (s-join "\n" (extract-rectangle (region-beginning) (region-end)))
                  (buffer-substring-no-properties (region-beginning) (region-end))))
         (new-args (append args (list text))))
    (apply 'ry/pyfunc module func-name new-args)))

(provide 'ry-pyfunc)
