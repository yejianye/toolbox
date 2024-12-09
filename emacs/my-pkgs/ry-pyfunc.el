(defvar ry/python-executable "python"
  "Python executable used in ry-pyfunc")

(defvar ry/pyfunc-object-type 'hash-table
  "JSON object type for return value in pyfunc")

(defun ry/prop-get (obj key)
  (if (hash-table-p obj)
      (gethash (substring (symbol-name key) 1) obj)
    (plist-get obj key)))

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
      (let* ((json-object-type ry/pyfunc-object-type)
             (result (ry/log-time "[pyfunc] Time spent in JSON Parsing"
                                  (json-read-from-string (buffer-string)))))
        (if (= (ry/prop-get result :rc) 0)
            (progn
              (when ry/log-time-enabled
                (message "[pyfunc] Time spent in actual python function: %dms"
                         (-> result
                             (ry/prop-get :debug)
                             (ry/prop-get :time_spent))))
              (ry/prop-get result :data))
          (progn
            (message (ry/prop-get result :data))
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
