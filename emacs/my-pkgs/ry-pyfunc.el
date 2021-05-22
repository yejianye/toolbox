(defvar ry/python-executable "python"
  "Python executable used in ry-pyfunc")

(defun ry/pyfunc (module func-name &rest args)
    (with-temp-buffer
      (if args
        (insert (json-encode args))
        (insert "[]"))
      (call-process-region (point-min) (point-max)
                           ry/python-executable
                           t t nil
                           "-mrypy.emacs" module func-name)
      (json-read-from-string (buffer-string))
      )
    )

(defun ry//use-rect-p ()
  (let ((coord (-first-item (extract-rectangle-bounds (region-beginning) (region-end)))))
    (< (car coord) (cdr coord)))
  )

(defun ry/pyfunc-on-region (module func-name &rest args)
  (let* ((text (if (ry//use-rect-p)
                   (s-join "\n" (extract-rectangle (region-beginning) (region-end)))
                  (buffer-substring-no-properties (region-beginning) (region-end))
                 ))
         (new-args (append args (list text))))
    (apply 'ry/pyfunc module func-name new-args)
    )
  )

(provide 'ry-pyfunc)
