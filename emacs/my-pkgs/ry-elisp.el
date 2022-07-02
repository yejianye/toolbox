(defun ry/elisp-add-to-watch (&optional region-start region-end)
  "Add the current variable to the *EDebug* window"
  (interactive "r")
  (let ((statement
         (if (and region-start region-end (use-region-p))
             (buffer-substring region-start region-end)
           (symbol-name (symbol-at-point)))))
    ;; open eval buffer
    (edebug-visit-eval-list)
    ;; jump to the end of it and add a newline
    (goto-char (point-max))
    (newline)
    ;; insert the variable
    (insert statement)
    ;; update the list
    (edebug-update-eval-list)
    ;; jump back to where we were
    (edebug-where)))

(defun ry/edebug-create-eval-buffer ()
  (unless edebug-eval-buffer
    (setq edebug-eval-buffer (get-buffer-create "*edebug*")))
  (pop-to-buffer edebug-eval-buffer)
  (edebug-eval-mode))

(defun ry/test-function ()
  "Run function set by `ry/test-set-function'"
  (interactive)
  (call-interactively ry-testing-function))

(defun ry/test-set-function ()
  "Set function at point as current testing function, which can be executed with `ry/test-function'"
  (interactive)
  (let ((command-name (symbol-at-point)))
    (message "Set %s as current testing function" command-name)
    (setq ry-testing-function command-name)))

(defmacro ry/log-time (name &rest body)
  "Print out time elapsed to execute FORM. NAME is used to help interpretation.
This macro returns same value as BODY."
  `(let* ((start-time (current-time))
          (result (progn ,@body))
          (time-spent (* 1000 (float-time (time-since start-time)))))
     (message "%s: time spent %dms" ,name time-spent)
     result))

(provide 'ry-elisp)
