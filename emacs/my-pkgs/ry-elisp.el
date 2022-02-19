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

(provide 'ry-elisp)
