(defun ry/cider-eval-buffer ()
  "Same as cider-eval-buffer but save buffer first"
  (interactive)
  (save-buffer)
  (cider-eval-buffer))

(defun ry/clj-send-current-sexp-to-repl ()
  (interactive)
  (spacemacs//cider-eval-in-repl-no-focus (format "(do (in-ns '%s) %s)"
                                                  (cider-current-ns)
                                                  (cider-list-at-point))))

(defun ry/clj-pprint-current-sexp-to-comment ()
  (interactive)
  (let ((cider-comment-prefix "\n;; => "))
    (cider-pprint-form-to-comment 'cider-list-at-point nil)))

(defun ry/clj-hot-reload-dep-interactive ()
  (interactive)
  (ry/clj-hot-reload-dep (s-trim (thing-at-point 'line))))

(defun ry/clj-hot-reload-dep (dep)
  "Hot-load specific dependencies without restarting REPL"
  (cider-insert-in-repl
    (format
      "(use '[cemerick.pomegranate :only (add-dependencies)])
        (add-dependencies :coordinates '[%s]
                          :repositories (merge cemerick.pomegranate.aether/maven-central
                                              {\"clojars\" \"https://clojars.org/repo\"}))"
      dep)
    t))
(provide 'ry-clj)
