;;; ry-writing.el ---                                -*- lexical-binding: t; -*-

(defun ry/writing-current-paragraph ()
  "Return the text of the current paragraph."
  (let (beg end)
    (save-excursion
      (setq beg (progn (backward-paragraph) (point)))
      (setq end (progn (forward-paragraph) (point))))
    (buffer-substring-no-properties beg end)))

(defun ry/writing-translate ()
  """ Translate text according to previous text """
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (read-string "Enter text to translate: ")))
         (context (ry/writing-current-paragraph))
         (translated-text))
    (message (format "Translate %s" text))
    (setq translated-text (ry/pyfunc "rypy.writingkit" "translate" text context))
    (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
    (insert translated-text)
    (message translated-text)))

(defun ry/writing-proofread ()
  """ Proofread the selected text """
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (result (ry/pyfunc "rypy.writingkit" "proofread" selected-text))
         (output (gethash "output" result))
         (proofread (gethash "proofread" result))
         (rewritten (gethash "rewritten" result))
         (prompt (format "%s\n [p] Use proofread version  [r] Use rewritten version [q] Quit "
                         output))
         (choice (read-char prompt)))
    (cond
     ((= choice ?p) (progn (delete-region beg end)
                           (insert proofread)
                           (when (string-suffix-p "\n" selected-text)
                             (insert "\n"))))
     ((= choice ?r) (progn (delete-region beg end)
                           (insert rewritten)
                           (when (string-suffix-p "\n" selected-text)
                             (insert "\n")))))))

(provide 'ry-writing)
