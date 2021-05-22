(defun ry/org-toggle-bold (&optional region-start region-end)
  "Toggle bold font in Orgmode"
  (interactive "r")
  (ry//org-toggle-style "*" region-start region-end))

(defun ry/org-toggle-italic (&optional region-start region-end)
  "Toggle italic font in Orgmode"
  (interactive "r")
  (ry//org-toggle-style "/" region-start region-end))

(defun ry/org-toggle-strikethrough (&optional region-start region-end)
  "Toggle strike through font in Orgmode"
  (interactive "r")
  (ry//org-toggle-style "+" region-start region-end))

(defun ry//org-toggle-style(style-char region-start region-end)
  "Add or remove CHAR from selected text region"
  (let* ((prev-char (char-to-string (char-before region-start)))
         (next-char (char-to-string (char-after region-end)))
         (begin (if (string= prev-char style-char) (1- region-start) region-start))
         (end (if (string= next-char style-char) (1+ region-end) region-end))
         (expanded-string (buffer-substring begin end))
         (clear-style (and (s-starts-with? style-char expanded-string)
                           (s-ends-with? style-char expanded-string)))
         (modified-string (if clear-style
                              (substring expanded-string 1 (1- (length expanded-string) ))
                            (s-concat style-char expanded-string style-char))))
    ;; (message "region-start:%d region-end:%d begin:%d end:%d"
    ;;          region-start region-end begin end)
    ;; (message "prev-char:%s next-char:%s expanded-string:%s clear-style:%s"
    ;;          prev-char next-char expanded-string clear-style)
    (replace-string expanded-string modified-string nil begin end))
  )

(provide 'ry-orgtext)
