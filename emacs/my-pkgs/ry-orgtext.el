(defun ry/org-toggle-bold-line ()
  "Toggle bold font for current line"
  (interactive)
  (ry//org-toggle-style "*" (ryc/line-begin-pos) (ryc/line-end-pos)))

(defun ry/org-toggle-underline-line ()
  "Toggle underline font for current line"
  (interactive)
  (ry//org-toggle-style "_" (ryc/line-begin-pos) (ryc/line-end-pos)))

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

(defun ry/org-toggle-underline (&optional region-start region-end)
  "Toggle underline font in Orgmode"
  (interactive "r")
  (ry//org-toggle-style "_" region-start region-end))

(defun ry/org-toggle-bold-current-word ()
  "Bold current word"
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (ry/org-toggle-bold (car bounds) (cdr bounds))))

(defun ry//beginning-of-line-p (pos)
  "Check if the position POS is at the beginning of a line."
  (save-excursion
    (goto-char pos)
    (bolp)))

(defun ry//end-of-line-p (pos)
  "Check if the position POS is at the beginning of a line."
  (save-excursion
    (goto-char pos)
    (eolp)))

(defun ry//org-toggle-style(style-char region-start region-end)
  "Add or remove CHAR from selected text region"
  (let* ((prev-char (char-to-string (or (char-before region-start) ?\s)))
         (next-char (char-to-string (or (char-after region-end) ?\s)))
         (begin (if (string= prev-char style-char) (1- region-start) region-start))
         (end (if (string= next-char style-char) (1+ region-end) region-end))
         (expanded-string (buffer-substring begin end))
         (style-char-prefix (if (or (ry//beginning-of-line-p region-start) (string= prev-char "") (string= prev-char " "))
                                style-char
                              (s-concat " " style-char)))
         (style-char-suffix (if (or (ry//end-of-line-p region-end) (string= next-char "") (string= next-char " "))
                                style-char
                              (s-concat style-char " ")))
         (clear-style (and (s-starts-with? style-char expanded-string)
                           (s-ends-with? style-char expanded-string)))
         (modified-string (if clear-style
                              (substring expanded-string 1 (1- (length expanded-string)))
                            (s-concat style-char-prefix expanded-string style-char-suffix))))
    (replace-string expanded-string modified-string nil begin end)))

(provide 'ry-orgtext)
