(require 'ry-orgapi)

(defun ry/calculate-total-time-in-region (&optional region-start region-end)
  "Sum time spent in selected region in Timesheet"
  (interactive "r")
  (let ((time-string (buffer-substring region-start region-end)))
    (insert (format "\nTotal time: %s\n" (ry/calculate-total-time time-string)))
    )
  )

(defun ry/calculate-total-time (time-string)
  (require 's)
  (require 'dash)
  (thread-last time-string
    (s-lines)
    (--map (s-split " - " it))
    (--map (-last-item it))
    (-map 'ry//string-to-mins)
    (-sum)
    (ry//hours-and-mins)
    )
  )

(defun ry//string-to-mins (string)
  (let ((mins (s-match "\\([0-9]+\\) mins" string))
        (hours (s-match "\\([0-9]+\\) hour" string)))
    (+
     (if mins
         (string-to-number (-last-item mins))
       0)
     (if hours
         (* 60 (string-to-number (-last-item hours)))
       0)
     )
    ))

(defun ry//hours-and-mins (total-mins)
  (let ((mins (mod total-mins 60))
        (hours (/ total-mins 60)))
    (concat (format "%s hours" hours)
            (if (> mins 0)
                (format " %s mins" mins)
              "")
            )
    )
  )

(defun ry/show-last-week-timesheet ()
  (interactive)
  (switch-to-buffer-other-window
   (ry//show-timesheet
    (ry/last-week-begin)
    (ry/last-week-end)
    (list (ry/journal-org-current-month) (ry/journal-org-last-month))
    )
  ))

(defun ry//show-timesheet (begin-date end-date org-files)
  (with-current-buffer (get-buffer-create "*Timesheet*")
    (org-mode)
    (erase-buffer)
    (insert
      (s-join "\n"
              (--map (ry//extract-timesheet begin-date end-date it) org-files)))
    )
  (get-buffer "*Timesheet*")
  )

(defun ry//string-between-p (item begin end)
  (and (or (string> item begin)
           (string= item begin))
       (or (string< item end)
           (string= item end))))

(defun ry//extract-timesheet (begin-date end-date org-file)
  (with-temp-buffer
    (insert-file-contents org-file)
    (let ((items (ry/orgapi-get-children
                  (ry/orgapi-get-root)
                  :title
                  (lambda (title)
                    (ry//string-between-p title begin-date end-date)))))
      (thread-last items
        (--map (ry/orgapi-get-children it :title "Timesheet"))
        (-flatten-n 1)
        (-map (lambda (item)
                (let* ((parent (org-element-property :parent item))
                       (date (org-element-property :title parent)))
                  (format "** %s\n%s"
                          date
                          (ry/orgapi-get-contents item)))))
        (s-join "\n")
        ))))

(provide 'ry-timesheet)
