(require 'ry-orgapi)
(require 's)
(require 'dash)

(defun ry/last-week-begin ()
  (let* ((days (+ (string-to-number (format-time-string "%w")) 6))
         (begin-date (seconds-to-time (- (float-time) (* days 86400)))))
    (format-time-string "%Y-%m-%d %A" begin-date)
    ))

(defun ry/last-week-end ()
  (let* ((days (string-to-number (format-time-string "%w")))
         (end-date (seconds-to-time (- (float-time) (* days 86400)))))
    (format-time-string "%Y-%m-%d %A" end-date)
    ))

(defun ry/timesheet--tasks (timesheet)
  (thread-last timesheet
    (s-lines)
    (--filter (s-starts-with? "- " it))))

(defun ry/timesheet--since-last (timesheet)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (last-update (thread-last timesheet
                        (s-lines)
                        (--first (s-starts-with? "Last update:" it))
                        (s-replace "Last update: " "")
                        (format "%s %s" today)
                        (date-to-time)))
         (time-delta (- (float-time) (float-time last-update)))
         (norm-time-delta (if (< time-delta 0)
                              (+ time-delta 86400)
                            time-delta))
         )
    (ry//hours-and-mins (floor (/ norm-time-delta 60)))))

(defun ry/timesheet--contain-time-cost? (item)
  (let ((parts (s-split " - " item)))
    (if (< (length parts) 2)
        nil
      (or
       (s-contains? " hour" (-last-item parts))
       (s-contains? " min" (-last-item parts))))))

(defun ry/timesheet--add-entry (timesheet item)
  (let* ((timed-item (if (ry/timesheet--contain-time-cost? item)
                         item
                       (s-concat item " - " (ry/timesheet--since-last timesheet))))
         (tasks (append (ry/timesheet--tasks timesheet)
                        (list (format "- %s\n" timed-item))))
         (rest (thread-last timesheet
                 (s-lines)
                 (--filter (not (s-starts-with? "- " it)))
                 (s-join "\n"))))
    (s-concat (s-join "\n" tasks) "\n" rest)
    )
  )

(defun ry/timesheet--update-checkpoint (timesheet)
  (let ((last-update (format-time-string "%H:%M")))
    (format "%s\n\nLast update: %s\n"
            (s-join "\n" (ry/timesheet--tasks timesheet))
            last-update)))

(defun ry/timesheet--add-total-time (timesheet)
  (let* ((tasks (ry/timesheet--tasks timesheet))
         (total-time (thread-last tasks
                      (--map (s-split " - " it))
                      (--map (-last-item it))
                      (-map 'ry//string-to-mins)
                      (-sum)
                      (ry//hours-and-mins)
                      ))
         )
    (s-concat (s-join "\n" tasks) (format "\n\nTotal time: %s\n" total-time))))

(defun ry/timesheet--today ()
    (let ((today (float-time))
          (yesterday (- (float-time) 86400)))
      (if (< (string-to-number (format-time-string "%k")) 4)
          (format-time-string "%Y-%m-%d %A" yesterday)
        (format-time-string "%Y-%m-%d %A" today))))

(defun ry/timesheet--find (&optional date)
  (ry/org-goto-journal)
  (let* ((root (ry/orgapi-get-root))
         (heading (or date (ry/timesheet--today)))
         )
    (thread-first root
      (ry/orgapi-first-child :title heading)
      (ry/orgapi-first-child :title "Timesheet"))
  ))

(defun ry/timesheet-calculate-total-time (&optional date)
  (interactive)
  (if (use-region-p)
      ;; Calculate total time for active region
      (let* ((start (region-beginning))
             (end (region-end))
             (timesheet (buffer-substring start end)))
        (delete-region start end)
        (goto-char start)
        (insert (ry/timesheet--add-total-time timesheet)))

    ;; Calculate total time for today or specific date
    (let* ((el (ry/timesheet--find date))
           (timesheet (ry/orgapi-get-contents el))
           )
      (ry/orgapi-set-contents
       el
       (ry/timesheet--add-total-time timesheet)))))

(defun ry/timesheet-add-entry (item &optional date)
  (interactive "sEntry: ")
  (let* ((el (ry/timesheet--find date))
         (timesheet (ry/orgapi-get-contents el)))
       (ry/orgapi-set-contents
        el
        (thread-first timesheet
          (ry/timesheet--add-entry (s-capitalize item))
          (ry/timesheet--update-checkpoint)))))

(defun ry/timesheet-update-checkpoint (&optional date)
  (interactive)
  (let* ((el (ry/timesheet--find date))
         (timesheet (ry/orgapi-get-contents el))
         )
    (ry/orgapi-set-contents
     el
     (ry/timesheet--update-checkpoint timesheet))))

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
    (concat (cond ((= hours 0) "")
                  ((= hours 1) "1 hour ")
                  (t (format "%s hours " hours)))
            (cond ((= mins 0) "")
                  ((= mins 1) "1 min")
                  (t (format "%s mins" mins))))))

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
