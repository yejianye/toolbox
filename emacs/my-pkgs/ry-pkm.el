;;; ry-pkm.el --- Ryan's Personal Knowledge Management System  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ryan Ye

;; Author: Ryan Ye <ryan@MWT9G39HLW>
;; Keywords: docs

(setq org-id-method 'ts
      org-id-ts-format "%Y%m%d-%H%M%S-%6N")

(defconst ry/pkm-category-choice-file "~/org/note-category.el")

(defun ry/pkm-init ()
  (if (file-exists-p ry/pkm-category-choice-file)
      (setq ry/pkm-category-choices (ryc/var-load ry/pkm-category-choice-file))
     (setq ry/pkm-category-choices
       '("default"
         "project"
         "thinking"
         "meeting"
         "diary"
         "one-on-one"
         "test"))))

(defvar ry/pkm-last-category-choice
  "default"
  "Store last choice when user selects note category")


(defun ry/pkm-note-create (title category &optional attr)
  (let* ((note-id (org-id-new))
         (fname (ry//pkm-note-filename category title))
         (dir (file-name-directory fname))
         (template (ry//pkm-note-get-template category))
         (now (format-time-string "%Y-%m-%d %H:%M:%S"))
         (content (thread-last template
                               (s-replace "{title}" title)
                               (s-replace "{note-id}" note-id)
                               (s-replace "{now}" now)
                               (s-replace "{category}" category)
                               (s-replace "{attr}" (ry//pkm-note-attr-string attr)))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (ryc/spit fname content)
    (org-id-add-location note-id fname)
    (ry/orgentry-db-sync fname)
    (list :id note-id
          :path fname)))

(defun ry//pkm-note-filename (category title &optional time-created)
  (let* ((ts (if time-created
                 (ryc/string-to-timestamp time-created "%Y-%m-%d %H:%M:%S")
               nil))
         (year (format-time-string "%Y" ts))
         (today (format-time-string "%Y%m%d" ts))
         (title (thread-last title
                             (s-replace "-" "")
                             (s-replace-regexp "[ ~!@#$&();:',.<>/?]+" "-")
                             (downcase)))
         (fname (format "%s/%s/%s/%s-%s.org"
                        org-directory category year today title))
         (suffix-count 1))
    (while (file-exists-p fname)
      (setq suffix-count (1+ suffix-count))
      (setq fname (format "%s/%s/%s/%s-%s-%d.org"
                          org-directory category year today title suffix-count)))
    fname))

(defun ry//pkm-note-attr-string (attr)
  (if attr
    (mapconcat 'identity
      (--map (format "\n:%s: %s" (upcase (car it)) (cdr it)) attr) "")
    ""))

(defun ry//pkm-note-get-template (category)
  (let ((default-template (format "%s/note-template/default.org" org-directory))
        (category-template (format "%s/note-template/%s.org" org-directory category)))
    (if (file-readable-p category-template)
        (ryc/slurp category-template)
      (ryc/slurp default-template))))

(defun ry//pkm-note-select-category ()
  "Select note category. If CATEGORY property exists in current heading or parent heading, use CATEGORY as default. Otherwise, use previous choice as default"
  (let* ((default-category (or (org-entry-get-with-inheritance "CATEGORY")
                               ry/pkm-last-category-choice))
         (choice (completing-read "Enter note category: " ry/pkm-category-choices nil nil default-category)))
    (setq ry/pkm-last-category-choice choice)
    (unless (-contains? ry/pkm-category-choices choice)
      (add-to-list 'ry/pkm-category-choices choice)
      (ryc/var-store ry/pkm-category-choices ry/pkm-category-choice-file))
    choice))

(defun ry/pkm-note-create-interactive (&optional title-initial)
  " Create new note interactively, return node id
- If no text is selected, prompt user to enter title and category. Create new note file and insert link to that note in current position.
- Otherwise, use selected text as title, prompt user to enter category. Create new note file and replace selected text with a link to new note
- Category will be inherited by default if category attribute is available in parent node PROPERTIES"
  (interactive)
  (let* ((title (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Enter note title: " title-initial)))
         (category (ry//pkm-note-select-category))
         (note (ry/pkm-note-create title category))
         (note-id (plist-get note :id))
         (link (format "[[id:%s][%s]]" note-id title))
         (insert-link-p (and (s-equals-p "org-mode" major-mode)
                             (or (use-region-p)
                                 (yes-or-no-p "Insert link into current file?")))))

    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (if insert-link-p
      (insert link)
      (org-open-link-from-string link))
    note-id))

(defun ry/pkm-note-from-subtree ()
  "Convert current heading to a separate note, and then replace the current heading (and its content) with a link to that note"
  (interactive)
  (let* ((title (substring-no-properties (org-get-heading)))
         (note-id (org-id-get-create))
         (time-created (or (org-entry-get nil "CREATED")
                           (format-time-string "%Y-%m-%d %H:%M:%S")))
         (category (ry//pkm-note-select-category))
         (fname (ry//pkm-note-filename category title time-created))
         (dir (file-name-directory fname))
         (link (format "[[id:%s][%s]]" note-id title)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (org-entry-put nil "CATEGORY" category)
    (org-entry-put nil "ROOT" "yes")
    (org-copy-subtree nil t)
    (insert (format "%s\n" link))
    (with-current-buffer (find-file-noselect fname)
      (unless (s-equals-p major-mode "org-mode")
        (org-mode))
      (org-paste-subtree)
      (save-buffer))))

(defun ry/pkm-note-goto-root-heading ()
  (interactive)
  (goto-char (point-min))
  (unless (org-get-heading)
    (org-next-visible-heading 1)))

(defun ry/pkm-note-change-category ()
  (interactive)
  (ry/pkm-note-goto-root-heading)
  (let* ((new-category (ry//pkm-note-select-category)))
    (org-entry-put nil "CATEGORY" new-category)
    (save-buffer) ;; save file before moving the file
    (ry/pkm-note-update-file-location)))

(defun ry/pkm-note-update-file-location ()
  "Update note file location based on its name and category"
  (interactive)
  (save-buffer) ; save buffer before moving to new location
  (ry/pkm-note-goto-root-heading)
  (let* ((title (substring-no-properties (org-get-heading)))
         (category (org-entry-get nil "CATEGORY"))
         (time-created (org-entry-get nil "CREATED"))
         (target-path (ry//pkm-note-filename category title time-created))
         (target-dir (file-name-directory target-path)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir))
    (rename-file (buffer-file-name) target-path)
    (set-visited-file-name target-path)
    (set-buffer-modified-p 1)
    (save-buffer)
    (ry/org-id-update-id-locations)))

(defun ry/pkm-note-set-root ()
  (interactive)
  (org-id-get-create)
  (org-entry-put nil "CATEGORY" (ry//pkm-note-select-category))
  (org-entry-put nil "ROOT" "YES"))

(defun ry/pkm-note-index ()
  (interactive)
  (org-id-get-create)
  (org-entry-put nil "CATEGORY" (ry//pkm-note-select-category)))

(defun ry/pkm-note-file-delete ()
  (interactive)
  (let ((fname (buffer-file-name)))
    (when (yes-or-no-p "Are you sure deleting the current note file? ")
      (kill-buffer)
      (delete-file fname)
      (ry/orgentry-db-sync fname))))

(defun ry/pkm-note-open (note-id)
  "Goto a specific note"
  (interactive)
  (let* ((link (format "[[id:%s]]" note-id)))
    (org-open-link-from-string link)))

(defun ry/pkm-note-diary-create ()
  "Create today's diary note"
  (interactive)
  (let* ((title (format "%s - Diary" (format-time-string "%Y-%m-%d")))
         (note (ry/pkm-note-create title "diary"))
         (note-id (plist-get note :id)))
    (ry/pkm-note-open note-id)
    (writeroom-mode)))

(defun ry//pkm-test-cases ()
  (ry/pkm-note-create "Note Create Test 3" "project" '(("status" . "WIP")))
  (ry//pkm-note-filename "default" "Test Note" "2023-01-20 10:12:33"))

(ry/pkm-init)
(provide 'ry-pkm)
