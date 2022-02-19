(require 'dash)

(defvar helm-org-heading-history nil)
(defvar helm-org-search-links-history nil)
(defvar helm-org-entry-category-list '("default"))

;; Org Search Heading
(defun ry//helm-org-headings-candidates ()
  (-map 'ry//helm-org-headings-item
         (ry/pyfunc "rypy.org-search" "search_headings_dir" (expand-file-name org-directory))))

(defun ry//helm-org-headings-item(x)
  (cons (format "%s: %s" (aref x 0) (aref x 1)) (aref x 2))
  )

(defclass ry//helm-org-headings-search-source (helm-source-sync)
  ((candidates :initform 'ry//helm-org-headings-candidates)
   (history :initform 'helm-org-heading-history)
   (action :initform 'ry//helm-org-headings-action-goto-link)
   ))

(defun ry//helm-org-headings-action-goto-link (candidate)
  (org-open-link-from-string candidate)
  (setq helm-org-heading-history
        (cons helm-pattern
              (delete helm-pattern helm-org-heading-history)))
  )

(defun ry/helm-org-headings-in-org-directory ()
  "Select headings from all org files in =org-directory="
  (interactive)
  (helm :sources (helm-make-source "Org Jump to Heading" 'ry//helm-org-headings-search-source)
        :buffer "*helm org jump to heading*")
  )



;; Org Insert Heading

(defclass ry//helm-org-headings-insert-source (helm-source-sync)
  ((candidates :initform 'ry//helm-org-headings-candidates)
   (action :initform 'ry//helm-org-headings-action-insert-link)
   ))

(defun ry//helm-org-headings-action-insert-link (candidate)
  (insert candidate)
  )

(defun ry/helm-org-insert-headings (&optional arg)
  "Insert selected heading into current position"
  (interactive "P")
  (helm :sources (helm-make-source "Org Insert Heading" 'ry//helm-org-headings-insert-source)
        :buffer "*helm org insert heading*")
  )



;; Org Search Links

(defun ry//helm-org-search-links--candidates ()
  (--map (cons (format "%s: %s" (aref it 0) (aref it 1)) (aref it 2))
        (ry/pyfunc "rypy.org-search" "search_links_dir" (expand-file-name org-directory))))

(defun ry//helm-org-search-links--save-history ()
  (setq helm-org-search-links-history
        (cons helm-pattern
              (delete helm-pattern helm-org-search-links-history)))
  )

(defun ry/helm-org-search-links (&optional arg)
  "Search all links in ORG-DIRECTORY"
  (interactive "P")
  (helm :sources
        (helm-build-sync-source "Org Search Links"
          :candidates 'ry//helm-org-search-links--candidates
          :history 'helm-org-search-links-history
          :action (list
                   (cons "Open link" (lambda (x)
                                       (browse-url x)
                                       (ry//helm-org-search-links--save-history)
                                       ))
                   (cons "Copy link" (lambda (x)
                                       (ry//copy-to-osx-clipboard x)
                                       (ry//helm-org-search-links--save-history)
                                       )))
          )
        :buffer "*helm org search links*")
  )

(provide 'ry-search)



;; Org Indexed Entries
(defun ry//helm-org-entry-get-candidates (category)
  (let ((entries (ry/orgentry-select
                  (format "category='%s'" category)
                  "time_modified"))
        )
    (--map (cons (gethash "headlines" it)
                 (ry//helm-org-entry-build-link it))
           entries)))

(defun ry//helm-org-entry-make-source (category)
  (helm-build-sync-source category
    :candidates (ry//helm-org-entry-get-candidates category)
    :candidate-number-limit 10
    :action (helm-make-actions
             "Go to entry" 'ry//helm-org-entry-goto
             "Insert entry link" 'ry//helm-org-entry-insert-link)
    ))

(defun ry//helm-org-entry-build-link (entry)
  (format "[[id:%s][%s]]"
          (gethash "id" entry)
          (thread-last ;; get last component from headline
              (gethash "headlines" entry)
            (s-split "/")
            (-last-item)
            (s-trim)))
  )

(defun ry//helm-org-entry-goto (link)
  (org-open-link-from-string link)
  )

(defun ry//helm-org-entry-insert-link (link)
  (insert link)
  )

(defun ry/helm-org-entries ()
  "Select headings from all indexed entries"
  (interactive)
  (let* ((category-sources (-map 'ry//helm-org-entry-make-source
                                 helm-org-entry-category-list)))
    (helm :sources category-sources
          :buffer "*helm org entries*")
    )
  )
