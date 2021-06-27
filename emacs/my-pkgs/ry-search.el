(require 'dash)

;; Org Search Heading
(defun ry//helm-org-headings-candidates ()
  (-map 'ry//helm-org-headings-item
         (ry/pyfunc "rypy.org-search" "search_headings_dir" (expand-file-name org-directory))))

(defun ry//helm-org-headings-item(x)
  (cons (format "%s: %s" (aref x 0) (aref x 1)) (aref x 2))
  )

(defclass ry//helm-org-headings-search-source (helm-source-sync)
  ((candidates :initform 'ry//helm-org-headings-candidates)
   (action :initform 'ry//helm-org-headings-action-goto-link)
   ))

(defun ry//helm-org-headings-action-goto-link (candidate)
  (org-open-link-from-string candidate)
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


(defun ry/helm-org-search-links (&optional arg)
  "Search all links in ORG-DIRECTORY"
  (interactive "P")
  (helm :sources
        (helm-build-sync-source "Org Search Links"
          :candidates 'ry//helm-org-search-links--candidates
          :action (list
                   (cons "Open link" (lambda (x) (browse-url x)))
                   (cons "Copy link" (lambda (x) (ry//copy-to-osx-clipboard x))))
          )
        :buffer "*helm org search links*")
  )

(provide 'ry-search)