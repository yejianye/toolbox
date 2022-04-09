(require 'dash)
(require 'request)

(defvar helm-org-heading-history nil)
(defvar helm-org-search-links-history nil)
(defvar helm-org-entry-category-list '("default"))

(defmacro ry/helm-run-action (action)
  `(lambda ()
     (interactive)
     (with-helm-alive-p
       (helm-exit-and-execute-action ,action))))

(defun ry/helm-make-keymap (&rest args)
  "Make helm keymap fro custom source"
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (cl-loop for (key fn) on args by #'cddr
             do (define-key map key fn))
    map))

;; Org Search Heading
(defun ry//helm-org-headings-candidates ()
  (-map 'ry//helm-org-headings-item
         (ry/pyfunc "rypy.org-search" "search_headings_dir" (expand-file-name org-directory))))

(defun ry//helm-org-headings-item(x)
  (cons (format "%s: %s" (aref x 0) (aref x 1)) (aref x 2)))

(defclass ry//helm-org-headings-search-source (helm-source-sync)
  ((candidates :initform 'ry//helm-org-headings-candidates)
   (history :initform 'helm-org-heading-history)
   (action :initform 'ry//helm-org-headings-action-goto-link)))

(defun ry//helm-org-headings-action-goto-link (candidate)
  (org-open-link-from-string candidate)
  (setq helm-org-heading-history
        (cons helm-pattern
              (delete helm-pattern helm-org-heading-history))))

(defun ry/helm-org-headings-in-org-directory ()
  "Select headings from all org files in =org-directory="
  (interactive)
  (helm :sources (helm-make-source "Org Jump to Heading" 'ry//helm-org-headings-search-source)
        :buffer "*helm org jump to heading*"))



;; Org Insert Heading

(defclass ry//helm-org-headings-insert-source (helm-source-sync)
  ((candidates :initform 'ry//helm-org-headings-candidates)
   (action :initform 'ry//helm-org-headings-action-insert-link)))

(defun ry//helm-org-headings-action-insert-link (candidate)
  (insert candidate))

(defun ry/helm-org-insert-headings (&optional arg)
  "Insert selected heading into current position"
  (interactive "P")
  (helm :sources (helm-make-source "Org Insert Heading" 'ry//helm-org-headings-insert-source)
        :buffer "*helm org insert heading*"))


;; Org Search Links
(defun ry/search-link-action-open (cdd)
  (browse-url cdd))

(defun ry/search-link-run-copy ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'ry/search-link-action-copy)))

(defun ry/search-link-action-copy (cdd)
  (ry//copy-to-osx-clipboard cdd))

(defun ry/search-link (term)
  (setq ry-search-link-response nil)
  (request "http://localhost:3000/search-link"
    :params (list (cons "term" term))
    :parser 'json-read
    :sync t
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (setq ry-search-link-response (request-response-data response)))))
  (ryc/al-get 'data ry-search-link-response))

(defun ry//helm-org-search-links--candidates ()
  (--map (cons (ryc/al-get 'desc it) (ryc/al-get 'link it))
         (ry/search-link helm-pattern)))

(defun ry//helm-org-search-links--no-sort (candidates _source)
  candidates)

(defun ry/helm-org-search-links (&optional arg)
  "Search all links in ORG-DIRECTORY"
  (interactive "P")
  (let ((helm-fuzzy-sort-fn 'ry//helm-org-search-links--no-sort))
    (helm :sources
          (helm-build-sync-source "Org Search Links"
            :candidates 'ry//helm-org-search-links--candidates
            :requires-pattern 3
            :match-dynamic t
            :keymap (ry/helm-make-keymap
                      (kbd "s-c") (ry/helm-run-action 'ry/search-link-action-copy))
            :action (helm-make-actions
                      "Open link" 'ry/search-link-action-open
                      "Copy link" 'ry/search-link-action-copy))
          ;; Todo need add insert link action
        :buffer "*helm org search links*")))



;; Org Indexed Entries
(defun ry//helm-org-entry-all-candidates ()
  (ryc/vector-to-list (ry/orgentry-select nil "time_modified")))

(defun ry//helm-org-entry-make-source (entries category)
  (let ((filtered-entries (->> entries
                            (-filter (fn (e) (string= (gethash "category" e) category)))
                            (-map 'ry//helm-org-entry-build-item))))
    (helm-build-sync-source category
      :candidates filtered-entries
      :candidate-number-limit 20
      :keymap (ry/helm-make-keymap
                (kbd "s-i") (ry/helm-run-action 'ry//helm-org-entry-insert-link))
      :action (helm-make-actions
               "Go to entry" 'ry//helm-org-entry-goto
               "Insert entry link" 'ry//helm-org-entry-insert-link))))

(defun ry//helm-org-entry-build-item (entry)
  (let* ((id (gethash "id" entry))
         (headlines (gethash "headlines" entry))
         (last-modified (thread-last (gethash "time_modified" entry)
                         (s-split " ")
                         (-first-item)))
         (display-item (concat
                        (propertize last-modified 'face font-lock-comment-face)
                        "  "
                        headlines)))
    (cons display-item (format "[[id:%s]]" id))))

(defun ry//helm-org-entry-build-link (entry)
  (let ((id (gethash "id" entry))
        (headline (thread-last (gethash "headlines" entry)
                    (s-split "/")
                    (-last-item)
                    (s-trim)))
        (last-modified (thread-last (gethash "time_modified" entry)
                         (s-split " ")
                         (-first-item))))
    (format "[[id:%s][%s | %s]]" id last-modified headline)))

(defun ry//helm-org-entry-goto (link)
  (org-open-link-from-string link))

(defun ry//helm-org-entry-insert-link (link)
  (insert link))

(defun ry/helm-org-entries ()
  "Select headings from all indexed entries"
  (interactive)
  (let* ((entries (ry//helm-org-entry-all-candidates))
         (categories (->> entries
                          (-map (fn (e) (gethash "category" e)))
                          (-distinct)
                          (-sort 'string<)))
         (sources (--map (ry//helm-org-entry-make-source entries it) categories)))
    (helm :sources sources
          :buffer "*helm org entries*")))

(provide 'ry-search)
