(require 'dash)
(require 'request)

(defvar helm-org-heading-history nil)
(defvar helm-org-search-links-history nil)

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
  (browse-url (plist-get cdd :link)))

(defun ry/search-link-action-insert (cdd)
  (insert (format "[[%s][%s]]" (plist-get cdd :link) (plist-get cdd :title))))

(defun ry/search-link-action-copy (cdd)
  (ry//copy-to-osx-clipboard (plist-get cdd :link)))

(defun ry/search-link (term)
  (ry/log-time "[search-link] Time spent in HTTP request"
    (->
      (ry/http-get "http://localhost:3000/search-link" (list :term term :limit 100))
      (ryc/plist-path '(:data :data)))))

(defun ry//helm-org-search-links--candidates ()
  (--map (cons (plist-get it :desc) it)
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
                      (kbd "s-c") (ry/helm-run-action 'ry/search-link-action-copy)
                      (kbd "s-i") (ry/helm-run-action 'ry/search-link-action-insert))
            :action (helm-make-actions
                      "Open link" 'ry/search-link-action-open
                      "Copy link" 'ry/search-link-action-copy
                      "Insert link" 'ry/search-link-action-insert))
          ;; Todo need add insert link action
        :buffer "*helm org search links*")))



;; Org Indexed Entries
(defun ry//helm-org-entry-candidates (source)
  (let ((all-entries (-> (ry/http-get "http://localhost:3000/org-entry-all")
                         (ryc/plist-path '(:data :data))
                         (ryc/vector-to-list))))
    (if source
        (--filter (string= (plist-get it :category) source) all-entries)
      all-entries)))

(defun ry//helm-org-entry-padding-category (entry maxlen)
  (let* ((category (plist-get entry :category))
         (padded-category (s-pad-right maxlen " " category)))
    (plist-put entry :category padded-category)))

(defun ry//helm-org-entry-make-source (entries &optional default-insert-link)
  (let* ((default-action (helm-make-actions
                          "Open entry in indirect buffer" 'ry//helm-org-entry-indirect-buffer
                          "Go to entry" 'ry//helm-org-entry-goto
                          "Insert entry link" 'ry//helm-org-entry-insert-link))
         (category-maxlen (->> entries
                               (--map (length (plist-get it :category)))
                               (apply 'max)))
         (entries (--map (ry//helm-org-entry-padding-category it category-maxlen) entries)))
    (helm-build-sync-source "Org Entries"
      :candidates (-map 'ry//helm-org-entry-build-item entries)
      :candidate-number-limit 100
      :keymap (ry/helm-make-keymap
                (kbd "s-<return>") (ry/helm-run-action 'ry//helm-org-entry-goto)
                (kbd "s-i") (ry/helm-run-action 'ry//helm-org-entry-insert-link))
      :action (if default-insert-link 'ry//helm-org-entry-insert-link default-action))))

(defun ry//helm-org-entry-build-item (entry)
  (let* ((id (plist-get entry :id))
         (headlines (plist-get entry :headlines))
         (category (plist-get entry :category))
         (last-modified (thread-last (plist-get entry :time_modified)
                         (s-split " ")
                         (-first-item)))
         (display-item (concat
                        (propertize (format "%s | %s" last-modified category)
                                    'face font-lock-comment-face)
                        "  "
                        headlines))
         (link (ry//helm-org-entry-build-link entry)))
    (cons display-item link)))

(defun ry//helm-org-entry-build-link (entry)
  (let ((id (plist-get entry :id))
        (headline (thread-last (plist-get entry :headlines)
                    (s-split "/")
                    (-last-item)
                    (s-trim))))
    (format "[[id:%s][%s]]" id headline)))

(defun ry//helm-org-entry-goto (link)
  (org-open-link-from-string link))

(defun ry//helm-org-entry-indirect-buffer (link)
  (org-open-link-from-string link)
  (ry/org-heading-to-indirect-buffer))

(defun ry//helm-org-entry-insert-link (link)
  (insert link))

(defun ry/helm-org-entries (&optional source default-insert-link)
  "Select headings from all indexed entries"
  (interactive)
  (let* ((entries (ry//helm-org-entry-candidates source)))
    (helm :sources (ry//helm-org-entry-make-source entries default-insert-link)
          :buffer "*helm org entries*")))

(defun ry/helm-org-entries-insert-link ()
  "Insert link from all indexed entries"
  (interactive)
  (ry/helm-org-entries nil t))

(provide 'ry-search)
