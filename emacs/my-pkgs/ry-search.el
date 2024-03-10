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
(defun ry/log-url-click (cdd)
  (ry/http-post "http://localhost:3000/log"
                (list :event "click"
                      :url (plist-get cdd :link)
                      :rank (plist-get cdd :rank)
                      :query_id (plist-get cdd :query_id))))

(defun ry/search-link-action-open (cdd)
  (browse-url (plist-get cdd :link))
  (ry/log-url-click cdd))

(defun ry/search-link-action-insert (cdd)
  (insert (format "[[%s][%s]]"
                  (plist-get cdd :link)
                  (fix-lark-title (plist-get cdd :title))))
  (ry/log-url-click cdd))

(defun ry/search-link-action-copy (cdd)
  (ry//copy-to-osx-clipboard (plist-get cdd :link))
  (ry/log-url-click cdd))

(defun ry/search-link (term)
  (let* ((result (-> (ry/http-get "http://localhost:3000/search-link" (list :term term :limit 100))
                     (plist-get :data)))
         (query-id (plist-get result :id)))
    (--map (plist-put it :query_id query-id) (plist-get result :data))))

(defun ry//helm-org-search-links--candidates ()
  (--map (cons (plist-get it :desc) it)
         (ry/search-link helm-pattern)))

(defun ry//helm-org-no-sort (candidates _source)
  candidates)

(defun ry/helm-org-search-links (&optional arg)
  "Search all links in Notes, Bookmarks and Browser Histories"
  (interactive "P")
  (let ((helm-fuzzy-sort-fn 'ry//helm-org-no-sort))
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
(defun ry/log-heading-click (entry)
  (ry/http-post "http://localhost:3000/log"
                (list :event "heading_click"
                      :heading_id (plist-get entry :id)
                      :query_id (plist-get entry :query_id)
                      :rank (plist-get entry :rank))))


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

(defun ry//helm-org-entry-candidate-transformer (candidates source)
  (if (> (length candidates) 0)
      candidates
    (list (cons "Create New Note?" (list :create-note 1)))))

(defun ry//helm-org-entry-make-source (&optional default-insert-link)
  (let* ((default-action (helm-make-actions
                          "Open entry in indirect buffer" 'ry//helm-org-entry-indirect-buffer
                          "Go to entry" 'ry//helm-org-entry-goto
                          "Insert entry link" 'ry//helm-org-entry-insert-link))
         (category-maxlen (->> ry/pkm-category-choices
                               (--map (length it))
                               (apply 'max)))
         (entries (--map (ry//helm-org-entry-padding-category it category-maxlen) entries)))
    (helm-build-sync-source "Org Entries"
      :candidates (-map 'ry//helm-org-entry-build-item entries)
      :candidate-number-limit 100
      :filtered-candidate-transformer 'ry//helm-org-entry-candidate-transformer
      :keymap (ry/helm-make-keymap
                (kbd "s-<return>") (ry/helm-run-action 'ry//helm-org-entry-goto)
                (kbd "s-i") (ry/helm-run-action 'ry//helm-org-entry-insert-link))
      :action (if default-insert-link 'ry//helm-org-entry-insert-link default-action))))

(defun ry//helm-org-entry-build-item (entry)
  (let* ((id (plist-get entry :id))
         (headlines (plist-get entry :title))
         (category (plist-get entry :category))
         (last-modified (thread-last (plist-get entry :time_modified)
                         (s-split " ")
                         (-first-item)))
         (display-item (concat
                        (propertize (format "%s | %s" last-modified category)
                                    'face font-lock-comment-face)
                        "  "
                        headlines)))
    (cons display-item entry)))

(defun ry//helm-org-entry-build-link (entry)
  (let ((id (plist-get entry :id))
        (headline (thread-last (plist-get entry :title)
                    (s-split "/")
                    (-last-item)
                    (s-trim))))
    (format "[[id:%s][%s]]" id headline)))

(defun ry//helm-org-entry-goto (entry)
  (ry/log-heading-click entry)
  (if (ry//helm-org-entry-new? entry)
      (ry/pkm-note-create-interactive helm-pattern)
    (org-open-link-from-string (ry//helm-org-entry-build-link entry))))

(defun ry//helm-org-entry-indirect-buffer (entry)
  (ry//helm-org-entry-goto entry)
  (unless (or (ry//helm-org-entry-new? entry)
              (plist-get (ry/orgx-node-at-point) :ROOT))
    (ry/org-heading-to-indirect-buffer)))

(defun ry//helm-org-entry-insert-link (entry)
  (ry/log-heading-click entry)
  (if (ry//helm-org-entry-new? entry)
      (ry/pkm-note-create-interactive helm-pattern)
    (insert (ry//helm-org-entry-build-link entry))))

(defun ry//helm-org-entry-new? (entry)
  (plist-get entry :create-note))

(defun ry/helm-org-entries (&optional source default-insert-link)
  "Search all notes in ORG-DIRECTORY"
  (interactive "P")
  (let* ((helm-fuzzy-sort-fn 'ry//helm-org-no-sort))
    (helm :sources (ry//helm-org-entry-make-source default-insert-link)
          :buffer "*helm org entries*")))

(defun ry/helm-org-entries-insert-link ()
  "Insert link from all indexed entries"
  (interactive)
  (ry/helm-org-entries nil t))


;; Org Search Links in current buffer

(defun ry//helm-buf-search-link--action (candidate)
  (browse-url candidate))

(defun ry//helm-buf-search-link--candidates ()
  "Get a list of links and their descriptions from current org-mode buffer."
  (let ((links '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
        (let* ((element (org-element-context))
               (link (org-element-property :raw-link element))
               (desc (org-element-property :contents-begin element))
               (desc (when desc
                       (buffer-substring-no-properties desc (org-element-property :contents-end element)))))
          (push (cons desc link) links))))
    links))

(defun ry/helm-buf-search-link ()
  "Search links in current org buffer"
  (interactive)
  (let ((links (ry//helm-buf-search-link--candidates)))
    (helm :sources
          (helm-build-sync-source "Search Link in Buffer"
            :candidates links
            :action 'ry//helm-buf-search-link--action)
          :buffer "*helm search link in org buffer*")))



(provide 'ry-search)
