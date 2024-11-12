;; -*- lexical-binding: t -*-

(require 'org-ql)

;; Select and Realize

(defun ry//orgx-real-node-at-point ()
  (let* ((ele (-> (org-element-at-point)
                  (plist-get 'headline)))
         (title (plist-get ele :raw-value)))
    (plist-put ele :title title)))

(defun ry//orgx-proxy-node-at-point ()
  (list :filepath (buffer-file-name)
        :outline (org-get-outline-path t)
        :ID (org-id-get)
        :level (org-current-level)))

(defun ry//orgx-node-location (proxy-node)
  (let* ((node-id (plist-get proxy-node :ID))
         (outline (plist-get proxy-node :outline))
         (filepath (plist-get proxy-node :filepath)))
    (if node-id
        (org-id-find node-id)
      (let ((pos (-> (org-ql-select filepath (-concat '(outline-path) outline))
                     (-first-item)
                     (ryc/plist-path (list 'headline :begin)))))
        (cons filepath pos)))))

(defun ry//orgx-node-realize (proxy-node)
  (if proxy-node
      (-let* (((fpath . pos) (ry//orgx-node-location proxy-node)))
        (with-file-buffer fpath
           (save-excursion
             (goto-char pos)
             (ryc/plist-merge proxy-node (ry//orgx-real-node-at-point)))))
    ;; Use current node if porxy-node is nil
    (ryc/plist-merge (ry//orgx-proxy-node-at-point) (ry//orgx-real-node-at-point))))

(defun ry/orgx-node-at-point (&optional lazy)
  (let ((proxy-node (ry//orgx-proxy-node-at-point)))
    (if lazy
        proxy-node
      (ryc/plist-merge proxy-node (ry//orgx-real-node-at-point)))))

(defun ry/orgx-select (&optional query file lazy)
  "Select nodes matching QUERY (org-ql query) in FILE.
If FILE is nil, current buffer is used.
If LAZY is non-nil, only Proxy Nodes are returned. Otherwise, nodes will be realized."
  (org-ql-select file query :action `(ry/orgx-node-at-point ,lazy)))

(defun ry/orgx-select-one (&optional query file lazy)
  (-first-item (ry/orgx-select query file lazy)))

(defun ry/orgx-select-by-id (node-id &optional lazy)
  (let ((file (org-id-find-id-file node-id)))
    (ry/orgx-select-one (list 'property "ID" node-id) file lazy)))

(defmacro ry/orgx-with-narrow-to-node (proxy-node &rest body)
  "Goto PROXY-NODE, narrow to the subtree and execute BODY"
  `(-let* (((fpath . pos) (ry//orgx-node-location ,proxy-node)))
     (with-file-buffer fpath
        (save-excursion
          (save-restriction
            (widen)
            (goto-char pos)
            (org-narrow-to-subtree)
            ,@body)))))

(defmacro ry/orgx-with-goto-node (proxy-node &rest body)
  "Goto PROXY-NODE and execute BODY"
  `(-let* (((fpath . pos) (ry//orgx-node-location ,proxy-node)))
     (with-file-buffer fpath
        (save-excursion
          (save-restriction
            (widen)
            (goto-char pos)
            ,@body)))))

(defun ry/orgx-select-children (&optional proxy-node query lazy)
  (let* ((level (plist-get proxy-node :level))
         (level-restriciton `(level > ,level))
         (query (if query
                    (list 'and level-restriciton query)
                  level-restriciton)))
    (ry/orgx-with-narrow-to-node proxy-node
      (org-ql-select nil query
        :action `(ry/orgx-node-at-point ,lazy)
        :narrow t))))

(defun ry/orgx-select-first-child (&optional proxy-node query lazy)
  (-first-item (ry/orgx-select-children proxy-node query lazy)))

(defun ry/orgx-select-last-child (&optional proxy-node query lazy)
  (-last-item (ry/orgx-select-children proxy-node query lazy)))

;; Helper functions for node updates

(defun ry//orgx-node-pos (&optional proxy-node)
  (let* ((node (ry//orgx-node-realize proxy-node))
         (child (ry/orgx-select-first-child proxy-node)))
    (list
     :contents-begin (or (plist-get node :contents-begin) (plist-get node :end))
     :contents-end (or (plist-get child :begin) (plist-get node :end))
     :children-begin (or (plist-get child :begin) (plist-get node :end))
     :children-end (plist-get node :end)
     :has-children (if child t nil)
     :filepath (plist-get node :filepath))))

;; Read and Update

(defun ry/orgx-content-get (&optional proxy-node)
  "Get content of the node. Child nodes will be excluded from the content.
TODO: think about how to deal with :PROPERTIES:"
  (-let* (((&plist :contents-begin begin
                   :contents-end end
                   :filepath filepath) (ry//orgx-node-pos proxy-node)))
    (if (>= begin end)
        ""
       (with-file-buffer filepath
         (buffer-substring-no-properties begin end)))))

(defun ry/orgx-content-prepend (proxy-node content)
  (ry/orgx-with-narrow-to-node proxy-node
     (goto-char (plist-get (ry//orgx-node-pos proxy-node) :contents-begin))
     (insert content)))

(defun ry/orgx-content-append (proxy-node content)
  (ry/orgx-with-narrow-to-node proxy-node
    (goto-char (plist-get (ry//orgx-node-pos proxy-node) :contents-end))
    (insert content)))

;; Create Node


(defun ry//orgx-build-node-string (title level &optional content)
  (let ((stars (s-repeat level "*"))
        (content (if content
                     (if (s-ends-with? "\n" content) content (s-concat content "\n"))
                   "")))
    (format "%s %s\n%s" stars title content)))

(cl-defun ry/orgx-child-insert (parent child-title &key content tset prepend)
  (let* ((existing (and tset
                        (ry/orgx-select-first-child parent `(heading ,child-title))))
         (level (+ (plist-get parent :level) 1))
         (child-string (ry//orgx-build-node-string child-title level content)))
    (or existing
        (ry/orgx-with-goto-node parent
           (let* ((node-pos (ry//orgx-node-pos parent))
                  (insert-pos (if prepend (plist-get node-pos :children-begin)
                                (plist-get node-pos :children-end))))
              (goto-char insert-pos)
              (insert child-string)
              (goto-char insert-pos)
              (ry/orgx-node-at-point))))))

(cl-defun ry/orgx-child-append (parent child-title &key content tset)
  (ry/orgx-child-insert parent child-title
                        :content content
                        :tset tset))

(cl-defun ry/orgx-child-prepend (parent child-title &key content tset)
  (ry/orgx-child-insert parent child-title
                        :prepend t
                        :content content
                        :tset tset))

(cl-defun ry/orgx-sibling-insert (proxy-node title &key content prepend)
    (ry/orgx-with-goto-node proxy-node
      (let* ((node (ry//orgx-node-realize proxy-node))
             (pos (if prepend (plist-get node :begin) (plist-get node :end)))
             (sibling-string (ry//orgx-build-node-string title (plist-get proxy-node :level) content)))
        (goto-char pos)
        (insert sibling-string)
        (goto-char pos)
        (ry/orgx-node-at-point))))

(cl-defun ry/orgx-sibling-prepend (proxy-node title &key content)
  (ry/orgx-sibling-insert proxy-node title :content content :prepend t))

(cl-defun ry/orgx-sibling-append (proxy-node title &key content)
  (ry/orgx-sibling-insert proxy-node title :content content))

;; Playground

(defun ry/orgx-playground ()
  (ry/orgx-select-one '(heading "Node Design") "~/org/topics/emacs.org"))

(provide 'ry-orgx)