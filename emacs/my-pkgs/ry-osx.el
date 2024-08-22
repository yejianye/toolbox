;; This package includes all function that interacts with Mac OS systems such as system clipboard or other Mac programs


;; There's a difference between emacs-plus and emacs-mac in `do-applescript`
;; emacs-mac will wrap the result with a pair of double-quote. For
;; `ry/osx-chrome-title` and `ry/osx-chrome-url`, the following implementation
;; needs be used instead.
;;
;; emacs-mac version
(setq default-browser 'chrome)

(setq browser-url-applescript
  '(chrome "tell application \"Google Chrome\" to return URL of active tab of front window"
    safari "tell application \"Safari\" to return URL of current tab of front window"
    edge "tell application \"Microsoft Edge Dev\" to return URL of active tab of front window"))

(setq browser-title-applescript
      '(chrome "tell application \"Google Chrome\" to return name of active tab of front window"
        edge "tell application \"Microsoft Edge Dev\" to return name of active tab of front window"
        safari "tell application \"Safari\" to return name of current tab of front window"))

(setq browser-process-names
      '(chrome "Google_Chrome"
        edge "Microsoft_Edge_Dev"
        safari "Safari"))

(setq ry/osx-browser-title-map
      '(("feishu.cn" fix-lark-title)
        ("larksuite.com" fix-lark-title)
        ("larkoffice.com" fix-lark-title)
        ("youtube.com" fix-youtube-title)))

(defun ry/last-used-browser ()
  (let ((recent-apps (shell-command-to-string "lsappinfo visibleProcessList")))
     (->> (ryc/plist-to-alist browser-process-names)
          (--map (cons (car it) (s-index-of (cdr it) recent-apps)))
          (--filter (cdr it))
          (--sort (< (cdr it) (cdr other)))
          (-first-item)
          (car))))

(defun ry/osx-browser-url (&optional browser)
  (substring
   (do-applescript (plist-get browser-url-applescript (or browser (ry/last-used-browser))))
   1 -1))

(defun ry/osx-browser-title (&optional browser)
  (substring
   (do-applescript (plist-get browser-title-applescript (or browser (ry/last-used-browser))))
   1 -1))
;;
;; emacs-plus version
;; (defun ry/osx-chrome-url ()
;;   (do-applescript "tell application \"Google Chrome\" to return URL of active tab of front window")
;;   )
;;
;; (defun ry/osx-chrome-title ()
;;   (do-applescript "tell application \"Google Chrome\" to return title of active tab of front window")
;;   )


(defun ry/osx-paste()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (when (eq evil-state 'visual)
    (kill-region (region-beginning) (region-end))
    (evil-normal-state))
  (insert (s-replace "\r" ""  (ry//clipboard-content))))

(defun ry/org-cliplink-osx ()
  "Takes a URL from the OSX clipboard and inserts an org-mode link
with the title of a page found by the URL into the current
buffer"
  (interactive)
  (org-cliplink-insert-transformed-title (shell-command-to-string "LANG=en_US.UTF-8 pbpaste")
                                         'org-cliplink-org-mode-link-transformer))

(defun ry/org-copy-as-rtf ()
  "Copy current region as rich text to clipboard.
This function makes use of command-line utility: pbcopy, textutil
and only works on MacOS."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (content (org-export-string-as (buffer-substring beg end) 'html t))
         (fname (make-temp-file "org-clipboard"))
         (style (ryc/read-file-content "~/.org-clipboard.css"))
         (content-with-style (format "<style>%s</style>%s" style content)))
    (write-region content-with-style nil fname)
    (shell-command (format "LANG=en_US.UTF-8 cat %s | textutil -stdin -format html -inputencoding UTF-8 -convert rtf -stdout | pbcopy" fname))
    (delete-file fname)))

(defun ry/org-copy-as-markdown ()
  "Copy current region as markdown to clipboard.
It only works in Mac OS "
  (interactive)
  (require 'ox-md)
  (let* ((beg (region-beginning))
         (end (region-end))
         (region-string (format "#+OPTIONS: toc:nil\n%s" (buffer-substring beg end)))
         (content (org-export-string-as region-string 'md t))
         (fname (make-temp-file "org-clipboard")))
    (write-region content nil fname)
    (shell-command (format "LANG=en_US.UTF-8 cat %s | pbcopy" fname))
    (delete-file fname)))

(defun ry/org-copy-as-html ()
  "Copy current region as html to clipboard.
It only works in Mac OS "
  (interactive)
  (require 'ox-md)
  (let* ((beg (region-beginning))
         (end (region-end))
         (region-string (format "#+OPTIONS: toc:nil num:0\n%s" (buffer-substring beg end)))
         (content (org-export-string-as region-string 'html t))
         (fname (make-temp-file "org-clipboard")))
    (write-region content nil fname)
    (shell-command (format "LANG=en_US.UTF-8 cat %s | ~/utils/copy-html-to-clipboard.sh" fname))
    (delete-file fname)))


(defun ry/org-protocol-org-id (info)
  "Handle org-protocol://org-id?id=xxxx link"
  (when-let ((id (plist-get info :id)))
    (org-id-goto id))
  nil)

(defun ry/org-protocol-copy-link ()
  "Copy org-protocol link of current org heading to clipboard"
  (interactive)
  (let ((link (format "http://localhost:3000/open-note?id=%s" (org-id-get))))
    (ry//copy-to-osx-clipboard link)
    (message link)))

(defun ry//copy-to-osx-clipboard (string)
  (shell-command (format "LANG=en_US.UTF-8 echo \"%s\" | pbcopy"
                         string)))

(defun ry//clipboard-content ()
  "Return clipboard content as string"
  (shell-command-to-string "LANG=en_US.UTF-8 pbpaste"))

(defun ry/copy-filename-to-osx-clipboard ()
  (interactive)
  (ry//copy-to-osx-clipboard (buffer-file-name)))

(defun ry/insert-file-link-from-osx-clipboard ()
  (interactive)
  (let* ((fpath (concat "file:" (ry//clipboard-content)))
         (fdesc (ry//org-desc-from-filepath fpath)))
     (insert (format "[[%s][%s]]" fpath fdesc))))

(defun ry/org-insert-browser-url-osx ()
  "Insert an org-mode link with the title and url of current tab in Chrome"
  (interactive)
  (insert (ry/osx-org-link-from-current-webpage)))

(defun fix-lark-title (title)
  "Zero-width characters have been added Lark document title
  for screening sharing tracking. This function remove those characters"
  (->> (string-to-list title)
       (--remove (< #x2000 it #x20ff))
       (--remove (= it #xfeff))
       (-map 'char-to-string)
       (s-join "")
       ;; Remove "飞书去文档" at the end of the title
       (s-replace " - 飞书云文档" "")))

(defun fix-youtube-title (title)
  (s-replace-regexp "^([0-9]*) *" "" title))

(defun ry/osx-org-link-from-current-webpage ()
  (let* ((url (ry/osx-browser-url))
         (domain (-last-item (s-match "http[s]?://\\(.*?\\)/.*" url)))
         (func (-> (--filter (s-contains? (-first-item it) domain) ry/osx-browser-title-map)
                   (-first-item)
                   (-last-item)))
         (title (->> (funcall (or func 'identity) (ry/osx-browser-title))
                     (s-replace "[" "{")
                     (s-replace "]" "}"))))
    (format "[[%s][%s]]" url title)))

(defun ry/osx-copy()
  "Copy region to OS X system pasteboard."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "LANG=en_US.UTF-8 pbcopy")
  (message "Copied to system clipboard.")
  (evil-normal-state))

(provide 'ry-osx)
