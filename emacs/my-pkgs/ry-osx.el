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
    safari "tell application \"Safari\" to return URL of current tab of front window"))

(setq browser-title-applescript
      '(chrome "tell application \"Google Chrome\" to return name of active tab of front window"
        safari "tell application \"Safari\" to return name of current tab of front window"))

(defun ry/osx-browser-url ()
  (substring
   (do-applescript (plist-get browser-url-applescript default-browser))
   1 -1))

(defun ry/osx-browser-title ()
  (substring
   (do-applescript (plist-get browser-title-applescript default-browser))
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
    (shell-command (format "LANG=en_US.UTF-8 cat %s | copy-html-to-clipboard.sh" fname))
    (delete-file fname)))

(defun ry/copy-org-protocol-to-osx-clipboard ()
  (interactive)
  (let ((link (ry//org-protocol-url-for-file (buffer-file-name))))
    (ry//copy-to-osx-clipboard link)
    (message (format "%s copied to clipboard."
                     (s-replace "%" "%%" link)))))

(defun ry//org-protocol-url-for-file (fname)
  (format "org-protocol://open-source?url=http://home-dir/%s"
          (s-replace " " "%20" (file-relative-name fname "/Users/ryan"))))

(defun ry//copy-to-osx-clipboard (string)
  (shell-command (format "LANG=en_US.UTF-8 echo \"%s\" | pbcopy"
                         string)))

(defun ry//clipboard-content ()
  "Return clipboard content as string"
  (shell-command-to-string "LANG=en_US.UTF-8 pbpaste"))

(defun ry/copy-filename-to-osx-clipboard ()
  (interactive)
  (ry//copy-to-osx-clipboard (buffer-file-name)))

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


(defun ry/osx-org-link-from-current-webpage ()
  (format "[[%s][%s]]" (ry/osx-browser-url)
        (thread-last (ry/osx-browser-title)
                    (fix-lark-title)
                    (s-replace "[" "{")
                    (s-replace "]" "}"))))

(defun ry/osx-copy()
  "Copy region to OS X system pasteboard."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "LANG=en_US.UTF-8 pbcopy")
  (message "Copied to system clipboard.")
  (evil-normal-state))

(provide 'ry-osx)
