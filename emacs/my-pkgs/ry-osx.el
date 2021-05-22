(defun ry/osx-chrome-url ()
   (do-applescript "tell application \"Google Chrome\" to return URL of active tab of front window")
  )

(defun ry/osx-chrome-title ()
   (do-applescript "tell application \"Google Chrome\" to return title of active tab of front window")
  )

;; There's a difference between emacs-plus and emacs-mac in `do-applescript`
;; emacs-mac will wrap the result with a pair of double-quote. For
;; `ry/osx-chrome-title` and `ry/osx-chrome-url`, the following implementation
;; needs be used instead.
;;
;; (defun ry/osx-chrome-url ()
;;   (substring
;;    (do-applescript "tell application \"Google Chrome\" to return URL of active tab of front window")
;;    1 -1)
;;   )
;;
;; (defun ry/osx-chrome-title ()
;;   (substring
;;    (do-applescript "tell application \"Google Chrome\" to return title of active tab of front window")
;;    1 -1)
;;   )

(defun ry/osx-paste()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (let ((clipboard (shell-command-to-string "LANG=en_US.UTF-8 pbpaste"))
        )
    (when (eq evil-state 'visual)
      (kill-region (region-beginning) (region-end))
      (evil-normal-state)
      )
    (insert (s-replace "\r" ""  clipboard))))

;; (defun ry/org-cliplink-osx ()
;;   "Takes a URL from the OSX clipboard and inserts an org-mode link
;; with the title of a page found by the URL into the current
;; buffer"
;;   (interactive)
;;   (org-cliplink-insert-transformed-title (shell-command-to-string "LANG=en_US.UTF-8 pbpaste")
;;                                          'org-cliplink-org-mode-link-transformer))

(provide 'ry-osx)
