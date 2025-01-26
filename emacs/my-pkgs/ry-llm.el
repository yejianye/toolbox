;;; ry-llm.el --- LLM related functions -*- lexical-binding: t -*-

;;; Commentary:
;; Functions for interacting with LLM services

;;; Code:

(require 'term)

(defcustom ry/aider-chat-window-height 10
  "Height of the aider chat window in lines."
  :type 'integer
  :group 'ry-llm)

(defun ry/git-root-dir ()
  "Get the git root directory for current buffer's file."
  (when-let ((file (buffer-file-name))
             (default-directory (file-name-directory file)))
    (locate-dominating-file default-directory ".git")))

(defun ry/aider-start ()
  "Start aider in terminal buffer.
Sets current dir, uses existing window or creates new one.
Requires the buffer to be in a git repository."
  (interactive)
  (let ((buffer-name "*aider*")
        (file-name (buffer-file-name))
        (dir (file-name-directory (buffer-file-name)))
        (git-root (ry/git-root-dir)))
    (unless git-root
      (user-error "Not in a git repository"))
    ;; Kill existing buffer if it exists
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    ;; Find or create window
    (if (one-window-p)
        (split-window-right)
      (other-window 1))
    ;; Create new terminal and start aider
    (let ((term-buffer (term "/bin/zsh")))
      (rename-buffer buffer-name)
      (with-current-buffer term-buffer
        (setq-local ry/aider-git-root git-root)
        (local-set-key (kbd "s-v") 'ry/aider-paste)
        ;; Add evil mode hooks
        (add-hook 'evil-normal-state-entry-hook 'term-line-mode nil t)
        (add-hook 'evil-insert-state-entry-hook 'term-char-mode nil t))
      (term-send-string term-buffer (format "cd %s\n" dir))
      (term-send-string term-buffer (format "aider --file %s\n" file-name)))
    ;; Return to original window
    (other-window -1)))

(defun ry/aider-stop ()
  "Stop aider by killing its buffer and process without confirmation."
  (interactive)
  (when-let ((buffer (get-buffer "*aider*")))
    (let ((process (get-buffer-process buffer)))
      (when process
        (delete-process process))
      (kill-buffer buffer))))

(defun ry/aider-restart ()
  "Restart aider by stopping and starting it again."
  (interactive)
  (ry/aider-stop)
  (ry/aider-start))

(defun ry/aider-generate (prompt)
  "Send PROMPT to aider for code generation or modification.
Requires aider to be already running. Adds current file to aider first.
If region is selected, modifies the selected lines. Otherwise inserts at current line."
  (interactive "sCode generation/modification prompt: ")
  (if-let ((term-buffer (get-buffer "*aider*"))
           (file-path (buffer-file-name)))
      (let ((region-content (when (use-region-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))))
            (line-num (line-number-at-pos))
            (current-git-root (ry/git-root-dir)))
        (with-current-buffer term-buffer
          (when (and ry/aider-git-root current-git-root
                     (not (string= ry/aider-git-root current-git-root)))
            (if (yes-or-no-p "Git root directory changed. Restart aider?")
                (progn
                  (ry/aider-restart)
                  (sleep-for ry/aider-restart-wait-time))  ;; Wait for restart
              (user-error "Git root directory mismatch")))
          (term-send-string nil (format "/add %s\n" file-path))
          (sleep-for 0.2)  ;; Brief pause to ensure file is added
          (if region-content
            (term-send-string nil (format "{\nModify the following code in %s according to the instruction:\n```\n%s\n```\nInstruction: %s\n}\n"
                                          (file-name-nondirectory file-path) region-content prompt))
            (term-send-string nil (format "{\n%s\nInsert the code at line %d\n}\n" prompt line-num)))))
    (message "aider is not started")))

(defun ry/aider-add-current-file ()
  "Add current file to aider if it's running."
  (interactive)
  (if-let ((term-buffer (get-buffer "*aider*"))
           (file-path (buffer-file-name)))
      (progn
        (with-current-buffer term-buffer
          (term-send-string nil (format "/add %s\n" file-path))
          (sleep-for 0.2)))  ;; Brief pause to ensure file is added
    (message "aider is not started")))

(defun ry/aider-switch-buffer ()
  "Switch to aider buffer in other window if it exists."
  (interactive)
  (if-let ((buffer (get-buffer "*aider*")))
      (switch-to-buffer-other-window buffer)
    (message "No aider buffer found")))

(defun ry/aider-paste ()
  "Send clipboard content to aider using macOS pbpaste."
  (interactive)
  (if-let ((term-buffer (get-buffer "*aider*")))
      (with-current-buffer term-buffer
        (let ((clipboard-content (string-trim (shell-command-to-string "pbpaste"))))
          (term-send-string nil clipboard-content)))
    (message "aider is not started")))

(defun ry/aider-send-chat-message ()
  "Send current buffer content to aider chat and clear buffer."
  (interactive)
  (when-let ((content (buffer-string))
             (aider-buffer (get-buffer "*aider*")))
    (with-current-buffer aider-buffer
      (term-send-string nil content)
      (term-send-string nil "\n"))
    (erase-buffer)))

(defun ry/aider-chat-message ()
  "Open or show chat buffer to send messages to aider.
Starts aider if it's not already running."
  (interactive)
  ;; Start aider if not running
  (unless (get-buffer "*aider*")
    (ry/aider-start))
  
  (let ((buf (get-buffer-create "*aider-chat-buffer*")))
    ;; If buffer is already visible, just switch to it
    (if (get-buffer-window buf)
        (select-window (get-buffer-window buf))
      ;; Otherwise create new window and show buffer
      (split-window-below)
      (other-window 1)
      (switch-to-buffer buf)
      (window-resize nil (- ry/aider-chat-window-height (window-height)))
      (text-mode)
      (use-local-map (copy-keymap text-mode-map))
      (local-set-key (kbd "C-c C-c") 'ry/aider-send-chat-message)
      (evil-local-set-key 'normal ",cc" 'ry/aider-send-chat-message)
      (evil-local-set-key 'normal "q" (lambda ()
                                       (interactive)
                                       (delete-window)))
      (setq-local header-line-format
                  "Type your message. C-c C-c to send, q to hide"))))

(defhydra ry/hydra-aider (:color blue :hint nil)
  "
_a_: start aider   _s_: stop aider   _r_: restart aider   _g_: generate code   _b_: switch buffer   _f_: add file   _c_: chat   _q_: quit
"
  ("a" ry/aider-start)
  ("s" ry/aider-stop)
  ("r" ry/aider-restart)
  ("g" ry/aider-generate)
  ("b" ry/aider-switch-buffer)
  ("f" ry/aider-add-current-file)
  ("c" ry/aider-chat-message)
  ("q" nil "quit"))

(provide 'ry-llm)
;;; ry-llm.el ends here
