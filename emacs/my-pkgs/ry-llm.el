;;; ry-llm.el --- LLM related functions -*- lexical-binding: t -*-

;;; Commentary:
;; Functions for interacting with LLM services

;;; Code:

(require 'term)

(defun ry/aider-start ()
  "Start aider in a terminal buffer.
Changes to current file's directory before starting aider."
  (interactive)
  (let ((buffer-name "*aider*")
        (default-directory (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
    ;; Kill existing buffer if it exists
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    ;; Create new terminal and start aider
    (let ((term-buffer (term "/bin/zsh")))
      (rename-buffer buffer-name)
      (term-send-string term-buffer "aider\n"))))

(defun ry/aider-stop ()
  "Stop aider by killing its buffer."
  (interactive)
  (when-let ((buffer (get-buffer "*aider*")))
    (kill-buffer buffer)))

(defun ry/aider-generate (prompt)
  "Send PROMPT to aider for code generation.
Requires aider to be already running. Adds current file to aider first."
  (interactive "sCode generation prompt: ")
  (if-let ((term-buffer (get-buffer "*aider*"))
           (file-path (buffer-file-name)))
      (progn
        (with-current-buffer term-buffer
          (term-send-string nil (format "/add %s\n" file-path))
          (sleep-for 0.2)  ;; Brief pause to ensure file is added
          (term-send-string nil (concat prompt "\n"))))
    (message "aider is not started")))

(provide 'ry-llm)
;;; ry-llm.el ends here
