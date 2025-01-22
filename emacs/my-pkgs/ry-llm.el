;;; ry-llm.el --- LLM related functions -*- lexical-binding: t -*-

;;; Commentary:
;; Functions for interacting with LLM services

;;; Code:

(require 'term)

(defun ry/aider-start ()
  "Start aider in terminal buffer.
Sets current dir, uses existing window or creates new one."
  (interactive)
  (let ((buffer-name "*aider*")
        (dir (file-name-directory (buffer-file-name))))
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
      (term-send-string term-buffer (format "cd %s\n" dir))
      (term-send-string term-buffer "aider\n"))
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

(defun ry/aider-generate (prompt)
  "Send PROMPT to aider for code generation or modification.
Requires aider to be already running. Adds current file to aider first.
If region is selected, modifies the selected lines. Otherwise inserts at current line."
  (interactive "sCode generation/modification prompt: ")
  (if-let ((term-buffer (get-buffer "*aider*"))
           (file-path (buffer-file-name)))
      (let ((region-content (when (use-region-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))))
            (line-num (line-number-at-pos)))
        (with-current-buffer term-buffer
          (term-send-string nil (format "/add %s\n" file-path))
          (sleep-for 0.2)  ;; Brief pause to ensure file is added
          (if region-content
            (term-send-string nil (format "{\nModify the following code in %s according to the instruction:\n```\n%s\n```\nInstruction: %s\n}\n"
                                          (file-name-nondirectory file-path) region-content prompt))
            (term-send-string nil (format "{\n%s\nInsert the code at line %d\n}\n" prompt line-num)))))
    (message "aider is not started")))

(provide 'ry-llm)
;;; ry-llm.el ends here
