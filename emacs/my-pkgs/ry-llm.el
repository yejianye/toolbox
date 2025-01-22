;;; ry-llm.el --- LLM related functions -*- lexical-binding: t -*-

;;; Commentary:
;; Functions for interacting with LLM services

;;; Code:

(require 'term)

(defun ry/aider-start ()
  "Start aider in a terminal buffer in another window.
Changes to current file's directory before starting aider.
Uses existing window if available, otherwise creates new one."
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
  "Send PROMPT to aider for code generation.
Requires aider to be already running. Adds current file to aider first.
Inserts new code at current line."
  (interactive "sCode generation prompt: ")
  (if-let ((term-buffer (get-buffer "*aider*"))
           (file-path (buffer-file-name))
           (line-num (line-number-at-pos)))
      (progn
        (with-current-buffer term-buffer
          (term-send-string nil (format "/add %s\n" file-path))
          (sleep-for 0.2)  ;; Brief pause to ensure file is added
          (term-send-string nil (format "{\n%s\nInsert the code at line %d\n}\n" prompt line-num))))
    (message "aider is not started")))

(defun ry/aider-generate-region (prompt)
  "Send PROMPT to aider for modifying the selected region.
Requires aider to be already running. Adds current file to aider first.
Modifies code between start and end lines of region."
  (interactive "sCode modification prompt: ")
  (if (not (use-region-p))
      (message "No region selected")
    (if-let ((term-buffer (get-buffer "*aider*"))
             (file-path (buffer-file-name))
             (start-line (line-number-at-pos (region-beginning)))
             (end-line (line-number-at-pos (region-end))))
        (progn
          (with-current-buffer term-buffer
            (term-send-string nil (format "/add %s\n" file-path))
            (sleep-for 0.2)  ;; Brief pause to ensure file is added
            (term-send-string nil (format "{\nModify lines %d-%d according to the following instruction:\n%s\n}\n"
                                          start-line end-line prompt))))
      (message "aider is not started"))))

(provide 'ry-llm)
;;; ry-llm.el ends here
