(defvar ry/orglark-default-folder "fldcn7xwsf8rv4MfhARtgNqFljf"
  "Default lark folder token")

(defun ry/orglark-create-doc (title &optional folder)
  (ry/pyfunc "rypy.lark" "create_doc" title (or folder ry/orglark-default-folder)))
  

(defun ry/orglark-create-sheet (title &optional folder)
  (ry/pyfunc "rypy.lark" "create_sheet" title (or folder ry/orglark-default-folder)))
  

(defun ry/orglark-insert-lark (title)
  "Create a lark doc and insert a link to the doc at current position"
  (interactive "sTitle: ")
  (let* ((doc-type (read-answer "d) Document, s) Spreadsheet "
                                '(("doc" ?d "Document")
                                  ("sheet" ?s "Sheet"))))
         (create-func (if (string= doc-type "doc")
                          'ry/orglark-create-doc
                        'ry/orglark-create-sheet))
         (url (funcall create-func title)))
    (insert (format "[[%s][%s]]" url title))))

(defun ry/orglark-region-to-docx ()
  "Create a docx from current region and open browser"
  (interactive)
  (require 'ox-md)
  (let* ((beg (region-beginning))
         (end (region-end))
         (region-string (format "#+OPTIONS: toc:nil\n%s" (buffer-substring beg end)))
         (content (org-export-string-as region-string 'md t))
         (title (format "Orgmode Note (%s)" (format-time-string "%Y-%m-%d %H:%M")))
         (url (-> (ry/http-post "http://localhost:3000/lark/markdown"
                                (list :title title :content content))
                  (ryc/plist-path '(:data :url)))))
    (org-open-link-from-string (format "[[%s]]" url))))

(defun ry/auto-translate (text)
  "Translate en->zh or zh->en depends on TEXT"
  (-> (ry/http-post "http://localhost:3000/lark/auto-translate"
                    (list :text text))
      (ryc/plist-path '(:data :text))))

(defun ry/auto-translate-region (beg end)
  "Auto-translate current region"
  (interactive "r")
  (let* ((text (ry/auto-translate (buffer-substring-no-properties beg end)))
         (translate-buffer (get-buffer-create "*auto-translate*")))
    (deactivate-mark)
    (with-current-buffer translate-buffer
      (erase-buffer)
      (insert text))
    (popwin:popup-buffer translate-buffer :stick t)
    (evil-local-set-key 'normal (kbd "q") 'quit-window)))


(provide 'ry-orglark)
