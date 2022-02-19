(defvar ry/orglark-default-folder "fldcn7xwsf8rv4MfhARtgNqFljf"
  "Default lark folder token")

(defun ry/orglark-create-doc (title &optional folder)
  (ry/pyfunc "rypy.lark" "create_doc" title (or folder ry/orglark-default-folder))
  )

(defun ry/orglark-create-sheet (title &optional folder)
  (ry/pyfunc "rypy.lark" "create_sheet" title (or folder ry/orglark-default-folder))
  )

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
    (insert (format "[[%s][%s]]" url title))
    )
  )


(provide 'ry-orglark)
