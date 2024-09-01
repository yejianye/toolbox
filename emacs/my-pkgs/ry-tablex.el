;;; ry-tablex.el --- Enhanced table with multi-line cell support  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ryan Ye

(defun org-dblock-write:tablex (params)
  (let ((table-id (plist-get params :id)))
    (ry/tablex-render table-id)))

(defun ry/tablex-render (table-id)
  (ryc/hash-to-plist
    (ry/pyfunc "rypy.tablex" "table_render" table-id)))

(provide 'ry-tablex)