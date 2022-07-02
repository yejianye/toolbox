;; -*- lexical-binding: t -*-
(require 'request)

(defun ry/http-get (url &optional query headers)
  "HTTP GET Request, expecting JSON response. QUERY is a plist with query params."
  (let ((json-key-type 'keyword)
        (json-object-type 'plist)
        status-code response-data)
    (request url
      :type "GET"
      :params (when query (ryc/plist-to-alist query))
      :parser 'json-read
      :headers (append '(("Content-Type" . "application/json")) headers)
      :sync 't
      :complete (cl-function
                  (lambda (&key response &allow-other-keys)
                    (setq response-data (request-response-data response)
                          status-code (request-response-status-code response)))))
    (list :status-code status-code
          :data response-data)))

(defun ry/http-post (url params &optional headers)
  "HTTP POST Request (JSON Request). PARAMS is a plist with request params"
  (let ((json-key-type 'keyword)
        (json-object-type 'plist)
        status-code response-data)
    (request url
      :type "POST"
      :data (json-encode-plist params)
      :parser 'json-read
      :headers (append '(("Content-Type" . "application/json")) headers)
      :sync 't
      :complete (cl-function
                  (lambda (&key response &allow-other-keys)
                    (setq response-data (request-response-data response)
                      status-code (request-response-status-code response)))))
    (list :status-code status-code
          :data response-data)))

(provide 'ry-http)
