(ert-deftest ry/org-get-file-property-test ()
  (with-temp-file-buffer "orgapi-test.org"
                 (should (string= "test" (ry/org-get-file-property "CATEGORY")))))

(ert-deftest ry/orgapi-get-node-test ()
  (with-temp-file-buffer "orgapi-test.org"
    (should (ry/orgapi-get-node-by-heading "Heading1" "Sub-heading-1-1"))
    (should-not (ry/orgapi-get-node-by-heading "Heading1" "Sub-heading-1-3"))))

(ert-deftest ry/orgapi-get-child-test ()
  (with-temp-file-buffer "orgapi-test.org"
    (let ((first-node (ry/orgapi-first-child (ry/orgapi-get-root)))
          (last-node (ry/orgapi-last-child (ry/orgapi-get-root))))
      (should (string= "Heading1" (org-element-property :title first-node)))
      (should (string= "Heading2" (org-element-property :title last-node)))
      (should (ry/orgapi-first-child first-node :title "Sub-heading-1-1"))
      (should-not (ry/orgapi-first-child first-node :title "Test"))
      (should (= 2 (length (ry/orgapi-get-children first-node)))))))