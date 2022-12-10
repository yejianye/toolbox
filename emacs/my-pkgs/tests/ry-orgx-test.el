(defmacro ry/orgx-with-test-file (&rest body)
  `(progn
     (kill-buffer (find-file-noselect "orgapi-test-copy.org"))
     (copy-file "orgapi-test.org" "orgapi-test-copy.org" t)
     (with-file-buffer "orgapi-test-copy.org"
        (with-silent-modifications ,@body))))

(ert-deftest ry/orgx-select-test ()
  (with-file-buffer "orgapi-test.org"
    (should (string= "6526D1F9-5776-4DFB-B895-E37560FE3A12"
                (-> (ry/orgx-select-one '(heading "Sub-heading-1-1"))
                    (plist-get :ID))))
    (let* ((node (ry/orgx-select-one '(heading "Sub-heading-1-2")))
           (filepath (plist-get node :filepath))
           (outline (plist-get node :outline)))
        (should (string= "orgapi-test.org" (-last-item (s-split "/" filepath))))
        (should (equal '("Heading1" "Sub-heading-1-2") outline)))))

(ert-deftest ry/orgx-select-children-test ()
  (let* ((node (ry/orgx-select-one '(heading "Heading1") "orgapi-test.org")))
    (should (= 2 (length (ry/orgx-select-children node))))
    (should (= 1 (length (ry/orgx-select-children node '(heading "-2")))))
    (should (= 0 (length (ry/orgx-select-children node '(heading "Sub-heading-2-1")))))
    (should (string= "Sub-heading-1-1" (-> (ry/orgx-select-first-child node)
                                           (plist-get :title))))))

(ert-deftest ry/orgx-node-realize-test ()
  (let* ((node1 (-> (ry/orgx-select-one
                     '(heading "Sub-heading-1-1")
                      "orgapi-test.org" t)
                    (ry//orgx-node-realize)))
         (node2 (-> (ry/orgx-select-one
                     '(heading "Sub-heading-1-2")
                     "orgapi-test.org" t)
                    (ry//orgx-node-realize))))
    (should (= 2 (plist-get node1 :level)))
    (should (string= "6526D1F9-5776-4DFB-B895-E37560FE3A12" (plist-get node1 :ID)))
    (should (string= "Sub-heading-1-2"  (plist-get node2 :title)))
    (should (equal '("Heading1" "Sub-heading-1-2") (plist-get node2 :outline)))))

(ert-deftest ry/orgx-content-get-test ()
  (with-file-buffer "orgapi-test.org"
    (let* ((node1 (ry/orgx-select-one '(heading "Sub-heading-1-2")))
           (node2 (ry/orgx-select-one '(heading "Sub-heading-2-1")))
           (node3 (ry/orgx-select-one '(heading "Heading1"))))
      (should (string= "Another sub-node\n" (ry/orgx-content-get node1)))
      (should (string= "" (ry/orgx-content-get node2)))
      (should (string= "Parent content\n" (ry/orgx-content-get node3))))))

(ert-deftest ry/orgx-content-update-test ()
  (ry/orgx-with-test-file
    (let ((node1 (ry/orgx-select-one '(heading "Sub-heading-1-2")))
          (node2 (ry/orgx-select-one '(heading "Sub-heading-2-1")))
          (node3 (ry/orgx-select-one '(heading "Heading1"))))
      (ry/orgx-content-prepend node1 "Prepend: ")
      (should (string= "Prepend: Another sub-node\n" (ry/orgx-content-get node1)))
      (ry/orgx-content-prepend node2 "Prepend test")
      (should (string= "Prepend test" (ry/orgx-content-get node2)))
      (ry/orgx-content-append node3 "Append test\n")
      (should (string= "Parent content\nAppend test\n" (ry/orgx-content-get node3))))))


(ert-deftest ry/orgx-create-child ()
  (ry/orgx-with-test-file
    (let* ((parent (ry/orgx-select-one '(heading "Heading1")))
           (child0 (ry/orgx-child-prepend parent "Sub-heading-1-0"))
           (child1 (ry/orgx-child-append parent "Sub-heading-1-3" :content "child1"))
           (child2 (ry/orgx-child-append parent "Sub-heading-1-2" :tset t))
           (child3 (ry/orgx-child-append child2 "New Child")))
      (should (string= "Sub-heading-1-0" (-> (ry/orgx-select-first-child parent)
                                             (plist-get :title))))
      (should (string= "child1\n" (ry/orgx-content-get child1)))
      (should (string= "Another sub-node\n" (ry/orgx-content-get child2)))
      (should (= 3 (plist-get child3 :level))))))

(ert-deftest ry/orgx-create-sibling ()
  (ry/orgx-with-test-file
   (let* ((parent (ry/orgx-select-one '(heading "Heading2")))
          (node (ry/orgx-select-one '(heading "Sub-heading-2-1")))
          (sibling1 (ry/orgx-sibling-prepend node "Sub-heading-2-0"))
          (sibling2 (ry/orgx-sibling-append node "Sub-heading-2-2")))
     (should (string= "Sub-heading-2-0" (-> (ry/orgx-select-first-child parent)
                                            (plist-get :title))))
     (should (string= "Sub-heading-2-2" (-> (ry/orgx-select-last-child parent)
                                            (plist-get :title)))))))