(asdf:defsystem #:setinka
  :description "Typed, documented, layered settings with durable stores and dependent values."
  :author      "Lambda Symbolics OÜ"
  :license     "COLL-Attribution"
  :version     "0.1.0"
  :serial      t
  :depends-on  (#:bordeaux-threads
                #:serapeum
                #:uiop)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file "package")
                              (:file "conditions")
                              (:file "classes")
                              (:file "setting")
                              (:file "registry")
                              (:file "store")
                              (:file "configuration")
                              (:file "construction"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:setinka/tests))))

(asdf:defsystem #:setinka/tests
  :description "Tests for setinka."
  :depends-on  (#:setinka)
  :serial      t
  :components  ((:module     "tests"
                 :serial     t
                 :components ((:file "test-support")
                              (:file "setting-tests")
                              (:file "configuration-tests")
                              (:file "tests"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (uiop:symbol-call '#:setinka '#:run-tests)))
