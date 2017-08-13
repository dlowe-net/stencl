;;;; stencl.asd

(asdf:defsystem #:stencl
  :description "Lightweight templating library"
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "stencl")))

(defmethod perform ((op test-op) (system (eql (asdf:find-system '#:stencl))))
  (asdf:load-system '#:stencl.test)
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'stencl.test::test)")))

