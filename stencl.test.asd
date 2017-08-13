(asdf:defsystem #:stencl.test
  :description "Tests for stencl"
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :license "MIT"
  :serial t
  :depends-on (#:stencl #:stefil)
  :components ((:file "test")))
