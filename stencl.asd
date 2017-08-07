;;;; stencl.asd

(asdf:defsystem #:stencl
  :description "Lightweight templating library"
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "stencl")))

