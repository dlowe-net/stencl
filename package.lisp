;;;; package.lisp

(defpackage #:stencl
  (:use #:cl)
  (:export #:from-stream
           #:from-string
           #:from-file
           #:to-stream
           #:to-string
           #:to-file
           #:format-template
           #:collect
           #:include
           #:out))
