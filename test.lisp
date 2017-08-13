(defpackage #:stencl.test
  (:use #:stefil #:stencl #:cl))

(in-package #:stencl.test)

(defsuite* (test :in root-suite) ()
  (run-child-tests))

(deftest string-identity ()
  (let ((test-string "This is a test."))
    (is (equal test-string
               (to-string (from-string test-string))))))

(deftest basic-eval ()
  (is (equal "The answer is 42."
             (to-string (from-string "The answer is [(* 6 7)].")))))

(deftest parameter ()
  (is (equal "baking soda and vinegar combine to create a foamy volcano!"
       (to-string (from-string
                   "[#{solvent}] and [#{solute}] combine to create [#{solution}]!")
                  :solvent "baking soda"
                  :solute "vinegar"
                  :solution "a foamy volcano"))))


(defparameter aux-template nil)
(deftest include-with-implict-param ()
  (setf aux-template (from-string "\"[#{quote}]\""))
  (is (equal "\"That which is to give light must endure burning.\" -- Viktor Frankl"
             (to-string (from-string "[(stencl:include stencl.test::aux-template)] -- [#{author}]")
                        :quote "That which is to give light must endure burning."
                        :author "Viktor Frankl"))))
