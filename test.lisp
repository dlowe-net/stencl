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

(deftest repeated-implicit-param ()
  (is (equal "It was the best of times, it was the worst of times."
       (to-string (from-string
                   "It was the best of [#{things}], it was the worst of [#{things}].")
                  :things "times"))))


(defparameter aux-template nil)
(deftest include-with-implict-param ()
  (setf aux-template (from-string "\"[#{quote}]\""))
  (is (equal "\"That which is to give light must endure burning.\" -- Viktor Frankl"
             (to-string (from-string "[(stencl:include stencl.test::aux-template)] -- [#{author}]")
                        :quote "That which is to give light must endure burning."
                        :author "Viktor Frankl"))))

(deftest collection ()
  (is (equal '("Testing" (:foo "bar" :bar "baz"))
             (multiple-value-list
              (format-template nil "Testing[(stencl:collect :foo \"bar\") (stencl:collect :bar \"baz\")]")))))

(deftest collect-with-include ()
  (setf aux-template (from-string "-*[(stencl:collect :foo \"bar\")]*-"))
  (is (equal '("Testing -**-" (:foo "bar"))
             (multiple-value-list
              (format-template nil "Testing [(stencl:include aux-template)]")))))
