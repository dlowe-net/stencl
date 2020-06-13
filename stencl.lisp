(in-package #:stencl)

(defun parse-template-bracket (stream char)
    "Used as a reader parser for the stencl syntax.  ] denotes the beginning of
a string.  [ denotes the end of a string, unless followed immediately by
another [."
    (declare (ignore char))
    (with-output-to-string (str)
      (loop
         for c = (read-char stream nil #\[ t)
         until (and
                (eql c #\[)
                (not (eql (peek-char nil stream nil nil t) #\[)))
         do (write-char c str)
           (when (eql c #\[)
             (write-char (read-char stream nil nil t))))))

(defun parse-param-brace (stream char num)
  (declare (ignore char num))
  (let ((sym (read-from-string
              (with-output-to-string (str)
                (loop
                   for c = (read-char stream nil #\{ t)
                   until (eql c #\})
                   do (write-char c str))))))
    (check-type sym symbol)
    sym))

(defun %parse-template (stream)
    "Parses an html template into a list of elements suitable for inserting
into a stencl expression"
    (let ((*readtable* (copy-readtable))
          (params nil))
      (set-macro-character #\] #'parse-template-bracket)
      (set-dispatch-macro-character #\# #\{
                                    (lambda (stream char num)
                                      (let ((sym (parse-param-brace stream char num)))
                                        (pushnew sym params)
                                        sym)))
      (loop with first-elt = (parse-template-bracket stream #\])
         for elt = (read stream nil nil)
         while elt
         collect elt into result
         finally (return (values (cons first-elt result) params)))))

(defun princ-not-nil (object)
  "Princs the object to *standard-output* only if object is non-NIL."
  (when object
    (princ object)))

(defun %compile-template (stream)
  "Returns a lambda expression to be compiled into a function."
  (multiple-value-bind (template-code params)
      (%parse-template stream)
    (let ((output (gensym))
          (result (gensym)))
      `(lambda (,output &rest other-params &key ,@params &allow-other-keys)
         (declare (ignorable other-params))
         (let ((*standard-output* ,output)
               (,result nil))
           (flet ((collect (&rest local-results)
                    (setf ,result (append ,result local-results))
                    nil))
             (declare (ignorable #'collect))
             (macrolet ((out (&rest objects)
                          `(progn
                             ,@(mapcar
                                (lambda (o)
                                  `(princ-not-nil ,o))
                                objects)
                             nil))
                        (include (v &rest local-params)
                          `(apply #'collect
                                  (apply ,v ,',output (append other-params
                                                              (list
                                                               ,@local-params))))))
               (out ,@template-code)
               ,result)))))))

(defun from-stream (stream)
  "Returns a compiled function which outputs to the given stream
the html which the html template is supposed to produce."
  (compile nil (%compile-template stream)))

(defun from-string (str)
  "Returns a compiled function which outputs to the given stream
the html which the html template is supposed to produce."
  (with-input-from-string (s str)
    (from-stream s)))

(defun from-file (path)
  "Returns a compiled function which outputs to the given stream
the html which the html template is supposed to produce."
  (with-open-file (inf path :direction :input)
    (from-stream inf)))

(defun to-stream (stream template &rest args)
  (apply template stream args))

(defun to-string (template &rest args)
  (let (result)
    (values
     (with-output-to-string (s)
       (setf result (apply 'to-stream s template args)))
     result)))

(defun to-file (path template &rest args)
  (with-open-file (ouf path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (apply 'to-stream ouf template args)))

(defun format-template (stream template &rest args)
  (cond
    ((null stream)
     (apply 'to-string (from-string template) args))
    ((eq stream t)
     (apply 'to-stream *standard-output* (from-string template) args))
    (t
     (apply 'to-stream stream (from-string template) args))))
