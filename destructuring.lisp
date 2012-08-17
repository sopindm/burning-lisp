(in-package #:burning-lisp)

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression
     ,@body))

