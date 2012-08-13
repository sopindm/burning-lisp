(in-package #:burning-lisp)

(defmacro awhen (expression &body body)
  `(let ((it ,expression))
     (when it ,@body)))

(defmacro awhile (expression &body body)
  `(do ((it ,expression ,expression))
       ((null it) t)
     ,@body))

(defmacro aif (expr then-form &optional else-form)
  `(let ((it ,expr))
     (if it ,then-form ,else-form)))

(defmacro acond (&rest forms)
  (cond
    ((null forms) '(cond))
    ((null (rest forms)) `(aif ,@(first forms)))
    (t `(aif ,@(first forms) (acond ,@(rest forms))))))