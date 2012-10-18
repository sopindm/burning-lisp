(in-package #:burning-lisp)

(defun package-exported-symbols (package)
  (let ((symbols ()))
    (do-external-symbols (symbol (find-package package))
      (push symbol symbols))
    symbols))

(defmacro define-merged-package (name &body packages)
  `(defpackage ,name
     (:use ,@packages)
     (:export ,@(mapcan #'package-exported-symbols packages))))