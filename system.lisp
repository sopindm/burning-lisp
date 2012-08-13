(in-package #:burning-lisp)

(defun quit ()
  #+sbcl
  (sb-ext:quit)
  #+ccl
  (ccl:quit)
  #-(or sbcl ccl)
  (error "quit no implemented"))