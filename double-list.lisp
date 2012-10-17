(in-package #:burning-lisp)

(defstruct (double-list (:copier nil))
  head
  tail)

(defun copy-double-list (list)
  (let ((dlist (make-double-list)))
    (mapc (lambda (elt) (double-list-push elt dlist)) (double-list-head list))
    dlist))

(defun double-list-last (list)
  (first (double-list-tail list)))

(defun double-list-push (value list)
  (with-slots (head tail) list
    (if head
	(let ((new (cons value nil)))
	  (setf (rest tail) new)
	  (setf tail new))
	(let ((new (cons value nil)))
	  (setf head new)
	  (setf tail new))))
  list)

