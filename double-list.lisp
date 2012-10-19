(in-package #:burning-lisp)

(defstruct (double-list (:copier nil) (:constructor %make-double-list))
  head
  tail)

(defun make-double-list (list)
  (%make-double-list :head list
		     :tail (last list)))

(defun copy-double-list (list)
  (make-double-list (double-list-head list)))

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

(defun double-list-remove (value list &key (test #'eql))
  (make-double-list (remove value (double-list-head list) :test test)))
