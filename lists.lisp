(in-package #:burning-lisp)

(defun every-nth (list n &optional (start 0))
  (labels ((%every-nth (list i)
	     (cond ((null list) nil)
		   ((= i 0) (cons (first list) (%every-nth (rest list) (1+ i))))
		   ((= i n) (%every-nth list 0))
		   (t (%every-nth (rest list) (1+ i))))))
    (%every-nth (nthcdr start list) 0)))

(defun group (list n)
  (labels ((%next-group (list i)
	     (cond ((null list) (values nil nil))
		   ((= i n) (values nil list))
		   (t (multiple-value-bind (group rest) (%next-group (rest list) (1+ i))
			(values (cons (first list) group) rest))))))
    (if list
	(multiple-value-bind (group rest) (%next-group list 0)
	  (cons group (group rest n))))))
	
	   
