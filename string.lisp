(in-package #:burning-lisp)

(defun line (arg)
  (cond 
    ((atom arg) (format nil "~a" arg))
    ((null (rest arg)) (line (first arg)))
    (t (concatenate 'string
		    (format nil "~a~va" (first arg) (second arg) "")
		    (line (rest (rest arg)))))))

(defun lines (&rest args)
  (cond
    ((null args) "")
    (t (concatenate 'string (line (first args)) #(#\Newline) (apply #'lines (rest args))))))

(defun lines* (&rest args)
  (string+ (apply #'lines (butlast args)) (line (first (last args)))))

(defun string+ (&rest strings)
  (apply #'concatenate 'string strings))


