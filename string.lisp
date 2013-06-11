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

(defun search-and-replace (where what replace-with &optional (start 0))
  (let ((pos (search what where :start2 start)))
    (if pos
	(values (string+ (subseq where 0 pos)
			 replace-with
			 (subseq where (+ pos (length what))))
		(+ pos (length replace-with)))
	(values where (length where)))))
	

(defun search-and-replace-all (where what replace-with &optional (start 0))
  (multiple-value-bind (string pos) (search-and-replace where what replace-with start)
    (if (= pos (length string))
	string
	(search-and-replace-all string what replace-with pos))))
  
		 
