(in-package #:burning-lisp)

(defun check-keywords (keywords list)
  (let ((keywords-list (find-keywords list)))
    (flet ((check-keyword (keyword)
	     (unless (member keyword keywords)
	       (error 'type-error :expected-type (cons 'member keywords) :datum keyword))))
      (mapc #'(lambda (x) (check-keyword (first x))) keywords-list)
      list)))

(defun find-keyword (name list)
  (cond
    ((null list) nil)
    ((eq (first list) name) (second list))
    (t (find-keyword name (rest list)))))

(defun find-keywords (list)
  (cond
    ((null list) (values nil nil))
    ((and (keywordp (first list)) (not (rest list))) 
     (error "Incorrect keyword arguments in ~a." (list (first list))))
    ((keywordp (first list)) (multiple-value-bind (keywords new-list) (find-keywords (rest (rest list)))
			       (values (acons (first list) (second list) keywords)
				       new-list)))
    (t (multiple-value-bind (keywords new-list) (find-keywords (rest list))
	 (values keywords (cons (first list) new-list))))))

(defun remove-keyword (name list)
  (cond
    ((null list) nil)
    ((and (eq (first list) name) (rest list)) (remove-keyword name (rest (rest list))))
    (t (cons (first list) (remove-keyword name (rest list))))))

(defun remove-keywords (list)
  (cond
    ((null list) nil)
    ((and (keywordp (first list)) (rest list)) (remove-keywords (rest (rest list))))
    (t (cons (first list) (remove-keywords (rest list))))))

