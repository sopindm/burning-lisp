(in-package #:burning-lisp)

(defun lambda-list-keyword-p (arg)
  (and (symbolp arg) 
       (let ((name (symbol-name arg)))
	 (and (char= (char name 0) #\&) (> (length name) 1)))))

(defun find-lambda-list-keyword (keyword list)
  (cond
    ((null list) nil)
    ((null keyword) (remove-lambda-list-keywords list))
    ((eq (first list) keyword) (remove-lambda-list-keywords (rest list)))
    (t (find-lambda-list-keyword keyword (rest list)))))

(defun find-lambda-list-keywords (list)
  (if (not (null list))
      (let ((rest (find-lambda-list-keywords (rest list))))
	(if (lambda-list-keyword-p (first list))
	    (acons (first list) (rest (assoc nil rest)) (remove nil rest :key #'first))
	    (acons nil (cons (first list) (rest (assoc nil rest))) (remove nil rest :key #'first))))))

(defun remove-lambda-list-keyword (keyword list)
  (cond ((null list) nil)
	((eq (first list) keyword)
	 (remove-lambda-list-keyword keyword (member-if #'lambda-list-keyword-p (rest list))))
	(t (cons (first list) (remove-lambda-list-keyword keyword (rest list))))))

(defun remove-lambda-list-keywords (list)
  (cond
    ((null list) nil)
    ((lambda-list-keyword-p (first list)) nil)
    (t (cons (first list) (remove-lambda-list-keywords (rest list))))))

(defgeneric bind-lambda-list-keyword (keyword lambda-list arguments &key macro-p generic-p))
(defgeneric lambda-list-keyword-arguments (keyword lambda-list &key macro-p generic-p))
(defgeneric check-lambda-list-keyword (keyword lambda-list &key macro-p generic-p))

(defmacro define-lambda-keyword-binder (keyword (list-arg arguments-arg &optional macro-p-arg generic-p-arg 
							  &rest key-args)
					&body body)
  (let ((macro-p-arg-name (or macro-p-arg (gensym)))
	(generic-p-arg-name (or generic-p-arg (gensym)))
	(keyword-arg (gensym)))
    `(defmethod bind-lambda-list-keyword ((,keyword-arg (eql ',keyword)) ,list-arg ,arguments-arg 
					  &key ((:macro-p ,macro-p-arg-name)) ((:generic-p ,generic-p-arg-name))
					  ,@key-args)
       ,@(unless (and macro-p-arg generic-p-arg)
		 `((declare (ignore ,@(if macro-p-arg nil (list macro-p-arg-name))
				    ,@(if generic-p-arg nil (list generic-p-arg-name))))))
       ,@body)))

(defmacro define-lambda-keyword-arguments (keyword (list-arg &optional macro-p-arg generic-p-arg) &body body)
  (let ((macro-p-arg-name (or macro-p-arg (gensym)))
	(generic-p-arg-name (or generic-p-arg (gensym)))
	(keyword-arg (gensym)))
    `(defmethod lambda-list-keyword-arguments ((,keyword-arg (eql ',keyword)) ,list-arg
					       &key ((:macro-p ,macro-p-arg-name)) 
					       ((:generic-p ,generic-p-arg-name)))
       ,@(unless (and macro-p-arg generic-p-arg)
		 `((declare (ignore ,@(if macro-p-arg nil (list macro-p-arg-name))
				    ,@(if generic-p-arg nil (list generic-p-arg-name))))))
       ,@body)))

(defmacro define-lambda-keyword-checker (keyword (list-arg &optional macro-p-arg generic-p-arg) &body body)
  (let ((macro-p-arg-name (or macro-p-arg (gensym)))
	(generic-p-arg-name (or generic-p-arg (gensym)))
	(keyword-arg (gensym)))
    `(defmethod check-lambda-list-keyword ((,keyword-arg (eql ',keyword)) ,list-arg
					   &key ((:macro-p ,macro-p-arg-name)) ((:generic-p ,generic-p-arg-name)))
       ,@(unless (and macro-p-arg generic-p-arg)
		 `((declare (ignore ,@(if macro-p-arg nil (list macro-p-arg-name))
				    ,@(if generic-p-arg nil (list generic-p-arg-name))))))
       ,@body)))

;;
;; Normal arguments
;;

(define-lambda-keyword-binder nil (list args)
  (when (< (length args) (length list))
    (error "Not enought arguments for lambda list ~a in list ~a." list args))
  (list (mapcar #'cons list args)
	(nthcdr (length list) args)))

(define-lambda-keyword-checker nil (list)
  (unless (every #'atom list)
    (error "Wrong ordinary lambda list ~a." list)))

(define-lambda-keyword-arguments nil (list) 
  list)

;;
;; Optional arguments
;;

(define-lambda-keyword-binder &optional (list args)
  (labels ((bind-arg (arg value have-value-p)
	     (unless (listp arg) (setf arg (list arg)))
	     (dbind (name &optional default (set-flag nil set-flag-p)) arg
	       (if have-value-p
		   (acons name value (if set-flag-p (acons set-flag t nil)))
		   (acons name default (if set-flag-p (acons set-flag nil nil))))))
	   (do-bind (list args)
	     (if (not (null list))
		 (dbind (bindings rest) (do-bind (rest list) (rest args))
		   (list (append (bind-arg (first list) (first args) args) bindings)
			 rest))
		 (list nil args))))
    (do-bind list args)))

(define-lambda-keyword-arguments &optional (list)
  (mapcan (lambda (x) (if (listp x) 
			  (cons (first x) (aif (third x) (list it)))
			  (list x)))
	  list))

(define-lambda-keyword-checker &optional (list)
  (unless (every (lambda (x) (if (listp x)
				 (and (>= (length x) 1) (symbolp (first x))
				      (if (= (length x) 3) (symbolp (third x)) (< (length x) 3)))
				 (symbolp x)))
		 list)
    (error "Wrong &optional lambda list ~a." list)))

;;
;; Rest arguments
;;

(define-lambda-keyword-binder &rest (list args)
  (list (acons (first list) args nil) args))

(define-lambda-keyword-arguments &rest (list)
  (list (first list)))

(define-lambda-keyword-checker &rest (list)
  (unless (and (= (length list) 1) (symbolp (first list)))
    (error "Wrong &rest lambda list ~a." list)))

;;
;; Key arguments
;;

(define-lambda-keyword-binder &key (list arguments nil nil allow-other-keys)
  (labels ((argument-key (arg)
	     (let ((arg (if (listp arg) (first arg) arg)))
	       (if (listp arg) (first arg)
		   (make-keyword (symbol-name arg)))))
	   (argument-name (arg)
	     (let ((name-form (if (listp arg) (first arg) arg)))
	       (if (listp name-form) (second name-form) name-form)))
	   (argument-default (arg)
	     (if (listp arg) (second arg)))
	   (argument-set-arg (arg)
	     (if (listp arg) (third arg)))
	   (make-argument (spec)
	     (list (argument-key spec) (argument-name spec) (argument-default spec) nil (argument-set-arg spec))))
    (let ((keys (mapcar #'(lambda (arg) (make-argument arg)) list)))
      (flet ((bind-arg (key value)
	       (if (eq key :allow-other-keys)
		   (when value (setf allow-other-keys t))
		   (let ((arg (assoc key keys)))
		     (unless (or arg allow-other-keys)
		       (error "Wrong key argument ~a. Possible values are ~a." key (mapcar #'first keys)))
		     (when (and arg (not (fourth arg)))
		       (setf (third arg) value)
		       (setf (fourth arg) t))))))
	(labels ((do-bind (args)
		   (if args (progn (unless (rest args) (error "Odd number of key arguments in ~a." arguments))
				   (bind-arg (first args) (second args))
				   (do-bind (rest (rest args)))))))
	  (do-bind arguments)
	  (list (mapcan #'(lambda (key) (cons (cons (second key) (third key))
					      (aif (fifth key) (list (cons it (fourth key))))))
			keys)
		nil))))))

(define-lambda-keyword-arguments &key (list)
  (mapcan (lambda (x) (if (listp x)
			  (cons (if (listp (first x))
				    (second (first x))
				    (first x))
				(aif (third x) (list it)))
			  (list x)))
	  list))

(define-lambda-keyword-checker &key (list)
  (labels ((check-name-form (form)
	     (if (listp form)
		 (and (every #'symbolp form)
		      (= (length form) 2))
		 (symbolp form))))
    (unless (every (lambda (x) 
		     (if (listp x)
			 (and (>= (length x) 1)
			      (check-name-form (first x))
			      (if (= (length x) 3)
				  (symbolp (third x))
				  (< (length x) 3)))
			 (symbolp x)))
		 list)
    (error "Wrong &key lambda list ~a." list))))

(define-lambda-keyword-arguments &allow-other-keys (list)
  (declare (ignore list))
  nil)

(define-lambda-keyword-checker &allow-other-keys (list)
  (unless (null list)
    (error "Wrong arguments ~a after &allow-other-keys." list)))

;;
;; Aux arguments
;;

(define-lambda-keyword-binder &aux (list args)
  (list (mapcar #'(lambda (arg) (if (listp arg) (cons (first arg) (second arg)) (cons arg nil))) list)
	args))

(define-lambda-keyword-arguments &aux (list)
  (mapcar (lambda (arg) (if (listp arg) (first arg) arg)) list))

(define-lambda-keyword-checker &aux (list)
  (unless (every (lambda (x)
		   (if (listp x)
		       (and (symbolp (first x)) (< (length x) 3))
		       (symbolp x)))
		 list)
    (error "Wrong &aux lambda list ~a." list)))

;;
;; Lambda list functions
;;

(defun bind-lambda-list (list args &key macro-p (allowed-keywords nil allowed-keywords-p))
  (declare (ignore allowed-keywords allowed-keywords-p))
  (check-lambda-list list)
  (let ((keywords (find-lambda-list-keywords list)))
    (labels ((bind-keyword (keyword list args)
	       (case keyword
		 (&key (bind-lambda-list-keyword '&key list args :generic-p nil :macro-p nil 
						 :allow-other-keys (assoc '&allow-other-keys keywords)))
		 (&allow-other-keys (list nil args))
		 (otherwise (bind-lambda-list-keyword keyword list args :generic-p nil :macro-p macro-p))))
	     (bind-keywords (keywords args)
	       (if keywords
		   (dbind (bindings rest) (bind-keyword (first (first keywords)) (rest (first keywords)) args)
		     (dbind (rest-bindings rest) (bind-keywords (rest keywords) rest)
		       (list (append bindings rest-bindings) rest)))
		   (list nil args))))
      (dbind (bindings rest) (bind-keywords keywords args)
	(when (and rest (not (assoc '&rest keywords)))
	  (error "Too much arguments for lambda list ~a in list ~a." list args))
	bindings))))

(defun check-lambda-list (list &key macro-p (allowed-keywords nil allowed-keywords-p))
  (let* ((keywords (find-lambda-list-keywords list))
	 (names (mapcar #'first keywords)))
    (labels ((check-order (names keys)
	       (cond ((and (null keys) (not (null names))) (error "Wrong lambda list keywords ~a." names))
		     ((null keys) t)
		     (t (aif (position (first keys) names)
			     (if (not (= it 0)) (error "Wrong lambda list ~a." list)
				 (check-order (rest names) keys))
			     (check-order names (rest keys)))))))
      (check-order names '(nil &optional &rest &key &allow-other-keys &aux)))
    (labels ((check-once (names)
	       (cond
		 ((null names) t)
		 ((find (first names) (rest names)) (error "Wrong lambda list ~a." list))
		 (t (check-once (rest names))))))
      (check-once names))
    (when (and (assoc '&allow-other-keys keywords) (not (assoc '&key keywords)))
      (error "Wrong lambda list ~a." list))
    (mapc (lambda (x) (check-lambda-list-keyword (first x) (rest x) :macro-p macro-p)) keywords)
    (let ((names (mapcan (lambda (x) (lambda-list-keyword-arguments (first x) (rest x) :macro-p macro-p))
			 keywords)))
      (unless (every (lambda (x) (and (symbolp x) (not (constantp x)))) names)
	(error "Wrong lambda list ~a." list))
      (labels ((find-duplicates (names)
		 (cond ((null names) nil)
		       ((member (first names) (rest names)) 
			(cons (first names) (find-duplicates (remove (first names) (rest names)))))
		       (t (find-duplicates (rest names))))))
	(awhen (find-duplicates names)
	  (error "Duplicated symbols ~a in lambda list ~a." it list))
	t))))

  

    
      
    
