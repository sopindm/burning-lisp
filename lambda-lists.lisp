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
  (cond ((null list) nil)
	((not (listp list)) (acons '\. (list list) nil))
	((eq (first list) '&whole) (acons '&whole (list (second list))
					  (find-lambda-list-keywords (rest (rest list)))))
	(t (let ((rest (find-lambda-list-keywords (rest list))))
	     (if (lambda-list-keyword-p (first list))
		 (acons (first list) (rest (assoc nil rest)) (remove nil rest :key #'first))
		 (acons nil (cons (first list) (rest (assoc nil rest))) 
			(remove nil rest :key #'first)))))))

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
(defgeneric check-lambda-list-keyword (keyword lambda-list &key macro-p generic-p allowed-keywords denied-keywords))
(defgeneric lambda-list-keyword= (keyword lambda-list1 lambda-list2 &key macro-p generic-p))

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

(defmacro define-lambda-keyword-checker (keyword (list-arg &optional macro-p-arg generic-p-arg 
							   allowed-keywords-arg denied-keywords-arg)
					 &body body)
  (let ((macro-p-arg-name (or macro-p-arg (gensym)))
	(generic-p-arg-name (or generic-p-arg (gensym)))
	(allowed-keywords-arg-name (or allowed-keywords-arg (gensym)))
	(denied-keywords-arg-name (or denied-keywords-arg (gensym)))
	(keyword-arg (gensym)))
    `(defmethod check-lambda-list-keyword ((,keyword-arg (eql ',keyword)) ,list-arg
					   &key 
					   ((:macro-p ,macro-p-arg-name)) 
					   ((:generic-p ,generic-p-arg-name))
					   ((:allowed-keywords ,allowed-keywords-arg-name))
					   ((:denied-keywords ,denied-keywords-arg-name)))
       ,@(unless (and macro-p-arg generic-p-arg allowed-keywords-arg denied-keywords-arg)
		 `((declare (ignore ,@(if macro-p-arg nil (list macro-p-arg-name))
				    ,@(if generic-p-arg nil (list generic-p-arg-name))
				    ,@(if allowed-keywords-arg nil (list allowed-keywords-arg-name))
				    ,@(if denied-keywords-arg nil (list denied-keywords-arg-name))))))
       ,@body)))

(defmacro define-lambda-keyword= (keyword (list1-arg list2-arg &optional macro-p-arg generic-p-arg) &body body)
  (let ((macro-p-arg-name (or macro-p-arg (gensym)))
	(generic-p-arg-name (or generic-p-arg (gensym)))
	(keyword-arg (gensym)))
    `(defmethod lambda-list-keyword= ((,keyword-arg (eql ',keyword)) ,list1-arg ,list2-arg
					   &key ((:macro-p ,macro-p-arg-name)) ((:generic-p ,generic-p-arg-name)))
       ,@(unless (and macro-p-arg generic-p-arg)
		 `((declare (ignore ,@(if macro-p-arg nil (list macro-p-arg-name))
				    ,@(if generic-p-arg nil (list generic-p-arg-name))))))
       ,@body)))

;;
;; Normal arguments
;;

(define-lambda-keyword-binder nil (list args macro-p generic-p)
  (labels ((no-arguments-error ()
	     (error "Not enought arguments for lambda list ~a in ~a." list args))
	   (bind-one (spec arg)
	     (cond ((and macro-p (listp spec)) (bind-lambda-list spec arg :macro-p t))
		   ((and generic-p (listp spec)) (list (cons (first spec) arg)))
		   (t (list (cons spec arg)))))
	   (do-bind (list args)
	     (cond ((null list) (list nil args))
		   ((null args) (no-arguments-error))
		   (t (dbind (bindings rest) (do-bind (rest list) (rest args))
			(list (append (bind-one (first list) (first args)) bindings)
			      rest))))))
    (do-bind list args)))

(define-lambda-keyword-checker nil (list macro-p generic-p allowed-keywords denied-keywords)
  (unless (listp list)
    (error "Wrong ordinary lambda list ~a." list))
  (unless (every (lambda (x)
		   (cond ((and macro-p (listp x)) (check-lambda-list x :macro-p t 
								     :allowed-keywords allowed-keywords
								     :denied-keywords denied-keywords))
			 ((and generic-p (listp x))
			  (and (= (length x) 2) (symbolp (first x))
			       (or (symbolp (second x)) (and (listp (second x))
							     (= (length (second x)) 2)
							     (eq (first (second x)) 'eql)))))
			 (t (symbolp x))))
		 list)
    (error "Wrong ordinary lambda list ~a." list)))

(define-lambda-keyword-arguments nil (list macro-p generic-p) 
  (mapcan (lambda (x)
	    (cond ((and macro-p (listp x)) (lambda-list-arguments x :macro-p t))
		  ((and generic-p (listp x)) (list (first x)))
		  (t (list x))))
	  list))

(define-lambda-keyword= nil (list1 list2 macro-p)
  (and (= (length list1) (length list2))
       (or (not macro-p)
	   (every (lambda (l1 l2) 
		    (or (and (symbolp l1) (symbolp l2))
			(and (listp l1) (listp l2) (lambda-list= l1 l2 :macro-p t)))) list1 list2))))

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
  (unless (every (lambda (x) (if (proper-list-p x)
				 (and (>= (length x) 1) (symbolp (first x))
				      (if (= (length x) 3) (symbolp (third x)) (< (length x) 3)))
				 (symbolp x)))
		 list)
    (error "Wrong &optional lambda list ~a." list)))

(define-lambda-keyword= &optional (list1 list2)
  (= (length list1) (length list2)))

;;
;; Whole argument
;; 

(define-lambda-keyword-binder &whole (list args macro-p)
  (list (if (and macro-p (listp (first list)))
	    (bind-lambda-list (first list) args)
	    (list (cons (first list) args)))
	args))

(define-lambda-keyword-checker &whole (list macro-p nil allowed-keywords denied-keywords)
  (when (and macro-p (listp (first list)))
    (check-lambda-list (first list) :macro-p t :allowed-keywords allowed-keywords :denied-keywords denied-keywords)))

(define-lambda-keyword-arguments &whole (list macro-p)
  (if (and macro-p (listp (first list)))
      (lambda-list-arguments (first list) :macro-p t)
      (first list)))

(define-lambda-keyword= &whole (l1 l2 macro-p)
  (or (not macro-p)
      (and (symbolp (first l1)) (symbolp (first l2)))
      (lambda-list= (first l1) (first l2) :macro-p t)))

;;
;; Rest, body and dot arguments
;;

(define-lambda-keyword-binder \. (list args macro-p)
  (bind-lambda-list-keyword '&rest list args :macro-p macro-p))

(define-lambda-keyword-checker \. (list macro-p nil allowed-keywords denied-keywords)
  (check-lambda-list-keyword '&rest list :macro-p macro-p 
			     :allowed-keywords allowed-keywords
			     :denied-keywords denied-keywords))

(define-lambda-keyword-arguments \. (list macro-p)
  (lambda-list-keyword-arguments '&rest list :macro-p macro-p))

(define-lambda-keyword= \. (list1 list2 macro-p)
  (lambda-list-keyword= '&rest list1 list2 :macro-p macro-p))

(define-lambda-keyword-binder &body (list args macro-p)
  (bind-lambda-list-keyword '&rest list args :macro-p macro-p))

(define-lambda-keyword-checker &body (list macro-p nil allowed-keywords denied-keywords)
  (check-lambda-list-keyword '&rest list :macro-p macro-p
			     :allowed-keywords allowed-keywords
			     :denied-keywords denied-keywords))

(define-lambda-keyword-arguments &body (list macro-p)
  (lambda-list-keyword-arguments '&rest list :macro-p macro-p))

(define-lambda-keyword= &body (l1 l2 macro-p)
  (lambda-list-keyword= '&rest l1 l2 :macro-p macro-p))

(define-lambda-keyword-binder &rest (list args macro-p)
  (if (and macro-p (not (symbolp (first list))))
      (list (bind-lambda-list (first list) args :macro-p t) args)
      (list (acons (first list) args nil) args)))

(define-lambda-keyword-arguments &rest (list macro-p)
  (if (and macro-p (not (symbolp (first list))))
      (lambda-list-arguments (first list) :macro-p t)
      (list (first list))))

(define-lambda-keyword-checker &rest (list macro-p nil allowed-keywords denied-keywords)
  (flet ((do-error () (error "Wrong &rest lambda list ~a." list)))
    (unless (= (length list) 1) (do-error))
    (unless (or macro-p (symbolp (first list))) (do-error))
    (if (and macro-p (not (symbolp (first list))))
	(check-lambda-list (first list) :macro-p t
			   :allowed-keywords allowed-keywords 
			   :denied-keywords denied-keywords)
	(symbolp (first list)))))

(define-lambda-keyword= &rest (l1 l2 macro-p)
  (or (not macro-p) 
      (and (symbolp (first l1)) (symbolp (first l2)))
      (and (listp (first l1)) (listp (first l2)) (lambda-list= (first l1) (first l2) :macro-p t))))

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
	     (if (proper-list-p form)
		 (and (every #'symbolp form)
		      (= (length form) 2))
		 (symbolp form))))
    (unless (every (lambda (x) 
		     (if (proper-list-p x)
			 (and (>= (length x) 1)
			      (check-name-form (first x))
			      (if (= (length x) 3)
				  (symbolp (third x))
				  (< (length x) 3)))
			 (symbolp x)))
		 list)
    (error "Wrong &key lambda list ~a." list))))

(define-lambda-keyword= &key (list1 list2 nil generic-p)
  (labels ((keywords= (list1 list2)
	     (cond ((and (null list1) (null list2)) t)
		   ((null list1) generic-p)
		   ((null list2) generic-p)
		   ((find (first list1) list2) (keywords= (rest list1) (remove (first list1) list2)))
		   ((find (first list2) list1) (keywords= (remove (first list2) list1) (rest list2)))))
	   (to-keyword (spec)
	     (if (listp spec)
		 (if (listp (first spec))
		     (first (first spec))
		     (make-keyword (first spec)))
		 (make-keyword spec))))
    (keywords= (mapcar #'to-keyword list1) (mapcar #'to-keyword list2))))
		   

(define-lambda-keyword-arguments &allow-other-keys (list)
  (declare (ignore list))
  nil)

(define-lambda-keyword-checker &allow-other-keys (list)
  (unless (null list)
    (error "Wrong arguments ~a after &allow-other-keys." list)))

(define-lambda-keyword= &allow-other-keys (list1 list2)
  (declare (ignore list1 list2))
  t)

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
		   (if (proper-list-p x)
		       (and (symbolp (first x)) (< (length x) 3))
		       (symbolp x)))
		 list)
    (error "Wrong &aux lambda list ~a." list)))

(define-lambda-keyword= &aux (list1 list2)
  (= (length list1) (length list2)))

;;
;; Lambda list functions
;;

(defun bind-lambda-list (list args &key macro-p generic-p (allowed-keywords nil allowed-keywords-p) denied-keywords)
  (check-lambda-list list 
		     :macro-p macro-p
		     :generic-p generic-p
		     :allowed-keywords (if allowed-keywords-p allowed-keywords lambda-list-keywords)
		     :denied-keywords denied-keywords)
  (let ((keywords (find-lambda-list-keywords list)))
    (labels ((bind-keyword (keyword list args)
	       (case keyword
		 (&key (bind-lambda-list-keyword '&key list args :generic-p nil :macro-p nil 
						 :allow-other-keys (assoc '&allow-other-keys keywords)))
		 (&allow-other-keys (list nil args))
		 (otherwise (bind-lambda-list-keyword keyword list args :generic-p generic-p :macro-p macro-p))))
	     (bind-keywords (keywords args)
	       (if keywords
		   (dbind (bindings rest) (bind-keyword (first (first keywords)) (rest (first keywords)) args)
		     (dbind (rest-bindings rest) (bind-keywords (rest keywords) rest)
		       (list (append bindings rest-bindings) rest)))
		   (list nil args))))
      (dbind (bindings rest) (bind-keywords keywords args)
	(when (and rest (not (or (assoc '&rest keywords)
				 (assoc '&body keywords)))
		   (proper-list-p list))
	  (error "Too much arguments for lambda list ~a in ~a." list args))
	bindings))))

(defun lambda-list-arguments (list &key macro-p generic-p)
  (when (and macro-p generic-p)
    (error "Lambda list cannot be macro and generic."))
  (let ((keywords (find-lambda-list-keywords list)))
    (mapcan (lambda (x) (lambda-list-keyword-arguments (first x) (rest x) :macro-p macro-p :generic-p generic-p)) 
	    keywords)))

(defun check-lambda-list (list &key macro-p generic-p (allowed-keywords lambda-list-keywords) denied-keywords)
  (unless (listp list)
    (error "Wrong lambda list ~a." list))
  (when (and macro-p generic-p)
    (error "Lambda list cannot be macro and generic."))
  (let* ((keywords (find-lambda-list-keywords list))
	 (names (mapcar #'first keywords)))
    (labels ((check-order (names keys)
	       (cond ((and (null keys) (not (null names))) (error "Wrong lambda list keywords ~a." names))
		     ((null names) t)
		     ((not (member (first names) keys)) 
		      (error "Wrong lambda list keywords ~a." (list (first names))))
		     (t (aif (position (first keys) names)
			     (if (not (= it 0)) (error "Wrong lambda list ~a." list)
				 (check-order (rest names) keys))
			     (check-order names (rest keys)))))))
      (check-order names (append (if macro-p '(&whole))
				 '(nil &optional)
				 (if macro-p '(\. &body)) '
				 (&rest &key &allow-other-keys &aux))))
    (labels ((check-once (names)
	       (cond
		 ((null names) t)
		 ((find (first names) (rest names)) (error "Wrong lambda list ~a." list))
		 ((and (eq (first names) '&body) (find '&rest names)) (error "Wrong lambda list ~a." list))
		 (t (check-once (rest names))))))
      (check-once names))
    (when (and (assoc '&allow-other-keys keywords) (not (assoc '&key keywords)))
      (error "Wrong lambda list ~a." list))
    (awhen (set-difference (remove-if (lambda (x) (member x '(nil \.))) names) 
			   allowed-keywords)
      (error "Keywords ~a aren't allowed." it))
    (awhen (and denied-keywords (intersection names denied-keywords))
      (error "Keywords ~a are denied." it))
    (mapc (lambda (x) (check-lambda-list-keyword (first x) (rest x)
						 :macro-p macro-p :generic-p generic-p
						 :allowed-keywords allowed-keywords
						 :denied-keywords denied-keywords))
	  keywords)
    (let ((names (lambda-list-arguments list :macro-p macro-p :generic-p generic-p)))
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

(defun lambda-list= (list1 list2 &key macro-p generic-p)
  (check-lambda-list list1 :macro-p macro-p :generic-p generic-p)
  (check-lambda-list list2 :macro-p macro-p :generic-p generic-p)
  (let ((keywords1 (find-lambda-list-keywords list1))
	(keywords2 (find-lambda-list-keywords list2)))
    (labels ((check-keyword (keyword args1 args2)
	       (lambda-list-keyword= keyword args1 args2 :macro-p macro-p :generic-p generic-p))
	     (check-keywords (keys1 keys2)
	       (acond ((and (null keys1) (null keys2)) t)
		      ((null keys1) nil)
		      ((null keys2) nil)
		      ((assoc (first (first keys1)) keys2)
		       (and (check-keyword (first (first keys1)) (rest (first keys1)) (rest it))
			    (check-keywords (rest keys1) (remove (first (first keys1)) keys2 :key #'first)))))))
      (check-keywords keywords1 keywords2))))
    
      
    
