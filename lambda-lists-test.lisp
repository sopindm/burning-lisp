(in-package #:burning-lisp-test)

(in-case lambda-lists-test)

(deftest lambda-list-keyword-p-test
  (?t (lambda-list-keyword-p '&a-keyword))
  (?null (lambda-list-keyword-p '&))
  (?null (lambda-list-keyword-p 'a-symbol)))

(deftest finding-lambda-list-keyword
  (macrolet ((?fk (keyword list result)
	       `(?equal (find-lambda-list-keyword ',keyword ',list) ',result)))
    (?fk &a-keyword (a b c &a-keyword d e f) (d e f))
    (?fk &some-keyword (a b c &some-keyword d e f &other-keyword g h i) (d e f))
    (?fk &some-keyword (a b c &first-keyword d e f &some-keyword g h i &lask-keyword) (g h i))
    (?fk &some-keyword (a b c &some-keyword) ())
    (?fk nil (a b c &key d e f &other-key g h i) (a b c))))

(deftest finding-lambda-list-keywords
  (macrolet ((?fk (list rest &rest keyworded)
	       (labels ((to-assoc (list)
			  (if (null list) nil
			      (acons (first list) (second list) (to-assoc (rest (rest list)))))))
		 `(?equal (find-lambda-list-keywords ',list)
			  ',(to-assoc (append (list nil rest) keyworded))))))
    (?fk (a b c &key1 d e f &key2 g h i &key3 &key4 k l m &key5)
	 (a b c)
	 &key1 (d e f)
	 &key2 (g h i)
	 &key3 ()
	 &key4 (k l m)
	 &key5 ())))

(deftest remove-lambda-list-keyword
  (macrolet ((?rk (keyword list result)
	       `(?equal (remove-lambda-list-keyword ',keyword ',list) 
			',result)))
    (?rk &a (&a) ())
    (?rk &a (a b c) (a b c))
    (?rk &a (a b c &a d e f) (a b c))
    (?rk &a (&a a b c &b d e f) (&b d e f))))

(deftest remove-lambda-list-keywords-test
  (macrolet ((?rk (list result)
	       `(?equal (remove-lambda-list-keywords ',list) ',result)))
    (?rk (a b c) (a b c))
    (?rk (a b c &a d e f &b g h i) (a b c))
    (?rk () ())
    (?rk (&a a b c) ())))

(defmacro ?bind= (lambda-list arguments &body bindings)
  `(let ((bound (bind-lambda-list ',lambda-list ',arguments)))
     (?equal bound ',(mapcar #'(lambda (binding) (cons (first binding) (second binding))) bindings))))

(deftest binding-normal-lambda-list
  (?bind= (a b) (1 2) 
    (a 1) (b 2)))

(defmacro ?bind-error (list args format-control &rest format-args)
  `(let ((list ',list)
	 (args ',args))
     (declare (ignorable list args))
     (?error (bind-lambda-list ',list ',args)
	     (format nil ,format-control ,@format-args))))

(deftest binding-normal-lambda-list-errors
  (?bind-error (a b c) (1 2) 
	       "Not enought arguments for lambda list ~a in list ~a." list args)
  (?bind-error (a b) (1 2 3)
	       "Too much arguments for lambda list ~a in list ~a." list args))

(deftest binding-optional-parameters
  (?bind= (a b &optional c) (1 2 3)
    (a 1) (b 2) (c 3))
  (?bind= (&optional c d) (1)
    (c 1) (d nil))
  (?bind= (a &optional b) (1)
    (a 1) (b nil))
  (?bind= (a &optional (b nil b-set-p) (c :c-default c-set-p)) (1 2)
    (a 1) (b 2) (b-set-p t) (c :c-default) (c-set-p nil))
  (?bind= (a &optional (b :default b-set-p)) (1 nil)
    (a 1) (b nil) (b-set-p t)))

(deftest binding-rest-parameter
  (?bind= (a &rest b) (1)
    (a 1) (b nil))
  (?bind= (a &rest b) (1 2 3)
    (a 1) (b (2 3))))

(deftest binding-aux-parameters
  (?bind= (&aux a b) ()
    (a nil) (b nil))
  (?bind= (a &rest b &aux c d) (1 2 3 4)
    (a 1)
    (b (2 3 4))
    (c nil)
    (d nil))
  (?bind= (&aux (b 1) (c 2)) ()
    (b 1) (c 2)))
	 
(deftest binding-key-parameters
  (?bind= (&key a b) ()
    (a nil) (b nil))
  (?bind= (&key a b) (:a 1 :b 2)
    (a 1) (b 2))
  (?bind= (&key a) (:a 0 :a 1)
    (a 0))
  (?bind= (&key (a 0) (b 2)) (:b 1)
    (a 0) (b 1))
  (?bind= (&key (a 0 a-p) (b 3 b-p)) (:b 2)
    (a 0) (a-p nil) (b 2) (b-p t)))

(deftest binding-key-parameters-errors
  (?bind-error (&key a b) (:a 1 :b 2 3)
	       "Odd number of key arguments in ~a." args)
  (?bind-error (&key a b) (:a 1 :c 3)
	       "Wrong key argument ~a. Possible values are ~a." :c '(:a :b)))

(deftest key-parameters-with-other-names
  (?bind= (&key ((a b)) ((:c d))) (a 1 :c 2)
    (b 1) (d 2)))

(deftest rest-and-key-arguments
  (?bind= (&rest a &key b c) (:b 1 :c 2)
    (a (:b 1 :c 2)) (b 1) (c 2)))

(deftest allowing-other-keys
  (?bind= (&key a &allow-other-keys) (:a 1 :b 2 :c 3)
    (a 1))
  (?bind= (&key a) (:a 1 :allow-other-keys t :b 2)
    (a 1))
  (?bind-error (&key a) (:a 1 :allow-other-keys nil :b 2)
	       "Wrong key argument ~a. Possible values are ~a." :b '(:a))
  (?bind= (&key &allow-other-keys) (:allow-other-keys nil :a 0)))

(deftest checking-key-names
  (?error (check-lambda-list '(&wrong-keyword))
	  (format nil "Wrong lambda list keywords ~a." '(&wrong-keyword))))

(deftest checking-keys-order
  (labels ((to-list (arg)
	     (if (listp arg) arg (list arg)))
	   (make-subsets (list prefix)
	     (if list
		 (append (make-subsets (rest list) (append prefix (to-list (first list))))
			 (make-subsets (rest list) prefix))
		 (list prefix)))
	   (check-subsets (list)
	     (mapc #'check-lambda-list (make-subsets list nil)))
	   (check-error (spec)
	     (?error (check-lambda-list spec)
		     (format nil "Wrong lambda list ~a." spec))))
    (check-subsets '(a b &optional c (&rest g) (&key d e &allow-other-keys) (&aux f)))
    (check-error '(&rest a &optional))
    (check-error '(&key &optional))
    (check-error '(&allow-other-keys &optional))
    (check-error '(&aux &optional))
    (check-error '(&key &rest a))
    (check-error '(&allow-other-keys &rest a))
    (check-error '(&aux &rest a))
    (check-error '(&allow-other-keys &key))
    (check-error '(&aux &key))
    (check-error '(&aux &allow-other-keys))))

(deftest checking-normal-argument-specs
  (check-lambda-list '(a b c))
  (flet ((check-error (list &optional (error "Wrong lambda list ~a.") (arg list))
	   (?error (check-lambda-list list) (format nil error arg))))
    (check-error '(1 2 3))
    (check-error '(:a b c))
    (check-error '((a 1) b c) "Wrong ordinary lambda list ~a.")
    (check-error '((a 2) (b 3) &optional (c 4)) "Wrong ordinary lambda list ~a." '((a 2) (b 3)))))

(deftest checking-optional-argument-specs
  (check-lambda-list '(a b &optional c d))
  (check-lambda-list '(&optional (c :default) d))
  (check-lambda-list '(&optional (c :default) (d nil d-set-p) e))
  (check-lambda-list '(&optional (c)))
  (flet ((check-error (list)
	   (?error (check-lambda-list (cons '&optional list))
		   (format nil "Wrong &optional lambda list ~a." list))))
    (check-error '((a b c d)))
    (check-error '(((a))))))

(deftest checking-rest-argument-specs
  (check-lambda-list '(&rest a))
  (flet ((check-error (list)
	   (?error (check-lambda-list (cons '&rest list))
		   (format nil "Wrong &rest lambda list ~a." list))))
    (check-error ())
    (check-error '(a b))
    (check-error '((a)))))

(deftest checking-key-argument-specs
  (check-lambda-list '(&key))
  (check-lambda-list '(&key a b c))
  (check-lambda-list '(&key a (b nil) (c nil c-p)))
  (check-lambda-list '(&key ((:a a)) b))
  (check-lambda-list '(&key ((:a a) :default a-p) b (c nil c-p)))
  (flet ((check-error (list)
	   (?error (check-lambda-list (cons '&key list))
		   (format nil "Wrong &key lambda list ~a." list))))
    (check-error '(()))
    (check-error '(((a))))
    (check-error '(((a b c))))
    (check-error '(((a b) c d e)))
    (check-error '((a b c d)))
    (check-error '((((a) b))))
    (check-error '(((a (b)))))
    (check-error '((a b (c d))))))
    
(deftest check-no-arguments-in-allow-other-keys
  (?error (check-lambda-list '(&allow-other-keys))
	  (format nil "Wrong lambda list ~a." '(&allow-other-keys)))
  (?error (check-lambda-list '(&key &allow-other-keys a))
	  (format nil "Wrong arguments ~a after &allow-other-keys." '(a))))

(deftest checking-aux-arguments-spec
  (check-lambda-list '(&aux a b))
  (check-lambda-list '(&aux (a b) (c d)))
  (flet ((check-error (list)
	   (?error (check-lambda-list (cons '&aux list))
		   (format nil "Wrong &aux lambda list ~a." list))))
    (check-error '((a b c) d))
    (check-error '(((a) b) c))))

(deftest checking-that-key-specified-only-once
  (let ((list '(a &optional b &optional c)))
    (?error (check-lambda-list list)
	    (format nil "Wrong lambda list ~a." list))))

(deftest checking-argument-names
  (macrolet ((check-error (list args)
	       `(?error (check-lambda-list ',list)
			(format nil "Duplicated symbols ~a in lambda list ~a." ',args ',list))))
    (check-error (a b c d a) (a))
    (check-error (a b c &optional d e b) (b))
    (check-error (a b &optional (a nil b)) (a b))
    (check-error (a b c &key a b c) (a b c))
    (check-error (a b &key (a nil b)) (a b))
    (check-error (a &rest a) (a))
    (check-error (a b &aux b a) (a b))
    (check-error (a b &optional (c nil a) &rest a &key b a &aux a b) (a b))))


(deftest checking-lambda-list-in-bind
  (?error (bind-lambda-list '(&allow-other-keys) '())
	  (format nil "Wrong lambda list ~a." '(&allow-other-keys))))

;;macro lambda lists (dots too)

;binding
;checking

;;allowed keywords
;;generic lambda lists
