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

(deftest checkinng-keys-order
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
    (check-subsets '(a b &optional c (&rest a) &key d e &allow-other-keys &aux f))
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

;checking keys order
;checking normal argument specs
;checking optional argument specs
;checking keyword argument specs
;checking rest argument spec
;checking aux argument spec

;checking that key specified only once
;checking argument names (additional arguments too)

;checking in bind

;;macro lambda lists (dots too)

;binding
;checking

;;allowed keywords
