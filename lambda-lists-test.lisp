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
  (macrolet ((?fk (list &rest keyworded)
	       (labels ((to-assoc (list)
			  (if (null list) nil
			      (acons (first list) (second list) (to-assoc (rest (rest list)))))))
		 `(?equal (find-lambda-list-keywords ',list)
			  ',(to-assoc keyworded)))))
    (?fk (a b c &key1 d e f &key2 g h i &key3 &key4 k l m &key5)
	 nil (a b c)
	 &key1 (d e f)
	 &key2 (g h i)
	 &key3 ()
	 &key4 (k l m)
	 &key5 ())
    (?fk (&whole a b c)
	 &whole (a)
	 nil (b c))
    (?fk (a b . c)
	 nil (a b)
	 burning-lisp::\. (c))))
	 

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
  (let ((macro-p (eq (first bindings) :macro))
	(generic-p (eq (first bindings) :generic))
	(bindings (if (keywordp (first bindings)) (rest bindings) bindings)))
    `(let ((bound (bind-lambda-list ',lambda-list ',arguments
				    ,@(if macro-p '(:macro-p t))
				    ,@(if generic-p '(:generic-p t)))))
				    
       (?equal bound ',(mapcar #'(lambda (binding) (cons (first binding) (second binding))) bindings)))))

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
	       "Not enought arguments for lambda list ~a in ~a." list args)
  (?bind-error (a b) (1 2 3)
	       "Too much arguments for lambda list ~a in ~a." list args))

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

(deftest binding-to-list-with-dots
  (?bind= (a b &rest c) (1 2 . 3)
	  (a 1) (b 2) (c 3)))

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

(defmacro ?wrong-lambda-list-keywords (list &optional (keywords list))
  `(?error (check-lambda-list ',list)
	   (format nil "Wrong lambda list keywords ~a." ',keywords)))

(deftest checking-key-names
  (?wrong-lambda-list-keywords (&wrong-keyword)))

(defmacro ?wrong-lambda-list (list 
			      &optional (error-string "Wrong lambda list ~a.") (error-arguments `(',list)))
  `(?error (check-lambda-list ',list)
	   (format nil ,error-string ,@error-arguments)))

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
    (?wrong-lambda-list (&rest a &optional))
    (?wrong-lambda-list (&key &optional))
    (?wrong-lambda-list (&allow-other-keys &optional))
    (?wrong-lambda-list (&aux &optional))
    (?wrong-lambda-list (&key &rest a))
    (?wrong-lambda-list (&allow-other-keys &rest a))
    (?wrong-lambda-list (&aux &rest a))
    (?wrong-lambda-list (&allow-other-keys &key))
    (?wrong-lambda-list (&aux &key))
    (?wrong-lambda-list (&aux &allow-other-keys))))

(deftest checking-normal-argument-specs
  (check-lambda-list '(a b c))
  (?wrong-lambda-list (:a b c))
  (?wrong-lambda-list (1 2 3) "Wrong ordinary lambda list ~a.")
  (?wrong-lambda-list ((a 1) b c) "Wrong ordinary lambda list ~a.")
  (?wrong-lambda-list ((a 2) (b 3) &optional (c 4)) "Wrong ordinary lambda list ~a." ('((a 2) (b 3)))))

(deftest checking-optional-argument-specs
  (check-lambda-list '(a b &optional c d))
  (check-lambda-list '(&optional (c :default) d))
  (check-lambda-list '(&optional (c :default) (d nil d-set-p) e))
  (check-lambda-list '(&optional (c)))
  (macrolet ((check-error (list)
	       `(?wrong-lambda-list (&optional ,@list) "Wrong &optional lambda list ~a." (',list))))
    (check-error ((a b c d)))
    (check-error (((a))))
    (check-error ((a . b)))))

(deftest checking-rest-argument-specs
  (check-lambda-list '(&rest a))
  (macrolet ((check-error (list)
	       `(?wrong-lambda-list (&rest ,@list) "Wrong &rest lambda list ~a." (',list))))
    (check-error ())
    (check-error (a b))
    (check-error ((a)))))

(deftest checking-key-argument-specs
  (check-lambda-list '(&key))
  (check-lambda-list '(&key a b c))
  (check-lambda-list '(&key a (b nil) (c nil c-p)))
  (check-lambda-list '(&key ((:a a)) b))
  (check-lambda-list '(&key ((:a a) :default a-p) b (c nil c-p)))
  (macrolet ((check-error (list)
	       `(?wrong-lambda-list (&key ,@list) "Wrong &key lambda list ~a." (',list))))
    (check-error (()))
    (check-error (((a))))
    (check-error (((a b c))))
    (check-error (((a b) c d e)))
    (check-error ((a b c d)))
    (check-error ((((a) b))))
    (check-error (((a (b)))))
    (check-error ((a b (c d))))
    (check-error ((a b . c)))
    (check-error (((a . b))))))
    
(deftest check-no-arguments-in-allow-other-keys
  (?wrong-lambda-list (&allow-other-keys))
  (?wrong-lambda-list (&key &allow-other-keys a) "Wrong arguments ~a after &allow-other-keys." ('(a))))

(deftest checking-aux-arguments-spec
  (check-lambda-list '(&aux a b))
  (check-lambda-list '(&aux (a b) (c d)))
  (macrolet ((check-error (list)
	       `(?wrong-lambda-list (&aux ,@list) "Wrong &aux lambda list ~a." (',list))))
    (check-error ((a b c) d))
    (check-error (((a) b) c))
    (check-error ((a . b)))))

(deftest checking-that-key-specified-only-once
  (?wrong-lambda-list (a &optional b &optional c)))

(deftest checking-argument-names
  (macrolet ((check-error (list args)
	       `(?wrong-lambda-list ,list "Duplicated symbols ~a in lambda list ~a." (',args ',list))))
    (check-error (a b c d a) (a))
    (check-error (a b c &optional d e b) (b))
    (check-error (a b &optional (a nil b)) (a b))
    (check-error (a b c &key a b c) (a b c))
    (check-error (a b &key (a nil b)) (a b))
    (check-error (a &rest a) (a))
    (check-error (a b &aux b a) (a b))
    (check-error (a b &optional (c nil a) &rest a &key b a &aux a b) (a b))))

(deftest checking-lambda-list-in-bind
  (?bind-error (&allow-other-keys) () "Wrong lambda list ~a." '(&allow-other-keys)))

(deftest dotted-lambda-lists-error
  (?wrong-lambda-list-keywords (a . b) (\.)))

;;
;;macro lambda lists
;;

(deftest binding-macro-lambda-lists
  (?bind= (a (b c) (d (e (f)))) (1 (2 3) (4 (5 (6)))) :macro
	  (a 1) (b 2) (c 3) (d 4) (e 5) (f 6))
  (?bind= (a (b &rest c) (d &key e f) (&optional g h &aux (i a))) (1 (2 3 4) (5 :e 6) (7)) :macro
	  (a 1) (b 2) (c (3 4)) (d 5) (e 6) (f nil) (g 7) (h nil) (i a)))

(deftest binding-simple-macro-lists-with-dots
  (?bind= (a b . c) (1 2 3 4 5) :macro
	  (a 1) (b 2) (c (3 4 5)))
  (?bind= (a &optional b . c) (1) :macro
	  (a 1) (b nil) (c nil))
  (?bind= (a &optional b . c) (1 2) :macro
	  (a 1) (b 2) (c nil))
  (?bind= (a &optional b . c) (1 2 3) :macro
	  (a 1) (b 2) (c (3))))

(deftest binding-macro-lists-with-complex-rest
  (?bind= (a b &rest (c d e &key f)) (1 2 3 4 5 :f 6) :macro
    (a 1) (b 2) (c 3) (d 4) (e 5) (f 6)))

(defmacro ?wrong-macro-lambda-list (list &optional (wrong-list list))
  `(?error (check-lambda-list ',list :macro-p t)
	   (format nil "Wrong lambda list ~a." ',wrong-list)))

(deftest checking-macro-lambda-lists
  (?wrong-macro-lambda-list ((&key a &rest b)) (&key a &rest b))
  (?wrong-macro-lambda-list (&rest (&key a &rest b)) (&key a &rest b)))

(deftest binding-whole-argumnet
  (?bind= (&whole a (&whole b c d) e f &rest g) ((1 2) 3 4 5) :macro
	  (a ((1 2) 3 4 5)) (b (1 2)) (c 1) (d 2) (e 3) (f 4) (g (5))))

(deftest binding-whole-argument-with-destructuring
  (?bind= (&whole (a &rest b) c &rest d) (1 2 3 4 5) :macro
	  (a 1) (b (2 3 4 5)) (c 1) (d (2 3 4 5))))

(deftest checking-whole-order
  (?wrong-macro-lambda-list (a &whole b)))

(deftest wrong-ordinary-lambda-list-keywords
  (?wrong-lambda-list-keywords (&whole a b) (&whole))
  (?wrong-lambda-list-keywords (a &body b) (&body)))

(deftest boby-binding
  (?bind= (a &body b) (1 2 3) :macro
	  (a 1) (b (2 3))))

(deftest body-order-checking
  (?wrong-macro-lambda-list (&body a &optional b))
  (?wrong-macro-lambda-list (&key a &body b)))

(deftest body-and-rest-arguments-error
  (?wrong-macro-lambda-list (&rest a &body b))
  (?wrong-macro-lambda-list (&body b &rest a)))

(deftest allowed-keywords-checking
  (check-lambda-list '(a b) :allowed-keywords nil)
  (macrolet ((check-error (list allowed &rest wrong-keywords)
	       `(?error (check-lambda-list ',list :allowed-keywords ',allowed)
			(format nil "Keywords ~a aren't allowed." ',wrong-keywords))))
    (check-error (a b &optional c &rest d) (&optional) &rest)
    (check-error (a b &key c) (&optional &rest &body) &key)))
  
(deftest allowed-keywords-in-bind
  (?error (bind-lambda-list '(&optional b c) '(1 2) :allowed-keywords '(&rest &key))
	  (format nil "Keywords ~a aren't allowed." '(&optional))))

(deftest denied-keywords-checking
  (?error (check-lambda-list '(a &optional b c) :denied-keywords '(&optional))
	  (format nil "Keywords ~a are denied." '(&optional))))

(deftest denied-keywords-in-bind
  (?error (bind-lambda-list '(a &optional b) '(1) :denied-keywords '(&optional))
	  (format nil "Keywords ~a are denied." '(&optional))))

(deftest arguments-in-generic-lambda-lists
  (?bind= ((a b) c) (1 2) :generic
	  (a 1) (c 2))
  (?bind= ((a (eql (b d e))) c) (1 2) :generic
	  (a 1) (c 2)))

(deftest generic-lambda-lists-checking
  (macrolet ((check-error (list)
	       `(?error (check-lambda-list ',list :generic-p t)
			(format nil "Wrong ordinary lambda list ~a." ',list))))
    (check-error (a (b c d)))
    (check-error (((a a) b) c))
    (check-error (a (b (c d)) e))
    (check-error (a (b (eql d e)) f))))

(deftest generic-and-macro-error
  (macrolet ((check-error ((call &rest args))
	       `(?error (,call ,@args :generic-p t :macro-p t)
			(format nil "Lambda list cannot be macro and generic."))))
    (check-error (bind-lambda-list () ()))
    (check-error (check-lambda-list ()))
    (check-error (lambda-list-arguments ()))))

;lambda-list=  
