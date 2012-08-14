(in-package #:burning-lisp-test)

(in-case #:keywords-test)

(deftest find-keyword-test
  (macrolet ((?fk (keyword value list)
	       `(?eq (find-keyword ,keyword ',list) ',value)))
    (?fk :a a (:a a))
    (?fk :b b (:a a b c :b b))
    (?fk :missed nil (:a a b c d :e e f))
    (?fk :last nil (:a b c d :last))))

(deftest find-keywords-test
  (macrolet ((?fk (list keywords-and-values remains)
	       (labels ((to-assoc (list)
			  (if list
			      (acons (first list) (second list) (to-assoc (rest (rest list))))
			      nil)))
		 `(multiple-value-bind (keywords rest) (find-keywords ',list)
		    (?equal keywords ',(to-assoc keywords-and-values))
		    (?equal rest ',remains)))))
    (?fk (a b c d e f) () (a b c d e f))
    (?fk (:a a b c d :e e) (:a a :e e) (b c d))
    (?fk (:a a) (:a a) ()))
  (?condition (find-keywords '(:a)) error))

(deftest remove-keyword-test
  (macrolet ((?rk (keyword list remains)
	       `(?equal (remove-keyword ,keyword ',list) ',remains)))
    (?rk :a (:a a) ())
    (?rk :a (:a a b c d) (b c d))
    (?rk :b (a b c :b b d e f) (a b c d e f))
    (?rk :c (a b c :c d) (a b c))
    (?rk :a (:a) (:a))))

(deftest remove-keywords-test
  (macrolet ((?rk (list remains)
	       `(?equal (remove-keywords ',list) ',remains)))
    (?rk (:a a) ())
    (?rk (:a a b c d :e e e f g) (b c d e f g))
    (?rk (:a a b c d :e e) (b c d))
    (?rk (:a :a b) (b))
    (?rk (:a) (:a))))

(deftest check-keywords-test
  (macrolet ((?ck (keywords list)
	       `(check-keywords ',keywords ',list))
	     (?ck-fail (keywords list fail)
	       `(?condition (check-keywords ',keywords ',list)
			    type-error 
			    (type-error-expected-type ',`(member ,@keywords))
			    (type-error-datum ,fail))))
    (?ck (:a :b :c) (:a a))
    (?ck (:a :b) (:a a :b :c c d))
    (?ck (:a) (b))
    (?ck-fail (:a :c) (:a b c :d e f) :d)))

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