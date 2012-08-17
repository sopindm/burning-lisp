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

