(in-package #:asdf)

(defsystem #:burning-lisp-test
    :description "TESTS"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "PRIVATE"
    :serial t
    :components ((:file "test-package")
		 (:file "keywords-test"))
    :depends-on (#:burning-lisp #:burning-testing))
