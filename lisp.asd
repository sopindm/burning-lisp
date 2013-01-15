(in-package #:asdf)

(defsystem #:burning-lisp
    :description "A base lisp package for burning"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
		 (:file "packages")
		 (:file "conditions")
		 (:file "config")
		 (:file "anaphors")
		 (:file "string")
		 (:file "keywords")
		 (:file "destructuring")
		 (:file "lambda-lists")
		 (:file "double-list")
		 (:file "system"))
    :depends-on (#:alexandria))
