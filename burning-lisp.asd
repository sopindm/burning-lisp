(in-package #:asdf)

(defsystem #:burning-lisp
    :description "A base lisp package for burning"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
		 (:file "config")
		 (:file "anaphors")
		 (:file "string")
		 (:file "system")))
