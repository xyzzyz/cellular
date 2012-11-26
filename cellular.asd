(in-package #:cl-user)

(defpackage #:cellular-system
    (:use #:cl #:asdf))

(in-package #:cellular-system)

(defsystem cellular
    :name "cellular"
    :author "Adam 'Dodek' Michalik"
    :version "0.1"
    :licence "GNU GPL v3 or later"
    :description "Cellular automatons generator."
    :depends-on
    (:pal)
    :properties ((#:author-email . "dodecki@gmail.com"))
    :serial t
    :components ((:file "package")
		 (:file "utils")
		 (:file "classes")
		 (:file "rules")
		 (:file "automatons")
		 (:file "drawing")))
