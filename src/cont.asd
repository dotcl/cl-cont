;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cont-asd
  (:use :cl :asdf))

(in-package :cont-asd)

(defsystem cont
  :name "cont"
  :version "0.0.1"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A library that implements continuations by
  transforming Common Lisp code to continuation passing style."
  :depends-on ("closer-mop")
  :components ((:file "cont")
	       (:file "special-transformers"
		      :depends-on ("cont"))
	       (:file "walker"
		      :depends-on ("cont" "special-transformers"))))

