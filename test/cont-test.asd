;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cont-test-asd
  (:use :cl :asdf))

(in-package :cont-test-asd)

(defsystem cont-test
  :name "cont-test"
  :version "0.0.1"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A test harness for cl-cont library."
  :depends-on ("cont" "rt")
  :components ((:file "cont-test")
	       (:file "cases"
		      :depends-on ("cont-test"))))


