(in-package :cl-user)

(defpackage #:cl-cont
  (:nicknames #:cont)
  (:use :cl)
  (:export #:defcpstransformer #:cpstransformer #:copy-transformation-context
	   #:expr-sequence->cps #:call/cc #:let/cc #:lambda/cc
	   #:with-call/cc #:expr->cps
	   #:defun/cc #:without-call/cc)
  (:documentation
   "A library that implements continuations by transforming Common
  Lisp code to continuation passing style."))
