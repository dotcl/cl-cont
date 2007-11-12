;;; Code shared accross the entire weblocks framework
(defpackage #:cl-cont
  (:nicknames #:cont)
  (:use :cl)
  (:documentation
   "A library that implements continuations by transforming Common
  Lisp code to continuation passing style."))

(in-package :cont)

(export '(defcpstransformer cpstransformer))

(defparameter *special-form-transformers* (make-hash-table :size 25)
  "A hashtable that maps symbols to functions. Each function acts as a
  transformer of a special form and must accept three arguments: an
  s-expression to transform to CPS-style, an an s-expression to
  continue to, and an environment object.")

(defmacro defcpstransformer (name lambda-list &body body)
  "A macro that allows defining special case CPS transformers."
  `(setf (gethash ',name *special-form-transformers*)
	 (lambda ,lambda-list
	   ,@body)))

(defun cpstransformer (name)
  "Returns a CPS transformer named by a given NAME."
  (gethash name *special-form-transformers*))

(defmacro expand-form (form &environment env)
  "A helper macro that expands forms by considering local macros."
  (multiple-value-bind (expansion expanded-p)
      (macroexpand form env)
    `(values ',expansion ',expanded-p)))

(defstruct (call/cc-context (:conc-name ctx-))
  "A structure that represents a context used during call/cc
transformation."
  (block-tags (make-hash-table))
  (local-functions nil))

