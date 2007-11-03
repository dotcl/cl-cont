
(in-package :cont)

(export '(defun/cc))

;;; DEFUN
(defcpstransformer defun (cons k-expr)
  "Transforms DEFUN expression to CPS form."
  (declare (ignore k-expr))
  (let ((name (cadr cons))
	(args (caddr cons))
	(body (cdddr cons)))
    `(progn
       (setf (fdefinition ',name)
	     ,(lambda-expr->cps `(lambda ,args
				   (block ,name
				     ,@body))
				nil))
       ',name)))

(defmacro defun/cc (name arglist &body body)
  "A helper macro to define a function that supports CALL/CC."
  `(with-call/cc
     (defun ,name ,arglist
       ,@body)))

