
(in-package :cont)

(export '(defun/cc))

;;; Nested WITH-CALL/CC
(defcpstransformer with-call/cc (cons k-expr)
  "Transforms a nested WITH-CALL/CC."
  (expr-sequence->cps (cdr cons) k-expr))

;;; DEFUN
(defcpstransformer defun (cons k-expr)
  "Transforms DEFUN expression to CPS form."
  (let ((name (cadr cons))
	(args (caddr cons))
	(body (cdddr cons)))
    `(progn
       (setf (fdefinition ',name)
	     ,(lambda-expr->cps `(lambda ,args
				   ,@(extract-declarations body)
				   (block ,name
				     ,@(remove-declarations body)))
				nil))
       (funcall ,k-expr ',name))))

(defmacro defun/cc (name arglist &body body)
  "A helper macro to define a function that supports CALL/CC."
  `(with-call/cc
     (defun ,name ,arglist
       ,@body)))

;;; FUNCALL/APPLY
(defcpstransformer funcall (cons k-expr)
  "Converts FUNCALL to FUNCALL/CC."
  (funcall->cps (cdr cons) k-expr nil))

(defcpstransformer apply (cons k-expr)
  "Converts APPLY to APPLY/CC."
  (apply->cps (cdr cons) k-expr nil))

