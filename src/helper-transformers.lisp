
(in-package :cont)

(export '(defun/cc without-call/cc))

;;; Nested WITH-CALL/CC
(defcpstransformer with-call/cc (cons k-expr env)
  "Transforms a nested WITH-CALL/CC."
  (expr-sequence->cps (cdr cons) k-expr env))

;;; Turning off WITH-CALL/CC
(defcpstransformer without-call/cc (cons k-expr env)
  (declare (ignore env))
  `(funcall ,k-expr (progn ,@(cdr cons))))

;;; DEFUN
(defcpstransformer defun (cons k-expr env)
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
				nil
				env))
       (funcall ,k-expr ',name))))

(defmacro defun/cc (name arglist &body body)
  "A helper macro to define a function that supports CALL/CC."
  `(with-call/cc
     (defun ,name ,arglist
       ,@body)))

;;; FUNCALL/APPLY
(defcpstransformer funcall (cons k-expr env)
  "Converts FUNCALL to FUNCALL/CC."
  (funcall->cps (cdr cons) k-expr nil env))

(defcpstransformer apply (cons k-expr env)
  "Converts APPLY to APPLY/CC."
  (apply->cps (cdr cons) k-expr nil env))

