
(in-package :cont)

(export '(with-call/cc expr->cps))

(defstruct (call/cc-context (:conc-name ctx-))
  "A structure that represents a context used during call/cc
transformation."
  (block-tags (make-hash-table))
  (local-functions nil))

(defmacro with-call/cc (&body body)
  "Gives access to CALL/CC by transforming BODY to continuation
passing style."
  (let ((*ctx* (make-call/cc-context)))
    (declare (special *ctx*))
    (expr-sequence->cps body '#'identity)))

(defun expr->cps (expr k-expr &optional env)
  "Transforms expression to CPS style."
  (if (atom expr)
      (atom->cps expr k-expr)
      (cons->cps expr k-expr env)))

(defun atom->cps (atom k-expr)
  "Transforms an atom to CPS style."
  `(funcall ,k-expr ,atom))

(defun cons->cps (cons k-expr &optional env)
  "Transforms a cons to CPS style."
  (let ((transformer (gethash (car cons) *special-form-transformers*)))
    (if transformer
	(funcall transformer cons k-expr)
	(multiple-value-bind (expansion expanded-p)
	    (macroexpand-1 cons env)
	  (if expanded-p
	      (expr->cps expansion k-expr)
	      (funcall->cps (cons `(function ,(car cons))
				  (cdr cons))
			    k-expr nil))))))

(defun funcall->cps (cons k-expr args)
  "Transforms a function call to CPS style."
  (if cons
      (expr->cps (car cons)
		 (let ((i (gensym)))
		   `(lambda (,i)
		      ,(funcall->cps (cdr cons)
				     k-expr
				     (cons i args)))))
      (let ((r-args (reverse args)))
	`(funcall/cc ,(car r-args) ,k-expr ,@(cdr r-args)))))

