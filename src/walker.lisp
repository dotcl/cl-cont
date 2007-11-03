
(in-package :cont)

(export '(with-call/cc expr->cps))

(defmacro with-call/cc (&body body)
  "Gives access to CALL/CC by transforming BODY to continuation
passing style."
  (let ((*ctx* (make-call/cc-context)))
    (declare (special *ctx*))
    (expr-sequence->cps body '#'values)))

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

(defun application->cps (app-sym cons k-expr args)
  "Transforms a function application to CPS style."
  (if cons
      (expr->cps (car cons)
		 (let ((i (gensym))
		       (rest-args (gensym)))
		   `(lambda (,i &rest ,rest-args)
		      (declare (ignorable ,i))
		      (declare (ignore ,rest-args))
		      ,(application->cps app-sym (cdr cons)
					 k-expr
					 (cons i args)))))
      (let ((r-args (reverse args)))
	`(,app-sym ,(car r-args) ,k-expr ,@(cdr r-args)))))

(defun funcall->cps (cons k-expr args)
  "Transforms FUNCALL to CPS style."
  (application->cps 'funcall/cc cons k-expr args))

(defun apply->cps (cons k-expr args)
  "Transforms APPLY to CPS style."
  (application->cps 'apply/cc cons k-expr args))

