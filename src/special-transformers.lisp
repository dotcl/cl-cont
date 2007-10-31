
(in-package :cont)

(export '(expr-sequence->cps call/cc let/cc))

;;; CALL/CC and friends
(defun call/cc (cc)
  "Implements delimited continuations."
  (declare (ignore cc))
  (error "Please ensure CALL/CC is caled from within WITH-CALL/CC
  macro."))

(defmacro let/cc (k &body body)
  "A shortcut for CALL/CC."
  `(call/cc (lambda (,k)
	      ,@body)))

(defcpstransformer call/cc (cons k-expr)
  "Implements call/cc operator."
  (assert (= (length cons) 2)
	  nil
	  "Please ensure that CALL/CC has one argument")
  `(funcall ,(cadr cons) ,k-expr))

;;; QUOTE
(defcpstransformer quote (cons k-expr)
  "Converts a quoted datum to CPS style."
  `(funcall ,k-expr ,cons))

;;; PROGN
(defun extract-declarations (expr-list)
  "Returns a list of declarations extracted from the EXPR-LIST."
  (loop for i in expr-list
       while (and (consp i)
		  (eq (car i) 'declare))
       collect i))

(defun remove-declarations (expr-list)
  "Removes declarations from the EXPR-LIST."
  (let ((c 0))
    (loop for i in expr-list
       while (and (consp i)
		  (eq (car i) 'declare))
       do (incf c))
    (nthcdr c expr-list)))

(defun expr-sequence->cps (expr-list k-expr &key env)
  "Converts a list of expressions to be evaluated in order to CPS
  style."
  (expr->cps (car expr-list)
	     (if (null (cdr expr-list))
		 k-expr
		 (let ((r (gensym)))
		   `(lambda (&optional ,r)
		      (declare (ignore ,r))
		      ,(expr-sequence->cps (cdr expr-list) k-expr :env env))))
	     env))

(defcpstransformer progn (cons k-expr)
  "Converts a PROGN form to CPS style."
  (expr-sequence->cps (cdr cons) k-expr))

;;; BLOCK
(defcpstransformer block (cons k-expr)
  "Transforms BLOCK to CPS style."
  (declare (special *ctx*))
  (assert (symbolp (cadr cons))
	  nil
	  "Please ensure the name of the BLOCK is a symbol.")
  (let ((k (gensym)))
    (unwind-protect
	 (progn
	   (push k (gethash (cadr cons) (ctx-block-tags *ctx*)))
	   `(let ((,k ,k-expr))
	      (declare (ignorable ,k))
	      ,(expr-sequence->cps (cddr cons) k)))
      (pop (gethash (cadr cons) (ctx-block-tags *ctx*))))))

;;; RETURN-FROM
(defcpstransformer return-from (cons k-expr)
  "Transforms RETURN-FROM to CPS style."
  (declare (ignore k-expr))
  (declare (special *ctx*))
  (expr->cps (caddr cons) (or (car (gethash (cadr cons) (ctx-block-tags *ctx*)))
			      (error "There is no block named ~A" (cadr cons)))))

;;; IF
(defcpstransformer if (cons k-expr)
  "Converts an IF form to CPS style."
  (let ((pred-expr (cadr cons))
	(pred-true-expr (caddr cons))
	(pred-false-expr (cadddr cons)))
    (expr->cps pred-expr
	       (let ((pred (gensym)))
		 `(lambda (,pred)
		    (if ,pred
			,(expr->cps pred-true-expr k-expr)
			,(expr->cps pred-false-expr k-expr)))))))

;;; FUNCTION
(defclass funcallable/cc ()
  ((function :accessor f/cc-function
	     :initarg :function
	     :documentation "A function object that accepts a
	     continuation argument."))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A structure that represents a funcallable object"))

(defmethod initialize-instance ((instance funcallable/cc) &rest initargs)
  (declare (ignorable initargs))
  (call-next-method)
  (c2mop:set-funcallable-instance-function
   instance
   (lambda (&rest args)
     (apply (f/cc-function instance) #'identity args))))

(defun funcall/cc (fdesignator k &rest args)
  "Implements FUNCALL for a CPS converter."
  (typecase fdesignator
    (funcallable/cc (apply (f/cc-function fdesignator) k args))
    (t (funcall k (apply fdesignator args)))))

(defun lambda-expr->cps (cons k-expr)
  "Converts a LAMBDA expression to CPS style."
  (declare (ignore k-expr))
  (let ((arg-list (cadr cons))
	(body (cddr cons)))
    (let ((k (gensym)))
      `(make-instance 'funcallable/cc
		      :function
		      (lambda (,k ,@arg-list)
			,@(extract-declarations body)
			,(expr-sequence->cps (remove-declarations body) k))))))

(defcpstransformer function (cons k-expr)
  (declare (special *ctx*))
  (let ((fdesignator (cadr cons)))
    (etypecase fdesignator
      (symbol (if (find fdesignator (ctx-local-functions *ctx*))
		  `(funcall ,k-expr (make-instance 'funcallable/cc
						   :function ,cons))
		  `(funcall ,k-expr ,cons)))
      (list (ecase (car fdesignator)
	      (setf `(funcall ,k-expr ,cons))
	      (lambda `(funcall ,k-expr ,(lambda-expr->cps fdesignator k-expr))))))))

;;; LET*
(defun let*-varlist->cps (varlist let-body k-expr)
  "Converts a list of variables from a LET* expression to CPS style."
  (let* ((var (car varlist))
	 (var-name (if (consp var) (car var) var))
	 (var-value (when (consp var) (cadr var))))
    (if var
	(expr->cps var-value
		   `(lambda (,var-name)
		      ,@(unless (cdr varlist)
			   (extract-declarations let-body))
		      ,(let*-varlist->cps (cdr varlist) let-body k-expr)))
	(expr-sequence->cps (remove-declarations let-body) k-expr))))

(defcpstransformer let* (cons k-expr)
  "Converts a LET* expression to CPS style."
  (let ((varlist (cadr cons))
	(forms (cddr cons)))
    (let*-varlist->cps varlist forms k-expr)))

;;; LET
(defun let-varlist->cons-pairs (varlist)
  "Converts a list of LET vars into name/value cons pairs."
  (mapcar (lambda (var)
	    (cons (if (consp var) (car var) var)
		  (when (consp var) (cadr var))))
	  varlist))

(defcpstransformer let (cons k-expr)
  "Converts a LET expression to CPS style."
  (let ((varlist (let-varlist->cons-pairs (cadr cons)))
	(forms (cddr cons)))
    (expr->cps `((lambda ,(mapcar #'car varlist)
		   ,@(extract-declarations forms)
		   ,@(remove-declarations forms))
		 ,@(mapcar #'cdr varlist))
	       k-expr)))

;;; SETQ
(defun setq->cps (setq-pairs k-expr)
  "Converts a list of SETQ pairs to CPS style."
  (when setq-pairs
    (expr->cps (cdar setq-pairs)
	       (let ((i (gensym)))
		 (assert (symbolp (caar setq-pairs))
			 nil
			 "The value ~A is not a symbol" (caar setq-pairs))
		 `(lambda (,i)
		    ,@(if (cdr setq-pairs)
			  `((setq ,(caar setq-pairs) ,i)
			    ,(setq->cps (cdr setq-pairs) k-expr))
			  `((funcall ,k-expr (setq ,(caar setq-pairs) ,i)))))))))

(defcpstransformer setq (cons k-expr)
  "Converts a SETQ expression to CPS style."
  (assert (evenp (length (cdr cons)))
	  nil
	  "Odd number of argumenuts to SETQ: ~A" cons)
  (setq->cps
   (loop for i in (cdr cons) by #'cddr
      for j in (cddr cons) by #'cddr
      collect (cons i j))
   k-expr))

;;; THE
(defcpstransformer the (cons k-expr)
  "Converts a THE expression to CPS style."
  (expr->cps (caddr cons) k-expr))

;;; FLET/LABELS
(defun transform-local-function (fn)
  "Transforms a local (FLET or LABELS) function to CPS style."
  (let ((fn-name (car fn))
	(fn-args (cadr fn))
	(fn-forms (cddr fn))
	(k (gensym)))
    (assert (and fn-name (symbolp fn-name))
	    nil
	    "Function name must be a non-NIL symbol")
    (assert (>= (length fn) 2)
	    nil
	    "Function arguments not specified")
    `(,fn-name (,k ,@fn-args)
	       ,@(extract-declarations fn-forms)
	       ,(expr-sequence->cps (remove-declarations fn-forms) k))))

(defun declare-function-names-local (names)
  "Declares function names local in a transformation environment."
  (declare (special *ctx*))
  (dolist (fn names)
    (push fn (ctx-local-functions *ctx*))))

(defun undeclare-function-names-local (names)
  "Undeclares function names local in a transformation environment."
  (declare (special *ctx*))
  (dolist (fn names)
    (setf (ctx-local-functions *ctx*)
	  (remove fn (ctx-local-functions *ctx*) :count 1))))

(defmacro with-local-function-names (names &body body)
  "Runs BODY within a transformation environment in which functions
  named by NAMES are declared as local."
  (let ((fn-list (gensym)))
    `(let ((,fn-list ,names))
       (unwind-protect
	    (progn
	      (declare-function-names-local ,fn-list)
	      ,@body)
	 (undeclare-function-names-local ,fn-list)))))

(defcpstransformer flet (cons k-expr)
  "Converts an FLET expression to CPS style."
  (let ((fn-list (cadr cons))
	(forms (cddr cons)))
    (assert (>= (length cons) 2)
	    nil
	    "Too few parameters to FLET")
    `(flet ,(mapcar #'transform-local-function fn-list)
       ,@(extract-declarations forms)
       ,(with-local-function-names (mapcar #'car fn-list)
           (expr-sequence->cps (remove-declarations forms) k-expr)))))

(defcpstransformer labels (cons k-expr)
  "Converts a LABELS expression to CPS style."
  (let ((fn-list (cadr cons))
	(forms (cddr cons)))
    (assert (>= (length cons) 2)
	    nil
	    "Too few parameters to LABELS")
    (with-local-function-names (mapcar #'car fn-list)
      `(labels ,(mapcar #'transform-local-function fn-list)
	 ,@(extract-declarations forms)
	 ,(expr-sequence->cps (remove-declarations forms) k-expr)))))

;;; MACROLET
(defmacro transform-macrolet-forms (forms k-expr transf-env &environment env)
  "Macroexpands code within a MACROLET and runs a CPS code walker on
it."
  (let ((*ctx* transf-env))
    (declare (special *ctx*))
    (expr-sequence->cps forms k-expr :env env)))

(defcpstransformer macrolet (cons k-expr)
  "Converts a MACROLET expression to CPS style."
  (declare (special *ctx*))
  `(macrolet ,(cadr cons)
     ,@(extract-declarations (cddr cons))
     (transform-macrolet-forms ,(remove-declarations (cddr cons)) ,k-expr ,*ctx*)))

