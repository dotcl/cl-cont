
(in-package :cont-test)

;;; DEFUN
(defun/cc test-fn/cc-1 (a b)
  (+ a b))

(deftest defun/cc-1
    (test-fn/cc-1 1 2)
  3)

(deftest defun/cc-2
    (with-call/cc (+ 1 (test-fn/cc-1 1 2)))
  4)

(defparameter *test-fn/cc* nil)

(defun/cc test-fn/cc-2 (a b)
  (+ a (let/cc k
	 (setf *test-fn/cc* k)
	 (funcall k b))))

(deftest defun/cc-3
    (values
     (test-fn/cc-2 1 2)
     (funcall *test-fn/cc* 10))
  3 11)

;;; explicit FUNCALL
(deftest explicit-funcall-1
    (let (cc)
      (values
       (with-call/cc (+ 1 (funcall (lambda (a)
				     (+ (let/cc k
					  (setf cc k)
					  (funcall k 1)) a)) 5)))
       (funcall cc 2)))
  7 8)

;;; explicit APPLY
(deftest explicit-apply-1
    (let (cc)
      (values
       (with-call/cc (+ 1 (apply (lambda (a)
				     (+ (let/cc k
					  (setf cc k)
					  (funcall k 1)) a)) (list 5))))
       (funcall cc 2)))
  7 8)

