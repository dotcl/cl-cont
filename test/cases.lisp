
(in-package :cont-test)

;; atom
(deftest atom-1
    (with-call/cc 1)
  1)

(deftest atom-2
    (let (cc)
      (values
       (with-call/cc (let/cc k
		       (setf cc k)
		       (funcall cc 1)))
       (funcall cc 2)))
  1 2)

;; funcall
(deftest funcall-1
    (with-call/cc (+ 1 2))
  3)

(deftest funcall-2
    (with-call/cc (- (+ 1 2) 1))
  2)

(deftest funcall-3
    (let (cc)
      (values
       (with-call/cc (+ (let/cc k
			  (setf cc k)
			  (funcall k 1))
			2))
       (funcall cc 2)
       (funcall cc 3)))
  3 4 5)

(deftest funcall-4
    (let (cc)
      (values
       (with-call/cc (+ (let/cc k
			  (setf cc k)
			  1)
			2))
       (funcall cc 2)
       (funcall cc 3)))
  1 4 5)

(deftest funcall-5
    (let (cc)
      (values
       (with-call/cc (+ 1
			(let/cc k
			  (setf cc k)
			  2)))
       (funcall cc 2)
       (funcall cc 3)))
  2 3 4)

(deftest funcall-6
    (let (cc)
      (values
       (with-call/cc (+ (* 3
			   (let/cc k
			     (setf cc k)
			     (funcall k 1)))
			2))
       (funcall cc 2)
       (funcall cc 3)))
  5 8 11)

(deftest funcall-7
    (with-call/cc ((lambda (a b)
		     (+ a b)) 1 2))
  3)

;; quote
(deftest quote-1
    (with-call/cc 'a)
  a)

(deftest quote-2
    (with-call/cc '(a))
  (a))

(deftest quote-3
    (let (cc)
      (values
       (with-call/cc (append '(a b)
			     (let/cc k
			       (setf cc k)
			       (funcall k '(c)))))
       (funcall cc '(d))
       (funcall cc '(e))))
  (a b c) (a b d) (a b e))

;; progn
(deftest progn-1
    (with-call/cc (progn 
		    1 2 3))
  3)

(deftest progn-2
    (let ((out (make-string-output-stream)))
      (with-call/cc (progn 
		      (prin1 1 out)
		      (prin1 2 out)
		      (prin1 3 out)))
      (get-output-stream-string out))
  "123")

(deftest progn-3
    (let (cc)
      (values
       (with-call/cc (progn 
		       1 (let/cc k
			   (setf cc k)
			   (funcall k 2)) 3))
       (funcall cc 5)))
  3 3)

(deftest progn-4
    (let (cc)
      (values
       (with-call/cc (progn 
		       1 (let/cc k
			   (setf cc k)
			   2) 3))
       (funcall cc 5)))
  2 3)

(deftest progn-5
    (with-call/cc (progn))
  nil)

;; test block
(deftest block-1
    (with-call/cc
      (block a 1 2))
  2)

(deftest block-2
    (let (cc)
      (values
       (with-call/cc
	 (block a (let/cc k
		    (setf cc k)
		    (funcall k 1))))
       (funcall cc 2)))
  1 2)

(deftest block-3
    (with-call/cc
      (block nil 1 (return 2) 3))
  2)

;; test return-from
(deftest return-from-1
    (with-call/cc
      (+ 1 (block a
	     1
	     (return-from a 10)
	     3)))
  11)

(deftest return-from-2
    (let (cc)
      (values
       (with-call/cc
	 (+ 1 (block a
		1
		(return-from a (let/cc k
				 (setf cc k)
				 (funcall k 10)))
		3)))
       (funcall cc 11)))
  11 12)

(deftest return-from-3
    (with-call/cc
      (+ 1 (block a
	     1
	     (block b
	       (return-from a 10))
	     3)))
  11)

(deftest return-from-4
    (with-call/cc
      (+ 1 (block a
	     1
	     (block b
	       (return-from b 10))
	     3)))
  4)

(deftest return-from-5
    (with-call/cc
      (+ 1 (block a
	     1
	     (block a
	       (return-from a 10))
	     3)))
  4)

;; test if
(deftest if-1
    (with-call/cc
      (if nil 1 2))
  2)

(deftest if-2
    (with-call/cc
      (if t 1 2))
  1)

(deftest if-3
    (let (cc)
      (values
       (with-call/cc
	 (if t (let/cc k
		 (setf cc k)
		 (funcall k 1)) 2))
       (funcall cc 10)))
  1 10)

(deftest if-4
    (let (cc)
      (with-call/cc
	(if nil (let/cc k
		  (setf cc k)
		  (funcall k 1)) 2))
      cc)
  nil)

(deftest if-5
    (let (cc)
      (values
       (with-call/cc
	 (if nil 1 (let/cc k
		     (setf cc k)
		     (funcall k 2))))
       (funcall cc 10)))
  2 10)

(deftest if-6
    (let (cc)
      (with-call/cc
	(if t 1 (let/cc k
		  (setf cc k)
		  (funcall k 2))))
      cc)
  nil)

(deftest if-7
    (let (cc)
      (values
       (with-call/cc
	 (if (let/cc k
	       (setf cc k)
	       (funcall k t))
	     1 2))
       (funcall cc nil)))
  1 2)

;;; function
(deftest function-1
    (let ((f (with-call/cc #'+)))
      (funcall f 1 2))
  3)

(deftest function-2
    (let ((f (with-call/cc
	       #'(lambda (a b) (+ a b)))))
      (funcall f 1 2))
  3)

(deftest function-3
    (let ((list (list 1 2 3))
	  (f (with-call/cc
	       #'(setf car))))
      (funcall f 2 list)
      list)
  (2 2 3))

(deftest function-4
    (let (cc)
      (values
       (with-call/cc
	 (+ 1 ((lambda (a b) (+ a b (let/cc k
				      (setf cc k)
				      (funcall k 0))))
	       1 2)))
       (funcall cc 1)
       (funcall cc 10)))
  4 5 14)

;;; let*
(deftest let*-1
    (with-call/cc
      (let* ((a 1)
	     (b (+ a 1)))
	(+ a b)))
  3)

(deftest let*-2
    (with-call/cc
      (let* ()
	1))
  1)

(deftest let*-3
    (with-call/cc
      (let* ()))
  nil)

(deftest let*-4
    (let (cc)
      (values
       (with-call/cc
	 (let* ((a (let/cc k
		     (setf cc k)
		     (funcall k 1)))
		(b (+ a 1)))
	   (+ a b)))
       (funcall cc 2)
       (funcall cc 3)))
  3 5 7)

(deftest let*-5
    (let (cc)
      (values
       (with-call/cc
	 (let* ((a 1)
		(b (+ a 1)))
	   (+ a b (let/cc k
		    (setf cc k)
		    (funcall k 10)))))
       (funcall cc 20)))
  13 23)

(deftest let*-6
    (with-call/cc
      (let* ((a 1)
	     (b (+ a 1))
	     c)
	(list (+ a b) c)))
  (3 nil))

;;; let
(deftest let-1
    (with-call/cc
      (let ((a 1)
	    (b 2))
	(+ a b)))
  3)

(deftest let-2
    (with-call/cc
      (let ((a 1)
	    (b 2))
	(let ((b a)
	      (a b))
	  (- b a))))
  -1)

(deftest let-3
    (let (cc)
      (values
       (with-call/cc
	 (let ((a (let/cc k
		    (setf cc k)
		    (funcall k 1)))
	       (b 2))
	   (+ a b)))
       (funcall cc 10)))
  3 12)

(deftest let-4
    (let (cc)
      (values
       (with-call/cc
	 (let ((a 1)
	       (b 2))
	   (+ a b (let/cc k
		    (setf cc k)
		    (funcall k 0)))))
       (funcall cc 10)))
  3 13)

;;; SETQ
(deftest setq-1
    (with-call/cc (setq))
  nil)

(deftest setq-2
    (let (a b)
      (with-call/cc (setq a 1 b 2))
      (values a b))
  1 2)

(deftest setq-3
    (let (a)
      (values (with-call/cc (setq a 1)) a))
  1 1)

(deftest setq-4
    (let (a b cc)
      (with-call/cc (setq a (let/cc k
			      (setf cc k)
			      (funcall k 1))
			  b 2))
      (setq b 3)
      (funcall cc 10)
      (values a b))
  10 2)

(deftest setq-5
    (let (a b)
      (with-call/cc (setq a 1 b a))
      (values a b))
  1 1)

;;; THE
(deftest the-1
    (with-call/cc (the integer 10))
  10)

(deftest the-2
    (let (cc)
      (values
       (with-call/cc (the integer (let/cc k
				    (setf cc k)
				    (funcall k 1))))
       (funcall cc 10)))
  1 10)

;;; FLET
(deftest flet-1
    (with-call/cc
      (flet ((a (i j)
	       (+ i j))
	     (b (i j)
	       (* i j)))
	(+ (a 1 2) (b 3 4))))
  15)

(deftest flet-2
    (let (cc)
      (values
       (with-call/cc
	 (flet ((a (i j)
		  (+ i j))
		(b (i j)
		  (* i j)))
	   (+ (a 1 (let/cc k
		     (setf cc k)
		     (funcall k 2))) (b 3 4))))
       (funcall cc 3)))
  15 16)

(deftest flet-3
    (let (cc)
      (values
       (with-call/cc
	 (flet ((a (i)
		  (+ i (let/cc k
			 (setf cc k)
			 (funcall k 2))))
		(b (i j)
		  (* i j)))
	   (+ (a 1) (b 3 4))))
       (funcall cc 3)))
  15 16)

(deftest flet-4
    (funcall (with-call/cc
	       (flet ((a (i j)
			(+ i j)))
		 #'a))
	     3 4)
  7)

(deftest flet-5
    (with-call/cc
      (flet ((a (i j)
	       (+ i j)))
	(flet ((a (i j)
		 (+ i j)))
	  1)
	(a 1 2)))
  3)

;;; LABELS
(deftest labels-1
    (with-call/cc
      (labels ((a (i j)
		 (+ i j))
	       (b (i j)
		 (* i j)))
	(+ (a 1 2) (b 3 4))))
  15)

(deftest labels-2
    (with-call/cc
      (labels ((a (i j)
		 (+ i j))
	       (b (i j)
		 (* (a i j) 3)))
	(+ (b 1 2))))
  9)

(deftest labels-3
    (let (cc)
      (values
       (with-call/cc
	 (labels ((a (i j)
		    (+ i j (let/cc k
			     (setf cc k)
			     (funcall k 0))))
		  (b (i j)
		    (* (a i j) 3)))
	   (+ (b 1 2))))
       (funcall cc 1)))
  9 12)

(deftest labels-4
    (let (cc)
      (values
       (with-call/cc
	 (labels ((a (i j)
		    (+ i j))
		  (b (i j)
		    (* (a i j) 3)))
	   (+ (b 1 2) (let/cc k
			(setf cc k)
			(funcall k 0)))))
       (funcall cc 1)))
  9 10)

(deftest labels-5
    (funcall
     (with-call/cc
       (labels ((a (i j)
		  (+ i j))
		(b ()
		  #'a))
	 (b)))
     1 2)
  3)

;;; MACROLET
(deftest macrolet-1
    (with-call/cc
      (macrolet ((foo (a) `(+ ,a ,a)))
	(foo 5)))
  10)

(deftest macrolet-2
    (let (cc)
      (values
       (with-call/cc
	 (macrolet ((foo (a) `(+ ,a (let/cc k
				      (setf cc k)
				      (funcall k ,a)))))
	   (foo 5)))
       (funcall cc 6)))
  10 11)

(deftest macrolet-3
    (let (cc)
      (values
       (with-call/cc
	 (macrolet ((foo (a) `(+ ,a ,a)))
	   (foo (let/cc k
		  (setf cc k)
		  (funcall k 5)))))
       (funcall cc 6)))
  10 11)

