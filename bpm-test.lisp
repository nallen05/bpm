;; -*- Mode: LISP;  Syntax: COMMON-LISP; Package: CL-USER; -*- 
;; Sun Nov 4 17:19:54 2007 by Nick Allen <nallen05@gmail.com>
;; bpm.lisp

;note: these tests are intended to be run w/ all `bpm' vars set to their default values

;note: these tests require ptester

;note: these tests should work both before and after loading `bpm-prettify.lisp'

(in-package :cl-user)

(defpackage :bpm.test
  (:use :cl :bpm :ptester))

(in-package :bpm.test)

(setf *break-on-test-failures* t)

(with-tests (:name "arrow syntax")

  (test '-> ->)

  (test '--> -->)
)

(with-tests (:name "CREATE-BPM-COMPILER variables")

  (test #\_ *logic-var-prefix-char*)

  (test t (funcall *logic-var-pred* '_foo))
  (test nil (funcall *logic-var-pred* 'foo))

  (test t (funcall *logic-var-wildcard-pred* '_))
  (test nil (funcall *logic-var-wildcard-pred* '_foo))

  (test t *destructure-simple-vectors-p*)

  (test 'eql *logic-var-equality-test*)
)

(defmacro test-match (pattern thing &rest kwd-args)
  `(test t
	 (funcall (coerce (funcall (create-bpm-compiler ',pattern)
				   t)
			  'function)
		  ',thing)
	 ,@kwd-args))

(defmacro test-no-match (pattern thing &rest kwd-args)
  `(test nil
	 (funcall (coerce (funcall (create-bpm-compiler ',pattern)
				   t)
			  'function)
		  ',thing)
	 ,@kwd-args))

(with-tests (:name "shotgun style BPM compiler test")

  (test-match t t)
  (test-no-match t nil)

  (test-match nil nil)
  (test-no-match nil t)

  (test-match 1 1)
  (test-no-match 1 2)

  (test-match (1 2 3) (1 2 3))
  (test-no-match (1 2 3) (1 2 4))
  (test-no-match (1 2 3) (1 2 3 4))

  (test-match (1 2 . 3) (1 2 . 3))
  (test-no-match (1 2 . 3) (1 2))
  (test-no-match (1 2 . 3) (1 2 3))
  (test-no-match (1 2 . 3) (1 2 . 4))

  (test-match (((1)) (2) 3) (((1)) (2) 3))
  (test-no-match (((1)) (2) 3) (((9)) (2) 3))
  (test-no-match (((1)) (2) 3) (((1)) (9) 3))
  (test-no-match (((1)) (2) 2) (((1)) (2) 9))
  (test-no-match (((1 9)) (2) 2) (((1)) (2) 3))
  (test-no-match (((1)) (2 9) 2) (((1)) (2) 3))
  (test-no-match (((1)) (2) 2 9) (((1)) (2) 3))

  (test-match _ 1)
  (test-match _ (1 2 3 4 5))

  (test-match _foo 1)
  (test-match _foo (1 2 3 4 5))

  (test-match (_1 _2) (1 2))

  (test-match (_1 _2 . _3) (1 2 . 3))
  (test-match (1 _2 . 3) (1 2 . 3))
  (test-match (1 _2 . 3) (1 (1 2 . 3) . 3))
  (test-no-match (1 _2 . 3) (1 2 . 9))
  (test-no-match (1 _2 . 3) (9 2 . 3))

  (test-match (_1 _1) (1 1))
  (test-no-match (_1 _1) (1 2))

  (test-match (_1 . _1) (1 . 1))
  (test-no-match (_1 . _1) (1 . 2))

  (test-match (((_1)) _1 . _1) (((1)) 1 . 1))
  (test-no-match (((_1)) _1 . _1) (((9)) 1 . 1))
  (test-no-match (((_1)) _1 . _1) (((1)) 9 . 1))
  (test-no-match (((_1)) _1 . _1) (((1)) 1 . 9))

  (test-match #(1 2 3) #(1 2 3))
  (test-no-match #(1 2 3) #(1 2 9))

  (test-match #(_ _ _) #(1 2 3))

  (test-match #(_1 _1 _1) #(1 1 1))
  (test-no-match #(_1 _1 _1) #(9 1 1))
  (test-no-match #(_1 _1 _1) #(1 9 1))
  (test-no-match #(_1 _1 _1) #(1 1 9))

  (test-match (#(_1 _1 _1)) (#(1 1 1)))
  (test-no-match (#(_1 _1 _1)) (#(9 1 1)))
  (test-no-match (#(_1 _1 _1)) (#(1 9 1)))
  (test-no-match (#(_1 _1 _1)) (#(1 1 9)))

  (test-match (#(((_1)) (_1) _1)) (#(((1)) (1) 1)))
  (test-no-match (#(((_1)) (_1) _1)) (#(((9)) (1) 1)))
  (test-no-match (#(((_1)) (_1) _1)) (#(((1)) (9) 1)))
  (test-no-match (#(((_1)) (_1) _1)) (#(((1)) (1) 9)))
)

(with-tests (:name "CREATE-BPM-COMPILER 2nd return value")

  (test nil
	(set-difference '(_1 _2 _3)
			(nth-value 1 (create-bpm-compiler '(((_1)) _2 . _3)))))

  (test nil
	(set-difference (nth-value 1 (create-bpm-compiler '(((_1)) _2 . _3)))
			'(_1 _2 _3)))

  (test nil
	(set-difference '(_1 _2 _3 _4 _5)
			(nth-value 1 (create-bpm-compiler '(((_1)) _2 . _3)
							  '(_4 _5)))))

  (test nil
	(set-difference (nth-value 1 (create-bpm-compiler '(((_1)) _2 . _3)
							  '(_4 _5)))
			'(_1 _2 _3 _4 _5)))
)

(with-tests (:name "other equality tests")
  (let ((*logic-var-equality-test* 'string-equal))

    (test-match "foo" "Foo")
    (test-no-match "foo" "Foo1")

    (test-match ("1" "2" #("3")) ("1" "2" #("3")))
    (test-no-match ("1" "2" #("3")) ("1" "2" #("9")))
))



(with-tests (:name "not destructuring simple vectors")
  (let ((*destructure-simple-vectors-p* nil))

    (test nil
	  (funcall (coerce (funcall (create-bpm-compiler #(1 2) ) t)
			   'function)
		   #(1 2)))
))


(defmacro test-rule-match (rule val)
  `(test t
	 (let ((fn (funcall ,rule ',val)))
	   (if fn
	       (funcall fn)))))

(defmacro test-rule-not-match (rule val)
  `(test nil
	 (let ((fn (funcall ,rule ',val)))
	   (if fn
	       (funcall fn)))))

(with-tests (:name "BPM-LAMBDA")

  (test-rule-match (bpm-lambda 1 (constantly t))
		   1)

  (test-rule-not-match (bpm-lambda 1 (constantly t))
		      9)

  (test-rule-match (bpm-lambda ((1)) (constantly t))
		   ((1)))

  (test-rule-not-match (bpm-lambda ((1)) (constantly t))
		      ((9)))
)

(with-tests (:name "MATCH")
  (test 'four (match 4
		(4 -> 'four)
		(5 -> 'five)))

  (test 'five (match 5
		(4 -> 'four)
		(5 -> 'five)))

  (test nil (match 6
	      (4 -> 'four)
	      (5 -> 'five)))

  (test '(2 1) (match '(1 2)
		 ((_1 _2) -> (list _2 _1)))
	:test #'equal))

(with-tests (:name "MATCH + WHERE/WHERE-NOT")

  (test nil
 	(match #(a a b b)
 	  (#(_1 _1 _2 _2) (where (eql _1 _2))
 	                  #(_1))))

  (test #(a)
 	(match #(a a a a)
 	  (#(_1 _1 _2 _2) (where (eql _1 _2))
 	                  (vector _1)))
	:test #'equalp)

  (test #(a b)
 	(match #(a a b b)
 	  (#(_1 _1 _2 _2) (where-not (eql _1 _2))
 	                  (vector _1 _2)))
	:test #'equalp)

  (test nil
 	(match #(a a a a a)
 	  (#(_1 _1 _2 _2) (where-not (eql _1 _2))
 	                  #(_1 _2)))))

(with-tests (:name "DEF! & DEF")

  (def! speller1 1 -> 'one)

  (test 'one (speller1 1))

  (test nil (speller1 2))

  (def speller1 2 -> 'two)

  (test 'one (speller1 1))

  (test 'two (speller1 2))

  (test nil (speller1 3))

  (def! speller2 3 -> 'three)

  (test nil (speller2 1))

  (test nil (speller2 2))

  (test 'three (speller2 3))

  (def speller2 _ -> 'other)

  (test 'three (speller2 3))

  (test 'other (speller2 4))

)

(with-tests (:name "DEF!/DEF + WHERE/WHERE-NOT")

  (def! fn1 #(_1 _1 _2 _2) 
    (where (eql _1 _2))
    --> (vector _1))

  (def fn1 _ --> 'otherwise)

  (test 'otherwise (fn1 #(a a b b)))
  (test #(a) (fn1 #(a a a a))
	:test #'equalp)

  (def! fn2 #(_1 _1 _2 _2)
    (where-not (eql _1 _2))
    --> (vector _1 _2))

  (def fn2 _ --> 'otherwise)

  (test #(a b) (fn2 #(a a b b))
	:test #'equalp)
  (test 'otherwise (fn2 #(a a a a a))))

;(defparameter x (create-bpm-compiler '(1 _1 #(_1 _2) _2)))

;(defparameter y (funcall x '(progn (print _1) (print _2))))