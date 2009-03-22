
(in-package :cl-user)

(defpackage :bpm.prettify
  (:use :cl :bpm))

(in-package :bpm.prettify)

;prettifying generated code

 (defun .mapcar* (fn improper-list)
   (if (not (atom improper-list))
       (destructuring-bind (1st . rest)
 	  improper-list
 	(cons (funcall fn 1st)
 	      (.mapcar* fn rest)))))

(defmacro def-prettifier (name pattern &body body)
  `(progn (def! ,name ,pattern ,@body)
	  (def ,name _x
	    (if (atom _x)
		_x
		(.mapcar* #',name _x)))))

(def-prettifier .clean-funcalls  (funcall (lambda (_x) . _code)
					  (_fn _y . _r))
  -> `(let ((,_x (,_fn ,_y ,@_r)))
	,@(.mapcar* #'.clean-funcalls _code)))

(def-prettifier .clean-initial-binding  (lambda (_x)
					  (let ((_y _x))
					    . _code))
  -> `(lambda (,_y) ,@_code))

;; (def-prettifier .clean-terminal-nil-match (let ((_y _x))
;; 					    (if (not _y)
;; 						+match+))
;;   -> (if (not _x)
;; 	 +match+))

(def-prettifier .clean-let1 (let ((_y _x))
			      (let ((_z _y))
				. _code))
  -> (.clean-let1 `(let ((,_z ,_x))
		     ,@_code)))

(def-prettifier .clean-let2 (let ((_y _x))
			      (let ((_z (_fn _y . _r)))
				. _code))
  -> (.clean-let2 `(let ((,_z (,_fn ,_x ,@_r)))
		     ,@_code)))

(def-prettifier .clean-bindings (let ((_y _x))
				   (if (_fn _y . _r)
				       . _code))

  ;; _y is gensym
  (where-not (symbol-package _y))

  ;; _y is not used is _code
  (where-not (.find-in-tree _y _code))

  ;; inline it
  (.clean-bindings `(if (,_fn ,_x . ,_r)
			,@_code)))

(defun .find-in-tree (x tree)
  (labels ((rfn (%)
	     (if (eql % x)
		 (return-from .find-in-tree %)
		 (when (not (atom %))
		   (rfn (first %))
		   (rfn (rest %))))))
    (rfn tree)))

(def-prettifier .collapse-if (if _x
				 (if _y
				     _code))
  -> (.collapse-if `(and ,_x
			 ,_y
			 ,@(..collapse-if _code))))

(def! ..collapse-if (if _x
			 _code)
  (cons _x (..collapse-if _code)))

(def ..collapse-if _x -> (list _x))

(def-prettifier .collapse-let (let _bindings
				(let (_b2)
				  . _code))
  (.collapse-let (if (some (lambda (v) (.find-in-tree v _b2))
			   (.mapcar* #'first _bindings))
		     `(let* (,@_bindings
			     ,_b2)
			,@_code)
		     `(let (,@_bindings
			    ,_b2)
			,@_code))))

(def-prettifier .declare-dynamic-gensyms (let _bindings 
					   . _code)
  `(let ,_bindings
     ,@(if (find-if-not #'symbol-package _bindings :key #'first)
	   `((declare (dynamic-extent ,@(remove-if #'symbol-package
						   (.mapcar* #'first _bindings))))))
     ,@(.declare-dynamic-gensyms _code)))

(def-prettifier .clean-initial-wildcard  (lambda (_x)
					   . _code)
  `(lambda (,_x)
     ,@(if (funcall *logic-var-wildcard-pred* _x)
	   `((declare (ignore ,_x))))
     ,@_code))

(def-prettifier .declare-initial-gensym (lambda (_x) . _code)
  `(lambda (,_x) 
     ,@(if (not (symbol-package _x))
	   `((declare (dynamic-extent ,_x))))
     ,@_code))

(setf bpm::*prettify-output-transformations*
      '(.clean-funcalls
	.clean-initial-binding
;	.clean-terminal-nil-match
	.clean-let1
	.clean-let2
	.clean-bindings
	.collapse-if
	.collapse-let
	.declare-dynamic-gensyms
	.clean-initial-wildcard
	.declare-initial-gensym))