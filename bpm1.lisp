;; -*- Mode: LISP;  Syntax: COMMON-LISP; Package: CL-USER; -*- 
;; Sun Nov 4 17:19:54 2007 by Nick Allen <nallen05@gmail.com>
;; bpm.lisp

;; this file contains all the code necessary for `bpm' to work.

;; the file `bpm-prettify.lisp'populates the parameter 
;; *PRETTIFY-OUTPUT-TRANSFORMATIONS* with some functions to make the
;; generated code smaller/cleaner/faster but its not really necessary.

;; the tests in `bpm-test.lisp' should pass with or without loading
;; `bpm-prettify.lisp'.

;; see DOC/BPM.HTML for a description of the API

;;  Copyright (c) 2007, Nick Allen
;;  All rights reserved.

;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are met:
;;      * Redistributions of source code must retain the above copyright
;;        notice, this list of conditions and the following disclaimer.
;;      * Redistributions in binary form must reproduce the above copyright
;;        notice, this list of conditions and the following disclaimer in the
;;        documentation and/or other materials provided with the distribution.
;;      * Neither the name of the <organization> nor the
;;        names of its contributors may be used to endorse or promote products
;;        derived from this software without specific prior written permission.

;;  THIS SOFTWARE IS PROVIDED BY <copyright holder> ``AS IS'' AND ANY
;;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
;;  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage :bpm
  (:use :cl)
  (:export ;; arrow syntax
           #:-->
           #:->

	   ;; MATCH macro
	   #:match

	   ;; DEF!/DEF macros
	   #:def!
	   #:def

	   ;; WHERE/WHERE-NOT clauses in MATCH and DEF!/DEF
	   #:where
	   #:where-not

	   ;;BPM-LAMBDA macro
	   #:bpm-lambda

	   ;; CREATE-BPM-COMPILER function
	   #:create-bpm-compiler

	   ;; tweaking logic var syntax
	   #:*logic-var-prefix-char*
	   #:*logic-var-pred*
	   #:*logic-var-wildcard-pred*

	   ;; other BMP compiler vars
	   #:*destructure-simple-vectors-p*
	   #:*logic-var-equality-test*))

(in-package :bpm)

;arrow syntax
(defconstant --> '-->)
(defconstant -> '->)

;MATCH

(defmacro match (form &body clauses)
  (let ((g (gensym)))
    `(let ((,g ,form))
       (funcall (or ,@(mapcar (lambda (c)
				(destructuring-bind (pat . cbody) c
				    `(funcall (.bpm-lambda* ,pat ,@cbody) ,g)))
			      clauses)
		    (constantly nil))))))

(defmacro .bpm-lambda* (patt &body body)
  (labels ((rfn (b)
	     (destructuring-bind (wc? . r) b
	       (if (and (listp wc?)
			(eq (first wc?) 'where))
		   `(when ,(second wc?)
		      ,(rfn r))
		   (if (and (listp wc?)
			    (eq (first wc?) 'where-not))
		       `(unless ,(second wc?)
			  ,(rfn r))
		       `(lambda () ,@b))))))
    `(bpm-lambda ,patt ,(rfn body))))

;DEF!/DEF macros

(defmacro def! (name patt &body body)
  `(progn
     (setf (get ',name 'rules)
	   (list (.bpm-lambda* ,patt ,@body)))
     (defun ,name (sexp)
       (funcall (or (some (lambda (f) (funcall f sexp))
			  (get ',name 'rules))
		    (constantly nil))))))

(defmacro def (name patt &body body)
  `(progn (setf (get ',name 'rules)
		(append (get ',name 'rules)
			(list (.bpm-lambda* ,patt ,@body))))
	  ',name))

;WHERE/WHERE-NOT symbols

(defun where (test)
  (error "the clause ~S has escaped from inside a bpm form! this symbol ~S does not name a function or a macro: it names a special keyword that only has meaning within the ~S, ~S, or ~S macros."
	 `(where ,test) 'where 'def 'def! 'match))

(defun where-not (test)
  (error "the clause ~S has escaped from inside a bpm form! this symbol ~S does not name a function or a macro: it names a special keyword that only has meaning within the ~S, ~S, or ~S macros."
	 `(where-not ,test) 'where-not 'def 'def! 'match))

;BPM-LAMBDA

(defmacro bpm-lambda (pattern &body body)
  (funcall (create-bpm-compiler pattern) `(progn ,@body)))

;logic var syntax

(defparameter *logic-var-prefix-char* #\_)

(defparameter *logic-var-pred*
  (lambda (xx)
    (and (symbolp xx)
	 (not (zerop (length (symbol-name xx))))
	 (char= (char (symbol-name xx) 0) *logic-var-prefix-char*))))

(defparameter *logic-var-wildcard-pred*
  (lambda (xx)
    (and (symbolp xx)
	 (= (length (symbol-name xx)) 1)
	 (char= (char (symbol-name xx) 0) *logic-var-prefix-char*))))

;other compile vars

(defparameter *destructure-simple-vectors-p* t)

(defparameter *logic-var-equality-test* 'eql)

;CREATE-BPM-COMPILER

(defvar *bound*)

(defvar +match+ '+match+)

(defun create-bpm-compiler (pattern &optional *bound*)
  (if (funcall *logic-var-wildcard-pred* pattern)
      (values (lambda (form)
		`(lambda (,pattern)
		   (declare (ignore ,pattern))
		   ,form))
	      *bound*)
      (let ((% (.create-match-template pattern)))
	(values (lambda (form)
		  (subst form +match+ (.prettify-output %)))
		*bound*))))

(defun .create-compiler (pattern)
  (let ((% (.create-match-template pattern)))
    (values (lambda (form) (subst form +match+ %))
	    *bound*)))

(defun .create-match-template (p)
  (let ((f (gensym)))
    `(lambda (,f)
       ,@(if (funcall *logic-var-wildcard-pred* p)
	  `((declare (ignore ,f))))
       ,(typecase p
	  (null  `(if (not ,f)
		      +match+))
	  (atom (cond ((funcall *logic-var-wildcard-pred* p)  +match+)
		      ((funcall *logic-var-pred* p) (if (find p *bound*)
							`(if (,*logic-var-equality-test* ,f ,p)
							     +match+)
							(progn (push p *bound*)
							       `(let ((,p ,f))
								  +match+))))
		      ((and (simple-vector-p p)
			    *destructure-simple-vectors-p*) (.handle-simple-vector p f))
		      (t `(if (,*logic-var-equality-test* ,f ',p)
			      +match+))))
	  (list (let ((g (gensym))
		      (compiler (.create-compiler (first p))))
		  `(let ((,g ,f))
		     (if (listp ,g)
			 (funcall ,(funcall compiler
					    `(funcall ,(funcall (.create-compiler (rest p))
								+match+)
						      (rest ,g)))
				  (first ,g))))))))))

(defun .handle-simple-vector (v form)
  (let ((g (gensym)))
    `(let ((,g ,form))
       (if (simple-vector-p ,g)
	   (if (= (length ,g) ,(length v))
	       ,(.expand-sv-compilers-and-indexes g (.collect-sv-compilers-and-indexes v)))))))

(defun .expand-sv-compilers-and-indexes (f xx)
  (if (not xx)
      +match+
      (destructuring-bind ((compiler . index) . rest) xx
	`(funcall ,(funcall compiler (.expand-sv-compilers-and-indexes f rest))
		  (svref ,f ,index)))))

(defun .collect-sv-compilers-and-indexes (v)
  (let (xx)
    (dotimes (n (length v) (nreverse xx))
      (let ((e (svref v n)))
	(if (not (funcall *logic-var-wildcard-pred* e))
	    (push (cons (.create-compiler e)
			n)
		  xx))))))

(defparameter *prettify-output-transformations* nil)

(defun .prettify-output (in)
  (labels ((rfn (x fs)
	     (if (not fs)
		 x
		 (destructuring-bind (f . rest) fs
		   (rfn (funcall (coerce f 'function) x) rest)))))
    (rfn in *prettify-output-transformations*)))