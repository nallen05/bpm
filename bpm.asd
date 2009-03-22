;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-

(in-package :cl-user)

(defpackage :bpm.system (:use :asdf :cl))
(in-package :bpm.system)

(defsystem bpm
  :name "bpm"
  :description "Pattern Matching for CL"
  :author "Nick Allen <nallen05@gmail.com>"
  :maintainer "Nick Allen <nallen05@gnmail.com>"
  :version "0.1"
  :components
  ((:file "bpm1")
   (:file "bpm-prettify" :depends-on ("bpm1"))))
