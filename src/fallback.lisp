(defpackage #:loop-continue
  (:use #:cl)
  (:export #:enable
           #:disable))

(in-package #:loop-continue)

(defun enable ()
  (error "This Common Lisp implementation is not supported."))

(defun disable ()
  (warn "This Common Lisp implementation is not supported."))
