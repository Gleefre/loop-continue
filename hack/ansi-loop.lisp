(defpackage #:loop-tag-go
  (:use #:cl)
  (:import-from #+sbcl #:sb-loop
                #+(or cmucl ccl) #:ansi-loop
                #+clasp #:core
                #+allegro #:excl
                #+abcl #:loop
                #:loop-disallow-conditional
                #:loop-get-form
                #:loop-emit-body
                #:loop-error
                #:*loop-ansi-universe*
                #-sbcl #:loop-universe-keywords
                #+sbcl #:keywords)
  (:export #:enable!
           #:disable!))

(in-package #:loop-tag-go)

(defmacro uni ()
  #-sbcl '(loop-universe-keywords *loop-ansi-universe*)
  #+sbcl '(slot-value *loop-ansi-universe* 'keywords))

(defun loop-tag ()
  (loop-disallow-conditional :tag)
  (let ((tag (loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (loop-emit-body tag)
        (loop-error "A go tag was expected, but ~S found." tag))))

(defun loop-go ()
  (let ((tag (loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (loop-emit-body `(go ,tag))
        (loop-error "A go tag was expected, but ~S found." tag))))

(defparameter *hacked!* NIL)

(defun enable! ()
  (unless *hacked!*
    (setf (gethash "TAG" (uni)) '(loop-tag)
          (gethash "GO" (uni)) '(loop-go)
          *hacked!* T)))

(defun disable! ()
  (when *hacked!*
    (remhash "TAG" (uni))
    (remhash "GO" (uni))
    (setf *hacked!* NIL)))
