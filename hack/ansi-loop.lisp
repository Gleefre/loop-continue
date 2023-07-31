(defpackage #:loop-tag-go
  (:use #:cl)
  (:import-from #+sbcl #:sb-loop
                #+(or cmucl ccl) #:ansi-loop
                #+clasp #:core
                #+allegro #:excl
                #+abcl #:loop
                #+ecl #:si
                #-ecl #:loop-disallow-conditional
                #-ecl #:loop-get-form
                #-ecl #:loop-emit-body
                #-ecl #:loop-error
                #:*loop-ansi-universe*
                #-sbcl #:loop-universe-keywords
                #+sbcl #:keywords)
  (:export #:enable!
           #:disable!))

(in-package #:loop-tag-go)

(defmacro uni ()
  #-sbcl '(loop-universe-keywords *loop-ansi-universe*)
  #+sbcl '(slot-value *loop-ansi-universe* 'keywords))

;; ECL "hides" some functions by declaring them (si::c-local)
#+ecl
(progn
  (defun loop-context ()
    (do ((l si::*loop-source-context* (cdr l)) (new nil (cons (car l) new)))
        ((eq l (cdr si::*loop-source-code*)) (nreverse new))))
  (defun loop-error (format-string &rest format-args)
    (si::simple-program-error "~?~%Current LOOP context:~{ ~S~}."
                              format-string format-args (loop-context)))
  (defun loop-pop-source ()
    (if si::*loop-source-code*
        (pop si::*loop-source-code*)
        (loop-error "LOOP source code ran out when another token was expected.")))
  (defun loop-get-form ()
    (if si::*loop-source-code*
        (loop-pop-source)
        (loop-error "LOOP code ran out where a form was expected.")))
  (defun loop-emit-body (form)
    (setf si::*loop-emitted-body* t)
    (push form si::*loop-body*))
  (defun loop-disallow-conditional (&optional kwd)
    (when si::*loop-inside-conditional*
      (loop-error "~:[This LOOP~;The LOOP ~:*~S~] clause is not permitted inside a conditional." kwd))))

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
