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
                #-sbcl #:*loop-after-body*
                #-sbcl #:*loop-before-loop*
                #-sbcl #:loop-universe-keywords
                #+sbcl #:keywords)
  (:export #:enable
           #:disable))

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

#+sbcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (define-symbol-macro *loop-after-body* (sb-loop::after-body sb-loop::*loop*))
  (define-symbol-macro *loop-before-loop* (sb-loop::before-loop sb-loop::*loop*)))

(defun loop-continue ()
  (unless (member 'continue *loop-after-body*)
    (setf *loop-after-body* (append *loop-after-body* (list 'continue)))
    (setf *loop-before-loop* (append *loop-before-loop* (list (gensym)))))
  (loop-emit-body '(go continue)))

(defparameter *enabled* NIL)

(defun enable ()
  (unless *enabled*
    (setf (gethash "TAG" (uni)) '(loop-tag)
          (gethash "GO" (uni)) '(loop-go)
          (gethash "CONTINUE" (uni)) '(loop-continue)
          *enabled* T)))

(defun disable ()
  (when *enabled*
    (remhash "TAG" (uni))
    (remhash "GO" (uni))
    (remhash "CONTINUE" (uni))
    (setf *enabled* NIL)))
