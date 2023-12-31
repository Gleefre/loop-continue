;;;; Copyright 2023 Gleefre
;;;;
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;;
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

(defpackage #:loop-continue
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

(in-package #:loop-continue)

(defun universe-keywords ()
  #-sbcl (loop-universe-keywords *loop-ansi-universe*)
  #+sbcl (slot-value *loop-ansi-universe* 'keywords))

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
    (setf (gethash "TAG" (universe-keywords)) '(loop-tag)
          (gethash "GO" (universe-keywords)) '(loop-go)
          (gethash "CONTINUE" (universe-keywords)) '(loop-continue)
          *enabled* T)))

(defun disable ()
  (when *enabled*
    (remhash "TAG" (universe-keywords))
    (remhash "GO" (universe-keywords))
    (remhash "CONTINUE" (universe-keywords))
    (setf *enabled* NIL)))
