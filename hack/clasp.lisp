(in-package #:loop-tag-go)

(defun loop-tag ()
  (core::loop-disallow-conditional :tag)
  (let ((tag (core::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (core::loop-emit-body tag)
        (core::loop-error "A go tag was expected, but ~S found." tag))))

(defun loop-go ()
  (let ((tag (core::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (core::loop-emit-body `(go ,tag))
        (core::loop-error "A go tag was expected, but ~S found." tag))))

(defparameter *hacked!* NIL)

(defun enable! ()
  (unless *hacked!*
    (setf (gethash "TAG" (core::loop-universe-keywords core::*loop-ansi-universe*)) '(loop-tag)
          (gethash "GO" (core::loop-universe-keywords core::*loop-ansi-universe*)) '(loop-go)
          *hacked!* T)))

(defun disable! ()
  (when *hacked!*
    (remhash "TAG" (core::loop-universe-keywords core::*loop-ansi-universe*))
    (remhash "GO" (core::loop-universe-keywords core::*loop-ansi-universe*))
    (setf *hacked!* NIL)))
