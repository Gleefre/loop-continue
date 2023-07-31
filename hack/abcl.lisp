(in-package #:loop-tag-go)

(defun loop-tag ()
  (loop::loop-disallow-conditional :tag)
  (let ((tag (loop::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (loop::loop-emit-body tag)
        (loop::loop-error "A go tag was expected, but ~S found." tag))))

(defun loop-go ()
  (let ((tag (loop::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (loop::loop-emit-body `(go ,tag))
        (loop::loop-error "A go tag was expected, but ~S found." tag))))

(defparameter *hacked!* NIL)

(defun enable! ()
  (unless *hacked!*
    (setf (gethash "TAG" (loop::loop-universe-keywords loop::*loop-ansi-universe*)) '(loop-tag)
          (gethash "GO" (loop::loop-universe-keywords loop::*loop-ansi-universe*)) '(loop-go)
          *hacked!* T)))

(defun disable! ()
  (when *hacked!*
    (remhash "TAG" (loop::loop-universe-keywords loop::*loop-ansi-universe*))
    (remhash "GO" (loop::loop-universe-keywords loop::*loop-ansi-universe*))
    (setf *hacked!* NIL)))
