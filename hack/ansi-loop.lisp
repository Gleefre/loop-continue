(in-package #:loop-tag-go)

(defun loop-tag ()
  (ansi-loop::loop-disallow-conditional :tag)
  (let ((tag (ansi-loop::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (ansi-loop::loop-emit-body tag)
        (ansi-loop::loop-error "A go tag was expected, but ~S found." tag))))

(defun loop-go ()
  (let ((tag (ansi-loop::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (ansi-loop::loop-emit-body `(go ,tag))
        (ansi-loop::loop-error "A go tag was expected, but ~S found." tag))))

(defparameter *hacked!* NIL)

(defun enable! ()
  (unless *hacked!*
    (setf (gethash "TAG" (ansi-loop::loop-universe-keywords ansi-loop::*loop-ansi-universe*)) '(loop-tag)
          (gethash "GO" (ansi-loop::loop-universe-keywords ansi-loop::*loop-ansi-universe*)) '(loop-go)
          *hacked!* T)))

(defun disable! ()
  (when *hacked!*
    (remhash "TAG" (ansi-loop::loop-universe-keywords ansi-loop::*loop-ansi-universe*))
    (remhash "GO" (ansi-loop::loop-universe-keywords ansi-loop::*loop-ansi-universe*))
    (setf *hacked!* NIL)))
