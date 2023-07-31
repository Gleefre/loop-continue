(in-package #:loop-tag-go)

(defun loop-tag ()
  (sb-loop::loop-disallow-conditional :tag)
  (let ((tag (sb-loop::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (sb-loop::loop-emit-body tag)
        (sb-loop::loop-error "A go tag was expected, but ~S found." tag))))

(defun loop-go ()
  (let ((tag (sb-loop::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (sb-loop::loop-emit-body `(go ,tag))
        (sb-loop::loop-error "A go tag was expected, but ~S found." tag))))

(defparameter *hacked!* NIL)

(defun enable! ()
  (unless *hacked!*
    (setf (gethash "TAG" (slot-value sb-loop::*loop-ansi-universe* 'sb-loop::keywords)) '(loop-tag)
          (gethash "GO" (slot-value sb-loop::*loop-ansi-universe* 'sb-loop::keywords)) '(loop-go)
          *hacked!* T)))

(defun disable! ()
  (when *hacked!*
    (remhash "TAG" (slot-value sb-loop::*loop-ansi-universe* 'sb-loop::keywords))
    (remhash "GO" (slot-value sb-loop::*loop-ansi-universe* 'sb-loop::keywords))
    (setf *hacked!* NIL)))
