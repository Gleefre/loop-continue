(in-package #:loop-tag-go)

(defun loop-tag ()
  (excl::loop-disallow-conditional :tag)
  (let ((tag (excl::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (excl::loop-emit-body tag)
        (excl::loop-error "A go tag was expected, but ~S found." tag))))

(defun loop-go ()
  (let ((tag (excl::loop-get-form)))
    (if (or (symbolp tag) (integerp tag))
        (excl::loop-emit-body `(go ,tag))
        (excl::loop-error "A go tag was expected, but ~S found." tag))))

(defparameter *hacked!* NIL)

(defun enable! ()
  (unless *hacked!*
    (setf (gethash "TAG" (excl::loop-universe-keywords excl::*loop-ansi-universe*)) '(loop-tag)
          (gethash "GO" (excl::loop-universe-keywords excl::*loop-ansi-universe*)) '(loop-go)
          *hacked!* T)))

(defun disable! ()
  (when *hacked!*
    (remhash "TAG" (excl::loop-universe-keywords excl::*loop-ansi-universe*))
    (remhash "GO" (excl::loop-universe-keywords excl::*loop-ansi-universe*))
    (setf *hacked!* NIL)))
