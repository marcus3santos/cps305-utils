;;;; utils.lisp

(in-package #:cps305-utils)

(defparameter *test-cases* nil)

(defmacro deftest (name params &body body)
  `(defun ,name ,params
     (push (list ',name ',body) *test-cases*)))

