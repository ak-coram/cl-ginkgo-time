;;;; clock.lisp

(in-package #:ginkgo-time)

(defparameter *clock-now* nil)
(defparameter *clock-now-function* nil)

(defun clock-now ()
  (cond
   (*clock-now* (values-list *clock-now*))
   (*clock-now-function* (funcall *clock-now-function*))
   (t (trivial-clock:now))))
