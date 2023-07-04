;;;; instant.lisp

(in-package #:ginkgo-time)

(defclass instant ()
  ((seconds :initarg :seconds
            :reader seconds
            :type integer
            :documentation "Number of seconds from the unix epoch")
   (nanos :initarg :nanos
          :reader nanos
          :type (integer 0 999999999)
          :initform 0
          :documentation "Number of nanoseconds"))
  (:documentation "Represent a single instantaneous point in time"))

(declaim (type instant *epoch-instant*)
         (ftype (function (instant instant) boolean) instant=)
         (ftype (function () (values instant &optional))
                instant-now))

(defparameter *epoch-instant* (make-instance 'instant :seconds 0))

(defun instant= (i1 i2)
  (or (eq i1 i2)
      (and (eql (seconds i1) (seconds i2))
           (eql (nanos i1) (nanos i2)))))

(defmethod print-object ((instant instant) stream)
  (print-unreadable-object (instant stream :type t)
    (format stream ":SECONDS ~d :NANOS ~d"
            (seconds instant)
            (nanos instant))))

(defun instant-now ()
  (multiple-value-bind (seconds nanos) (clock-now)
    (make-instance 'instant
                   :seconds seconds
                   :nanos nanos)))
