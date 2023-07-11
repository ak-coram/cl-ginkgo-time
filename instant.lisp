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
                instant-now)
         (ftype (function (integer &optional integer)
                          (values instant &optional))
                instant-of-epoch-second)
         (ftype (function (integer)
                          (values instant &optional))
                instant-of-epoch-milli)
         (ftype (function (instant) (values integer &optional))
                instant-to-epoch-second
                instant-to-epoch-milli))

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

(defmethod plus ((i instant) (d duration))
  (multiple-value-bind (seconds-carry nanos)
      (floor (+ (nanos i) (nanos d)) +nanos-per-second+)
    (make-instance 'instant
                   :seconds (+ (seconds i) (seconds d) seconds-carry)
                   :nanos nanos)))

(defun instant-of-epoch-second (epoch-second &optional nano-adjustment)
  (if nano-adjustment
      (multiple-value-bind (seconds-carry nanos)
          (floor nano-adjustment +nanos-per-second+)
        (make-instance 'instant :seconds (+ epoch-second seconds-carry)
                                :nanos nanos))
      (make-instance 'instant :seconds epoch-second)))

(defun instant-of-epoch-milli (epoch-milli)
  (multiple-value-bind (seconds millis)
      (floor epoch-milli +millis-per-second+)
    (make-instance 'instant :seconds seconds
                            :nanos (* millis +nanos-per-milli+))))

(defun instant-to-epoch-second (instant)
  (seconds instant))

(defun instant-to-epoch-milli (instant)
  (+ (* (seconds instant) +millis-per-second+)
     (floor (nanos instant) +nanos-per-milli+)))
