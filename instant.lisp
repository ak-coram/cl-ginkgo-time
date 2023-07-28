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

(declaim (ftype (function (instant &optional (unsigned-byte 54))
                          fuuid:uuid)
                instant-to-v8-uuid))
(defun instant-to-v8-uuid (instant &optional padding)
  (multiple-value-bind (millis nanos) (floor (nanos instant)
                                             +nanos-per-milli+)
    (let ((epoch-millis (+ (* (seconds instant) +millis-per-second+) millis)))
      (if (<= 0 epoch-millis #xFFFFFFFFFFFF)
          (fuuid:make-minara-from-components
           epoch-millis
           nanos
           (or padding
               (crypto:strong-random fuuid:+minara-max-random+)))
          (error "~A out of range for UUID encoding" instant)))))

(declaim (ftype (function (fuuid:uuid)
                          (values instant (unsigned-byte 54)))
                instant-from-v8-uuid))
(defun instant-of-v8-uuid (uuid)
  (multiple-value-bind (millis nanos padding) (fuuid:minara-components uuid)
    (multiple-value-bind (seconds millis) (floor millis
                                                 +millis-per-second+)
      (values (instant-of-epoch-second seconds
                                       (+ (* millis +nanos-per-milli+)
                                          nanos))
              padding))))
