;;;; duration.lisp

(in-package #:ginkgo-time)

(defclass duration ()
  ((seconds :initarg :seconds
            :accessor seconds
            :type integer
            :documentation "Number of seconds in the duration")
   (nanos :initarg :nanos
          :accessor nanos
          :type (integer 0 999999999)
          :initform 0
          :documentation "Number of nanoseconds in the duration"))
  (:documentation "Represent an amount of time in terms of seconds and nanoseconds"))

(declaim (type duration *zero-duration*)
         (ftype (function (duration duration) boolean) duration=)
         (ftype (function (integer &optional integer)
                          (values duration &optional))
                duration-of-seconds)
         (ftype (function (integer)
                          (values duration &optional))
                duration-of-days
                duration-of-hours
                duration-of-minutes
                duration-of-millis
                duration-of-micros
                duration-of-nanos)
         (ftype (function (chrono-unit) duration) chrono-unit-duration))

(defparameter *zero-duration* (make-instance 'duration :seconds 0))

(defun duration= (d1 d2)
  (or (eq d1 d2)
      (and (eql (seconds d1) (seconds d2))
           (eql (nanos d1) (nanos d2)))))

(defmethod print-object ((duration duration) stream)
  (print-unreadable-object (duration stream :type t)
    (format stream ":SECONDS ~d :NANOS ~d"
            (seconds duration)
            (nanos duration))))

(defmethod plus ((d1 duration) (d2 duration))
  (duration-of-seconds (+ (seconds d1) (seconds d2))
                       (+ (nanos d1) (nanos d2))))

(defmethod to-string ((d duration))
  (if (duration= d *zero-duration*)
      "PT0S"
      (let* ((seconds (seconds d))
             (nanos (nanos d))
             (is-negative (minusp seconds))
             (total-seconds (if (and (minusp seconds)
                                     (plusp nanos))
                                (1+ seconds)
                                seconds))
             (hours (truncate total-seconds +seconds-per-hour+))
             (positive-total-seconds (abs total-seconds))
             (minutes (truncate (mod positive-total-seconds +seconds-per-hour+)
                                +seconds-per-minute+))
             (remaining-seconds (mod positive-total-seconds
                                     +seconds-per-minute+))
             (has-hours (not (zerop hours)))
             (has-minutes (not (zerop minutes)))
             (has-seconds (not (zerop remaining-seconds)))
             (has-nanos (not (zerop nanos)))
             (fraction (when has-nanos
                         (string-right-trim
                          '(#\0) 
                          (subseq (format nil "~d"
                                          (if is-negative
                                              (- (* 2 +nanos-per-second+) nanos)
                                              (+ +nanos-per-second+ nanos)))
                                  1)))))
        (format nil "PT~{~a~}"
                `(,@(when has-hours (list hours 'h))
                  ,@(when has-minutes
                      (append (when is-negative '(#\-)) (list minutes 'm)))
                  ,@(when (or has-seconds
                              has-nanos
                              (and (not has-hours) 
                                   (not has-minutes)))
                      (append (when is-negative '(#\-))
                              `(,remaining-seconds
                                ,@(when has-nanos
                                    (list #\. fraction))
                                s))))))))

(defun duration-of-days (days)
  (make-instance 'duration :seconds (* days +seconds-per-day+)))

(defun duration-of-hours (hours)
  (make-instance 'duration :seconds (* hours +seconds-per-hour+)))

(defun duration-of-minutes (minutes)
  (make-instance 'duration :seconds (* minutes +seconds-per-minute+)))

(defun duration-of-seconds (seconds &optional nanos)
  (if nanos
      (multiple-value-bind (second-offset remainder)
          (floor nanos +nanos-per-second+)
        (make-instance 'duration :seconds (+ seconds second-offset)
                                 :nanos remainder))
      (make-instance 'duration :seconds seconds)))

(defun duration-of-millis (millis)
  (multiple-value-bind (seconds remainder)
      (floor millis +millis-per-second+)
    (make-instance 'duration :seconds seconds
                             :nanos (* remainder +nanos-per-milli+))))

(defun duration-of-micros (micros)
  (multiple-value-bind (seconds remainder)
      (floor micros +micros-per-second+)
    (make-instance 'duration :seconds seconds
                             :nanos (* remainder +nanos-per-micro+))))

(defun duration-of-nanos (nanos)
  (multiple-value-bind (seconds remainder)
      (floor nanos +nanos-per-second+)
    (make-instance 'duration :seconds seconds
                             :nanos remainder)))

(defun duration-of (&key days hours minutes seconds millis nanos)
  (loop :with result := *zero-duration*
        :for d :in (list (when days (duration-of-days days))
                         (when hours (duration-of-hours hours))
                         (when minutes (duration-of-minutes minutes))
                         (when seconds (duration-of-seconds seconds))
                         (when millis (duration-of-millis millis))
                         (when nanos (duration-of-nanos nanos)))
        :when d
          :do (setf result (plus result d))
        :finally (return result)))

(let* ((seconds-in-year 31556952)
       (nanos (duration-of-nanos 1))
       (micros (duration-of-micros 1))
       (millis (duration-of-millis 1))
       (seconds (duration-of-seconds 1))
       (minutes (duration-of-minutes 1))
       (hours (duration-of-hours 1))
       (half-days (duration-of-hours 12))
       (days (duration-of-days 1))
       (weeks (duration-of-days 7))
       (months (duration-of-seconds (/ seconds-in-year 12)))
       (years (duration-of-seconds seconds-in-year))
       (decades (duration-of-seconds (* seconds-in-year 10)))
       (centuries (duration-of-seconds (* seconds-in-year 100)))
       (millenia (duration-of-seconds (* seconds-in-year 1000)))
       (eras (duration-of-seconds (* seconds-in-year 1000000000))))
  (defun chrono-unit-duration (chrono-unit)
    (ecase chrono-unit
      (:nanos nanos)
      (:micros micros)
      (:millis millis)
      (:seconds seconds)
      (:minutes minutes)
      (:hours hours)
      (:half-days half-days)
      (:days days)
      (:weeks weeks)
      (:months months)
      (:years years)
      (:decades decades)
      (:centuries centuries)
      (:millenia millenia)
      (:eras eras))))
