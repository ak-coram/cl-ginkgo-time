;;;; chrono-unit.lisp

(in-package #:ginkgo-time)

(deftype time-based-chrono-unit ()
  '(member :nanos :micros :millis :seconds :minutes :hours :half-days))

(deftype date-based-chrono-unit ()
  '(member :days :weeks :months :years :decades :centuries :millenia :eras))

(deftype chrono-unit () '(or time-based-chrono-unit date-based-chrono-unit))

(declaim (ftype (function (chrono-unit) boolean)
                chrono-unit-time-based-p
                chrono-unit-date-based-p))

(defun chrono-unit-time-based-p (chrono-unit)
  (typep chrono-unit 'time-based-chrono-unit))

(defun chrono-unit-date-based-p (chrono-unit)
  (typep chrono-unit 'date-based-chrono-unit))
