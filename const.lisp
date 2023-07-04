;;;; const.lisp

(in-package #:ginkgo-time)

(defconstant +hours-per-day+ 24)
(defconstant +minutes-per-hour+ 60)
(defconstant +minutes-per-day+ (* +minutes-per-hour+ +hours-per-day+))
(defconstant +seconds-per-minute+ 60)
(defconstant +seconds-per-hour+ (* +seconds-per-minute+ +minutes-per-hour+))
(defconstant +seconds-per-day+ (* +seconds-per-hour+ +hours-per-day+))
(defconstant +millis-per-second+ 1000)
(defconstant +millis-per-day+ (* +millis-per-second+ +seconds-per-day+))
(defconstant +micros-per-second+ 1000000)
(defconstant +micros-per-day+ (* +micros-per-second+ +seconds-per-day+))
(defconstant +nanos-per-micro+ 1000)
(defconstant +nanos-per-milli+ 1000000)
(defconstant +nanos-per-second+ 1000000000)
(defconstant +nanos-per-minute+ (* +nanos-per-second+ +seconds-per-minute+))
(defconstant +nanos-per-hour+ (* +nanos-per-minute+ +minutes-per-hour+))
(defconstant +nanos-per-day+ (* +nanos-per-hour+ +hours-per-day+))

(defconstant +universal-time-epoch-offset+
  (encode-universal-time 0 0 0 1 1 1970 0))
