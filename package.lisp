;;;; package.lisp

(defpackage #:ginkgo-time
  (:nicknames #:ginkgo)
  (:use #:cl)
  (:export

   ;; chrono-unit.lisp
   #:chrono-unit-time-based-p
   #:chrono-unit-date-based-p

   ;; duration.lisp
   #:duration
   #:*zero-duration*
   #:duration-of-days
   #:duration-of-hours
   #:duration-of-minutes
   #:duration-of-seconds
   #:duration-of-millis
   #:duration-of-micros
   #:duration-of-nanos
   #:duration=
   #:chrono-unit-duration))
