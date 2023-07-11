;;;; package.lisp

(defpackage #:ginkgo-time
  (:nicknames #:ginkgo)
  (:use #:cl)
  (:export
   ;; generic.lisp
   #:plus
   #:to-string

   ;; chrono-unit.lisp
   #:chrono-unit-time-based-p
   #:chrono-unit-date-based-p

   ;; duration.lisp
   #:*zero-duration*
   #:duration-of-days
   #:duration-of-hours
   #:duration-of-minutes
   #:duration-of-seconds
   #:duration-of-millis
   #:duration-of-micros
   #:duration-of-nanos
   #:duration=
   #:chrono-unit-duration

   ;; clock.lisp
   #:*clock-now*
   #:*clock-now-function*
   #:clock-now

   ;; instant.lisp
   #:*epoch-instant*
   #:instant=
   #:instant-now))
