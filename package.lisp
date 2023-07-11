;;;; package.lisp

(defpackage #:ginkgo-time
  (:nicknames #:ginkgo)
  (:use #:cl)
  (:export

   ;; const.lisp
   +hours-per-day+
   +minutes-per-hour+
   +minutes-per-day+
   +seconds-per-minute+
   +seconds-per-hour+
   +seconds-per-day+
   +millis-per-second+
   +millis-per-day+
   +micros-per-second+
   +micros-per-day+
   +nanos-per-micro+
   +nanos-per-milli+
   +nanos-per-second+
   +nanos-per-minute+
   +nanos-per-hour+
   +nanos-per-day+

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
   #:instant-now
   #:instant-of-epoch-second
   #:instant-of-epoch-milli
   #:instant-to-epoch-second
   #:instant-to-epoch-milli

   ;; Misc. accessors
   seconds
   nanos))
