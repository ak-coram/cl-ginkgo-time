;;;; ginkgo-time.asd

(asdf:defsystem #:ginkgo-time
  :description
  "Common Lisp library inspired by the Java API for dates, times, instants, and durations"
  :author "Ákos Kiss <ak@coram.pub>"
  :license  "MIT License"
  :serial t
  :depends-on (#:zoneinfo
               #:chronogram
               #:trivial-clock
               #:ironclad/prngs
               #:frugal-uuid/non-frugal)
  :components ((:file "package")
               (:file "const")
               (:file "generic")
               (:file "chrono-unit")
               (:file "duration")
               (:file "clock")
               (:file "instant")
               (:file "ginkgo-time"))
  :in-order-to ((test-op (test-op "ginkgo-time/test"))))

(asdf:defsystem #:ginkgo-time/test
  :depends-on (#:ginkgo-time
               #:fiveam)
  :components ((:file "ginkgo-time-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :ginkgo-time)))

(asdf:defsystem #:ginkgo-time/*
  :depends-on (#:ginkgo-time
               #:ginkgo-time/test))
