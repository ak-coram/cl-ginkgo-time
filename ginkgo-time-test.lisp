;;;; ginkgo-time-test.lisp

(defpackage #:ginkgo-time-test
  (:use #:cl #:fiveam)
  (:local-nicknames (:gt :ginkgo-time)))

(in-package #:ginkgo-time-test)

(def-suite :ginkgo-time)
(in-suite :ginkgo-time)

(test instant-now
  (let ((unix-time (- (get-universal-time)
                      trivial-clock:+universal-time-epoch-offset+))
        (now (gt:instant-now)))
    (is (<= unix-time (gt:instant-to-epoch-second now))))
  (let* ((gt:*clock-now* (list 0 0))
         (i (gt:instant-now)))
    (gt:instant= i gt:*epoch-instant*))
  (let* ((gt:*clock-now-function* (lambda () (values 0 0)))
         (i (gt:instant-now)))
    (gt:instant= i gt:*epoch-instant*)))

(test instant-epoch-second-conversion
  (let ((i1 (gt:instant-of-epoch-second 3 1))
        (i2 (gt:instant-of-epoch-second 4 -999999999))
        (i3 (gt:instant-of-epoch-second 2 1000000001)))
    (is (gt:instant= i1 i2))
    (is (gt:instant= i2 i3))
    (loop :for i :in (list i1 i2 i3)
          :do (is (eql 3 (gt:instant-to-epoch-second i))))))

(test instant-epoch-milli-conversion
  (let* ((epoch-milli 54321)
         (i (gt:instant-of-epoch-milli epoch-milli)))
    (is (eql epoch-milli (gt:instant-to-epoch-milli i)))))

(test instant-v8-uuid-conversion
  (let ((i (gt:instant-now)))
    (gt:instant= i (gt:instant-of-v8-uuid (gt:instant-to-v8-uuid i)))))

(test instant-plus-duration
  (let ((d (gt:plus (ginkgo:chrono-unit-duration :days)
                    (ginkgo:chrono-unit-duration :nanos))))
    (is (gt:instant= (gt:instant-of-epoch-second
                      gt:+seconds-per-day+ 1)
                     (ginkgo:plus gt:*epoch-instant* d)))))
