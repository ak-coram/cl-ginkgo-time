;;;; ginkgo-time-test.lisp

(defpackage #:ginkgo-time-test
  (:use #:cl #:fiveam))

(in-package #:ginkgo-time-test)

(def-suite :ginkgo-time)
(in-suite :ginkgo-time)

(test instant-now
  (let ((unix-time (- (get-universal-time)
                      trivial-clock:+universal-time-epoch-offset+))
        (now (ginkgo-time:instant-now)))
    (is (<= unix-time (ginkgo-time:instant-to-epoch-second now))))
  (let* ((ginkgo-time:*clock-now* (list 0 0))
         (i (ginkgo-time:instant-now)))
    (ginkgo-time:instant= i ginkgo-time:*epoch-instant*))
  (let* ((ginkgo-time:*clock-now-function* (lambda () (values 0 0)))
         (i (ginkgo-time:instant-now)))
    (ginkgo-time:instant= i ginkgo-time:*epoch-instant*)))

(test instant-epoch-second-conversion
  (let ((i1 (ginkgo-time:instant-of-epoch-second 3 1))
        (i2 (ginkgo-time:instant-of-epoch-second 4 -999999999))
        (i3 (ginkgo-time:instant-of-epoch-second 2 1000000001)))
    (is (ginkgo-time:instant= i1 i2))
    (is (ginkgo-time:instant= i2 i3))
    (loop :for i :in (list i1 i2 i3)
          :do (is (eql 3 (ginkgo-time:instant-to-epoch-second i))))))

(test instant-epoch-milli-conversion
  (let* ((epoch-milli 54321)
         (i (ginkgo-time:instant-of-epoch-milli epoch-milli)))
    (is (eql epoch-milli (ginkgo-time:instant-to-epoch-milli i)))))

(test instant-plus-duration
  (let ((d (ginkgo-time:plus (ginkgo:chrono-unit-duration :days)
                             (ginkgo:chrono-unit-duration :nanos))))
    (is (ginkgo-time:instant= (ginkgo-time:instant-of-epoch-second
                               ginkgo-time:+seconds-per-day+ 1)
                              (ginkgo:plus ginkgo-time:*epoch-instant* d)))))
