;;;; clock.lisp

(in-package #:ginkgo-time)

#+linux
(progn
  (cffi:defcstruct linux-timespec
    (tv-sec :uint64)
    (tv-nsec :uint64))

  (cffi:defcfun clock-gettime :int
    (clock-id :int)
    (out-timespec (:pointer (:struct linux-timespec))))

  (defun get-linux-realtime ()
    (cffi:with-foreign-object (p-timespec '(:pointer (:struct linux-timespec)))
      (clock-gettime 0 p-timespec) ;; Use CLOCK_REALTIME
      (cffi:with-foreign-slots ((tv-sec tv-nsec)
                                p-timespec
                                (:struct linux-timespec))
        (values tv-sec tv-nsec)))))

(defun clock-now ()
  ;; Use more accurate clock on Linux
  #+linux (get-linux-realtime)
  #-linux
  (values (- (get-universal-time)
             +universal-time-epoch-offset+)
          0))
