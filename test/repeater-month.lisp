(cl:in-package #:chronicity-repeater-test)

(define-test month-offset
  (let* ((now (make-datetime 2006 8 16 14))
         (repeater (create-tag 'repeater-month :month))
         (span (make-span now (datetime-1+ now :hour)))
         (offset-span nil))

    ;; future
    (setf offset-span (r-offset repeater span 1 :future))
    (assert-datetime= (make-datetime 2006 9 16 14) (span-start offset-span))
    (assert-datetime= (make-datetime 2006 9 16 15) (span-end offset-span))

    ;; past
    (setf offset-span (r-offset repeater span 1 :past))
    (assert-datetime= (make-datetime 2006 7 16 14) (span-start offset-span))
    (assert-datetime= (make-datetime 2006 7 16 15) (span-end offset-span))))
