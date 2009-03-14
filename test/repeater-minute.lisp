(cl:in-package #:chronicity-test)

(define-test repeater-minute-next-future
  (let* ((now (make-datetime 2008 6 25 7 15 30))
         (minutes (create-tag 'repeater-minute :minute :now now))
         (next-minute))

    (setf next-minute (r-next minutes :future))
    (assert-datetime= (make-datetime 2008 6 25 7 16) (span-start next-minute))
    (assert-datetime= (make-datetime 2008 6 25 7 17) (span-end next-minute))

    (setf next-minute (r-next minutes :future))
    (assert-datetime= (make-datetime 2008 6 25 7 17) (span-start next-minute))
    (assert-datetime= (make-datetime 2008 6 25 7 18) (span-end next-minute))))

(define-test repeater-minute-next-past
  (let* ((now (make-datetime 2008 6 25 7 15 30))
         (minutes (create-tag 'repeater-minute :minute :now now))
         (next-minute))

    (setf next-minute (r-next minutes :past))
    (assert-datetime= (make-datetime 2008 6 25 7 14) (span-start next-minute))
    (assert-datetime= (make-datetime 2008 6 25 7 15) (span-end next-minute))

    (setf next-minute (r-next minutes :past))
    (assert-datetime= (make-datetime 2008 6 25 7 13) (span-start next-minute))
    (assert-datetime= (make-datetime 2008 6 25 7 14) (span-end next-minute))))

