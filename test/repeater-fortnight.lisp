(cl:in-package #:chronicity-test)

(define-test repeater-fortnight-next-future
  (let* ((now (make-datetime 2006 8 16 14))
         (fortnights (create-tag 'repeater-fortnight :fortnight :now now))
         (next-fortnight))
    (setf next-fortnight (r-next fortnights :future))
    (assert-datetime= (make-datetime 2006 8 20) (span-start next-fortnight))
    (assert-datetime= (make-datetime 2006 9 3) (span-end next-fortnight))

    (setf next-fortnight (r-next fortnights :future))
    (assert-datetime= (make-datetime 2006 9 3) (span-start next-fortnight))
    (assert-datetime= (make-datetime 2006 9 17) (span-end next-fortnight))))

(define-test repeater-fortnight-next-past
  (let* ((now (make-datetime 2006 8 16 14))
         (fortnights (create-tag 'repeater-fortnight :fortnight :now now))
         (next-fortnight))
    (setf next-fortnight (r-next fortnights :past))
    (assert-datetime= (make-datetime 2006 7 30) (span-start next-fortnight))
    (assert-datetime= (make-datetime 2006 8 13) (span-end next-fortnight))

    (setf next-fortnight (r-next fortnights :past))
    (assert-datetime= (make-datetime 2006 7 16) (span-start next-fortnight))
    (assert-datetime= (make-datetime 2006 7 30) (span-end next-fortnight))))

(define-test repeater-fortnight-this-future
  (let* ((now (make-datetime 2006 8 16 14))
         (fortnights (create-tag 'repeater-fortnight :fortnight :now now))
         (this-fortnight (r-this fortnights :future)))
    (assert-datetime= (make-datetime 2006 8 16 15) (span-start this-fortnight))
    (assert-datetime= (make-datetime 2006 8 27) (span-end this-fortnight))))

(define-test repeater-fortnight-this-past
  (let* ((now (make-datetime 2006 8 16 14))
         (fortnights (create-tag 'repeater-fortnight :fortnight :now now))
         (this-fortnight (r-this fortnights :past)))
    (assert-datetime= (make-datetime 2006 8 13 0) (span-start this-fortnight))
    (assert-datetime= (make-datetime 2006 8 16 14) (span-end this-fortnight))))

(define-test repeater-fortnight-offset
  (let* ((now (make-datetime 2006 8 16 14))
         (span (make-span now (datetime-incr now :sec)))
         (offset-span (r-offset (create-tag 'repeater-fortnight :fortnight)
                                span 3 :future)))
    (assert-datetime= (make-datetime 2006 9 27 14) (span-start offset-span))
    (assert-datetime= (make-datetime 2006 9 27 14 0 1) (span-end offset-span))))



