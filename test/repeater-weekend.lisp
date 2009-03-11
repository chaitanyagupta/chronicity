(cl:in-package #:chronicity-repeater-test)

(define-test weekend-next-future
  (let* ((now (make-datetime 2006 8 16 14))
         (weekend (create-tag 'repeater-weekend :weekend :now now))
         (next-weekend (r-next weekend :future)))

    (assert-datetime= (make-datetime 2006 8 19) (span-start next-weekend))
    (assert-datetime= (make-datetime 2006 8 21) (span-end next-weekend))))

(define-test weekend-next-past
  (let* ((now (make-datetime 2006 8 16 14))
         (weekend (create-tag 'repeater-weekend :weekend :now now))
         (next-weekend (r-next weekend :past)))

    (assert-datetime= (make-datetime 2006 8 12) (span-start next-weekend))
    (assert-datetime= (make-datetime 2006 8 14) (span-end next-weekend))))

(define-test weekend-this-future
  (let* ((now (make-datetime 2006 8 16 14))
         (weekend (create-tag 'repeater-weekend :weekend :now now))
         (next-weekend (r-this weekend :future)))

    (assert-datetime= (make-datetime 2006 8 19) (span-start next-weekend))
    (assert-datetime= (make-datetime 2006 8 21) (span-end next-weekend))))

(define-test weekend-this-past
  (let* ((now (make-datetime 2006 8 16 14))
         (weekend (create-tag 'repeater-weekend :weekend :now now))
         (next-weekend (r-this weekend :past)))

    (assert-datetime= (make-datetime 2006 8 12) (span-start next-weekend))
    (assert-datetime= (make-datetime 2006 8 14) (span-end next-weekend))))

(define-test weekend-this-none
  (let* ((now (make-datetime 2006 8 16 14))
         (weekend (create-tag 'repeater-weekend :weekend :now now))
         (next-weekend (r-this weekend :none)))

    (assert-datetime= (make-datetime 2006 8 19) (span-start next-weekend))
    (assert-datetime= (make-datetime 2006 8 21) (span-end next-weekend))))

(define-test weekend-offset
  (let* ((now (make-datetime 2006 8 16 14))
         (span (make-span now (datetime-1+ now :sec)))
         (repeater (create-tag 'repeater-weekend :weekend))
         (offset-span))

    (setf offset-span (r-offset repeater span 3 :future))
    (assert-datetime= (make-datetime 2006 9 2) (span-start offset-span))
    (assert-datetime= (make-datetime 2006 9 2 0 0 1) (span-end offset-span))

    (setf offset-span (r-offset repeater span 1 :past))
    (assert-datetime= (make-datetime 2006 8 12) (span-start offset-span))
    (assert-datetime= (make-datetime 2006 8 12 0 0 1) (span-end offset-span))

    (setf offset-span (r-offset repeater span 0 :future))
    (assert-datetime= (make-datetime 2006 8 12) (span-start offset-span))
    (assert-datetime= (make-datetime 2006 8 12 0 0 1) (span-end offset-span))))