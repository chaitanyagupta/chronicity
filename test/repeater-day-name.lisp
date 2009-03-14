(cl:in-package #:chronicity-test)

(define-test repeater-day-name-match
  (let* ((token (create-token "saturday"))
         (repeater (chronicity::scan-for-day-names token)))
    (assert-true (typep repeater 'repeater-day-name) (class-of repeater))
    (assert-eql :saturday (tag-type repeater)))
  (let* ((token (create-token "sunday"))
         (repeater (chronicity::scan-for-day-names token)))
    (assert-true (typep repeater 'repeater-day-name) (class-of repeater))
    (assert-eql :sunday (tag-type repeater))))

(define-test repeater-day-name-next-future
  (let* ((now (make-datetime 2006 8 16 14))
         (mondays (create-tag 'repeater-day-name :monday :now now))
         (span nil))
    (setf span (r-next mondays :future))
    (assert-datetime= (make-datetime 2006 8 21) (span-start span))
    (assert-datetime= (make-datetime 2006 8 22) (span-end span)) 

    (setf span (r-next mondays :future))
    (assert-datetime= (make-datetime 2006 8 28) (span-start span))
    (assert-datetime= (make-datetime 2006 8 29) (span-end span))))

(define-test repeater-day-name-next-past
  (let* ((now (make-datetime 2006 8 16 14))
         (mondays (create-tag 'repeater-day-name :monday :now now))
         (span nil))
    (setf span (r-next mondays :past))
    (assert-datetime= (make-datetime 2006 8 14) (span-start span))
    (assert-datetime= (make-datetime 2006 8 15) (span-end span)) 

    (setf span (r-next mondays :past))
    (assert-datetime= (make-datetime 2006 8 7) (span-start span))
    (assert-datetime= (make-datetime 2006 8 8) (span-end span))))






