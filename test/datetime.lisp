(cl:in-package #:chronicity-test)

(define-test datetime-basics
  (let ((ts (make-datetime 2009 3 14 5 45 03)))
    (assert-eql 2009 (year-of ts))
    (assert-eql 3 (month-of ts))
    (assert-eql 14 (day-of ts))
    (assert-eql 5 (hour-of ts))
    (assert-eql 45 (minute-of ts))
    (assert-eql 3 (sec-of ts))
    (assert-eql 6 (dow-of ts)))
  (let ((ts (make-date 2009 3 14)))
    (assert-eql 2009 (year-of ts))
    (assert-eql 3 (month-of ts))
    (assert-eql 14 (day-of ts))
    (assert-true (every #'zerop (list (hour-of ts) (minute-of ts) (sec-of ts))))
    (assert-eql 6 (dow-of ts)))
  (let ((ts (make-time 5 45 03)))
    (assert-eql 5 (hour-of ts))
    (assert-eql 45 (minute-of ts))
    (assert-eql 3 (sec-of ts))))

(define-test datetime-universal-1
  (let* ((ts (now))
         (universal (datetime-to-universal ts)))
    (multiple-value-bind (sec minute hour day month year)
        (decode-universal-time universal)
      (assert-eql (year-of ts) year)
      (assert-eql (month-of ts) month)
      (assert-eql (day-of ts) day)
      (assert-eql (hour-of ts) hour)
      (assert-eql (minute-of ts) minute)
      (assert-eql (sec-of ts) sec))))

(define-test datetime-universal-2
  (let* ((universal (get-universal-time))
         (ts (universal-to-datetime universal)))
    (multiple-value-bind (sec minute hour day month year)
        (decode-universal-time universal)
      (assert-eql (year-of ts) year)
      (assert-eql (month-of ts) month)
      (assert-eql (day-of ts) day)
      (assert-eql (hour-of ts) hour)
      (assert-eql (minute-of ts) minute)
      (assert-eql (sec-of ts) sec))))

(define-test datetime-calc-and-comparisons-1
  (let* ((now (now))
         (sec-ago (datetime-decr now :sec))
         (sec-later (datetime-incr now :sec))
         (minute-ago (datetime-decr now :minute))
         (minute-later (datetime-incr now :minute))
         (hour-ago (datetime-decr now :hour))
         (hour-later (datetime-incr now :hour))
         (day-ago (datetime-decr now :day))
         (day-later (datetime-incr now :day))
         (week-ago (datetime-decr now :week))
         (week-later (datetime-incr now :week))
         (month-ago (datetime-decr now :month))
         (month-later (datetime-incr now :month))
         (year-ago (datetime-decr now :year))
         (year-later (datetime-incr now :year)))
    (assert-true (datetime< year-ago month-ago week-ago day-ago hour-ago minute-ago sec-ago
                            now
                            sec-later minute-later hour-later day-later week-later month-later year-later))
    (assert-true (datetime> year-later month-later week-later day-later hour-later minute-later sec-later
                            now
                            sec-ago minute-ago hour-ago day-ago week-ago month-ago year-ago))

    (assert-datetime= sec-later (datetime-incr sec-ago :sec 2))
    (assert-datetime= minute-ago (datetime-decr minute-later :minute 2))
    (assert-datetime= hour-later (datetime-incr hour-ago :hour 2))
    (assert-datetime= day-ago (datetime-decr day-later :day 2))
    (assert-datetime= week-later (datetime-incr week-ago :week 2))
    (assert-datetime= month-ago (datetime-decr month-later :month 2))
    (assert-datetime= year-later (datetime-incr year-ago :year 2))

    (assert-true (datetime= now now))
    (assert-true (datetime<= now now day-later))
    (assert-true (datetime>= now now day-ago))

    (assert-true (datetime/= day-ago now day-later))))

(define-test datetime-calc-and-comparisons-2
  (let ((ts1 (make-date 2009 3 14))
        (ts2 (make-date 2009 3 15))
        (ts3 (make-time 16 0 0))
        (ts4 (make-time 5 0 0)))
    (assert-true (datetime< ts1 ts2))
    (assert-true (datetime> ts2 ts1))
    (assert-true (datetime< ts4 ts3))
    (assert-true (datetime> ts3 ts4))))

