(cl:in-package #:chronicity)

(defclass repeater-month-name (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-month-name) pointer)
  (with-slots (current now)
      repeater
    (if (not current)
        (let* ((target-month (month-index (tag-type repeater)))
               (now-month (month-of now))
               (now-year (year-of now))
               (target-year (ecase pointer
                              (:future (cond ((< now-month target-month) now-year)
                                             ((> now-month target-month) (1+ now-year))))
                              (:none (cond ((<= now-month target-month) now-year)
                                           (t (1+ now-year))))
                              (:past (cond ((> now-month target-month) now-year)
                                           ((< now-month target-month) (1- now-year)))))))
          (or target-year (error "TARGET-YEAR should have been set by now."))
          (setf current (make-date target-year target-month)))
        (ecase pointer
          (:future (setf current (datetime-incr current :year 1)))
          (:past (setf current (datetime-decr current :year 1)))))
    (make-span current (datetime-incr current :month 1))))

(defmethod r-this ((repeater repeater-month-name) pointer)
  (ecase pointer
    (:past (r-next repeater :past))
    ((:future :none) (r-next repeater :none))))

(defmethod r-width ((repeater repeater-month-name))
  +month-seconds+)

(defmethod r-index ((repeater repeater-month-name))
  (month-index (tag-type repeater)))