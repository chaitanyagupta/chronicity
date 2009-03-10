(cl:in-package #:chronicity)

(defclass repeater-month (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-month) pointer)
  (let ((offset (if (eql pointer :future) 1 -1)))
    (with-slots (current now)
        repeater
      (if (not current)
          (setf current (start-of-month (date-time+ now offset :month)))
          (setf current (start-of-month (date-time+ current offset :month))))
      (make-span current (date-time-1+ (copy-date current) :month)))))

(defmethod r-this ((repeater repeater-month) pointer)
  (with-slots (now)
      repeater
    (destructuring-bind (month-start month-end)
        (ecase pointer
          (:future (list (date-time-1+ now :day)
                         (date-time-1+ (copy-date now :day 1) :month)))
          (:past (list (copy-date now :day 1)
                       (copy-date now)))
          (:none (list (copy-date now :day 1)
                       (date-time-1+ (copy-date now :day 1) :month))))
      (make-span month-start month-end))))

;;;; TODO: Do we need this?
#|(defmethod r-offset ((repeater repeater-month) span amount pointer)
  (let ((offset (* (if (eql pointer :future) 1 -1) amount)))
    (make-span (date-time+ (span-start span) offset :month)
               (date-time+ (span-end span) offset :month))))|#

(defmethod r-width ((repeater repeater-month))
  +month-seconds+)