(cl:in-package #:chronicity)

(defclass repeater-month (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-month) pointer)
  (let ((offset (if (eql pointer :future) 1 -1)))
    (with-slots (current now)
        repeater
      (if (not current)
          (setf current (start-of-month (datetime+ now offset :month)))
          (setf current (start-of-month (datetime+ current offset :month))))
      (make-span current (datetime-1+ (copy-date current) :month)))))

(defmethod r-this ((repeater repeater-month) pointer)
  (with-slots (now)
      repeater
    (destructuring-bind (month-start month-end)
        (ecase pointer
          (:future (list (datetime-1+ now :day)
                         (datetime-1+ (copy-date now :day 1) :month)))
          (:past (list (copy-date now :day 1)
                       (copy-date now)))
          (:none (list (copy-date now :day 1)
                       (datetime-1+ (copy-date now :day 1) :month))))
      (make-span month-start month-end))))

;;;; TODO: Do we need this?
#|(defmethod r-offset ((repeater repeater-month) span amount pointer)
  (let ((offset (* (if (eql pointer :future) 1 -1) amount)))
    (make-span (datetime+ (span-start span) offset :month)
               (datetime+ (span-end span) offset :month))))|#

(defmethod r-width ((repeater repeater-month))
  +month-seconds+)