(cl:in-package #:chronicity)

(defclass repeater-week (repeater)
  ((current-week-start :initform nil)))

(defmethod r-next ((repeater repeater-week) pointer)
  (with-slots (current-week-start now)
      repeater
    (if (not current-week-start)
        (setf current-week-start
              (case pointer
                (:future (date-time-1+ (start-of-week now) :week))
                (:past (date-time-1- (start-of-week now) :week))))
        (if (eql pointer :future)
            (setf current-week-start (date-time-1+ current-week-start :week))
            (setf current-week-start (date-time-1- current-week-start :week))))
    (make-span current-week-start (date-time+ current-week-start 7 :day))))

(defmethod r-this ((repeater repeater-week) pointer)
  (with-slots (now)
      repeater
    (ecase pointer
      (:future (make-span (date-time-1+ (start-of-hour now) :hour)
                          (date-time-1+ (start-of-week now) :week)))
      (:past (make-span (start-of-week now) (start-of-hour now)))
      (:none (make-span (start-of-week now) (date-time-1+ (start-of-week now) :week))))))

;;; TODO: R-OFFSET?

(defmethod r-width ((repeater repeater-week))
  +week-seconds+)

