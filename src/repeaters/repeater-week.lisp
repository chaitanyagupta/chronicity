(cl:in-package #:chronicity)

(defclass repeater-week (repeater)
  ((current-week-start :initform nil)))

(defmethod r-next ((repeater repeater-week) pointer)
  (with-slots (current-week-start now)
      repeater
    (if (not current-week-start)
        (setf current-week-start
              (case pointer
                (:future (datetime-1+ (start-of-week now) :week))
                (:past (datetime-1- (start-of-week now) :week))))
        (if (eql pointer :future)
            (setf current-week-start (datetime-1+ current-week-start :week))
            (setf current-week-start (datetime-1- current-week-start :week))))
    (make-span current-week-start (datetime+ current-week-start 7 :day))))

(defmethod r-this ((repeater repeater-week) pointer)
  (with-slots (now)
      repeater
    (ecase pointer
      (:future (make-span (datetime-1+ (start-of-hour now) :hour)
                          (datetime-1+ (start-of-week now) :week)))
      (:past (make-span (start-of-week now) (start-of-hour now)))
      (:none (make-span (start-of-week now) (datetime-1+ (start-of-week now) :week))))))

(defmethod r-offset ((repeater repeater-week) span amount pointer)
  (span+ span (* amount (if (eql pointer :future) 1 -1)) :week))

(defmethod r-width ((repeater repeater-week))
  +week-seconds+)

