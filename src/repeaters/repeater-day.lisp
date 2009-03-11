(cl:in-package #:chronicity)

(defclass repeater-day (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-day) pointer)
  (with-slots (current now)
      repeater
    (when (not current)
      (setf current (copy-date now)))
    (let ((direction (if (eql pointer :future) 1 -1)))
      (setf current (datetime+ current direction :day))
      (make-span current (datetime-1+ current :day)))))

(defmethod r-this ((repeater repeater-day) pointer)
  (with-slots (current now)
      repeater
    (destructuring-bind (day-start day-end)
        (ecase pointer
          (:future (list (start-of-hour (datetime-1+ now :hour))
                         (start-of-day (datetime-1+ now :day))))
          (:past (list (copy-date now)
                       (copy-datetime now :minute 0 :sec 0)))
          (:none (list (copy-date now)
                       (start-of-day (datetime-1+ now :day)))))
      (make-span day-start day-end))))

;;; TODO: R-OFFSET?

(defmethod r-width ((repeater repeater-day))
  +day-seconds+)