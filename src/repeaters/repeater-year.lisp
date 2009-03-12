(cl:in-package #:chronicity)

(defclass repeater-year (repeater)
  ((current-year :initform nil)))

(defmethod r-next ((repeater repeater-year) pointer)
  (with-slots (current-year now)
      repeater
    (if (not current-year)
        (case pointer
          (:future (setf current-year (year-of (datetime-1+ now :year))))
          (:past (setf current-year (year-of (datetime-1- now :year)))))
        (setf current-year (if (eql pointer :future)
                               (1+ current-year)
                               (1- current-year))))
    (make-span (make-date current-year) (make-date (1+ current-year)))))

(defmethod r-this ((repeater repeater-year) pointer)
  (with-slots (now)
      repeater
    (destructuring-bind (year-start year-end)
        (case pointer
          (:future (list (datetime-1+ (copy-date now) :day)
                         (start-of-year (datetime-1+ now :year))))
          (:past (list (start-of-year now)
                       (copy-date now)))
          (:none (list (start-of-year now)
                       (start-of-year (datetime-1+ now :year)))))
      (make-span year-start year-end))))

(defmethod r-offset ((repeater repeater-year) span amount pointer)
  (span+ span (* amount (if (eql pointer :future) 1 -1)) :year))

(defmethod r-width ((repeater repeater-year))
  +year-seconds+)








