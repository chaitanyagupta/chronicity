(cl:in-package #:chronicity)

(defclass repeater-day-name (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-day-name) pointer)
  (let ((direction (if (eql pointer :future) 1 -1)))
    (with-slots (current now)
        repeater
      (if (not current)
          (setf current (datetime+ (copy-date now) direction :day))
          (setf current (datetime+ current direction :day)))
      (loop
         with dow-index = (dow-index (tag-type repeater))
         while (/= (dow-of current) dow-index)
         do (setf current (datetime+ current direction :day)))
      (make-span current (datetime-1+ current :day)))))

(defmethod r-this ((repeater repeater-day-name) pointer)
  (when (member pointer (list :future :none))
    (setf pointer :future))
  (r-next repeater pointer))

(defmethod r-width ((repeater repeater-day-name))
  +day-seconds+)