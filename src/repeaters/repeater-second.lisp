(cl:in-package #:chronicity)

(defclass repeater-sec (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-sec) pointer)
  (with-slots (current now)
      repeater
    (if (not current)
        (case pointer
          (:future (setf current (datetime-incr now :sec)))
          (:past   (setf current (datetime-decr now :sec))))
        (case pointer
          (:future (datetime-incf current :sec))
          (:past   (datetime-decf current :sec))))
    (make-span current (datetime-incr current :sec))))

(defmethod r-this ((repeater repeater-sec) pointer)
  (with-slots (now)
      repeater
    (make-span now (datetime-incr now :sec))))

(defmethod r-offset ((repeater repeater-sec) span amount pointer)
  (span+ span (* amount (if (eql pointer :future) 1 -1)) :sec))

(defmethod r-width ((repeater repeater-sec))
  +sec-seconds+)

