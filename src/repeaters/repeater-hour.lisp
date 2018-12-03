;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-hour.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defclass repeater-hour (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-hour) pointer)
  (with-slots (current now)
      repeater
    (if (not current)
        (case pointer
          (:future (setf current (start-of-hour (datetime-incr now :hour))))
          (:past   (setf current (start-of-hour (datetime-decr now :hour)))))
        (case pointer
          (:future (datetime-incf current :hour))
          (:past   (datetime-decf current :hour))))
    (make-span current (datetime-incr current :hour))))

(defmethod r-this ((repeater repeater-hour) pointer)
  (with-slots (now)
      repeater
    (case pointer
      (:future (make-span now
                          (start-of-hour (datetime-incr now :hour))))
      (:past   (make-span (start-of-hour now)
                          now))
      (:none   (make-span (start-of-hour now)
                          (start-of-hour (datetime-incr now :hour))
                          nil
                          now)))))

(defmethod r-offset ((repeater repeater-hour) span amount pointer)
  (span+ span (* amount (if (eql pointer :future) 1 -1)) :hour))

(defmethod r-width ((repeater repeater-hour))
  +hour-seconds+)

