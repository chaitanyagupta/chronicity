;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-minute.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defclass repeater-minute (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-minute) pointer)
  (with-slots (current now)
      repeater
    (if (not current)
        (case pointer
          (:future (setf current (start-of-minute (datetime-incr now :minute))))
          (:past   (setf current (start-of-minute (datetime-decr now :minute)))))
        (case pointer
          (:future (datetime-incf current :minute))
          (:past   (datetime-decf current :minute))))
    (make-span current (datetime-incr current :minute))))

(defmethod r-this ((repeater repeater-minute) pointer)
  (with-slots (now)
      repeater
    (case pointer
      (:future (make-span now
                          (start-of-minute (datetime-incr now :minute))))
      (:past   (make-span (start-of-minute now)
                          now))
      (:none   (make-span (start-of-minute now)
                          (start-of-minute (datetime-incr now :minute)))))))

(defmethod r-offset ((repeater repeater-minute) span amount pointer)
  (span+ span (* amount (if (eql pointer :future) 1 -1)) :minute))

(defmethod r-width ((repeater repeater-minute))
  +minute-seconds+)

