;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-day.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defclass repeater-day (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-day) pointer)
  (with-slots (current now)
      repeater
    (when (not current)
      (setf current (copy-date now)))
    (let ((direction (if (eql pointer :future) 1 -1)))
      (setf current (datetime-incr current :day direction))
      (make-span current (datetime-incr current :day) t))))

(defmethod r-this ((repeater repeater-day) pointer)
  (with-slots (current now)
      repeater
    (ecase pointer
      (:future (make-span now (start-of-day (datetime-incr now :day)) t))
      (:past (make-span (copy-date now) now t))
      (:none (make-span (copy-date now)
                        (start-of-day (datetime-incr now :day))
                        t
                        now)))))

(defmethod r-offset ((repeater repeater-day) span amount pointer)
  (let ((offset (* (if (eql pointer :future) 1 -1) amount)))
    (span+ span offset :day)))

(defmethod r-width ((repeater repeater-day))
  +day-seconds+)
