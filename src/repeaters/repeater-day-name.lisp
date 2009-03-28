;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-day-name.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defclass repeater-day-name (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-day-name) pointer)
  (let ((direction (if (eql pointer :future) 1 -1)))
    (with-slots (current now)
        repeater
      (if (not current)
          (setf current (datetime-incr (copy-date now) :day direction))
          (setf current (datetime-incr current :day direction)))
      (loop
         with dow-index = (dow-index (tag-type repeater))
         while (/= (dow-of current) dow-index)
         do (setf current (datetime-incr current :day direction)))
      (make-span current (datetime-incr current :day) t))))

(defmethod r-this ((repeater repeater-day-name) pointer)
  (when (member pointer (list :future :none))
    (setf pointer :future))
  (r-next repeater pointer))

(defmethod r-width ((repeater repeater-day-name))
  +day-seconds+)