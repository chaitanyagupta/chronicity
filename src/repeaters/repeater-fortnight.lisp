;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-fortnight.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defclass repeater-fortnight (repeater)
  ((current-fortnight-start :initform nil)))

(defmethod r-next ((repeater repeater-fortnight) pointer)
  (with-slots (current-fortnight-start now)
      repeater
    (let ((sunday (start-of-week now)))
      (if (not current-fortnight-start)
          (case pointer
            (:future (setf current-fortnight-start (datetime-incr sunday :week)))
            (:past   (setf current-fortnight-start (datetime-decr sunday :week 2))))
          (let ((amount (* 2 (if (eql pointer :future) 1 -1))))
            (datetime-incf current-fortnight-start :week amount)))
      (make-span current-fortnight-start
                 (datetime-incr current-fortnight-start :week 2)))))

(defmethod r-this ((repeater repeater-fortnight) pointer)
  (with-slots (now)
      repeater
    (let ((sunday (start-of-week now)))
      (case pointer
        (:future (make-span (start-of-hour (datetime-incr now :hour))
                            (datetime-incr sunday :week 2)))
        (:past   (make-span sunday (start-of-hour now)))
        (:none   (make-span sunday (datetime-incr sunday :week 2)))))))

(defmethod r-offset ((repeater repeater-fortnight) span amount pointer)
  (span+ span (* 2 amount (if (eql pointer :future) 1 -1)) :week))

(defmethod r-width ((repeater repeater-fortnight))
  +fortnight-seconds+)

