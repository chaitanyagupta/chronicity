;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-week.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defclass repeater-week (repeater)
  ((current-week-start :initform nil)))

(defmethod r-next ((repeater repeater-week) pointer)
  (with-slots (current-week-start now)
      repeater
    (if (not current-week-start)
        (setf current-week-start
              (case pointer
                (:future (datetime-incr (start-of-week now) :week))
                (:past (datetime-decr (start-of-week now) :week))))
        (if (eql pointer :future)
            (datetime-incf current-week-start :week)
            (datetime-decf current-week-start :week)))
    (make-span current-week-start (datetime-incr current-week-start :day 7))))

(defmethod r-this ((repeater repeater-week) pointer)
  (with-slots (now)
      repeater
    (ecase pointer
      (:future (make-span now (datetime-incr (start-of-week now) :week)))
      (:past (make-span (start-of-week now) now))
      (:none (make-span (start-of-week now)
                        (datetime-incr (start-of-week now) :week)
                        nil
                        now)))))

(defmethod r-offset ((repeater repeater-week) span amount pointer)
  (span+ span (* amount (if (eql pointer :future) 1 -1)) :week))

(defmethod r-width ((repeater repeater-week))
  +week-seconds+)

