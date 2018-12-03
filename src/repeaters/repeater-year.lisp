;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-year.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defclass repeater-year (repeater)
  ((current-year :initform nil)))

(defmethod r-next ((repeater repeater-year) pointer)
  (with-slots (current-year now)
      repeater
    (if (not current-year)
        (case pointer
          (:future (setf current-year (year-of (datetime-incr now :year))))
          (:past (setf current-year (year-of (datetime-decr now :year)))))
        (setf current-year (if (eql pointer :future)
                               (1+ current-year)
                               (1- current-year))))
    (make-span (make-date current-year) (make-date (1+ current-year)))))

(defmethod r-this ((repeater repeater-year) pointer)
  (with-slots (now)
      repeater
    (case pointer
      (:future (make-span now (start-of-year (datetime-incr now :year))))
      (:past (make-span (start-of-year now) now))
      (:none (make-span (start-of-year now)
                        (start-of-year (datetime-incr now :year)))))))

(defmethod r-offset ((repeater repeater-year) span amount pointer)
  (span+ span (* amount (if (eql pointer :future) 1 -1)) :year))

(defmethod r-width ((repeater repeater-year))
  +year-seconds+)








