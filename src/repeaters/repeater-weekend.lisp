;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-weekend.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defclass repeater-weekend (repeater)
  ((current-weekend-start :initform nil)))

(defmethod r-next ((repeater repeater-weekend) pointer)
  (with-slots (current-weekend-start now)
      repeater
    (if (not current-weekend-start)
        (case pointer
          (:future
           (let ((sat-repeater (create-tag 'repeater-day-name :saturday)))
             (setf (tag-now sat-repeater) now)
             (setf current-weekend-start (span-start (r-next sat-repeater :future)))))
          (:past
           (let ((sat-repeater (create-tag 'repeater-day-name :saturday)))
             (setf (tag-now sat-repeater) (datetime-incr now :day))
             (setf current-weekend-start (span-start (r-next sat-repeater :past))))))
        (let ((direction (if (eql pointer :future) 1 -1)))
          (setf current-weekend-start (datetime-incr current-weekend-start :week direction))))
    (make-span current-weekend-start (datetime-incr current-weekend-start :day 2))))

;;; TODO: We should fix, and understand this better
(defmethod r-this ((repeater repeater-weekend) pointer)
  (with-slots (now)
      repeater
    (case pointer
      ((:future :none)
       (let ((sat-repeater (create-tag 'repeater-day-name :saturday)))
         (setf (tag-now sat-repeater) now)
         (let ((saturday (span-start (r-next sat-repeater :future))))
           (make-span saturday (datetime-incr saturday :day 2)))))
      (:past
       (let ((sat-repeater (create-tag 'repeater-day-name :saturday)))
         (setf (tag-now sat-repeater) now)
         (let ((saturday (span-start (r-next sat-repeater :past))))
           (make-span saturday (datetime-incr saturday :day 2))))))))

(defmethod r-offset ((repeater repeater-weekend) span amount pointer)
  (let* ((direction (if (eql pointer :future) 1 -1))
         (weekend-repeater (create-tag 'repeater-weekend
                                       :weekend
                                       :now (span-start span)))
         (start (datetime-incr (span-start (r-next weekend-repeater pointer))
                           :week
                           (* (1- amount) direction))))
    (make-span start (datetime-incr start :sec (span-width span)))))

(defmethod r-width ((repeater repeater-weekend))
  +weekend-seconds+)

