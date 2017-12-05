;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-month.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity-test)

(define-test repeater-month-offset
  (let* ((repeater (create-tag 'repeater-month :month)))

    ;; random date
    (let* ((now (make-datetime 2006 8 16 14))
           (span (make-span now (datetime-incr now :hour)))
           offset-span)
      (setf offset-span (r-offset repeater span 1 :future))
      (assert-datetime= (make-datetime 2006 9 16 14) (span-start offset-span))
      (assert-datetime= (make-datetime 2006 9 16 15) (span-end offset-span))
      (setf offset-span (r-offset repeater span 1 :past))
      (assert-datetime= (make-datetime 2006 7 16 14) (span-start offset-span))
      (assert-datetime= (make-datetime 2006 7 16 15) (span-end offset-span)))

    ;; month boundary start
    (let* ((now (make-datetime 2006 1 1 0))
           (span (make-span now (datetime-incr now :hour)))
           offset-span)
      (setf offset-span (r-offset repeater span 1 :future))
      (assert-datetime= (make-datetime 2006 2 1 0) (span-start offset-span))
      (assert-datetime= (make-datetime 2006 2 1 1) (span-end offset-span))
      (setf offset-span (r-offset repeater span 1 :past))
      (assert-datetime= (make-datetime 2005 12 1 0) (span-start offset-span))
      (assert-datetime= (make-datetime 2005 12 1 1) (span-end offset-span)))

    ;; month boundary end
    (let* ((now (make-datetime 2006 1 31 23))
           (span (make-span now (datetime-incr now :hour)))
           offset-span)
      (setf offset-span (r-offset repeater span 1 :future))
      (assert-datetime= (make-datetime 2006 2 28 23) (span-start offset-span))
      (assert-datetime= (make-datetime 2006 3 1 0) (span-end offset-span))
      (setf offset-span (r-offset repeater span 1 :past))
      (assert-datetime= (make-datetime 2005 12 31 23) (span-start offset-span))
      (assert-datetime= (make-datetime 2006 1 1 0) (span-end offset-span)))

    ;; month boundar start - leap year
    (let* ((now (make-datetime 2008 2 1 0))
           (span (make-span now (datetime-incr now :hour)))
           offset-span)
      (setf offset-span (r-offset repeater span 1 :future))
      (assert-datetime= (make-datetime 2008 3 1 0) (span-start offset-span))
      (assert-datetime= (make-datetime 2008 3 1 1) (span-end offset-span))
      (setf offset-span (r-offset repeater span 1 :past))
      (assert-datetime= (make-datetime 2008 1 1 0) (span-start offset-span))
      (assert-datetime= (make-datetime 2008 1 1 1) (span-end offset-span)))))
