;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-month-name.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity-test)

(define-test repeater-month-name-next
  (let* ((now (make-datetime 2006 8 16 14))
         (mays (create-tag 'repeater-month-name :may :now now))
         (decembers (create-tag 'repeater-month-name :december :now now))
         (next-may nil)
         (next-december nil))
    ;; future
    (setf next-may (r-next mays :future))
    (assert-datetime= (make-datetime 2007 5) (span-start next-may))
    (assert-datetime= (make-datetime 2007 6) (span-end next-may))

    (setf next-may (r-next mays :future))
    (assert-datetime= (make-datetime 2008 5) (span-start next-may))
    (assert-datetime= (make-datetime 2008 6) (span-end next-may))

    (setf next-december (r-next decembers :future))
    (assert-datetime= (make-datetime 2006 12) (span-start next-december))
    (assert-datetime= (make-datetime 2007 1) (span-end next-december))

    ;; past
    (setf mays (create-tag 'repeater-month-name :may :now now))

    (setf next-may (r-next mays :past))
    (assert-datetime= (make-datetime 2006 5) (span-start next-may))

    (setf next-may (r-next mays :past))
    (assert-datetime= (make-datetime 2005 5) (span-start next-may))))

(define-test repeater-month-name-this
  (let* ((now (make-datetime 2006 8 16 14))
         (octobers (create-tag 'repeater-month-name :october :now now))
         (this-october (r-this octobers :future))
         (aprils (create-tag 'repeater-month-name :april :now now))
         (this-april (r-this aprils :past)))
    (assert-datetime= (make-datetime 2006 10 1) (span-start this-october))
    (assert-datetime= (make-datetime 2006 11 1) (span-end this-october))
    
    (assert-datetime= (make-datetime 2006 4 1) (span-start this-april))
    (assert-datetime= (make-datetime 2006 5 1) (span-end this-april))))

