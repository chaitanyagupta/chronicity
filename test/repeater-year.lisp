;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-year.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity-test)

(define-test repeater-year-next-future
  (let* ((now (make-datetime 2006 8 16 14))
         (years (create-tag 'repeater-year :year :now now))
         (next-year))

    (setf next-year (r-next years :future))
    (assert-datetime= (make-datetime 2007 1 1) (span-start next-year))
    (assert-datetime= (make-datetime 2008 1 1) (span-end next-year))

    (setf next-year (r-next years :future))
    (assert-datetime= (make-datetime 2008 1 1) (span-start next-year))
    (assert-datetime= (make-datetime 2009 1 1) (span-end next-year))))

(define-test repeater-year-next-past
  (let* ((now (make-datetime 2006 8 16 14))
         (years (create-tag 'repeater-year :year :now now))
         (next-year))

    (setf next-year (r-next years :past))
    (assert-datetime= (make-datetime 2005 1 1) (span-start next-year))
    (assert-datetime= (make-datetime 2006 1 1) (span-end next-year))

    (setf next-year (r-next years :past))
    (assert-datetime= (make-datetime 2004 1 1) (span-start next-year))
    (assert-datetime= (make-datetime 2005 1 1) (span-end next-year))))

(define-test repeater-year-this
  (let* ((now (make-datetime 2006 8 16 14))
         (years (create-tag 'repeater-year :year :now now))
         (this-year))

    (setf this-year (r-this years :future))
    (assert-datetime= (make-datetime 2006 8 17) (span-start this-year))
    (assert-datetime= (make-datetime 2007 1 1) (span-end this-year))

    (setf this-year (r-this years :past))
    (assert-datetime= (make-datetime 2006 1 1) (span-start this-year))
    (assert-datetime= (make-datetime 2006 8 16) (span-end this-year))))

(define-test repeater-year-offset
  (let* ((now (make-datetime 2006 8 16 14))
         (span (make-span now (datetime-incr now :sec)))
         (repeater (create-tag 'repeater-year :year))
         (offset-span))

    (setf offset-span (r-offset repeater span 3 :future))
    (assert-datetime= (make-datetime 2009 8 16 14) (span-start offset-span))
    (assert-datetime= (make-datetime 2009 8 16 14 0 1) (span-end offset-span))

    (setf offset-span (r-offset repeater span 10 :past))
    (assert-datetime= (make-datetime 1996 8 16 14) (span-start offset-span))
    (assert-datetime= (make-datetime 1996 8 16 14 0 1) (span-end offset-span))))

