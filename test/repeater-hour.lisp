;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-hour.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity-test)

(define-test repeater-hour-next-future
  (let* ((now (make-datetime 2006 8 16 14))
         (hours (create-tag 'repeater-hour :hour :now now))
         (next-hour))

    (setf next-hour (r-next hours :future))
    (assert-datetime= (make-datetime 2006 8 16 15) (span-start next-hour))
    (assert-datetime= (make-datetime 2006 8 16 16) (span-end next-hour))
    
    (setf next-hour (r-next hours :future))
    (assert-datetime= (make-datetime 2006 8 16 16) (span-start next-hour))
    (assert-datetime= (make-datetime 2006 8 16 17) (span-end next-hour))))

(define-test repeater-hour-next-past
  (let* ((now (make-datetime 2006 8 16 14))
         (hours (create-tag 'repeater-hour :hour :now now))
         (next-hour))

    (setf next-hour (r-next hours :past))
    (assert-datetime= (make-datetime 2006 8 16 13) (span-start next-hour))
    (assert-datetime= (make-datetime 2006 8 16 14) (span-end next-hour))
    
    (setf next-hour (r-next hours :past))
    (assert-datetime= (make-datetime 2006 8 16 12) (span-start next-hour))
    (assert-datetime= (make-datetime 2006 8 16 13) (span-end next-hour))))

(define-test repeater-hour-this
  (let* ((now (make-datetime 2006 8 16 14 30))
         (hours (create-tag 'repeater-hour :hour :now now))
         (this-hour))

    (setf this-hour (r-this hours :future))
    (assert-datetime= (make-datetime 2006 8 16 14 31) (span-start this-hour))
    (assert-datetime= (make-datetime 2006 8 16 15) (span-end this-hour))
    
    (setf this-hour (r-this hours :past))
    (assert-datetime= (make-datetime 2006 8 16 14) (span-start this-hour))
    (assert-datetime= (make-datetime 2006 8 16 14 30) (span-end this-hour))))

(define-test repeater-hour-offset
  (let* ((now (make-datetime 2006 8 16 14))
         (span (make-span now (datetime-incr now :sec)))
         (offset-span)
         (repeater (create-tag 'repeater-hour :hour)))

    (setf offset-span (r-offset repeater span 3 :future))
    (assert-datetime= (make-datetime 2006 8 16 17) (span-start offset-span))
    (assert-datetime= (make-datetime 2006 8 16 17 0 1) (span-end offset-span))

    (setf offset-span (r-offset repeater span 24 :past))
    (assert-datetime= (make-datetime 2006 8 15 14) (span-start offset-span))
    (assert-datetime= (make-datetime 2006 8 15 14 0 1) (span-end offset-span))))

