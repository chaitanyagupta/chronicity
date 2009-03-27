;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-time.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity-test)

(define-test repeater-time-next-future
  (let* ((now (make-datetime 2006 8 16 14))
         (tr (create-tag 'repeater-time "4:00" :now now)))

    (assert-datetime= (make-datetime 2006 8 16 16) (span-start (r-next tr :future)))
    (assert-datetime= (make-datetime 2006 8 17 4) (span-start (r-next tr :future)))

    (setf tr (create-tag 'repeater-time "13:00" :now now))
    (assert-datetime= (make-datetime 2006 8 17 13) (span-start (r-next tr :future)))
    (assert-datetime= (make-datetime 2006 8 18 13) (span-start (r-next tr :future)))

    (setf tr (create-tag 'repeater-time "0400" :now now))
    (assert-datetime= (make-datetime 2006 8 17 4) (span-start (r-next tr :future)))
    (assert-datetime= (make-datetime 2006 8 18 4) (span-start (r-next tr :future)))))

(define-test repeater-time-next-past
  (let* ((now (make-datetime 2006 8 16 14))
         (tr (create-tag 'repeater-time "4:00" :now now)))

    (assert-datetime= (make-datetime 2006 8 16 4) (span-start (r-next tr :past)))
    (assert-datetime= (make-datetime 2006 8 15 16) (span-start (r-next tr :past)))

    (setf tr (create-tag 'repeater-time "13:00" :now now))
    (assert-datetime= (make-datetime 2006 8 16 13) (span-start (r-next tr :past)))
    (assert-datetime= (make-datetime 2006 8 15 13) (span-start (r-next tr :past)))))

(define-test repeater-time-test-type
  (let ((tr nil))
    (flet ((!create-tag (str)
             (create-tag 'repeater-time str))
           (!time (tr)
             (tick-time (tag-type tr))))
      (setf tr (!create-tag "4"))
      (assert-datetime= (make-time 4) (!time tr))
    
      (setf tr (!create-tag "14"))
      (assert-datetime= (make-time 14) (!time tr))

      (setf tr (!create-tag "4:00"))
      (assert-datetime= (make-time 4) (!time tr))

      (setf tr (!create-tag "4:30"))
      (assert-datetime= (make-time 4 30) (!time tr))

      (setf tr (!create-tag "1400"))
      (assert-datetime= (make-time 14) (!time tr))

      (setf tr (!create-tag "0400"))
      (assert-datetime= (make-time 4) (!time tr))

      (setf tr (!create-tag "04"))
      (assert-datetime= (make-time 4) (!time tr))

      (setf tr (!create-tag "400"))
      (assert-datetime= (make-time 4) (!time tr)))))

