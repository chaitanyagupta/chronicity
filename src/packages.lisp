;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; packages.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:cl-user)

(defpackage #:chronicity
  (:use #:cl)
  (:export #:parse
           #:*now*
           #:*context*
           #:*guess*
           #:*ambiguous-time-range*
           #:*endian-preference*
           ;; Date/time
           #:datetime
           #:make-datetime
           #:make-date
           #:year-of
           #:month-of
           #:day-of
           #:hour-of
           #:minute-of
           #:sec-of
           #:dow-of
           ;; Span
           #:span
           #:span-start
           #:span-end
           #:span-end-included-p)
  (:import-from #:cl-ppcre #:scan))

