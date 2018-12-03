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
           ;; Datetime
           #:datetime
           #:make-datetime
           #:make-date
           #:make-time
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
           #:span-end-included-p
           #:span-default
           ;; Miscellaneous datetime data
           #:month-name
           #:dow-name
           ;; Token
           #:token-word
           #:token-tags)
  (:import-from #:cl-ppcre #:scan))

