;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; chronicity.asd

;;; See the LICENSE file for licensing information.

(cl:defpackage #:chronicity-system
  (:use #:cl #:asdf))

(cl:in-package #:chronicity-system)

(defsystem #:chronicity
  :version "0.2.2"
  :author "Chaitanya Gupta"
  :maintainer "Chaitanya Gupta"
  :depends-on (:cl-ppcre :cl-interpol :local-time)
  :components
  ((:module src
            :serial t
            :components
            ((:file "packages")
             (:file "datetime")
             (:file "utils")
             (:file "numerize")
             (:file "chronicity")
             (:file "repeater")
             (:module repeaters
                      :components
                      ((:file "repeater-year")
                       ;; (:file "repeater-season")
                       ;; (:file "repeater-season-name")
                       (:file "repeater-month")
                       (:file "repeater-month-name")
                       (:file "repeater-fortnight")
                       (:file "repeater-week")
                       (:file "repeater-weekend")
                       (:file "repeater-day")
                       (:file "repeater-day-name")
                       (:file "repeater-day-portion")
                       (:file "repeater-hour")
                       (:file "repeater-minute")
                       (:file "repeater-second")
                       (:file "repeater-time")))
             (:file "grabber")
             (:file "pointer")
             (:file "scalar")
             (:file "ordinal")
             (:file "separator")
             (:file "handlers")
             (:file "handler-defs")))))

(defmethod perform ((o test-op) (c (eql (find-system :chronicity))))
  (operate 'load-op :chronicity-test)
  (funcall (intern (symbol-name :run-suite) :chronicity-test)))
