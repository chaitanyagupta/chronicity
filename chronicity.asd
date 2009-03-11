;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(cl:defpackage #:chronicity-system
  (:use #:cl #:asdf))

(cl:in-package #:chronicity-system)

(defsystem #:chronicity
  :depends-on (:cl-ppcre :cl-interpol)
  :serial t
  :components ((:file "packages")
               (:file "datetime")
               (:file "utils")
               (:file "numerize")
               (:file "chronicity")
               (:file "repeater")
               (:module repeaters
                        (:components ((:file "repeater-year")
                                      ;; (:file "repeater_season")
                                      ;; (:file "repeater_season_name")
                                      (:file "repeater_month")
                                      (:file "repeater_month_name")
                                      ;; (:file "repeater_fortnight")
                                      (:file "repeater_week")
                                      (:file "repeater_weekend")
                                      (:file "repeater_day")
                                      (:file "repeater_day_name")
                                      (:file "repeater_day_portion")
                                      ;; (:file "repeater_hour")
                                      ;; (:file "repeater_minute")
                                      ;; (:file "repeater_second")
                                      (:file "repeater_time"))))
               (:file "grabber")
               (:file "pointer")
               (:file "scalar")
               (:file "ordinal")
               (:file "separator")))

(asdf:defsystem #:chronicity-test
  :depends-on (:chronicity :rt)
  :serial t
  :components ((:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :chronicity))))
  (operate 'load-op :chronicity-test)
  (funcall (intern (symbol-name :run-all-tests) :chronicity-test)))