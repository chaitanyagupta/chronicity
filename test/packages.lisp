;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; packages.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:cl-user)

(defpackage #:chronicity-test
  (:use #:cl #:lisp-unit #:chronicity)
  (:export #:run-suite))

(do-symbols (s :chronicity)
  (let ((name (symbol-name s)))
    (when (and (eql (symbol-package s) (find-package :chronicity))
               (or (member name (list "PARSE"
                                      "NUMERIZE"
                                      "CREATE-TOKEN"
                                      "CREATE-TAG"
                                      "TAG-TYPE"
                                      "TAG-NOW"
                                      "R-NEXT"
                                      "R-THIS"
                                      "R-WIDTH"
                                      "R-OFFSET"
                                      "NOW"
                                      "TICK-TIME"
                                      "YEAR-OF"
                                      "MONTH-OF"
                                      "DAY-OF"
                                      "HOUR-OF"
                                      "MINUTE-OF"
                                      "SEC-OF"
                                      "DOW-OF"
                                      "DAY-SEC-OF")
                           :test #'string=)
                   (search "REPEATER" name)
                   (search "DATE" name)
                   (search "TIME" name)
                   (search "DATETIME" name)
                   (search "SPAN" name)))
      (import s :chronicity-test))))

(in-package #:chronicity-test)

(defun run-suite ()
  (run-all-tests :chronicity-test))

