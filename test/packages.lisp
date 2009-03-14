(cl:in-package #:cl-user)

(defpackage #:chronicity-test
  (:use #:cl #:lisp-unit)
  (:export #:run-suite)
  (:import-from #:chronicity
                ))

(defpackage #:chronicity-repeater-test
  (:use #:cl #:lisp-unit)
  (:import-from #:chronicity
                #:create-token
                #:create-tag
                #:tag-type
                #:tag-now
                #:r-next
                #:r-this
                #:r-width
                #:r-offset
                #:now
                #:tick-time))

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
      (import s :chronicity-test)
      (import s :chronicity-repeater-test))))

(import 'chronicity-test::assert-datetime= :chronicity-repeater-test)

(in-package #:chronicity-test)

(defun run-suite ()
  (run-all-tests :chronicity-test)
  (run-all-tests :chronicity-repeater-test))

