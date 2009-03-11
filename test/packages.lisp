(cl:in-package #:cl-user)

(defpackage #:chronicity-test
  (:use #:cl #:lisp-unit)
  (:export #:run-all-tests)
  (:import-from #:chronicity
                #:parse
                #:numerize))

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
               (or (search "REPEATER" name)
                   (search "DATE" name)
                   (search "TIME" name)
                   (search "DATETIME" name)
                   (search "SPAN" name)))
      (import s :chronicity-repeater-test))))

