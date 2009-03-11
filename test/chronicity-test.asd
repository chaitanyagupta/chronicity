(cl:defpackage #:chronicity-test-system
  (:use #:cl #:asdf))

(cl:in-package #:chronicity-test-system)

(defsystem #:chronicity-test
    :depends-on (:chronicity :rt)
    :serial t
    :components ((:file "numerize")))

