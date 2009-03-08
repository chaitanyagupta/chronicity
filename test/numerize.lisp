;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; numerize.lisp

;;; See the LICENSE file for licensing information.

(in-package #:chronicity-test)

;;; Numerizer tests

(defparameter *numerizer-test-map*
  '(("one" 1)
    ("five" 5)
    ("ten" 10)
    ("eleven" 11)
    ("twelve" 12)
    ("thirteen" 13)
    ("fourteen" 14)
    ("fifteen" 15)
    ("sixteen" 16)
    ("seventeen" 17)
    ("eighteen" 18)
    ("nineteen" 19)
    ("twenty" 20)
    ("twenty seven" 27)
    ("thirty-one" 31)
    ("fifty nine" 59)
    ("one hundred" 100)
    ("one hundred and fifty" 150)
    ("two-hundred" 200)
    ("nine hundred and ninety nine" 999)
    ("one thousand" 1000)
    ("twelve hundred" 1200)
    ("one thousand two hundred" 1200)
    ("seventeen thousand" 17000)
    ("a hundred" "a 100")
    ("seventy four thousand and two" 74002)
    ("ninety nine thousand nine hundred ninety nine" 99999)
    ("two hundred fifty thousand" 250000)
    ("one million" 1000000)
    ("one million two hundred fifty thousand and seven" 1250007)
    ("one billion" 1000000000)
    ("one billion and one" 1000000001)
    ("two foo" "2 foo")
    ("a four" "a 4")
    ("foo seven bar" "foo 7 bar")
    ("seven hours before now" "7 hours before now")
    ("three hundred and sixty two days from now" "362 days from now")))

(define-test numerizer-test
  (loop
     for (string result) in *numerizer-test-map*
     for result-string = (format nil "~A" result)
     do (assert-equal result-string (chronicity-numerizer:numerize string)
                      string)))

