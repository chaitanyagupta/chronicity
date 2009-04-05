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
    ("first" "1st")
    ("third" "3rd")
    ("fifth" "5th")
    ("nineteenth" "19th")
    ("sixtieth" "60th")
    ("twenty seven" 27)
    ("thirty-one" 31)
    ("fifty nine" 59)
    ("one hundred" 100)
    ("one hundredth" "100th")
    ("one hundred and fifty" 150)
    ("one hundred and fiftieth" "150th")
    ("two-hundred" 200)
    ("nine hundred and ninety nine" 999)
    ("nine hundred and ninety ninth" "999th")
    ("one thousand" 1000)
    ("twelve hundred" 1200)
    ("twelve hundredth" "1200th")
    ("one thousand two hundred" 1200)
    ("seventeen thousand" 17000)
    ("a hundred" "a 100")
    ("seventy four thousand and two" 74002)
    ("ninety nine thousand nine hundred ninety nine" 99999)
    ("ninety nine thousand nine hundred ninety nineth" "99999th")
    ("one lakh ten thousand two hundred and seven" "110207")
    ("three crore one lakh ten thousand two hundred and seven" "30110207")
    ("two hundred fifty thousand" 250000)
    ("one million" 1000000)
    ("one million two hundred fifty thousand and seven" 1250007)
    ("one million two hundred fifty thousand and seventh" "1250007th")
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

