(cl:in-package #:cl-user)

(defpackage #:cl-chronic-test
  (:use #:cl #:rt)
  (:import-from #:cl-chronic #:numerize))

(in-package #:cl-chronic-test)

;;; Numerizer tests

(defmacro def-numerizer-test (name string result)
  `(deftest ,name (numerize ,string) ,result))

(def-numerizer-test num-d1 "one" "1")
(def-numerizer-test num-d2 "two foo" "2 foo")
(def-numerizer-test num-d3 "a four" "a 4")
(def-numerizer-test num-d4 "twelve" "12")
(def-numerizer-test num-d5 "foo seven bar" "foo 7 bar")
(def-numerizer-test num-t1 "thirty eight" "38")
(def-numerizer-test num-t2 "Seventy 8" "78")
(def-numerizer-test num-t3 "fourty" "40")
(def-numerizer-test num-b1 "Two hundred and fifty one" "251")
(def-numerizer-test num-b2 "one hundred eleven" "111")
(def-numerizer-test num-b3 "four hundred and fourty four" "444")
(def-numerizer-test num-m1 "seven hours before now" "7 hours before now")
(def-numerizer-test num-m2 "three hundred and sixty two days from now" "362 days from now")