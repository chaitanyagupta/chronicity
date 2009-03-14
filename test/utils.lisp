(cl:in-package #:chronicity-test)

(defmacro assert-datetime= (expected form)
  (let ((value (gensym "RESULT-")))
    `(let ((,value ,form))
       (assert-true (datetime= ,expected ,value) ,value))))

