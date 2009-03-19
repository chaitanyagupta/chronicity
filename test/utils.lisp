(cl:in-package #:chronicity-test)

(defmacro assert-datetime= (expected form)
  (let ((value (gensym "RESULT-")))
    `(let ((,value ,form))
       (assert-true (and ,value (datetime= ,expected ,value)) ,value))))

