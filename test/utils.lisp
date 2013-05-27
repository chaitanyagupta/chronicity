;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; utils.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity-test)

(defmacro assert-datetime= (expected form)
  (let ((value (gensym "RESULT-")))
    `(let ((,value ,form))
       (assert-true (and ,value (datetime= ,expected ,value)) ,value))))
