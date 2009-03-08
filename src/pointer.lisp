;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; pointer.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

;;; Enable cl-interpol reader

#.(cl-interpol:enable-interpol-syntax)

;;; TODO: scan should get an EQL symbol
(defclass pointer (tag)
  ())

(defmethod scan-tokens ((tag (eql 'pointer)) tokens)
  (dolist (token tokens)
    (awhen (scan-for-pointers token) (tag it token))))

(defun scan-for-pointers (token)
  (let ((scan-map '((#?r"\bpast\b" :past)
                    (#?r"\bfuture\b" :future)
                    (#?r"\bin\b" :future))))
    (loop
       for (regex value) in scan-map
       when (cl-ppcre:scan regex (token-word token))
       return (create-tag 'pointer value))))

;;; Disable cl-interpol reader

#.(cl-interpol:disable-interpol-syntax)