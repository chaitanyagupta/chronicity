;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; grabber.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

;;; Enable cl-interpol reader

#.(cl-interpol:enable-interpol-syntax)

(defclass grabber (tag)
  ())

(defmethod scan-tokens ((tag (eql 'grabber)) tokens)
  (let ((scan-map '(("last" :last)
                    ("this" :this)
                    ("next" :next))))
    (dolist (token tokens tokens)
      (loop
         for (regex value) in scan-map
         when (cl-ppcre:scan regex (token-word token))
         do (tag (create-tag 'grabber value) token)))))

;;; Disable cl-interpol reader

#.(cl-interpol:disable-interpol-syntax)