;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; ordinal.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

;;; Enable cl-interpol reader

#.(cl-interpol:enable-interpol-syntax)

(defclass ordinal (tag)
  ())

(defmethod scan-tokens ((tag (eql 'ordinal)) tokens)
  (dolist (token tokens tokens)
    (awhen (scan-for-ordinals token) (tag it token))
    (awhen (scan-for-ordinal-days token) (tag it token))))

(defun scan-for-ordinals (token)
  (when (cl-ppcre:scan #?r"^(\d*)(st|nd|rd|th)$" (token-word token))
    (create-tag 'ordinal (parse-integer (token-word token)
                                        :junk-allowed t))))

(defclass ordinal-day (ordinal)
  ())

(defun scan-for-ordinal-days (token)
  (when (cl-ppcre:scan #?r"^(\d*)(st|nd|rd|th)$" (token-word token))
    (let ((num (parse-integer (token-word token) :junk-allowed t)))
      (when (<= num 31)
        (create-tag 'ordinal-day num)))))

;;; Disable cl-interpol reader

#.(cl-interpol:disable-interpol-syntax)

