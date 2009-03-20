(cl:in-package #:chronicity)

;;; Enable cl-interpol reader

#.(cl-interpol:enable-interpol-syntax)

(defclass scalar (tag)
  ())

(defmethod scan-tokens ((tag (eql 'scalar)) tokens)
  (loop
     for (token post-token) on tokens
     do
       (awhen (scan-for-scalars token post-token) (tag it token))
       (awhen (scan-for-scalar-days token post-token) (tag it token))
       (awhen (scan-for-scalar-months token post-token) (tag it token))
       (awhen (scan-for-scalar-years token post-token) (tag it token)))
  tokens)

(defparameter *day-portions*
  (list :am :pm :morning :afternoon :evening :night))

(defun check-post-token (token)
  (or (not token)
      (not (member (token-word token)
                   (mapcar #'string *day-portions*)
                   :test #'equalp))))

(defun scan-for-scalars (token post-token)
  (when (and (cl-ppcre:scan #?r"^\d*$" (token-word token))
             (check-post-token post-token))
    (create-tag 'scalar (parse-integer (token-word token)))))

(defclass scalar-day (scalar)
  ())

(defun scan-for-scalar-days (token post-token)
  (when (and (cl-ppcre:scan #?r"^\d\d?$" (token-word token))
             (<= (parse-integer (token-word token)) 31)
             (check-post-token post-token))
    (create-tag 'scalar-day (parse-integer (token-word token)))))

(defclass scalar-month (scalar)
  ())

(defun scan-for-scalar-months (token post-token)
  (when (and (cl-ppcre:scan #?r"^\d\d?$" (token-word token))
             (<= (parse-integer (token-word token)) 12)
             (check-post-token post-token))
    (create-tag 'scalar-month (parse-integer (token-word token)))))

(defclass scalar-year (scalar)
  ())

(defun scan-for-scalar-years (token post-token)
  (when (and (cl-ppcre:scan #?r"^([1-9]\d)?\d\d?$" (token-word token))
             (check-post-token post-token))
    (let* ((year (parse-integer (token-word token)))
           (guessed-year (cond
                           ((< year 70) (+ year 2000))
                           ((< year 100) (+ year 1900))
                           (t year))))
      (create-tag 'scalar-year guessed-year))))

;;; Disable cl-interpol reader

#.(cl-interpol:disable-interpol-syntax)