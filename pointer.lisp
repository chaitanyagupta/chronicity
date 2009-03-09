(cl:in-package #:cl-chronic)

;;; TODO: scan should get an EQL symbol
(defclass pointer (tag)
  ())

(defmethod scan ((pointer pointer) tokens)
  (dolist (token tokens)
    (awhen (scan-pointer token) (tag it token))))

(defun scan-pointer (token)
  (let ((scan-map '((#?r"\bpast\b" :past)
                    (#?r"\bfuture\b" :future)
                    (#?r"\bin\b" :in))))
    (loop
       for (regex value) in scan-map
       when (cl-ppcre:scan regex (token-word token))
       return (create-tag 'pointer value))))