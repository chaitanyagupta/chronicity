(cl:in-package #:cl-chronic)

(defclass separator (tag)
  ())

(defmethod scan ((tag (eql 'separator)) tokens)

  )

(defun scan-for-commas (token)
  (and (cl-ppcre:scan "^,$" (token-word token))
       (create-tag 'separator :comma)))