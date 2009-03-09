(cl:in-package #:chronicity)

;;; Enable cl-interpol reader

(cl-interpol:enable-interpol-syntax)

(defclass separator (tag)
  ())

(defmethod scan-tokens ((tag (eql 'separator)) tokens)

  )

(defun scan-for-commas (token)
  (and (cl-ppcre:scan "^,$" (token-word token))
       (create-tag 'separator :comma)))

;;; Disable cl-interpol reader

(cl-interpol:disable-interpol-syntax)