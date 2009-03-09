(cl:in-package #:chronicity)

;;; TODO: the 'of' separator? or is it a pointer?

;;; Enable cl-interpol reader

(cl-interpol:enable-interpol-syntax)

(defclass separator (tag)
  ())

(defmethod scan-tokens ((tag (eql 'separator)) tokens)
  (dolist (token tokens tokens)
    (awhen (scan-for-commas token) (tag it token))
    (awhen (scan-for-slash-or-dash token) (tag it token))
    (awhen (scan-for-at token) (tag it token))
    (awhen (scan-for-in token) (tag it token))))

(defclass separator-comma (tag)
  ())

(defun scan-for-commas (token)
  (and (scan "^,$" (token-word token))
       (create-tag 'separator-comma :comma)))

(defclass separator-slash-or-dash (tag)
  ())

(defun scan-for-slash-or-dash (token)
  (let ((scan-map '((#?r"^-$" :comma)
                    (#?r"^\/$" :slash))))
    (loop
       for (regex value) in scan-map
       when (scan regex (token-word token))
       return (create-tag 'separator-slash-or-dash value))))

(defclass separator-at (tag)
  ())

(defun scan-for-at (token)
  (and (scan #?r"^(at}@)$" (token-word token))
       (create-tag 'separator-at :at)))

(defclass separator-in (tag)
  ())

(defun scan-for-in (token)
  (and (scan #?r"^in$" (token-word token))
       (create-tag 'separator-in :in)))

;;; Disable cl-interpol reader

(cl-interpol:disable-interpol-syntax)