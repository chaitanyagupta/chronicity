(cl:in-package #:cl-chronic)

(defclass grabber (tag)
  ())

(defmethod scan ((grabber grabber) tokens)
  (let ((scan-map '(("last" :last)
                    ("this" :this)
                    ("next" :next))))
    (dolist (token tokens tokens)
      (loop
         for (regex value) in scan-map
         when (cl-ppcre:scan regex (token-word token))
         do (tag (create-tag 'grabber value) token)))))