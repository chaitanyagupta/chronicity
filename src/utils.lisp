(cl:in-package #:chronicity)

(defmacro aif (test then else)
  `(let ((it ,test))
     (if it
         ,then
         ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro rr-all-f (place regex replacement &rest args)
  `(setf ,place (cl-ppcre:regex-replace-all ,regex ,place ,replacement ,@args)))

