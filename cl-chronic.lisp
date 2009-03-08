(cl:in-package #:cl-user)

(defpackage #:cl-chronic
  (:use #:cl))

(in-package #:cl-chronic)

(cl-interpol:enable-interpol-syntax)

(defmacro rr-all-f (place regex replacement &rest args)
  `(setf ,place (cl-ppcre:regex-replace-all ,regex ,place ,replacement ,@args)))

(defun parse (string &key context now guess ambiguous-time-range)
  "The API."
  

  )

(defun pre-normalize (text)
  (setf text (string-downcase text))
  ;; (setf text (numericize-numbers text))
  (rr-all-f text #?/['\"\.]/ "")
  (rr-all-f text #?/([\/\-\,\@])/ " \\1 ")
  (rr-all-f text #?/\btoday\b/ "this day")
  (rr-all-f text #?/\btomm?orr?ow\b/ "next day")
  (rr-all-f text #?/\byesterday\b/ "last day")
  (rr-all-f text #?/\bnoon\b/ "12:00")
  (rr-all-f text #?/\bmidnight\b/ "24:00")
  (rr-all-f text #?/\bbefor now\b/  "past")
  (rr-all-f text #?/\bnow\b/ "this second")
  (rr-all-f text "\\b(ago|before)\\b" "past")
  (rr-all-f text #?/\bthi past\b/ "last")
  (rr-all-f text #?/\bthi last\b/ "last")
  (rr-all-f text "\\b(?:in|during) the (morning)\\b" "\\1")
  (rr-all-f text "\\b(?:in the|during the|at) (afternoon|evening|night)\\b" "\\1")
  (rr-all-f text #?/\btonight\b/ "this night")
  (rr-all-f text "(?=\\w)([ap]m|oclock)\\b" "\\1")
  (rr-all-f text "\\b(hence|after|from)\\b" "future")
  ;; (setf text (numericize-ordinals text))
  text)

