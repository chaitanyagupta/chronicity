;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; chronicity.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

;;; Some constants

(defconstant +sec-seconds+ 1)
(defconstant +minute-seconds+ 60)
(defconstant +hour-seconds+ (* 60 60))
(defconstant +day-seconds+ (* 24 60 60))
(defconstant +weekend-seconds+ (* 2 24 60 60))
(defconstant +week-seconds+ (* 7 24 60 60))
(defconstant +fortnight-seconds+ (* 14 24 60 60))
(defconstant +month-seconds+ (* 30 24 60 60))
(defconstant +year-seconds+ (* 365 24 60 60))
(defconstant +year-months+ 12)

;;; Enable CL-INTERPOL's special reader syntax

#.(cl-interpol:enable-interpol-syntax)

(defvar *context* :future
  "The default value for :CONTEXT.")

(defvar *now* nil
  "The default value for :NOW. If NIL, :NOW is assumed to be this
  instant.")

(defvar *endian-preference* :little
  "The default value for :ENDIAN-PREFERENCE.")

(defvar *guess* t
  "The default value for :GUESS.")

(defvar *ambiguous-time-range* 6
  "The default value for :AMBIGUOUS-TIME-RANGE.")

(defun parse (text &key
              ((:context *context*) *context*)
              ((:now *now*) (or *now* (now)))
              (guess *guess*)
              ((:ambiguous-time-range *ambiguous-time-range*) *ambiguous-time-range*)
              ((:endian-preference *endian-preference*) *endian-preference*))
  "Parse the string in TEXT and return either a DATETIME or a SPAN
  object. Also returns a list of tokens as the second value.

CONTEXT (default *CONTEXT*) can be either :PAST or :FUTURE.

NOW (default *NOW* or this instant) should be a DATETIME instance,
relative to which the date/time will be calculated.

GUESS (default *GUESS*) if NIL, PARSE returns a SPAN object, otherwise
returns the start, end or middle of the span if the it is :START, :END
or :MIDDLE respectively. If it is T, it will return the default value
of a span if it has one (SPAN-DEFAULT), otherwise it will return the
start of span.

For AMBIGUOUS-TIME-RANGE (default *AMBIGUOUS-TIME-RANGE*), if an
integer is given, ambiguous times (like 5:00) will be assumed to be
within the range of that time in the AM to that time in the PM. For
example, if you set it to 7, then the parser will look for the time
between 7am and 7pm. In the case of 5:00, it would assume that means
5:00pm. If NIL is given, no assumption will be made, and the first
matching instance of that time will be used."
  (let ((tokens (tokenize-and-tag (pre-normalize text))))
    (pre-process-tokens tokens)
    (values (guess-span (tokens-to-span tokens) guess) tokens)))

(defun pre-normalize (text)
  (setf text (string-downcase text))
  (setf text (chronicity-numerizer:numerize text))
  (rr-all-f text #?/['\"\.]/ "")
  (rr-all-f text #?/([\/\-\,\@])/ " \\1 ")
  (rr-all-f text #?/\btoday\b/ "this day")
  (rr-all-f text #?/\btomm?orr?ow\b/ "next day")
  (rr-all-f text #?/\byesterday\b/ "last day")
  (rr-all-f text #?/\bnoon\b/ "12:00")
  (rr-all-f text #?/\bmidnight\b/ "24:00")
  (rr-all-f text #?/\bbefor now\b/  "past")
  (rr-all-f text #?/\bnow\b/ "this second")
  (rr-all-f text #?r"^a(\s+)" "1\\1")
  (rr-all-f text "\\b(ago|before)\\b" "past")
  (rr-all-f text #?/\bthi past\b/ "last")
  (rr-all-f text #?/\bthi last\b/ "last")
  (rr-all-f text "\\b(?:in|during) the (morning)\\b" "\\1")
  (rr-all-f text "\\b(?:in the|during the|at) (afternoon|evening|night)\\b" "\\1")
  (rr-all-f text #?/\btonight\b/ "this night")
  (rr-all-f text "(\\d)([ap]m|oclock)\\b" "\\1 \\2")
  (rr-all-f text "\\b(hence|after|from)\\b" "future")
  text)

(defun tokenize (text)
  (mapcar #'create-token
          (cl-ppcre:split #?r"\s+" text)))

(defun tokenize-and-tag (text)
  (let ((tokens (tokenize text)))
    (loop
       for type in (list 'repeater 'grabber 'pointer 'scalar 'ordinal 'separator)
       do (scan-tokens type tokens))
    tokens))

(defun pre-process-tokens (tokens)
  (dotimes (i (length tokens))
    (symbol-macrolet ((current (elt tokens i))
                      (next (elt tokens (1+ i))))
      ;; Resolve ambiguity related to "second"
      (when (and (string-equal (token-word current) "second")
                 (and (< (1+ i) (length tokens))
                      (find-tag 'repeater next)))
        (untag 'repeater-sec current)
        (tag (create-tag 'ordinal 2) current)
        (tag (create-tag 'ordinal-day 2) current)))))

(defun guess-span (span guess)
  (when span
    (ecase guess
      ((t) (or (span-default span)
               (span-start span)))
      (:start (span-start span))
      (:end (if (span-end-included-p span)
                (span-end span)
                (datetime-decr (span-end span) :sec)))
      (:middle (span-middle span))
      ((nil) span))))

(defclass token ()
  ((word :initarg :word
         :reader token-word)
   (tags :initarg :tags
         :initform nil
         :accessor token-tags)))

(defmethod print-object ((x token) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~A~@[ [~{~A~^, ~}]~]"
            (token-word x)
            (mapcar #'type-of (token-tags x)))))

(defun create-token (word &rest tags)
  (make-instance 'token
                 :word word
                 :tags tags))

(defclass tag ()
  ((type :initarg :type
         :reader tag-type)
   (now :initarg :now
        :accessor tag-now
        :initform nil)))

(defmethod print-object ((x tag) stream)
  (print-unreadable-object (x stream :type t)
    (when (slot-boundp x 'type)
      (princ (tag-type x) stream))
    (when (tag-now x)
      (format stream " ~A" (tag-now x)))))

(defun create-tag (class type &key now)
  (make-instance class :type type :now now))

(defmethod tag (tag token)
  (push tag (token-tags token)))

(defmethod untag ((tag tag) (token token))
  (setf (token-tags token) (remove tag (token-tags token))))

(defmethod untag ((x class) (token token))
  (untag (class-name x) token))

(defmethod untag ((x symbol) token)
  (setf (token-tags token) (remove-if #'(lambda (tag)
                                          (typep tag x))
                                      (token-tags token))))

(defun token-has-tag-p (token tag-name)
  (some #'(lambda (tag) (typep tag tag-name)) (token-tags token)))

(defun find-tag (tag-name token)
  (find-if #'(lambda (x)
               (typep x tag-name))
           (token-tags token)))

(defun token-tag-type (tag-name token)
  (awhen (find-tag tag-name token)
    (tag-type it)))

;;; Generic token scanner

(defgeneric scan-tokens (tag tokens)
  (:documentation "Scan the list of TOKENS and tag the appropriately."))

;;; Disable CL-INTERPOL's special reader syntax

#.(cl-interpol:disable-interpol-syntax)





