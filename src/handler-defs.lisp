;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; handler-defs.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(clear-handlers)

;;; Date handlers

(define-handler (date handle-rmn-sd-rt-sy)
    (tokens)
    ((repeater-month-name scalar-day repeater-time scalar-year))
  (let* ((year-tag (find-tag 'scalar-year (fourth tokens)))
         (month-name-tag (find-tag 'repeater-month-name (first tokens)))
         (day-tag (find-tag 'scalar-day (second tokens)))
         (date-start (make-date (tag-type year-tag)
                                (month-index (tag-type month-name-tag))
                                (tag-type day-tag))))
    (merge-time-tokens-day (list (third tokens)) date-start)))

(define-handler (date handle-rmn-sd-sy)
    (tokens)
    ((repeater-month-name scalar-day scalar-year)
     (repeater-month-name scalar-day scalar-year (? separator-at) (? p time)))
  (setf tokens (remove-separators tokens))
  (let* ((year-tag (find-tag 'scalar-year (third tokens)))
         (month-name-tag (find-tag 'repeater-month-name (first tokens)))
         (day-tag (find-tag 'scalar-day (second tokens)))
         (date-start (make-date (tag-type year-tag)
                                (month-index (tag-type month-name-tag))
                                (tag-type day-tag))))
    (merge-time-tokens-day (nthcdr 3 tokens) date-start)))

(defun guess-year (month day)
  (let* ((today (copy-date *now*))
         (this-year (year-of *now*))
         (this-year-date (make-date this-year month day)))
    (ecase *context*
      (:future (if (datetime< this-year-date today) (1+ this-year) this-year))
      (:past (if (datetime> this-year-date today) (1- this-year) this-year)))))

(define-handler (date handle-rmn-sd)
    (tokens)
    ((repeater-month-name scalar-day (? separator-at) (? p time)))
  (setf tokens (remove-separators tokens))
  (let* ((month-name-tag (find-tag 'repeater-month-name (first tokens)))
         (day-tag (find-tag 'scalar-day (second tokens)))
         (month (month-index (tag-type month-name-tag)))
         (day (tag-type day-tag))
         (year (guess-year month day))
         (date-start (make-date year month day)))
    (merge-time-tokens-day (nthcdr 2 tokens) date-start)))

(define-handler (date)
    (tokens)
    ((repeater-time (? repeater-day-portion) (? separator-on) repeater-month-name scalar-day))
  (setf tokens (remove-separators tokens))
  (cond
    ((= (length tokens) 3)
     (handle-rmn-sd (list (second tokens) (third tokens) (first tokens))))
    ((= (length tokens) 4)
     (handle-rmn-sd (list (third tokens) (fourth tokens) (first tokens) (second tokens))))
    (t
     (error "Wrong number of tokens passed to HANDLE-RMN-SD-ON.~%Tokens:~%~S" tokens))))

(define-handler (date handle-rmn-od-sy)
    (tokens)
    ((repeater-month-name ordinal-day scalar-year (? separator-at) (? p time)))
  (let* ((day-token (second tokens))
         (day (token-tag-type 'ordinal-day day-token)))
    (tag (create-tag 'scalar-day day) day-token)
    (handle-rmn-sd-sy (list* (first tokens) day-token (third tokens) (nthcdr 3 tokens)))))

(define-handler (date handle-rmn-od)
    (tokens)
    ((repeater-month-name ordinal-day (? separator-at) (? p time)))
  (let* ((day-token (second tokens))
         (day (token-tag-type 'ordinal-day day-token)))
    (tag (create-tag 'scalar-day day) day-token)
    (handle-rmn-sd (list* (first tokens) day-token (nthcdr 2 tokens)))))

(define-handler (date)
    (tokens)
    ((repeater-time (? repeater-day-portion) (? separator-on) repeater-month-name ordinal-day))
  (setf tokens (remove-separators tokens))
  (cond
    ((= (length tokens) 3)
     (handle-rmn-od (list (second tokens) (third tokens) (first tokens))))
    ((= (length tokens) 4)
     (handle-rmn-od (list (third tokens) (fourth tokens) (first tokens) (second tokens))))
    (t
     (error "Wrong number of tokens passed to HANDLE-RMN-OD-ON.~%Tokens:~%~S" tokens))))

(define-handler (date)
    (tokens)
    ((repeater-month-name scalar-year))
  (let* ((month-name (token-tag-type 'repeater-month-name (first tokens)))
         (month (month-index month-name))
         (year (token-tag-type 'scalar-year (second tokens)))
         (start (make-date year month)))
    (make-span start (datetime-incr start :month))))

(define-handler (date)
    (tokens)
    ((scalar-day repeater-month-name scalar-year (? separator-at) (? p time)))
  (handle-rmn-sd-sy (list* (second tokens) (first tokens) (nthcdr 2 tokens))))

(define-handler (date)
    (tokens)
    ((ordinal-day repeater-month-name scalar-year (? separator-at) (? p time)))
  (let* ((day-token (first tokens))
         (day (token-tag-type 'ordinal-day day-token)))
    (tag (create-tag 'scalar-day day) day-token)
    (handle-rmn-sd-sy (list* (second tokens) day-token (nthcdr 2 tokens)))))

(define-handler (date)
    (tokens)
    ((scalar-year separator-slash-or-dash scalar-month separator-slash-or-dash scalar-day (? separator-at) (? p time)))
  (setf tokens (remove-separators tokens))
  (let* ((year (token-tag-type 'scalar-year (first tokens)))
         (month (token-tag-type 'scalar-month (second tokens)))
         (day (token-tag-type 'scalar-day (third tokens)))
         (date-start (make-date year month day)))
    (merge-time-tokens-day (nthcdr 3 tokens) date-start)))

(define-handler (date handle-ambiguous-dmy)
    (original-tokens &aux tokens)
    ((scalar-month separator-slash-or-dash scalar-month separator-slash-or-dash scalar-year (? separator-at) (? p time))
     (scalar-month separator-slash-or-dash scalar-month (? separator-at) (? p time)))
  (setf tokens (remove-separators original-tokens))
  (destructuring-bind (day month)
      (ecase *endian-preference*
        (:little (list (token-tag-type 'scalar-day (first tokens))
                       (token-tag-type 'scalar-month (second tokens))))
        (:middle (list (token-tag-type 'scalar-day (second tokens))
                       (token-tag-type 'scalar-month (first tokens)))))
    (let ((year (if (and (fourth original-tokens)
                         (find-tag 'separator-slash-or-dash (fourth original-tokens)))
                    (token-tag-type 'scalar-year (third tokens))
                    (guess-year month day))))
      (merge-time-tokens-day (nthcdr 3 tokens) (make-date year month day)))))

(define-handler (date)
    (tokens)
    ((scalar-day separator-slash-or-dash scalar-month separator-slash-or-dash scalar-year (? separator-at) (? p time))
     (scalar-day separator-slash-or-dash scalar-month (? separator-at) (? p time))
     (scalar-month separator-slash-or-dash scalar-day separator-slash-or-dash scalar-year (? separator-at) (? p time))
     (scalar-month separator-slash-or-dash scalar-day (? separator-at) (? p time)))
  (let ((selected-pattern (handler-pattern *handler*)))
    (if (or (equalp selected-pattern (first *handler-patterns*))
            (equalp selected-pattern (second *handler-patterns*)))
        (let ((*endian-preference* :little))
          (handle-ambiguous-dmy tokens))
        (let ((*endian-preference* :middle))
          (handle-ambiguous-dmy tokens)))))

(define-handler (date)
    (tokens)
    ((scalar-month separator-slash-or-dash scalar-year))
  (setf tokens (remove-separators tokens))
  (let ((month (token-tag-type 'scalar-month (first tokens)))
        (year (token-tag-type 'scalar-year (second tokens))))
    (make-span (make-date year month)
               (datetime-incr (make-date year month) :month))))

;;; Anchors

(define-handler (anchor handle-r)
    (tokens)
    (((? grabber) repeater (? separator-at) (? repeater) (? repeater))
     ((? grabber) repeater repeater (? separator-at) (? repeater) (? repeater)))
  (get-anchor (dealias-and-disambiguate-time tokens)))

(define-handler (anchor)
    (tokens)
    ((repeater grabber repeater))
  (handle-r (list (second tokens) (first tokens) (third tokens))))

;;; Arrows

(defun handle-srp (tokens span)
  (let ((distance (tag-type (find-if #'(lambda (x)
                                         (eql (type-of x) 'scalar))
                                     (token-tags (first tokens)))))
        (repeater (find-tag 'repeater (second tokens)))
        (pointer (token-tag-type 'pointer (third tokens))))
    (when (and repeater span)
      (r-offset repeater span distance pointer))))

(define-handler (arrow handle-s-r-p)
    (tokens)
    (((? scalar) repeater pointer))
  (when (= (length tokens) 2)
    (push (create-token "1" (create-tag 'scalar 1)) tokens))
  (let ((span (parse "this second" :guess nil :now *now*)))
    (handle-srp tokens span)))

(define-handler (arrow handle-p-s-r)
    (tokens)
    ((pointer scalar repeater))
  (handle-s-r-p (list (second tokens) (third tokens) (first tokens))))

(define-handler (arrow handle-s-r-p-a)
    (tokens)
    ((scalar repeater pointer (? p anchor)))
  (let ((anchor-span (awhen (nthcdr 3 tokens)
                       (get-anchor it))))
    (handle-srp tokens anchor-span)))

(define-handler (arrow)
    (tokens)
    ((repeater pointer (? p anchor)))
  (handle-s-r-p-a (cons (create-token "1" (create-tag 'scalar 1)) tokens)))

;;; Narrow

(defun handle-orr (tokens outer-span)
  (let ((repeater (find-tag 'repeater (second tokens)))
        (ordinal (token-tag-type 'ordinal (first tokens))))
    (setf (tag-now repeater) (datetime-decr (span-start outer-span) :sec))
    (loop
       repeat ordinal
       for span = (r-next repeater :future)
       if (datetime> (span-start span) (span-end outer-span))
       return nil
       finally (return span))))

(define-handler (narrow)
    (tokens)
    ((ordinal repeater separator-in scalar-year)
     (ordinal repeater separator-in repeater))
  (let ((outer-span (aif (token-tag-type 'scalar-year (fourth tokens))
                         (make-span (make-date it) (make-date (1+ it)))
                         (get-anchor (list (fourth tokens))))))
    (handle-orr (list (first tokens) (second tokens)) outer-span)))

(define-handler (narrow)
    (tokens)
    ((ordinal repeater grabber repeater))
  (let ((outer-span (get-anchor (list (third tokens) (fourth tokens)))))
    (handle-orr tokens outer-span)))

;;; Time handlers

(define-handler (time)
    (tokens)
    ((repeater-time (? repeater-day-portion)))
  (get-anchor (dealias-and-disambiguate-time tokens)))
