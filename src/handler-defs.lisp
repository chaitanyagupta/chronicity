(cl:in-package #:chronicity)

(clear-handlers)

;;; Date handlers

(defun merge-time-tokens-day (tokens date-start)
  (let ((time (awhen tokens
                (let ((*now* date-start))
                  (tokens-to-span it)))))
    (if time
        (make-span (merge-datetime date-start (span-start time))
                   (merge-datetime date-start (span-end time)))
        (make-span date-start (datetime-incr date-start :day)))))

(define-handler (date handle-rmn-sd-sy)
    ((repeater-month-name scalar-day scalar-year)
     (repeater-month-name scalar-day scalar-year (? separator-at) (? p time)))
    (tokens)
  (setf tokens (remove-separators tokens))
  (let* ((year-tag (find-tag 'scalar-year (third tokens)))
         (month-name-tag (find-tag 'repeater-month-name (first tokens)))
         (day-tag (find-tag 'scalar-day (second tokens)))
         (date-start (make-date (tag-type year-tag)
                                (month-index (tag-type month-name-tag))
                                (tag-type day-tag))))
    (merge-time-tokens-day (nthcdr 3 tokens) date-start)))

(define-handler (date handle-rmn-sd)
    ((repeater-month-name scalar-day (? separator-at) (? p time)))
    (tokens)
  (setf tokens (remove-separators tokens))
  (let* ((month-name-tag (find-tag 'repeater-month-name (first tokens)))
         (day-tag (find-tag 'scalar-day (second tokens)))
         (month (month-index (tag-type month-name-tag)))
         (day (tag-type day-tag))
         (this-year (year-of *now*))
         (this-year-date (make-date this-year month day))
         (year (ecase *context*
                 (:future (if (datetime< this-year-date *now*) (1+ this-year) this-year))
                 (:past (if (datetime> this-year-date *now*) (1- this-year) this-year))))
         (date-start (make-date year month day)))
    (merge-time-tokens-day (nthcdr 2 tokens) date-start)))

(define-handler (date)
    ((repeater-time (? repeater-day-portion) (? separator-on) repeater-month-name scalar-day))
    (tokens)
  (setf tokens (remove-separators tokens))
  (cond
    ((= (length tokens) 3)
     (handle-rmn-sd (list (second tokens) (third tokens) (first tokens))))
    ((= (length tokens) 4)
     (handle-rmn-sd (list (third tokens) (fourth tokens) (first tokens) (second tokens))))
    (t
     (error "Wrong number of tokens passed to HANDLE-RMN-SD-ON.~%Tokens:~%~S" tokens))))

(define-handler (date handle-rmn-od)
    ((repeater-month-name ordinal-day (? separator-at) (? p time)))
    (tokens)
  (let* ((day-token (second tokens))
         (day (tag-type (find-tag 'ordinal-day day-token))))
    (tag (create-tag 'scalar-day day) day-token)
    (handle-rmn-sd (list* (first tokens) day-token (nthcdr 2 tokens)))))

(define-handler (date)
    ((repeater-time (? repeater-day-portion) (? separator-on) repeater-month-name ordinal-day))
    (tokens)
  (setf tokens (remove-separators tokens))
  (cond
    ((= (length tokens) 3)
     (handle-rmn-od (list (second tokens) (third tokens) (first tokens))))
    ((= (length tokens) 4)
     (handle-rmn-od (list (third tokens) (fourth tokens) (first tokens) (second tokens))))
    (t
     (error "Wrong number of tokens passed to HANDLE-RMN-OD-ON.~%Tokens:~%~S" tokens))))

(define-handler (date)
    ((repeater-month-name scalar-year))
    (tokens)
  (let* ((month-name (tag-type (find-tag 'repeater-month-name (first tokens))))
         (month (month-index month-name))
         (year (tag-type (find-tag 'scalar-year (second tokens))))
         (start (make-date year month)))
    (make-span start (datetime-incr start :month))))

(define-handler (date)
    ((scalar-day repeater-month-name scalar-year (? separator-at) (? p time)))
    (tokens)
  (handle-rmn-sd-sy (list* (second tokens) (first tokens) (nthcdr 2 tokens))))

(define-handler (date)
    ((ordinal-day repeater-month-name scalar-year (? separator-at) (? p time)))
    (tokens)
  (let* ((day-token (first tokens))
         (day (tag-type (find-tag 'ordinal-day day-token))))
    (tag (create-tag 'scalar-day day) day-token)
    (handle-rmn-sd-sy (list* (second tokens) day-token (nthcdr 2 tokens)))))

;;; Time handlers

(defun dealias-time (time &optional day-portion &aux
                     (repeater-time (first (token-tags time)))
                     (repeater-day-portion (and day-portion (first (token-tags day-portion)))))
  (unless (tag-now repeater-time)
    (setf (tag-now repeater-time) *now*))
  (let* ((tick (tag-type repeater-time))
         (day-portion-type (and day-portion (tag-type repeater-day-portion))))
    (when (and (tick-ambiguousp tick) repeater-day-portion)
      (setf (tick-ambiguousp tick) nil)
      (ecase day-portion-type
        ((:am :morning) nil)
        ((:pm :afternoon :evening :night)
         (if (> (hour-of (tick-time tick)) 12)
             (error "AMBIGUOUSP given when hour-of TIME is greater than 12.~%Time: ~A"
                    time)
             (datetime-incf (tick-time tick) :hour 12)))))
    (r-next repeater-time *context*)))

(define-handler (time)
    ((repeater-time (? repeater-day-portion)))
    (tokens)
  (apply #'dealias-time tokens))

