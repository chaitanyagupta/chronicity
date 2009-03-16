(cl:in-package #:chronicity)

(clear-handlers)

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

(define-handler (time-rt-rdp
                 (repeater-time (? repeater-day-portion)))
    (tokens)
  (apply #'dealias-time tokens))

;;; Date handlers

(defun merge-time-tokens-day (tokens date-start)
  (let ((time (awhen tokens
                (let ((*now* date-start))
                  (tokens-to-span it)))))
    (if time
        (make-span (merge-datetime date-start (span-start time))
                   (merge-datetime date-start (span-end time)))
        (make-span date-start (datetime-incr date-start :day)))))

(define-handler (date-rmn-sd-sy
                 (repeater-month-name scalar-day scalar-year)
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

(define-handler (date-rmn-sd
                 (repeater-month-name scalar-day (? separator-at) (? p time)))
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

(define-handler (date-rmn-sd-on
                 (repeater-time (? repeater-day-portion) (? separator-on) repeater-month-name scalar-day))
    (tokens)
  (setf tokens (remove-separators tokens))
  (cond
    ((= (length tokens) 3)
     (date-rmn-sd (list (second tokens) (third tokens) (first tokens))))
    ((= (length tokens) 4)
     (date-rmn-sd (list (third tokens) (fourth tokens) (first tokens) (second tokens))))
    (t
     (error "Wrong number of tokens passed to DATE-RMN-SD-ON.~%Tokens:~%~S" tokens))))



