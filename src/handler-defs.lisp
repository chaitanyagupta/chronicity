(cl:in-package #:chronicity)

(clear-handlers)

;;; Date handlers

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

(defun guess-year (month day)
  (let* ((today (copy-date *now*))
         (this-year (year-of *now*))
         (this-year-date (make-date this-year month day)))
    (ecase *context*
      (:future (if (datetime< this-year-date today) (1+ this-year) this-year))
      (:past (if (datetime> this-year-date today) (1- this-year) this-year)))))

(define-handler (date handle-rmn-sd)
    ((repeater-month-name scalar-day (? separator-at) (? p time)))
    (tokens)
  (setf tokens (remove-separators tokens))
  (let* ((month-name-tag (find-tag 'repeater-month-name (first tokens)))
         (day-tag (find-tag 'scalar-day (second tokens)))
         (month (month-index (tag-type month-name-tag)))
         (day (tag-type day-tag))
         (year (guess-year month day))
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
         (day (token-tag-type 'ordinal-day day-token)))
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
  (let* ((month-name (token-tag-type 'repeater-month-name (first tokens)))
         (month (month-index month-name))
         (year (token-tag-type 'scalar-year (second tokens)))
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
         (day (token-tag-type 'ordinal-day day-token)))
    (tag (create-tag 'scalar-day day) day-token)
    (handle-rmn-sd-sy (list* (second tokens) day-token (nthcdr 2 tokens)))))

(define-handler (date)
    ((scalar-year separator-slash-or-dash scalar-month separator-slash-or-dash scalar-day (? separator-at) (? p time)))
    (tokens)
  (setf tokens (remove-separators tokens))
  (let* ((year (token-tag-type 'scalar-year (first tokens)))
         (month (token-tag-type 'scalar-month (second tokens)))
         (day (token-tag-type 'scalar-day (third tokens)))
         (date-start (make-date year month day)))
    (merge-time-tokens-day (nthcdr 3 tokens) date-start)))

(define-handler (date handle-ambiguous-dmy)
    ((scalar-month separator-slash-or-dash scalar-month separator-slash-or-dash scalar-year (? separator-at) (? p time))
     (scalar-month separator-slash-or-dash scalar-month (? separator-at) (? p time)))
    (original-tokens &aux tokens)
  (setf tokens (remove-separators original-tokens))
  (destructuring-bind (day month)
      (ecase *endian-preference*
        (:little (list (token-tag-type 'scalar-day (first tokens))
                       (token-tag-type 'scalar-month (second tokens))))
        (:middle (list (token-tag-type 'scalar-month (second tokens))
                       (token-tag-type 'scalar-day (first tokens)))))
    (let ((year (if (and (fourth original-tokens)
                         (find-tag 'separator-slash-or-dash (fourth original-tokens)))
                    (token-tag-type 'scalar-year (third tokens))
                    (guess-year month day))))
      (merge-time-tokens-day (nthcdr 3 tokens) (make-date year month day)))))

(define-handler (date)
    ((scalar-day separator-slash-or-dash scalar-month separator-slash-or-dash scalar-year (? separator-at) (? p time))
     (scalar-day separator-slash-or-dash scalar-month (? separator-at) (? p time))
     (scalar-month separator-slash-or-dash scalar-day separator-slash-or-dash scalar-year (? separator-at) (? p time))
     (scalar-month separator-slash-or-dash scalar-day (? separator-at) (? p time)))
    (tokens)
  (let ((selected-pattern (handler-pattern *handler*)))
    (if (or (equalp selected-pattern (first *handler-patterns*))
            (equalp selected-pattern (second *handler-patterns*)))
        (let ((*endian-preference* :little))
          (handle-ambiguous-dmy tokens))
        (let ((*endian-preference* :middle))
          (handle-ambiguous-dmy tokens)))))

(define-handler (date)
    ((scalar-month separator-slash-or-dash scalar-year))
    (tokens)
  (setf tokens (remove-separators tokens))
  (let ((month (token-tag-type 'scalar-month (first tokens)))
        (year (token-tag-type 'scalar-year (second tokens))))
    (make-span (make-date year month)
               (datetime-incr (make-date year month) :month))))

;;; Anchors

(define-handler (anchor handle-r)
    (((? grabber) repeater (? separator-at) (? repeater) (? repeater))
     ((? grabber) repeater repeater (? separator-at) (? repeater) (? repeater)))
    (tokens)
  (get-anchor (dealias-and-disambiguate-time tokens)))

(define-handler (anchor)
    ((repeater grabber repeater))
    (tokens)
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
    ((scalar repeater pointer))
    (tokens)
  (let ((span (parse "this second" :guess nil :now *now*)))
    (handle-srp tokens span)))

(define-handler (arrow handle-p-s-r)
    ((pointer scalar repeater))
    (tokens)
  (handle-s-r-p (list (second tokens) (third tokens) (first tokens))))

(define-handler (arrow)
    ((scalar repeater pointer (? p anchor)))
    (tokens)
  (let ((anchor-span (awhen (nthcdr 3 tokens)
                       (get-anchor it))))
    (handle-srp tokens anchor-span)))

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
    ((ordinal repeater separator-in repeater))
    (tokens)
  (let ((outer-span (get-anchor (list (fourth tokens)))))
    (handle-orr (list (first tokens) (second tokens)) outer-span)))

(define-handler (narrow)
    ((ordinal repeater grabber repeater))
    (tokens)
  (let ((outer-span (get-anchor (list (third tokens) (fourth tokens)))))
    (handle-orr tokens outer-span)))

;;; Time handlers

(define-handler (time)
    ((repeater-time (? repeater-day-portion)))
    (tokens)
  (get-anchor (dealias-and-disambiguate-time tokens)))

