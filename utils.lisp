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

;;; Date and Time constructors

(defun make-date (year &optional (month 1) (day 1))
  (local-time:encode-timestamp 0 0 0 0 day month year))

(defun make-time (hour &optional (minute 0) (sec 0))
  (local-time:adjust-timestamp (local-time:today)
    (offset :hour hour)
    (offset :minute minute)
    (offset :sec sec)))

(defun make-date-time (year &optional (month 1) (day 1) (hour 0) (minute 0) (sec 0))
  (local-time:encode-timestamp 0 sec minute hour day month year))

(defun copy-date (from &key
                  (year (year-of from))
                  (month (month-of from))
                  (day (day-of from)))
  (make-date year month day))

(defun copy-time (from &key
                  (hour (hour-of from))
                  (minute (minute-of from))
                  (sec (sec-of from)))
  (make-time hour minute sec))

(defun copy-date-time (from &key
                       (year (year-of from))
                       (month (month-of from))
                       (day (day-of from))
                       (hour (hour-of from))
                       (minute (minute-of from))
                       (sec (sec-of from)))
  (make-date-time year month day hour minute sec))

(defun now ()
  (local-time:now))

(defun today ()
  (local-time:today))

;;; Date time Readers

(defun year-of (date-time)
  (local-time:timestamp-year date-time))

(defun month-of (date-time)
  (local-time:timestamp-month date-time))

(defun day-of (date-time)
  (local-time:timestamp-day date-time))

(defun hour-of (date-time)
  (local-time:timestamp-hour date-time))

(defun minute-of (date-time)
  (local-time:timestamp-minute date-time))

(defun sec-of (date-time)
  (local-time:timestamp-second date-time))

;;; Date time Calculations

;;;; Looks like LOCAL-TIME's month addition/subtraction is broken :(
;;;; Direct translation of Chronic's Chronic::RepeaterMonth.offset_by
(defun date-time-month+ (date-time amount)
  (let* ((amount-years (floor amount 12))
         (amount-months (mod amount 12))
         (new-year (+ (year-of date-time) amount-years))
         (new-month (+ (month-of date-time) amount-months)))
    (when (> new-month 12)
      (incf new-year)
      (decf new-month 12))
    (copy-date-time date-time :year new-year :month new-month)))

(defun date-time+ (date-time amount unit)
  (case unit
    (:month (date-time-month+ date-time amount))
    (t (local-time:timestamp+ date-time amount unit))))

(defun date-time- (date-time amount unit)
  (date-time+ date-time (- amount) unit))

(defun date-time-1+ (date-time unit)
  (date-time+ date-time 1 unit))

(defun date-time-1- (date-time unit)
  (date-time- date-time 1 unit))

(defun date-time-adjust (date-time value part)
  (local-time:adjust-timestamp date-time (set part value)))

(defun date-time-maximize-part (date-time part)
  (local-time:timestamp-maximize-part date-time part))

(defun date-time-minimize-part (date-time part)
  (local-time:timestamp-minimize-part date-time part))

;;; Time span

(defclass span ()
  ((start :initarg :start
          :reader span-start)
   (end :initarg :end
        :reader span-end)
   (end-included-p :initarg :end-included-p
                   :reader span-end-included-p
                   :initform t)))

(defmethod print-object ((x span) stream)
  (flet ((!format-span ()
           (format stream "~A~:[...~;..~]~A"
                (span-start x)
                (span-end-included-p x)
                (span-end x))))
    (if *print-escape*
        (print-unreadable-object (x stream :type t)
          (!format-span))
        (!format-span))))

(defun make-span (start end &optional (end-included-p t))
  (make-instance 'span :start start :end end :end-included-p end-included-p))



