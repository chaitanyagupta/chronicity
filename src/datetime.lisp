;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; datetime.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

;;; Date and Time constructors

(deftype datetime ()
  'local-time:timestamp)

(defun make-date (year &optional (month 1) (day 1))
  (local-time:encode-timestamp 0 0 0 0 day month year))

(defun make-time (hour &optional (minute 0) (sec 0))
  (local-time:adjust-timestamp
      (local-time:timestamp-minimize-part (local-time:make-timestamp) :hour)
    (offset :hour hour)
    (offset :minute minute)
    (offset :sec sec)))

(defun make-datetime (year &optional (month 1) (day 1) (hour 0) (minute 0) (sec 0))
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

(defun copy-datetime (from &key
                       (year (year-of from))
                       (month (month-of from))
                       (day (day-of from))
                       (hour (hour-of from))
                       (minute (minute-of from))
                       (sec (sec-of from)))
  (make-datetime year month day hour minute sec))

(defun merge-datetime (date time)
  (let* ((epoch-time (make-time 0 0 0))
         (diff-sec (local-time:timestamp-difference time epoch-time))
         (diff-days (truncate diff-sec #.(* 24 60 60))))
    (datetime-incr (make-datetime (year-of date) (month-of date) (day-of date)
                                  (hour-of time) (minute-of time) (sec-of time))
                   :day
                   diff-days)))

(defun now ()
  (local-time:now))

(defun today ()
  (local-time:today))

;;; Date time Readers

(defun year-of (datetime)
  (local-time:timestamp-year datetime))

(defun month-of (datetime)
  (local-time:timestamp-month datetime))

(defun day-of (datetime)
  (local-time:timestamp-day datetime))

(defun hour-of (datetime)
  (local-time:timestamp-hour datetime))

(defun minute-of (datetime)
  (local-time:timestamp-minute datetime))

(defun sec-of (datetime)
  (local-time:timestamp-second datetime))

(defun dow-of (datetime)
  (local-time:timestamp-day-of-week datetime))

(defun day-sec-of (datetime)
  "Returns the second of the day."
  (+ (* (hour-of datetime) 3600)
     (* (minute-of datetime) 60)
     (sec-of datetime)))

;;; Date time Calculations

(defun datetime-incr (datetime unit &optional (amount 1))
  (case unit
    (:week (datetime-incr datetime :day (* 7 amount)))
    (t (local-time:timestamp+ datetime amount unit))))

(defun datetime-decr (datetime unit &optional (amount 1))
  (datetime-incr datetime unit (- amount)))

(defun datetime-adjust (datetime part value)
  (case part
    (:day-of-week (local-time:adjust-timestamp datetime (offset part value)))
    (t (local-time:adjust-timestamp datetime (set part value)))))

(defmacro datetime-incf (place unit &optional (amount 1))
  `(setf ,place (datetime-incr ,place ,unit ,amount)))

(defmacro datetime-decf (place unit &optional (amount 1))
  `(setf ,place (datetime-decr ,place ,unit ,amount)))

;;; Date time comparisons

(defun datetime< (&rest args)
  (apply #'local-time:timestamp< args))

(defun datetime<= (&rest args)
  (apply #'local-time:timestamp<= args))

(defun datetime> (&rest args)
  (apply #'local-time:timestamp> args))

(defun datetime>= (&rest args)
  (apply #'local-time:timestamp>= args))

(defun datetime= (&rest args)
  (apply #'local-time:timestamp= args))

(defun datetime/= (&rest args)
  (apply #'local-time:timestamp/= args))

(defun datetime-to-universal (datetime)
  (local-time:timestamp-to-universal datetime))

(defun universal-to-datetime (universal-time)
  (local-time:universal-to-timestamp universal-time))

;;; Miscellaneous query operations on datetime objects

(defun start-of-year (datetime)
  (local-time:timestamp-minimize-part datetime :month))

(defun end-of-year (datetime)
  (local-time:timestamp-maximize-part datetime :month))

(defun start-of-month (datetime)
  (local-time:timestamp-minimize-part datetime :day))

(defun end-of-month (datetime)
  (local-time:timestamp-maximize-part datetime :day))

(defun start-of-day (datetime)
  (local-time:timestamp-minimize-part datetime :hour))

(defun end-of-day (datetime)
  (local-time:timestamp-maximize-part datetime :hour))

(defun start-of-hour (datetime)
  (local-time:timestamp-minimize-part datetime :min))

(defun end-of-hour (datetime)
  (local-time:timestamp-maximize-part datetime :min))

(defun start-of-minute (datetime)
  (local-time:timestamp-minimize-part datetime :sec))

(defun end-of-minute (datetime)
  (local-time:timestamp-maximize-part datetime :sec))

(defun start-of-week (datetime)
  (start-of-day (datetime-adjust datetime :day-of-week :sunday)))

(defun end-of-week (datetime)
  (end-of-day (datetime-adjust datetime :day-of-week :saturday)))

;;; Time span

(defclass span ()
  ((start :initarg :start
          :reader span-start)
   (end :initarg :end
        :reader span-end)
   (end-included-p :initarg :end-included-p
                   :reader span-end-included-p
                   :initform nil)))

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

(defun make-span (start end &optional (end-included-p nil))
  (make-instance 'span :start start :end end :end-included-p end-included-p))

(defun span-width (span)
  (- (datetime-to-universal (span-end span))
     (datetime-to-universal (span-start span))))

(defun span+ (span amount unit)
  (make-span (datetime-incr (span-start span) unit amount)
             (datetime-incr (span-end span) unit amount)))

(defun span- (span amount unit)
  (make-span (datetime-decr (span-start span) unit amount)
             (datetime-decr (span-end span) unit amount)))

(defun span-includes-p (span datetime)
  (if (span-end-included-p span)
      (datetime<= (span-start span) datetime (span-end span))
      (and (datetime<= (span-start span) datetime)
           (datetime< datetime (span-end span)))))

;;; Miscellaneous data

(defparameter *months*
  (list :JANUARY
        :FEBRUARY
        :MARCH
        :APRIL
        :MAY
        :JUNE
        :JULY
        :AUGUST
        :SEPTEMBER
        :OCTOBER
        :NOVEMBER
        :DECEMBER))

(defun month-name (index)
  (elt *months* (1- index)))

(defun month-index (name)
  (1+ (position name *months*)))

(defparameter *days-of-week*
  (list :SUNDAY
        :MONDAY
        :TUESDAY
        :WEDNESDAY
        :THURSDAY
        :FRIDAY
        :SATURDAY))

(defun dow-index (name)
  (position name *days-of-week*))

(defun dow-name (index)
  (elt *days-of-week* index))

