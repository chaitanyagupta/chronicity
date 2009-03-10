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

;;; Date and Time

;;;; Constructors

(defun make-date (year &optional (month 1) (day 1))
  (local-time:encode-timestamp 0 0 0 0 day month year))

(defun make-time (hour &optional (minute 0) (sec 0))
  (local-time:adjust-timestamp (local-time:today)
    (offset :hour hour)
    (offset :minute minute)
    (offset :sec sec)))

(defun make-date-time (year &optional (month 1) (day 1) (hour 0) (minute 0) (sec 0))
  (local-time:encode-timestamp 0 sec minute hour day month year))

(defun now ()
  (local-time:now))

(defun today ()
  (local-time:today))

;;;; Readers

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

;;;; Date time Calculations

(defun date-time+ (date-time amount unit)
  (local-time:timestamp+ date-time amount unit))

(defun date-time- (date-time amount unit)
  (local-time:timestamp- date-time amount unit))

(defun date-time-adjust (date-time value part)
  (local-time:adjust-timestamp date-time (set part value)))

;;; Time span

(defclass span ()
  ((start :initarg :start
          :reader span-start)
   (end :initarg :end
        :reader span-end)
   (end-included-p :initarg :end-included-p
                   :reader span-end-included-p
                   :initform t)))

(defun make-span (start end &optional (end-included-p t))
  (make-instance 'span :start start :end end :end-included-p end-included-p))



