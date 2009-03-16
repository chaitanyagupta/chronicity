(cl:in-package #:chronicity)

#.(cl-interpol:enable-interpol-syntax)

(defclass repeater-time (repeater)
  ((current :initform nil)))

(defclass tick ()
  ((time :initarg :time :accessor tick-time)
   (ambiguousp :initarg :ambiguousp :accessor tick-ambiguousp)))

(defmethod print-object ((x tick) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~A~:[~; AMBIGUOUS~]"
            (tick-time x)
            (tick-ambiguousp x))))

(defun make-tick (time ambiguousp)
  (make-instance 'tick :time time :ambiguousp ambiguousp))

(defmethod initialize-instance :after ((repeater repeater-time) &key type)
  (let* ((ts (cl-ppcre:regex-replace-all #?r"\:" type ""))
         (ts-size (length ts)))
    (setf (slot-value repeater 'type)
          (cond
            ((<= 1 ts-size 2)
             (let ((hours (parse-integer ts)))
               (if (= hours 12)
                   (make-tick (make-time 0) t) ; Now why would I do that?
                   (make-tick (make-time hours) t))))
            ((= ts-size 3)
             (make-tick (make-time (parse-integer ts :start 0 :end 1)
                                   (parse-integer ts :start 1))
                        t))
            ((= ts-size 4)
             (let* ((hours (parse-integer ts :end 2))
                    (minutes (parse-integer ts :start 2))
                    (ambiguousp (and (scan ":" type)
                                     (/= (parse-integer ts :end 1) 0) ; Redundant?
                                     (<= hours 12))))
               (if (= hours 12)
                   (make-tick (make-time 0 minutes) ambiguousp) ; Still makes no sense to me
                   (make-tick (make-time hours minutes) ambiguousp))))
            ((= ts-size 5)
             (make-tick (make-time (parse-integer ts :end 1)
                                   (parse-integer ts :start 1 :end 3)
                                   (parse-integer ts :start 3 :end 5))
                        t))
            ((= ts-size 6)
             (let* ((hours (parse-integer ts :end 2))
                    (minutes (parse-integer ts :start 2 :end 4))
                    (secs (parse-integer ts :start 4))
                    (ambiguousp (and (scan ":" type)
                                     (/= (parse-integer ts :end 1) 0) ; Redundant?
                                     (<= hours 12))))
               (if (= hours 12)
                   (make-tick (make-time 0 minutes secs) ambiguousp)
                   (make-tick (make-time hours minutes secs) ambiguousp))))
            (t (error "TIME cannot exceed 6 digits."))))))

(defmethod r-next ((repeater repeater-time) pointer)
  (let* ((first-time-p nil)
         (tick (tag-type repeater))
         (tick-time (tick-time tick))
         (halfday-hours 12))
    (with-slots (current now)
        repeater
      (unless current
        (setf first-time-p t)
        (let* ((midnight (copy-date now))
               (yesterday-midnight (datetime-decr midnight :day))
               (tomorrow-midnight (datetime-incr midnight :day))
               (midnight+tick (merge-datetime midnight tick-time))
               (midday+tick (datetime-incr midnight+tick :hour halfday-hours))
               (tomorrow+tick (merge-datetime tomorrow-midnight tick-time))
               (yesterday+tick (merge-datetime yesterday-midnight tick-time)))
          (if (eql pointer :future)
              (if (tick-ambiguousp tick)
                  (loop
                     for time in (list midnight+tick midday+tick tomorrow+tick)
                     thereis (and (datetime>= time now) (setf current time)))
                  (loop
                     for time in (list midnight+tick tomorrow+tick)
                     thereis (and (datetime>= time now) (setf current time))))
              (if (tick-ambiguousp tick)
                  (loop
                     for time in (list midday+tick midnight+tick (datetime-incr yesterday+tick :hour halfday-hours))
                     thereis (and (datetime<= time now) (setf current time)))
                  (loop
                     for time in (list midnight+tick yesterday+tick)
                     thereis (and (datetime<= time now) (setf current time)))))))
      (unless first-time-p
        (setf current (if (tick-ambiguousp tick)
                          (datetime-incr current :hour (if (eql pointer :future) halfday-hours (- halfday-hours)))
                          (datetime-incr current :day (if (eql pointer :future) 1 -1)))))
      (make-span current (datetime-incr current :sec)))))

(defmethod r-this ((repeater repeater-time) pointer)
  (when (eql pointer :none)
    (setf pointer :future))
  (r-next repeater pointer))

(defmethod r-width ((repeater repeater-time))
  1)

#.(cl-interpol:disable-interpol-syntax)