(cl:in-package #:chronicity)

(defparameter *morning* (make-span (make-time 6) (make-time 12)))
(defparameter *afternoon* (make-span (make-time 13) (make-time 17)))
(defparameter *evening* (make-span (make-time 17) (make-time 20)))
(defparameter *night* (make-span (make-time 20) (make-time 24)))

(defclass repeater-day-portion (repeater)
  ((range :initform nil)
   (current :initform nil)))

(defmethod initialize-instance :after ((repeater repeater-day-portion) &key type)
  (with-slots (range current)
      repeater
    (etypecase type
      (integer (setf range (make-span (make-time type) (make-time (+ type 12)))))
      (symbol (setf range (ecase type
                            (:am (make-span (make-time 0) (make-time 12) nil))
                            (:pm (make-span (make-time 12) (make-time 24) nil))
                            (:morning *morning*)
                            (:afternoon *afternoon*)
                            (:evening *evening*)
                            (:night *night*)))))))

(defmethod r-next ((repeater repeater-day-portion) pointer)
  (with-slots (range current now)
      repeater
    (if (not current)
        (let* ((now-sec (day-sec-of now))
               (range-start (span-start range))
               (range-start-sec (day-sec-of range-start))
               (start (cond
                        ((< now-sec range-start-sec)
                         (case pointer
                           (:future (merge-datetime now range-start))
                           (:past (merge-datetime (datetime-decr now :day) range-start))))
                        ((> now-sec range-start-sec)
                         (case pointer
                           (:future (merge-datetime (datetime-incr now :day) range-start))
                           (:past (merge-datetime now range-start))))
                        (t
                         (case pointer
                           (:future (merge-datetime (datetime-incr now :day) range-start))
                           (:past (merge-datetime (datetime-decr now :day) range-start)))))))
          (if start
              (setf current (make-span start (merge-datetime start (span-end range))))
              (error "Start should not be NIL.")))
        (case pointer
          (:future (setf current (span+ current 1 :day)))
          (:past (setf current (span- current 1 :day)))))))

(defmethod r-this ((repeater repeater-day-portion) pointer)
  (with-slots (range current now)
      repeater
    (let ((start (merge-datetime now (span-start range))))
      (setf current (make-span start (merge-datetime start (span-end range)))))))

;;; TODO: R-OFFSET?

(defmethod r-width ((repeater repeater-day-portion))
  (with-slots (range current now)
      repeater
    (or range (error "RANGE is not set.")) ; Isn't this redundant?
    (if current
        (- (datetime-to-universal (span-end current))
           (datetime-to-universal (span-start current)))
        (- (datetime-to-universal (span-end range))
           (datetime-to-universal (span-start range))))))