(cl:in-package #:chronicity)

(defclass repeater-weekend (repeater)
  ((current-weekend-start :initform nil)))

(defmethod r-next ((repeater repeater-weekend) pointer)
  (with-slots (current-weekend-start now)
      repeater
    (if (not current-weekend-start)
        (case pointer
          (:future
           (let ((sat-repeater (create-tag 'repeater-day-name :saturday)))
             (setf (tag-now sat-repeater) now)
             (setf current-weekend-start (span-start (r-next sat-repeater :future)))))
          (:past
           (let ((sat-repeater (create-tag 'repeater-day-name :saturday)))
             (setf (tag-now sat-repeater) (datetime-1+ now :day))
             (setf current-weekend-start (span-start (r-next sat-repeater :past))))))
        (let ((direction (if (eql pointer :future) 1 -1)))
          (setf current-weekend-start (datetime+ current-weekend-start direction :week))))
    (make-span current-weekend-start (datetime+ current-weekend-start 2 :day))))

;;; TODO: We should fix, and understand this better
(defmethod r-this ((repeater repeater-weekend) pointer)
  (with-slots (now)
      repeater
    (case pointer
      ((:future :none)
       (let ((sat-repeater (create-tag 'repeater-day-name :saturday)))
         (setf (tag-now sat-repeater) now)
         (let ((saturday (span-start (r-next sat-repeater :future))))
           (make-span saturday (datetime+ saturday 2 :day)))))
      (:past
       (let ((sat-repeater (create-tag 'repeater-day-name :saturday)))
         (setf (tag-now sat-repeater) now)
         (let ((saturday (span-start (r-next sat-repeater :past))))
           (make-span saturday (datetime+ saturday 2 :day))))))))

;;; TODO: R-OFFSET?

(defmethod r-width ((repeater repeater-weekend))
  +weekend-seconds+)

