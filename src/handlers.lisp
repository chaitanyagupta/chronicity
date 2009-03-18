(cl:in-package #:chronicity)

(defun make-handler (class pattern fn)
  (list class pattern fn))

(defun handler-class (handler)
  (first handler))

(defun handler-pattern (handler)
  (second handler))

(defun handler-fn (handler)
  (third handler))

(defparameter *handlers* nil)
(defvar *handler-patterns*)

(defun clear-handlers ()
  (setf *handlers* nil))

(defun find-class-handlers (class)
  (remove class *handlers* :key #'handler-class :test (complement #'eql)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-handler-name ()
    (intern (format nil "HANDLE-~A" (incf *gensym-counter*)) :chronicity)))

(defmacro define-handler ((class &optional (name (make-handler-name)))
                          patterns &optional lambda-list
                          &body body)
  `(progn
     (defun ,name ,lambda-list
       (let ((*handler-patterns* ',patterns))
         ,@body))
     (add-handler ',class ',patterns ',name)))

(defun add-handler (class patterns fn)
  (let ((handlers (loop
                     for pattern in patterns
                     collect (make-handler class pattern fn))))
    (setf *handlers* (nconc *handlers* handlers))))

(progn
      (list ; (make-handler 'time '(repeater-time (? repeater-day-portion)) nil)
            ))

(progn
      (list (make-handler 'date '(repeater-day-name repeater-month-name scalar-day repeater-time (? separator-slash-or-dash) time-zone scalar-year) 'handle-rdn-rmn-sd-t-tz-sy)
            ; (make-handler '(repeater-month-name scalar-day scalar-year) 'handle-rmn-sd-sy)
            ; (make-handler '(repeater-month-name scalar-day scalar-year (? separator-at) (? p time)) 'handle-rmn-sd-sy)
            ; (make-handler '(repeater-month-name scalar-day (? separator-at) (? p time)) 'handle-rmn-sd)
            ; (make-handler '(repeater-time (? repeater-day-portion) (? separator-on) repeater-month-name scalar-day) 'handle-rmn-sd-on)
            ; (make-handler '(repeater-month-name ordinal-day (? separator-at) (? p time)) 'handle-rmn-od)
            ; (make-handler '(repeater-time (? repeater-day-portion) (? separator-on) repeater-month-name ordinal-day) 'handle-rmn-od-on)
            ; (make-handler '(repeater-month-name scalar-year) 'handle-rmn-sy)
            ; (make-handler '(scalar-day repeater-month-name scalar-year (? separator-at) (? p time)) 'handle-sd-rmn-sy)
            ; *endian-handler-1*
            ; *endian-handler-2*
            ; (make-handler 'date '(scalar-year separator-slash-or-dash scalar-month separator-slash-or-dash scalar-day (? separator-at) (? p 'time)) 'handle-sy-sm-sd)
            ; (make-handler 'date '(scalar-day separator-slash-or-dash scalar-month) 'handle-sd-sm)
            ; (make-handler 'date '(scalar-month separator-slash-or-dash scalar-year) 'handle-sm-sy)
            ))

(progn
      (list (make-handler 'anchor '((? grabber) repeater (? separator-at) (? repeater) (? repeater)) 'handle-r)
            (make-handler 'anchor '((? grabber) repeater repeater (? separator-at) (? repeater) (? repeater)) 'handle-r)
            (make-handler 'anchor '(repeater grabber repeater) 'handle-r-g-r)))

(progn
      (list (make-handler 'arrow '(scalar repeater pointer) 'handle-s-r-p)
            (make-handler 'arrow '(pointer scalar repeater) 'handle-p-s-r)
            (make-handler 'arrow '(scalar repeater pointer (? p 'anchor)) 'handle-s-r-p-a)))

(progn
      (list (make-handler 'narrow '(ordinal repeater separator-in repeater) 'handle-o-r-s-r)
            (make-handler 'narrow '(ordinal repeater grabber repeater) 'handle-o-r-g-r)))

;;; TODO: Remove the unwanted tokens

(defvar *handler* nil
  "Current handler -- The one which matched the token.")

(defun match-tokens (handler tokens)
  (let ((tokens* tokens)
        (token-index 0)
        (pattern (handler-pattern handler)))
    (flet ((!next-token ()
             (incf token-index)
             (setf tokens* (cdr tokens*))
             (car tokens*))
           (!optionalp (element)
             (unless (atom element)
               (member '? element))))
      (loop
         for pattern* on pattern
         for element = (car pattern*)
         for token = (or (first tokens*)
                         (if (every #'!optionalp pattern*)
                             (return token-index)
                             (return nil)))
         for name = (if (atom element) element (first (last element)))
         for optionalp = (!optionalp element)
         for sub-handler-p = (unless (atom element)
                               (member 'p element))
         ;; Make sure NAME is actually a defined class
         unless sub-handler-p do (find-class name t)
         if (not sub-handler-p)
         do (if (token-has-tag-p token name)
                (!next-token)
                (unless optionalp (return nil)))
         else do (let ((sub-handlers (find-class-handlers name)))
                   (loop named inner-loop
                      for sub-handler in sub-handlers
                      thereis (awhen (match-tokens sub-handler tokens*)
                                (return (+ token-index it)))
                      finally (unless optionalp (return nil))))
         finally (if tokens*
                     (return nil)
                     (return token-index))))))

#|(defun tokens-to-span (tokens)
  (flet ((!match (class)
           (let ((handlers (find-class-handlers class)))
             (dolist (handler handlers)
               (awhen (match-tokens handler tokens)
                 (return-from tokens-to-span (funcall it tokens)))))))
    (!match 'date)
    (!match 'anchor)
    (!match 'arrow)
    (!match 'marrow)
    (!match 'time)))|#

(defun tokens-to-span (tokens)
  (dolist (handler *handlers* nil)
    (when (match-tokens handler tokens)
      (let ((*handler* handler))
        (return-from tokens-to-span (funcall (handler-fn handler) tokens))))))

(defun remove-separators (tokens)
  (remove-if #'(lambda (token)
                 (and (= (length (token-tags token)) 1)
                      (token-has-tag-p token 'separator)))
             tokens))

;;; Helpers

(defun get-anchor (tokens)
  (let ((grabber (create-tag 'grabber :this))
        (pointer :future)
        (repeaters (get-repeaters tokens))
        (head nil))
    (setf tokens (remove-if #'(lambda (x)
                                (token-has-tag-p x 'repeater))
                            tokens))
    (when (and (first tokens)
               (token-has-tag-p (first tokens) 'grabber))
      (setf grabber (find-tag 'grabber (first tokens))))
    (setf head (pop repeaters))
    (setf (tag-now head) *now*)
    (let ((outer-span nil))
      (ecase (tag-type grabber)
        (:last (setf outer-span (r-next head :past)))
        (:this (if (plusp (length repeaters))
                   (setf outer-span (r-this head :none))
                   (setf outer-span (r-this head *context*))))
        (:next (setf outer-span (r-next head :future))))
      (find-within repeaters outer-span pointer))))

(defun get-repeaters (tokens)
  (let ((repeaters (loop
                      for token in tokens
                      when (find-tag 'repeater token)
                      collect it)))
    (sort repeaters #'> :key #'r-width)))

(defun find-within (tags span pointer)
  (when (zerop (length tags))
    (return-from find-within span))
  (destructuring-bind (head &rest rest)
      tags
    (setf (tag-now head) (ecase pointer
                           (:future (span-start span))
                           (:past (span-end span))))
    (let ((h (r-this head :none)))
      (if (or (span-includes-p span (span-start h))
              (span-includes-p span (span-end h)))
          (find-within rest h pointer)
          nil))))

(defun merge-time-tokens-day (tokens date-start)
  (let ((time (awhen tokens
                (let ((*now* date-start))
                  (tokens-to-span it)))))
    (if time
        (make-span (merge-datetime date-start (copy-time (span-start time)))
                   (merge-datetime date-start (copy-time (span-end time))))
        (make-span date-start (datetime-incr date-start :day)))))

(defun dealias-time (tokens)
  (let* ((time-token (find-if #'(lambda (x)
                                  (find-tag 'repeater-time x))
                              tokens))
         (dp-token (find-if #'(lambda (x)
                                         (find-tag 'repeater-day-portion x))
                                     tokens)))
    (when (and dp-token time-token)
      (let ((dp-tag (find-tag 'repeater-day-portion dp-token)))
        (case (tag-type dp-tag)
          (:morning
           (untag 'repeater-day-portion dp-token)
           (tag (create-tag 'repeater-day-portion :am) dp-token))
          ((:afternoon :evening :night)
           (untag 'repeater-day-portion dp-token)
           (tag (create-tag 'repeater-day-portion :pm) dp-token)))))
    tokens))

