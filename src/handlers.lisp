(cl:in-package #:chronicity)

(defparameter *handlers* (make-hash-table))
(defvar *endian-handler-1* nil)
(defvar *endian-handler-2* nil)

(defun make-handler (pattern fn)
  (cons pattern fn))

(defun handler-pattern (handler)
  (car handler))

(defun handler-fn (handler)
  (cdr handler))

(defun clear-handlers ()
  (clrhash *handlers*))

(defun find-class-handlers (class)
  (gethash class *handlers*))

(defmacro define-handler ((name &rest patterns) &optional lambda-list &body body)
  (let ((class (intern (first (cl-ppcre:split "-" (string name))) :chronicity)))
    `(progn
       , (when (find #\- (string name))
           `(defun ,name ,lambda-list
              ,@body))
       (add-handler ',class ',patterns ',name))))

(defun add-handler (class patterns fn)
  (let ((handlers (loop
                     for pattern in patterns
                     collect (make-handler pattern fn))))
    (setf (gethash class *handlers*)
          (nconc (gethash class *handlers*) handlers))))

(setf (gethash 'time *handlers*)
      (list (make-handler '(repeater-time (? repeater-day-portion)) nil))) ; DONE

(setf (gethash 'date *handlers*)
      (list (make-handler '(repeater-day-name repeater-month-name scalar-day repeater-time (? separator-slash-or-dash) time-zone scalar-year) 'handle-rdn-rmn-sd-t-tz-sy)
            (make-handler '(repeater-month-name scalar-day scalar-year) 'handle-rmn-sd-sy) ; DONE
            (make-handler '(repeater-month-name scalar-day scalar-year (? separator-at) (? p time)) 'handle-rmn-sd-sy) ; DONE
            (make-handler '(repeater-month-name scalar-day (? separator-at) (? p time)) 'handle-rmn-sd) ; DONE
            (make-handler '(repeater-time (? repeater-day-portion) (? separator-on) repeater-month-name scalar-day) 'handle-rmn-sd-on) ; DONE
            (make-handler '(repeater-month-name ordinal-day (? separator-at) (? p time)) 'handle-rmn-od)
            (make-handler '(repeater-time (? repeater-day-portion) (? separator-on) repeater-month-name ordinal-day) 'handle-rmn-od-on)
            (make-handler '(repeater-month-name scalar-year) 'handle-rmn-sy)
            (make-handler '(scalar-day repeater-month-name scalar-year (? separator-at) (? p time)) 'handle-sd-rmn-sy)
            *endian-handler-1*
            *endian-handler-2*
            (make-handler '(scalar-year separator-slash-or-dash scalar-month separator-slash-or-dash scalar-day (? separator-at) (? p 'time)) 'handle-sy-sm-sd)
            (make-handler '(scalar-day separator-slash-or-dash scalar-month) 'handle-sd-sm)
            (make-handler '(scalar-month separator-slash-or-dash scalar-year) 'handle-sm-sy)))

(setf (gethash 'anchor *handlers*)
      (list (make-handler '((? grabber) repeater (? separator-at) (? repeater) (? repeater)) 'handle-r)
            (make-handler '((? grabber) repeater repeater (? separator-at) (? repeater) (? repeater)) 'handle-r)
            (make-handler '(repeater grabber repeater) 'handle-r-g-r)))

(setf (gethash 'arrow *handlers*)
      (list (make-handler '(scalar repeater pointer) 'handle-s-r-p)
            (make-handler '(pointer scalar repeater) 'handle-p-s-r)
            (make-handler '(scalar repeater pointer (? p 'anchor)) 'handle-s-r-p-a)))

(setf (gethash 'narrow *handlers*)
      (list (make-handler '(ordinal repeater separator-in repeater) 'handle-o-r-s-r)
            (make-handler '(ordinal repeater grabber repeater) 'handle-o-r-g-r)))

;;; TODO: Remove the unwanted tokens
(defun match-tokens (handler tokens)
  (let ((tokens* tokens)
        (pattern (handler-pattern handler)))
    (flet ((!next-token ()
             (setf tokens* (cdr tokens*))
             (car tokens*))
           (!optionalp (element)
             (unless (atom element)
               (member '? element))))
      (loop named loop
         for pattern* on pattern
         for element = (car pattern*)
         for token = (or (first tokens*)
                         (if (every #'!optionalp pattern*)
                             (return-from loop nil)
                             (return-from match-tokens nil)))
         for name = (if (atom element) element (second element))
         for optionalp = (!optionalp element)
         for sub-handler-p = (unless (atom element)
                               (member 'p element))
         ;; Make sure NAME is actually a defined class
         unless sub-handler-p do (find-class name t)
         if (not sub-handler-p)
         do (if (token-has-tag-p token name)
                (!next-token)
                (unless optionalp (return-from match-tokens nil)))
         else do (let ((sub-handlers (find-class-handlers name)))
                   (loop
                      for sub-handler in sub-handlers
                      thereis (match-tokens sub-handler tokens*)
                      finally (unless optionalp (return-from match-tokens nil))))))
    (handler-fn handler)))

;;; TODO: Do this!
(defun tokens-to-span (tokens)
  (flet ((!match (class)
           (let ((handlers (find-class-handlers class)))
             (dolist (handler handlers)
               (awhen (match-tokens handler tokens)
                 (return-from tokens-to-span (funcall it tokens)))))))
    (!match 'date)
    (!match 'anchor)
    (!match 'arrow)
    (!match 'marrow)
    (!match 'time)))

(defun remove-separators (tokens)
  (remove-if #'(lambda (token)
                 (and (= (length (token-tags token)) 1)
                      (token-has-tag-p token 'separator)))
             tokens))



