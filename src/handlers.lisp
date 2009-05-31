;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; handlers.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defun make-handler (class pattern fn)
  (list class pattern fn))

(defun handler-class (handler)
  (first handler))

(defun handler-pattern (handler)
  (second handler))

(defun handler-fn (handler)
  (third handler))

(defvar *handlers* nil)
(defvar *handler-patterns*)

(defun clear-handlers ()
  (setf *handlers* nil))

(defun find-class-handlers (class)
  (remove class *handlers* :key #'handler-class :test (complement #'eql)))

(defmacro define-handler ((class &optional (name (gentemp "HANDLE-" :chronicity)))
                          lambda-list patterns
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

;;; Token matcher

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

(defun tokens-to-span (tokens)
  (flet ((!match (class tokens)
           (let ((handlers (find-class-handlers class)))
             (dolist (handler handlers)
               (when (match-tokens handler tokens)
                 (let ((*handler* handler))
                   (return-from tokens-to-span (funcall (handler-fn handler) tokens))))))))
    (!match 'date (remove-if #'(lambda (x)
                                 (and (find-tag 'separator x)
                                      (not (find-tag 'separator-slash-or-dash x))))
                             tokens))
    (!match 'anchor (remove-if #'(lambda (x)
                                   (and (find-tag 'separator x)
                                        (not (find-tag 'separator-in x))))
                               tokens))
    (!match 'arrow (remove-if #'(lambda (x)
                                  (or (find-tag 'separator-at x)
                                      (find-tag 'separator-slash-or-dash x)
                                      (find-tag 'separator-comma x)))
                              tokens))
    (!match 'narrow tokens)
    (!match 'time tokens)))

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
                  (get-anchor (dealias-and-disambiguate-time tokens))))))
    (or time
        (make-span date-start (datetime-incr date-start :day)))))

(defun dealias-and-disambiguate-time (tokens)
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
    (when *ambiguous-time-range*
      (let ((time-tag (and time-token (find-tag 'repeater-time time-token))))
        (when (and time-tag
                   (tick-ambiguousp (tag-type time-tag))
                   (not dp-token))
          (push (create-token "disambiguator"
                              (create-tag 'repeater-day-portion *ambiguous-time-range*))
                tokens))))
    tokens))

