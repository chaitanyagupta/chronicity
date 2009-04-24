;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; numerize.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:cl-user)

(defpackage #:chronicity-numerizer
  (:use #:cl)
  (:export #:numerize))

(in-package #:chronicity-numerizer)

#.(cl-interpol:enable-interpol-syntax)

(defvar *direct-nums*
  '(("(eleven|eleventh)" 11)
    ("(twelve|twelfth)" 12)
    ("(thirteen|thirteenth)" 13)
    ("(fourteen|fourteenth)" 14)
    ("(fifteen|fifteenth)" 15)
    ("(sixteen|sixteenth)" 16)
    ("(seventeen|seventeenth)" 17)
    ("(eighteen|eighteenth)" 18)
    ("(nineteen|ninteen|nineteenth)" 19)    ; Common mis-spelling
    ("(zero|zeroth)" 0)
    ("(one|first)" 1)
    ("two" 2) ; Numerization for second is taken care of after
              ; processing tokens, see CHRONICITY::PRE-PROCESS-TOKENS
    ("(three|third)" 3)
    (#?r"(\bfour\b|fourth|forth)" 4)         ; So that it matches four but not fourty
    ("(five|fifth)" 5)
    (#?r"(\bsix\b|sixth)" 6)
    (#?r"(\bseven\b|seventh)" 7)
    (#?r"(\beight\b|eighth)" 8)
    (#?r"(\bnine\b|ninth|nineth)" 9)
    ("(ten|tenth)" 10)
    ; (#?r"\ba[\b^$]" 1) ; doesn't make sense for an 'a' at the end to be a 1
    ))

(defvar *ten-prefixes*
  '(("(twenty|twentieth)" 20)
    ("(thirty|thirtieth)" 30)
    ("(fourty|forty|fourtieth|fortieth)" 40)
    ("(fifty|fiftieth)" 50)
    ("(sixty|sixtieth)" 60)
    ("(seventy|seventieth)" 70)
    ("(eighty|eightieth)" 80)
    ("(ninety|ninetieth)" 90)))

(defvar *big-prefixes*
  '(("(hundred|hundredth)" 100)
    ("(thousand|thousandth)" 1000)
    ("(lakh|lac)" 100000)
    ("(million|millionth)" 1000000)
    ("crore" 10000000)
    ("(billion|billionth)" 1000000000)
    ("(trillion|trillionth)" 1000000000000)))

(defvar *big-detector-regex*
  (let ((big-or (format nil "(~{~A~^|~})"
                        (mapcar #'first (append *direct-nums*
                                                *ten-prefixes*
                                                *big-prefixes*)))))
    #?r"${big-or}((\s|-)+${big-or})*"))

(defun numerize (string)
  ;; Some normalization
  (setf string (cl-ppcre:regex-replace-all #?r"\band\b" string ""))
  (let ((start 0)
        (diff 0))
    (loop
       (multiple-value-bind (start2 end2)
           (and (< start (length string))
                (detect-numeral-sequence string :start start))
         (unless start2
           (return))
         (let ((number-string (numerize-aux (subseq string start2 end2))))
           (when number-string
             (setf (values string diff)
                   (replace-numeral-sequence string start2 end2 number-string)))
           (setf start (- end2 diff)))))
    string))

(defun detect-numeral-sequence (string &key (start 0))
  (cl-ppcre:scan *big-detector-regex* string :start start))

(defun numerize-aux (string)
  (let ((tokens (reverse (cl-ppcre:split #?r"(\s|-)+" string))))
    (setf tokens (remove-if-not #'numeric-token-p tokens))
    (let* ((number (tokens-to-number tokens))
           (number-string (format nil "~A" number)))
      ;; Check if the numeral was an ordinal. If so, append the
      ;; (st|nd|rd|th) to the end of the number
      (multiple-value-bind (match regs)
          (cl-ppcre:scan-to-strings "(fir(st)|seco(nd)|thi(rd)|\\w+(th)$)"
                                    (first tokens))
        (if match
            (concatenate 'string number-string (find-if #'identity (subseq regs 1)))
            number-string)))))

(defun replace-numeral-sequence (string start end number-string)
  (values
   (concatenate 'string
                (subseq string 0 start)
                number-string
                (subseq string end))
   (- (- end start) (length number-string))))

(defun tokens-to-number (tokens)
  ;; TOKENS should be in reverse order i.e. the rightmost token in the
  ;; string should be first.
  (when (big-prefix-p (first (last tokens)))
    (setf tokens (append tokens (list "one"))))
  (let* ((sum 0)
         (multiplier 1)
         (tsum 0)
         (tokens* tokens))
    (loop
       (unless tokens* (return))
       (let ((token (first tokens*)))
         (cond
           ((and (big-prefix-p token)
                 (> (token-numeric-value token) multiplier))
            (incf sum (* tsum multiplier))
            (setf tsum 0
                  multiplier (token-numeric-value token)))
           ((big-prefix-p token)
            (let ((next-big-multiplier (or (position-if #'(lambda (x)
                                                            (> (token-numeric-value x) multiplier))
                                                        tokens*)
                                           (length tokens*))))
              (let ((new-sum (tokens-to-number (subseq tokens* 0 next-big-multiplier))))
                (incf tsum new-sum)
                (setf tokens* (nthcdr (1- next-big-multiplier) tokens*)))))
           (t (incf tsum (token-numeric-value token)))))
       (setf tokens* (cdr tokens*)))
    (incf sum (* tsum multiplier))
    sum))

(defun numeric-token-p (string)
  (dolist (list (list *direct-nums* *ten-prefixes* *big-prefixes*))
    (dolist (numeral-pair list)
      (when (cl-ppcre:scan (first numeral-pair) string)
        (return-from numeric-token-p t)))))

(defun token-numeric-value (string)
  (dolist (list (list *direct-nums* *ten-prefixes* *big-prefixes*))
    (dolist (numeral-pair list)
      (when (cl-ppcre:scan (first numeral-pair) string)
        (return-from token-numeric-value (second numeral-pair))))))

(defun big-prefix-p (string)
  (dolist (numeral-pair *big-prefixes*)
    (when (cl-ppcre:scan (first numeral-pair) string)
      (return-from big-prefix-p t))))

;;; Disable cl-interpol reader

#.(cl-interpol:disable-interpol-syntax)









