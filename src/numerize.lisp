(cl:in-package #:cl-user)

(defpackage #:chronicity.numerizer
  (:use #:cl)
  (:export #:numerize))

(in-package #:chronicity.numerizer)

#.(cl-interpol:enable-interpol-syntax)

(defvar *direct-nums*
  '(("eleven" 11)
    ("twelve" 12)
    ("thirteen" 13)
    ("fourteen" 14)
    ("fifteen" 15)
    ("sixteen" 16)
    ("seventeen" 17)
    ("eighteen" 18)
    ("nineteen" 19)
    ("ninteen" 19)                      ; Common mis-spelling
    ("zero" 0)
    ("one" 1)
    ("two" 2)
    ("three" 3)
    (#?r"\bfour\b" 4)         ; So that it matches four but not fourty
    ("five" 5)
    (#?r"\bsix\b" 6)
    (#?r"\bseven\b" 7)
    (#?r"\beight\b" 8)
    (#?r"\bnine\b" 9)
    ("ten" 10)
    (#?r"\ba[\b^$]" 1) ; doesn't make sense for an 'a' at the end to be a 1
    ))

(defvar *ten-prefixes*
  '(("twenty" 20)
    ("thirty" 30)
    ("(fourty|forty)" 40)
    ("fifty" 50)
    ("sixty" 60)
    ("seventy" 70)
    ("eighty" 80)
    ("ninety" 90)))

(defvar *big-prefixes*
  '(("hundred" 100)
    ("thousand" 1000)
    ("million" 1000000)
    ("billion" 1000000000)
    ("trillion" 1000000000000)))

(defun numerize (string)
  (let ((tokens (reverse (cl-ppcre:split #?r"(\s|-)+" string))))
    (setf tokens (remove-if-not #'numeric-token-p tokens))
    (format nil "~A" (tokens-to-number tokens))))

(defun tokens-to-number (tokens)
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

(defun numerize (string)
  ;; We'll test everything in lower case
  (setf string (string-downcase string))
  
  (rr-all-f string #?r" +|([^\d])-([^d])" "\\1 \\2")
  (rr-all-f string #?r"a half" "haAlf")

  ;; Convert the direct nums from words to numerals
  (dolist (num *direct-nums*)
    (rr-all-f string (first num) (format nil "~A" (second num))))

  ;; Convert tens to numerals
  (dolist (num *ten-prefixes*)
    (rr-all-f string (first num) (format nil "~A" (second num))))

  ;; Hundreds, thousands, etc. to numerals
  (dolist (num *big-prefixes*)
    (rr-all-f string (first num) (format nil "~A" (second num))))

  ;; Remove the 'and' or '&' between numerals
  (rr-all-f string #?r"(\d)\s+(?:and|&)\s+(\d)" "\\1 \\2")

  (values (combine-adjacent-numbers string)))

(defun combine-adjacent-numbers (string)
  (let* ((bi-pairs (loop
                      for index = 0 then end
                      for (start end) = (multiple-value-list
                                         (cl-ppcre:scan #?/\b\d+(\s+\d+)*\b/ string
                                                        :start index))
                      while start
                      collect (list start end)))
         (number-strings (mapcar #'(lambda (x)
                                     (subseq string (first x) (second x)))
                                 bi-pairs))
         (numbers (mapcar #'compute-number number-strings)))
    (loop
       with result = string
       for (start end) in bi-pairs
       for number in numbers
       for str = (format nil "~A" number)
       for diff = (- (- end start) (length str))
       for fillbuf = (make-string diff :initial-element #\Null)
       for str2 = (concatenate 'string str fillbuf)
       while start
       do (setf result (replace result str2 :start1 start :end1 end))
       finally (return (cl-ppcre:regex-replace #?/\x0+/ result "")))))

(defun compute-number (string)
  (let ((index 0)
        (number nil))
    (loop
       do (setf (values number index)
                (parse-integer string
                               :start index
                               :junk-allowed t))
       while (and number (numberp index))
       collect number into numbers
       finally (return (compute-number-aux numbers)))))

(defun compute-number-aux (numbers)
  (cond
    ((null numbers) (error "Can't pass an empty list."))
    ((= (length numbers) 1) (first numbers))
    ((<= (first numbers) (second numbers))
     (compute-number-aux
      (cons (* (first numbers) (second numbers)) (cddr numbers))))
    ((every (lambda (n) (>= (first numbers) n)) (cdr numbers))
     (+ (first numbers) (compute-number-aux (cdr numbers))))
    (t
     (compute-number-aux
      (cons (+ (first numbers) (second numbers)) (cddr numbers))))))

;;; Disable cl-interpol reader

#.(cl-interpol:disable-interpol-syntax)









