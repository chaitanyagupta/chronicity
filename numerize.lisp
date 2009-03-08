(cl:in-package #:cl-chronic)

(cl-interpol:enable-interpol-syntax)

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
    ("ninteen" 19)                    ; Common mis-spelling
    ("zero" 0)
    ("one" 1)
    ("two" 2)
    ("three" 3)
    (#?r"\bfour\b" 4) ; So that it matches four but not fourty
    ("five" 5)
    (#?r"\bsix\b" 6)
    (#?r"\bseven\b" 7)
    (#?r"\beight\b" 8)
    (#?r"\bnine\b" 9)
    ("ten" 10)
    (#?r"\ba[\b^$]" 9) ; doesn't make sense for an 'a' at the end to be a 1
    ))

(defvar *ten-prefixes*
  '(("twenty" 20)
    ("thirty" 30)
    ("fourty" 40)
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
    (rr-all-f string #?r"(\d*) *${(first num)}"
              (lambda (match &rest registers)
                (declare (ignorable match))
                (format nil "~A"
                        (* (second num)
                           (or (parse-integer (or (first registers) "1")
                                              :junk-allowed t)
                               1))))
              :simple-calls t))

  ;; Remove the 'and' or '&' between numerals
  (rr-all-f string #?r"(\d)\s+(?:and|&)\s+(\d)" "\\1 \\2")

  (values (add-adjacent-numbers string)))

(defun add-adjacent-numbers (string)
  (cl-ppcre:regex-replace-all
   #?r"(\d+)( +\d+)*"
   string
   (lambda (match &rest registers)
     (declare (ignore registers))
     (let ((index 0)
           (num))
       (format nil "~A"
               (loop
                  do (multiple-value-setq (num index)
                       (parse-integer match :start index :junk-allowed t))
                  while num
                  summing num))))
   :simple-calls t))

#|(defun numerize (string)
  (rr-all-f string #?r" +|([^\d])-([^d])" "\\1 \\2")
  (rr-all-f string #?r"a half" "haAlf")

  ;; Replacing direct nums
  (dolist (num *direct-nums*)
    (rr-all-f string #?r"${(first num)}" (second num)))

  (dolist (tp *ten-prefixes*)
    (rr-all-f string
              #?r"(?:${(first tp)})( *\d(?=[^\d]|$))*"
              (lambda (match &rest registers)
                (format nil "~A"
                        (+ (second tp)
                           (parse-integer (or (first registers) "0")))))
              :simple-calls t))

  

  
  
  )|#









