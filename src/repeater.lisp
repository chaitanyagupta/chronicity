(cl:in-package #:chronicity)

;;; Enable cl-interpol reader

#.(cl-interpol:enable-interpol-syntax)

;;; TODO: Class definitions for each kind of repeater, also return
;;; these class instances instead of a keyword

(defclass repeater (tag)
  ())

(defmethod scan-tokens ((tag (eql 'repeater)) tokens)
  (dolist (token tokens tokens)
    (awhen (scan-for-month-names token) (tag it token))
    (awhen (scan-for-day-names token) (tag it token))
    (awhen (scan-for-day-portions token) (tag it token))
    (awhen (scan-for-times token) (tag it token))
    (awhen (scan-for-units token) (tag it token))))

(defun scan-for-month-names (token &aux (word (token-word token)))
  (let ((scan-map '((#?r"^jan\.?(uary)?$" :january)
                    (#?r"^feb\.?(ruary)?$" :february)
                    (#?r"^mar\.?(ch)?$" :march)
                    (#?r"^apr\.?(il)?$" :april)
                    (#?r"^may$" :may)
                    (#?r"^jun\.?e?$" :june)
                    (#?r"^jul\.?y?$" :july)
                    (#?r"^aug\.?(ust)?$" :august)
                    (#?r"^sep\.?(t\.?|tember)?$" :september)
                    (#?r"^oct\.?(ober)?$" :october)
                    (#?r"^nov\.?(ember)?$" :november)
                    (#?r"^dec\.?(ember)?$" :december))))
    (loop
       for (regex keyword) in scan-map
       when (cl-ppcre:scan regex word)
       return (create-tag 'repeater-month-name keyword))))

;;; TODO: Check for spelling mistakes
(defun scan-for-day-names (token &aux (word (token-word token)))
  (loop
     for day in *days-of-week*
     for regex = (string day)
     when (cl-ppcre:scan (cl-ppcre:create-scanner regex :case-insensitive-mode t)
                         word)
     return (create-tag 'repeater-day-name day)))

(defun scan-for-day-portions (token &aux (word (token-word token)))
  (let ((scan-map '(("^ams?$" :am)
                    ("^pms?$" :pm)
                    ("^mornings?$" :morning)
                    ("^afternoons?$" :afternoon)
                    ("^evenings?$" :evening)
                    ("^(night|nite)s?$" :night))))
    (loop
       for (regex keyword) in scan-map
       when (cl-ppcre:scan regex word)
       return (create-tag 'repeater-day-portion keyword))))

;;; TODO: repeater.rb has options here, what does it do?
(defun scan-for-times (token &aux (word (token-word token)))
  (when (cl-ppcre:scan #?r"^\d{1,2}(:?\d{2})?([\.:]?\d{2})?$" word)
    (create-tag 'repeater-time word)))

(defun scan-for-units (token &aux (word (token-word token)))
  (let ((scan-map '((#?/^years?$/  :year)
                    (#?/^seasons?$/  :season)
                    (#?/^months?$/  :month)
                    (#?/^fortnights?$/  :fortnight)
                    (#?/^weeks?$/  :week)
                    (#?/^weekends?$/  :weekend)
                    (#?/^days?$/  :day)
                    (#?/^hours?$/  :hour)
                    (#?/^minutes?$/  :minute)
                    (#?/^seconds?$/  :second))))
    (loop
       for (regex keyword) in scan-map
       when (cl-ppcre:scan regex word)
       return (create-tag (intern (format nil "REPEATER-~A" keyword) :chronicity)
                          keyword))))

;;; We wrap CHECK-POINTER around the R-NEXT and R-THIS so that pointer
;;; is checked for sanity before any invocation of these methods

(defun check-pointer (pointer)
  (let ((list (list :future :none :past)))
    (unless (member pointer list)
      (error "POINTER must be one of ~{~S~^, ~}" list))))

(defmethod r-next :around ((repeater repeater) pointer)
  (check-pointer pointer)
  (call-next-method))

(defmethod r-this :around ((repeater repeater) pointer)
  (check-pointer pointer)
  (call-next-method))

;;; Disable cl-interpol reader

#.(cl-interpol:disable-interpol-syntax)


