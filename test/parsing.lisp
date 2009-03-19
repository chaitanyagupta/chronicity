(cl:in-package #:chronicity-test)

(define-test parse-guess-dates
  (let ((*now* (make-datetime 2006 8 16 14 0 0))
        (*guess* :middle))

    ;; rm_sd

    (assert-datetime= (make-datetime 2007 5 27 12) (parse "may 27"))
    
    (assert-datetime= (make-datetime 2006 5 28 12)
                      (parse "may 28" :context :past))
    
    (assert-datetime= (make-datetime 2006 5 28 17)
                      (parse "may 28 5pm" :context :past))
    
    (assert-datetime= (make-datetime 2006 5 28 17)
                      (parse "may 28 at 5pm" :context :past))
    
    (assert-datetime= (make-datetime 2006 5 28 17 32 19)
                      (parse "may 28 at 5:32.19pm" :context :past))
    
    ;; rm_sd_on
    
    (assert-datetime= (make-datetime 2007 5 28 17) (parse "5pm on may 28"))

    (assert-datetime= (make-datetime 2007 5 28 17) (parse "5pm may 28"))

    (assert-datetime= (make-datetime 2007 5 28 05)
                      (parse "5 on may 28" :ambiguous-time-range nil))

    ;; rm_od
    
    (assert-datetime= (make-datetime 2007 5 27 12) (parse "may 27th"))
    
    (assert-datetime= (make-datetime 2006 5 27 12)
                      (parse "may 27th" :context :past))
    
    (assert-datetime= (make-datetime 2006 5 27 17)
                      (parse "may 27th 5:00 pm" :context :past))
    
    (assert-datetime= (make-datetime 2006 5 27 17)
                      (parse "may 27th at 5pm" :context :past))
    
    (assert-datetime= (make-datetime 2007 5 27 5)
                      (parse "may 27th at 5" :ambiguous-time-range nil))
    
    ;; rm_od_on
    
    (assert-datetime= (make-datetime 2006 5 27 17)
                      (parse "5:00 pm may 27th" :context :past))
    
    (assert-datetime= (make-datetime 2006 5 27 17)
                      (parse "5pm on may 27th" :context :past))
    
    (assert-datetime= (make-datetime 2007 5 27 5)
                      (parse "5 on may 27th" :ambiguous-time-range nil))

    ;; rm_sy
    
    (assert-datetime= (make-datetime 1979 6 16 0) (parse "June 1979"))
    
    (assert-datetime= (make-datetime 1979 12 16 12) (parse "dec 79"))
    
    ;; rm_sd_sy
    
    (assert-datetime= (make-datetime 2010 1 3 12) (parse "jan 3 2010"))
    
    (assert-datetime= (make-datetime 2010 1 4 0) (parse "jan 3 2010 midnight"))
    
    (assert-datetime= (make-datetime 2010 1 4 0) (parse "jan 3 2010 at midnight"))
    
    (assert-datetime= (make-datetime 2010 1 3 4)
                      (parse "jan 3 2010 at 4" :ambiguous-time-range nil))
    
    ;;time = parse_now("January 12, '00")
    ;;assert_equal Time.local(2000, 1, 12, 12), time
    
    (assert-datetime= (make-datetime 1979 5 27 12) (parse "may 27, 1979"))
    
    (assert-datetime= (make-datetime 1979 5 27 12) (parse "may 27 79"))
    
    (assert-datetime= (make-datetime 1979 5 27 16 30) (parse "may 27 79 4:30"))
    
    (assert-datetime= (make-datetime 1979 5 27 4 30)
                      (parse "may 27 79 at 4:30" :ambiguous-time-range nil))
    
    ;; sd_rm_sy

    (assert-datetime= (make-datetime 2010 1 3 12) (parse "3 jan 2010"))
    
    (assert-datetime= (make-datetime 2010 1 3 16) (parse "3 jan 2010 4pm"))
    
    (assert-datetime= (make-datetime 2006 10 27 19 30) (parse "27 Oct 2006 7:30pm"))
    
    ;; sm_sd_sy

    (assert-datetime= (make-datetime 1979 5 27 12) (parse "27/5/1979"))
    
    (assert-datetime= (make-datetime 1979 5 27 4) (parse "27/5/1979 4am"))
    
    ;; sd_sm_sy
    
    (assert-datetime= (make-datetime 1979 5 27 12) (parse "27/5/1979"))
    
    (assert-datetime= (make-datetime 1979 5 27 7) (parse "27/5/1979 @ 0700"))
    
    ;; sm_sd
    
    (assert-datetime= (make-datetime 2007 6 5 12) (parse "05/06"))
    
    (assert-datetime= (make-datetime 2007 6 12 12) (parse "12/06"))
    
    (assert-datetime= (make-datetime 2007 06 13 12) (parse "13/06"))
    
    ;; sy_sm_sd
    
    (assert-datetime= (make-datetime 2000 1 1 12) (parse "2000-1-1"))
    
    (assert-datetime= (make-datetime 2006 8 20 12) (parse "2006-08-20"))
    
    (assert-datetime= (make-datetime 2006 8 20 19) (parse "2006-08-20 7pm"))
    
    (assert-datetime= (make-datetime 2006 8 20 3) (parse "2006-08-20 03:00"))
    
    (assert-datetime= (make-datetime 2006 8 20 3 30 30) (parse "2006-08-20 03:30:30"))
    
    (assert-datetime= (make-datetime 2006 8 20 15 30 30) (parse "2006-08-20 15:30:30"))
    
    (assert-datetime= (make-datetime 2006 8 20 15 30 30) (parse "2006-08-20 15:30.30"))
    
    ;; rdn_rm_rd_rt_rtz_ry
    
;;;     time = parse_now("Mon Apr 02 17:00:00 PDT 2007")
;;;     assert_equal 1175558400, time.to_i
    
;;;     now = Time.now
;;;     time = parse_now(now.to_s)
;;;     assert_equal now.to_s, time.to_s
    
    ;; rm_sd_rt
    
    ;;time = parse_now("jan 5 13:00")
    ;;assert_equal Time.local(2007, 1, 5, 13), time
    
    (assert-datetime= (make-datetime 2040 5 16 12) (parse "may 40"))
    
    (assert-datetime= (make-datetime 2040 5 27 12) (parse "may 27 40"))))
  
(define-test parse-guess-r
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    (assert-datetime= (make-datetime 2006 8 18 12) (parse "friday"))
    
    (assert-datetime= (make-datetime 2006 8 22 12) (parse "tue"))
    
    (assert-datetime= (make-datetime 2006 8 16 17) (parse "5"))
    
    (assert-datetime= (make-datetime 2006 8 16 5)
                      (parse "5" :now (make-datetime 2006 8 16 3 0 0) :ambiguous-time-range nil))
    
    (assert-datetime= (make-datetime 2006 8 17 13) (parse "13:00"))
    
    (assert-datetime= (make-datetime 2006 8 17 13 45) (parse "13:45"))
    
    (assert-datetime= (make-datetime 2006 11 16) (parse "november"))
    ))
  
(define-test parse-guess-rr
  (let ((*now* (make-datetime 2006 8 16 14 0 0))
        (*guess* :middle))
    (assert-datetime= (make-datetime 2006 8 18 13) (parse "friday 13:00"))
    
    (assert-datetime= (make-datetime 2006 8 21 16) (parse "monday 4:00"))
    
    (assert-datetime= (make-datetime 2006 8 19 4)
                      (parse "sat 4:00" :ambiguous-time-range nil))
    
    (assert-datetime= (make-datetime 2006 8 20 4 20)
                      (parse "sunday 4:20" :ambiguous-time-range nil))
    
    (assert-datetime= (make-datetime 2006 8 16 16) (parse "4 pm"))
    
    (assert-datetime= (make-datetime 2006 8 16 4)
                      (parse "4 am" :ambiguous-time-range nil))
    
    (assert-datetime= (make-datetime 2006 8 16 12) (parse "12 pm"))
    
    (assert-datetime= (make-datetime 2006 8 16 12 1) (parse "12:01 pm"))
    
    (assert-datetime= (make-datetime 2006 8 16 0 1) (parse "12:01 am"))
    
    (assert-datetime= (make-datetime 2006 8 16) (parse "12 am"))
    
    (assert-datetime= (make-datetime 2006 8 16 4) (parse "4:00 in the morning"))
    
    (assert-datetime= (make-datetime 2006 11 4 12) (parse "november 4"))
    
    (assert-datetime= (make-datetime 2006 8 24 12) (parse "aug 24"))
    ))
  
(define-test parse-guess-rrr
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    (assert-datetime= (make-datetime 2006 8 18 13) (parse "friday 1 pm"))
    
    (assert-datetime= (make-datetime 2006 8 18 23) (parse "friday 11 at night"))
    
    (assert-datetime= (make-datetime 2006 8 18 23) (parse "friday 11 in the evening"))
    
    (assert-datetime= (make-datetime 2006 8 20 6) (parse "sunday 6am"))
    
    (assert-datetime= (make-datetime 2006 8 18 19) (parse "friday evening at 7"))
    ))
  
(define-test parse-guess-gr
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    ;; year
    
    (assert-datetime= (make-datetime 2006 10 24 12 30) (parse "this year"))
    
    (assert-datetime= (make-datetime 2006 4 24 12 30)
                      ("this year" :context :past))
    
    ;; month
    
    (assert-datetime= (make-datetime 2006 8 24 12) (parse "this month"))
    
    (assert-datetime= (make-datetime 2006 8 8 12)
                      ("this month" :context :past))
    
    (assert-datetime= (make-datetime 2006 12 16 12)
                      (parse "next month" :now (make-datetime 2006 11 15)))
    
    ;; month name
    
    (assert-datetime= (make-datetime 2005 11 16) (parse "last november"))
    
    ;; fortnight
    
    (assert-datetime= (make-datetime 2006 8 21 19 30) (parse "this fortnight"))
    
    (assert-datetime= (make-datetime 2006 8 14 19)
                      ("this fortnight" :context :past))
    
    ;; week
    
    (assert-datetime= (make-datetime 2006 8 18 7 30) (parse "this week"))
    
    (assert-datetime= (make-datetime 2006 8 14 19)
                      ("this week" :context :past))
    
    ;; weekend
    
    (assert-datetime= (make-datetime 2006 8 20) (parse "this weekend"))
    
    (assert-datetime= (make-datetime 2006 8 13)
                      ("this weekend" :context :past))
    
    (assert-datetime= (make-datetime 2006 8 13) (parse "last weekend"))
    
    ;; day
    
    (assert-datetime= (make-datetime 2006 8 16 19 30) (parse "this day"))
    
    (assert-datetime= (make-datetime 2006 8 16 7)
                      ("this day" :context :past))
    
    (assert-datetime= (make-datetime 2006 8 16 19 30) (parse "today"))
    
    (assert-datetime= (make-datetime 2006 8 15 12) (parse "yesterday"))
    
    (assert-datetime= (make-datetime 2006 8 17 12) (parse "tomorrow"))
    
    ;; day name
    
    (assert-datetime= (make-datetime 2006 8 22 12) (parse "this tuesday"))
    
    (assert-datetime= (make-datetime 2006 8 22 12) (parse "next tuesday"))
    
    (assert-datetime= (make-datetime 2006 8 15 12) (parse "last tuesday"))
    
    (assert-datetime= (make-datetime 2006 8 23 12) (parse "this wed"))
    
    (assert-datetime= (make-datetime 2006 8 23 12) (parse "next wed"))
    
    (assert-datetime= (make-datetime 2006 8 9 12) (parse "last wed"))
    
    ;; day portion
    
    (assert-datetime= (make-datetime 2006 8 16 9) (parse "this morning"))
    
    (assert-datetime= (make-datetime 2006 8 16 22) (parse "tonight"))
    
    ;; minute
    
    (assert-datetime= (make-datetime 2006 8 16 14 1 30) (parse "next minute"))
    
    ;; second
    
    (assert-datetime= (make-datetime 2006 8 16 14) (parse "this second"))
    
    (assert-datetime= (make-datetime 2006 8 16 14)
                      ("this second" :context :past))
    
    (assert-datetime= (make-datetime 2006 8 16 14 0 1) (parse "next second"))
    
    (assert-datetime= (make-datetime 2006 8 16 13 59 59) (parse "last second"))
    ))
  
(define-test parse-guess-grr
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    (assert-datetime= (make-datetime 2006 8 15 16) (parse "yesterday at 4:00"))
    
    (assert-datetime= (make-datetime 2006 8 16 9) (parse "today at 9:00"))
    
    (assert-datetime= (make-datetime 2006 8 16 21) (parse "today at 2100"))
    
    (assert-datetime= (make-datetime 2006 8 16 9) (parse "this day at 0900"))
    
    (assert-datetime= (make-datetime 2006 8 17 9) (parse "tomorrow at 0900"))
    
    (assert-datetime= (make-datetime 2006 8 15 4)
                      ("yesterday at 4:00" :ambiguous-time-range nil))
    
    (assert-datetime= (make-datetime 2006 8 11 16) (parse "last friday at 4:00"))
    
    (assert-datetime= (make-datetime 2006 8 23 16) (parse "next wed 4:00"))
    
    (assert-datetime= (make-datetime 2006 8 15 15) (parse "yesterday afternoon"))
    
    (assert-datetime= (make-datetime 2006 8 8 12) (parse "last week tuesday"))
    
    (assert-datetime= (make-datetime 2006 8 16 19) (parse "tonight at 7"))
    
    (assert-datetime= (make-datetime 2006 8 16 19) (parse "tonight 7"))
    
    (assert-datetime= (make-datetime 2006 8 16 19) (parse "7 tonight"))
    ))
    
(define-test parse-guess-grrr
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    (assert-datetime= (make-datetime 2006 8 16 18) (parse "today at 6:00pm"))
    
    (assert-datetime= (make-datetime 2006 8 16 6) (parse "today at 6:00am"))
    
    (assert-datetime= (make-datetime 2006 8 16 18) (parse "this day 1800"))
    
    (assert-datetime= (make-datetime 2006 8 15 16) (parse "yesterday at 4:00pm"))
    
    (assert-datetime= (make-datetime 2006 8 17 19) (parse "tomorrow evening at 7"))
    
    (assert-datetime= (make-datetime 2006 8 17 5 30) (parse "tomorrow morning at 5:30"))
    
    (assert-datetime= (make-datetime 2006 8 21 00 1) (parse "next monday at 12:01 am"))
    
    (assert-datetime= (make-datetime 2006 8 21 12 1) (parse "next monday at 12:01 pm"))
    ))
  
(define-test parse-guess-rgr
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    (assert-datetime= (make-datetime 2006 8 15 15) (parse "afternoon yesterday"))
    
    (assert-datetime= (make-datetime 2006 8 8 12) (parse "tuesday last week"))
    ))
  
(define-test parse-guess-s-r-p
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    ;; past
    
    (assert-datetime= (make-datetime 2003 8 16 14) (parse "3 years ago"))
    
    (assert-datetime= (make-datetime 2006 7 16 14) (parse "1 month ago"))
    
    (assert-datetime= (make-datetime 2006 8 2 14) (parse "1 fortnight ago"))
    
    (assert-datetime= (make-datetime 2006 7 19 14) (parse "2 fortnights ago"))
    
    (assert-datetime= (make-datetime 2006 7 26 14) (parse "3 weeks ago"))
    
    (assert-datetime= (make-datetime 2006 8 5) (parse "2 weekends ago"))
    
    (assert-datetime= (make-datetime 2006 8 13 14) (parse "3 days ago"))
    
    ;;time = parse_now("1 monday ago")
    ;;assert_equal Time.local(2006, 8, 14, 12), time
    
    (assert-datetime= (make-datetime 2006 8 12 9) (parse "5 mornings ago"))
    
    (assert-datetime= (make-datetime 2006 8 16 7) (parse "7 hours ago"))
    
    (assert-datetime= (make-datetime 2006 8 16 13 57) (parse "3 minutes ago"))
    
    (assert-datetime= (make-datetime 2006 8 16 13 59 40) (parse "20 seconds before now"))

    ;; future
    
    (assert-datetime= (make-datetime 2009 8 16 14 0 0) (parse "3 years from now"))
    
    (assert-datetime= (make-datetime 2007 2 16 14) (parse "6 months hence"))
    
    (assert-datetime= (make-datetime 2006 9 27 14) (parse "3 fortnights hence"))
    
    (assert-datetime= (make-datetime 2006 8 23 14 0 0) (parse "1 week from now"))
    
    (assert-datetime= (make-datetime 2006 8 19) (parse "1 weekend from now"))
    
    (assert-datetime= (make-datetime 2006 8 26) (parse "2 weekends from now"))
    
    (assert-datetime= (make-datetime 2006 8 17 14) (parse "1 day hence"))
    
    (assert-datetime= (make-datetime 2006 8 21 9) (parse "5 mornings hence"))
    
    (assert-datetime= (make-datetime 2006 8 16 15) (parse "1 hour from now"))
    
    (assert-datetime= (make-datetime 2006 8 16 14 20) (parse "20 minutes hence"))
    
    (assert-datetime= (make-datetime 2006 8 16 14 0 20) (parse "20 seconds from now"))
    
    (assert-datetime= (make-datetime 2007 1 7 23 30)
                      ("2 months ago" :now (make-datetime 2007 3 7 23 30)))
    ))
  
(define-test parse-guess-p-s-r
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    (assert-datetime= (make-datetime 2006 8 16 17) (parse "in 3 hours"))
    ))
  
(define-test parse-guess-s-r-p-a
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    ;; past
    
    (assert-datetime= (make-datetime 2003 8 17 12) (parse "3 years ago tomorrow"))
    
    (assert-datetime= (make-datetime 2003 8 18 12) (parse "3 years ago this friday"))
    
    (assert-datetime= (make-datetime 2006 5 19 17) (parse "3 months ago saturday at 5:00 pm"))
    
    (assert-datetime= (make-datetime 2006 8 18 14) (parse "2 days from this second"))
    
    (assert-datetime= (make-datetime 2006 8 17 17) (parse "7 hours before tomorrow at midnight"))
    
    ;; future
    ))
  
(define-test parse-guess-o-r-s-r
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    (assert-datetime= (make-datetime 2006 11 15 12) (parse "3rd wednesday in november"))
    
    (assert-equal nil (parse "10th wednesday in november"))
    
    ;; time = parse_now("3rd wednesday in 2007")
    ;; assert_equal Time.local(2007, 1, 20, 12), time
    ))
  
(define-test parse-guess-o-r-g-r
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    (assert-datetime= (make-datetime 2007 3 16 12 30) (parse "3rd month next year"))
    
    (assert-datetime= (make-datetime 2006 9 21 12) (parse "3rd thursday this september"))
    
    (assert-datetime= (make-datetime 2006 8 9 12) (parse "4th day last week"))
    ))
  
(define-test parse-guess-nonsense
  (let ((*now* (make-datetime 2006 8 16 14 0 0)))
    (assert-equal nil (parse "some stupid nonsense"))
    
    (assert-equal nil (parse "Ham Sandwich"))
    ))
  
;; (define-test parse-span
;;   (let ((*now* (make-datetime 2006 8 16 14 0 0)))
;;     span = parse_now("friday", :guess => false)
;;     assert_equal Time.local(2006, 8, 18), span.begin
;;     assert_equal Time.local(2006, 8, 19), span.end
    
;;     span = parse_now("november", :guess => false)
;;     assert_equal Time.local(2006, 11), span.begin
;;     assert_equal Time.local(2006, 12), span.end
    
;;     span = Chronic.parse("weekend" , :now => @time_2006_08_16_14_00_00, :guess => false)
;;     assert_equal Time.local(2006, 8, 19), span.begin
;;     assert_equal Time.local(2006, 8, 21), span.end
;;     ))
  
;;;   def test_parse_with_endian_precedence
;;;     date = '11/02/2007'

;;;     expect_for_middle_endian = Time.local(2007, 11, 2, 12)
;;;     expect_for_little_endian = Time.local(2007, 2, 11, 12)

;;;     ;; default precedence should be toward middle endianness
;;;     assert_equal expect_for_middle_endian, Chronic.parse(date)

;;;     assert_equal expect_for_middle_endian, Chronic.parse(date, :endian_precedence => [:middle, :little])

;;;     assert_equal expect_for_little_endian, Chronic.parse(date, :endian_precedence => [:little, :middle])
;;;   end

;; (define-test parse-words
;;   (let ((*now* (make-datetime 2006 8 16 14 0 0)))
;;     assert_equal parse_now("33 days from now"), parse_now("thirty-three days from now")
;;     assert_equal parse_now("2867532 seconds from now"), parse_now("two million eight hundred and sixty seven thousand five hundred and thirty two seconds from now")
;;     assert_equal parse_now("may 10th"), parse_now("may tenth")
;;     ))
  
;; (define-test parse-only-complete-pointers
;;   (let ((*now* (make-datetime 2006 8 16 14 0 0)))
;;     assert_equal parse_now("eat pasty buns today at 2pm"), @time_2006_08_16_14_00_00
;;     assert_equal parse_now("futuristically speaking today at 2pm"), @time_2006_08_16_14_00_00
;;     assert_equal parse_now("meeting today at 2pm"), @time_2006_08_16_14_00_00
;;     ))
  
;; (define-test am-pm
;;   (let ((*now* (make-datetime 2006 8 16 14 0 0)))
;;     assert_equal Time.local(2006, 8, 16), parse_now("8/16/2006 at 12am")
;;     assert_equal Time.local(2006, 8, 16, 12), parse_now("8/16/2006 at 12pm")
;;     ))
  
;; (define-test a-p
;;   (let ((*now* (make-datetime 2006 8 16 14 0 0)))
;;     assert_equal Time.local(2006, 8, 16, 0, 15), parse_now("8/16/2006 at 12:15a")
;;     assert_equal Time.local(2006, 8, 16, 18, 30), parse_now("8/16/2006 at 6:30p")
;;     ))
  
;;;   def test_argument_validation
;;;     assert_raise(Chronic::InvalidArgumentException) do
;;;       time = Chronic.parse("may 27", :foo => :bar)
;;;     end
    
;;;     assert_raise(Chronic::InvalidArgumentException) do
;;;       time = Chronic.parse("may 27", :context => :bar)
;;;     end
;;;   end
  
;;;   def test_seasons
;;;     t = parse_now("this spring", :guess => false)
;;;     assert_equal Time.local(2007, 3, 20), t.begin
;;;     assert_equal Time.local(2007, 6, 20), t.end
    
;;;     t = parse_now("this winter", :guess => false)
;;;     assert_equal Time.local(2006, 12, 22, 23), t.begin
;;;     assert_equal Time.local(2007, 3, 19), t.end
    
;;;     t = parse_now("last spring", :guess => false)
;;;     assert_equal Time.local(2006, 3, 20, 23), t.begin
;;;     assert_equal Time.local(2006, 6, 20), t.end
    
;;;     t = parse_now("last winter", :guess => false)
;;;     assert_equal Time.local(2005, 12, 22, 23), t.begin
;;;     assert_equal Time.local(2006, 3, 19, 23), t.end
    
;;;     t = parse_now("next spring", :guess => false)
;;;     assert_equal Time.local(2007, 3, 20), t.begin
;;;     assert_equal Time.local(2007, 6, 20), t.end
;;;   end
  
  ;; regression
  
  ;; def test_partial
  ;;   assert_equal '', parse_now("2 hours")
  ;; end
  
;;;   def test_days_in_november
;;;     t1 = Chronic.parse('1st thursday in november', :now => Time.local(2007))
;;;     assert_equal Time.local(2007, 11, 1, 12), t1
    
;;;     t1 = Chronic.parse('1st friday in november', :now => Time.local(2007))
;;;     assert_equal Time.local(2007, 11, 2, 12), t1
    
;;;     t1 = Chronic.parse('1st saturday in november', :now => Time.local(2007))
;;;     assert_equal Time.local(2007, 11, 3, 12), t1
    
;;;     t1 = Chronic.parse('1st sunday in november', :now => Time.local(2007))
;;;     assert_equal Time.local(2007, 11, 4, 11), t1
    
;;;     ;; Chronic.debug = true
;;;     ;; 
;;;     ;; t1 = Chronic.parse('1st monday in november', :now => Time.local(2007))
;;;     ;; assert_equal Time.local(2007, 11, 5, 11), t1
;;;   end




  

  


  