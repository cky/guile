;;;; "psxtime.scm" Posix time conversion routines
;;; Copyright (C) 1994, 1997 Aubrey Jaffer.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;; No, it doesn't do leap seconds.

(define time:days/month
  '#(#(31 28 31 30 31 30 31 31 30 31 30 31) ; Normal years.
     #(31 29 31 30 31 30 31 31 30 31 30 31)))
(define (leap-year? year)
  (and (zero? (remainder year 4))
       (or (not (zero? (remainder year 100)))
	   (zero? (remainder year 400))))) ; Leap years.

;;; Returns the `struct tm' representation of T,
;;; offset TM_GMTOFF seconds east of UCT.
(define (time:split t tm_isdst tm_gmtoff tm_zone)
  (set! t (difftime t tm_gmtoff))
  (let* ((secs (modulo t 86400))	; SECS/DAY
	 (days (+ (quotient t 86400)	; SECS/DAY
		  (if (and (negative? t) (positive? secs)) -1 0))))
    (let ((tm_hour (quotient secs 3600))
	  (secs (remainder secs 3600))
	  (tm_wday (modulo (+ 4 days) 7))) ; January 1, 1970 was a Thursday.
      (let loop ((tm_year 1970)
		 (tm_yday days))
	(let ((diy (if (leap-year? tm_year) 366 365)))
	  (cond
	   ((negative? tm_yday) (loop (+ -1 tm_year) (+ tm_yday diy)))
	   ((>= tm_yday diy) (loop (+ 1 tm_year) (- tm_yday diy)))
	   (else
	    (let* ((mv (vector-ref time:days/month (- diy 365))))
	      (do ((tm_mon 0 (+ 1 tm_mon))
		   (tm_mday tm_yday (- tm_mday (vector-ref mv tm_mon))))
		  ((< tm_mday (vector-ref mv tm_mon))
		   (vector
		    (remainder secs 60) ; Seconds.	[0-61] (2 leap seconds)
		    (quotient secs 60)	; Minutes.	[0-59]
		    tm_hour		; Hours.	[0-23]
		    (+ tm_mday 1)	; Day.		[1-31]
		    tm_mon		; Month.	[0-11]
		    (- tm_year 1900)	; Year	- 1900.
		    tm_wday		; Day of week.	[0-6]
		    tm_yday		; Days in year. [0-365]
		    tm_isdst		; DST.		[-1/0/1]
		    tm_gmtoff		; Seconds west of UTC.
		    tm_zone		; Timezone abbreviation.
		    )))))))))))

(define (time:gmtime t)
  (time:split t 0 0 "GMT"))

(define (time:localtime caltime . tz)
  (require 'time-zone)
  (set! tz (if (null? tz) (tzset) (car tz)))
  (apply time:split caltime (tz:params caltime tz)))

(define time:year-70
  (let* ((t (current-time)))
    (offset-time (offset-time t (- (difftime t 0))) (* -70 32140800))))

(define (time:invert decoder target)
  (let* ((times '#(1 60 3600 86400 2678400 32140800))
	 (trough			; rough time for target
	  (do ((i 5 (+ i -1))
	       (trough time:year-70
		       (offset-time trough (* (vector-ref target i)
					      (vector-ref times i)))))
	      ((negative? i) trough))))
;;;    (print 'trough trough 'target target)
    (let loop ((guess trough)
	       (j 0)
	       (guess-tm (decoder trough)))
;;;      (print 'guess guess 'guess-tm guess-tm)
      (do ((i 5 (+ i -1))
	   (rough time:year-70
		  (offset-time rough (* (vector-ref guess-tm i)
					(vector-ref times i))))
	   (sign (let ((d (- (vector-ref target 5)
			     (vector-ref guess-tm 5))))
		   (and (not (zero? d)) d))
		 (or sign
		     (let ((d (- (vector-ref target i)
				 (vector-ref guess-tm i))))
		       (and (not (zero? d)) d)))))
	  ((negative? i)
	   (let* ((distance (abs (- trough rough))))
	     (cond ((and (zero? distance) sign)
;;;		    (print "trying to jump")
		    (set! distance (if (negative? sign) -86400 86400)))
		   ((and sign (negative? sign)) (set! distance (- distance))))
	     (set! guess (offset-time guess distance))
;;;	     (print 'distance distance 'sign sign)
	     (cond ((zero? distance) guess)
		   ((> j 5) #f)		;to prevent inf loops.
		   (else
		    (loop guess
			  (+ 1 j)
			  (decoder guess))))))))))

(define (time:mktime univtime . tz)
  (require 'time-zone)
  (set! tz (if (null? tz) (tzset) (car tz)))
  (+ (gmktime univtime) (tz:std-offset tz)))

(define (time:gmktime univtime)
  (time:invert time:gmtime univtime))

(define (time:asctime decoded)
  (let ((days   '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
	(months '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
			 "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	(number->2digits
	 (lambda (n ch)
	   (set! n (number->string n))
	   (if (= 1 (string-length n))
	       (string-append ch n)
	       n))))
    (string-append
     (vector-ref days (vector-ref decoded 6)) " "
     (vector-ref months (vector-ref decoded 4)) " "
     (number->2digits (vector-ref decoded 3) " ") " "
     (number->2digits (vector-ref decoded 2) "0") ":"
     (number->2digits (vector-ref decoded 1) "0") ":"
     (number->2digits (vector-ref decoded 0) "0") " "
     (number->string (+ 1900 (vector-ref decoded 5)))
     (string #\newline))))

(define (time:ctime . args)
  (time:asctime (apply time:localtime args)))

(define (time:gtime time)
  (time:asctime (time:gmtime time)))

;;;	GMT				Local -- take optional 2nd TZ arg
(define gmtime time:gmtime)	(define localtime time:localtime)
(define gmktime time:gmktime)	(define mktime time:mktime)
(define gtime time:gtime)	(define ctime time:ctime)

(define asctime time:asctime)
