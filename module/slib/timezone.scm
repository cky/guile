;;;; "timezone.scm" Compute timezones and DST from TZ environment variable.
;;; Copyright (C) 1994, 1996, 1997 Aubrey Jaffer.
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

;; The C-library support for time in general and time-zones in particular
;; stands as a fine example of how *not* to create interfaces.
;;
;; Functions are not consistently named.  Support for GMT is offered in one
;; direction only; The localtime function returns some timezone data in the
;; structure which it returns, and some data in shared global variables.
;; The structure which localtime returns is overwritten with each
;; invocation.  There is no way to find local time in zones other than GMT
;; and the local timezone.
;;
;; The tzfile(5) format encodes only a single timezone per file.  There is
;; no dispatch on zone names, so multiple copies of a timezone file exist
;; under different names.  The TZ `:' specification is unix filesystem
;; specific.  The tzfile(5) format makes no provision for byte-order
;; differences; It mixes 32-bit integer data with characters; specifying
;; ASCII bytes, it is incompatible with different character sizes.  The
;; binary format makes it impossible to easily inspect a file for
;; corruption.
;;
;; I have corrected most of the failings of the C-library time interface in
;; SLIB while maintaining compatablility.  I wrote support for Linux
;; timezone files because on a system where TZ is not set, there is no
;; other way to reveal this information.  HP-UX appears to have a more
;; sensible arrangement; I invite you to add support for it and other
;; platforms.
;;
;; Writing this was a long, tedious, and unenlightening process.  I hope it
;; is useful.
;;
;; Sat Nov 15 00:15:33 1997  Aubrey Jaffer  <jaffer@martigny.ai.mit.edu>

(provide 'time-zone)
(require 'scanf)

(define daylight? #f)
(define *timezone* 0)
(define tzname '#("UTC" "???"))

(define tz:default #f)

;;; This definition is here so that READ-TZFILE can verify the
;;; existence of these files before loading tzfile.scm to actually
;;; read them.
(define tzfile:vicinity (make-vicinity
			 (if (file-exists? "/usr/share/zoneinfo/.")
			     "/usr/share/zoneinfo/"
			     "/usr/lib/zoneinfo/")))

(define (read-tzfile path)
  (let ((realpath
	 (cond ((not path) (in-vicinity tzfile:vicinity "localtime"))
	       ((or (char-alphabetic? (string-ref path 0))
		    (char-numeric? (string-ref path 0)))
		(in-vicinity tzfile:vicinity path))
	       (else path))))
    (and (file-exists? realpath)
	 (let ((zone #f))
	   (require 'tzfile)
	   (set! zone (tzfile:read realpath))
	   (if zone (list->vector (cons 'tz:file zone))
	       (slib:error 'read-tzfile realpath))))))

;;; Parse Posix TZ string.

(define (string->transition-day-time str)
  (let ((month 0) (week 0) (day #f) (junk #f))
    (or (case (sscanf str "J%u%s" day junk)
	  ((1) (and (<= 1 day 365)
		    (list #f #f day)))
	  (else #f))
	(case (sscanf str "%u%s" day junk)
	  ((1) (and (<= 0 day 365)
		    (list #f #t day)))
	  (else #f))
	(case (sscanf str "M%u.%u.%u%s" month week day junk)
	  ((3) (and (<= 1 month 12)
		    (<= 1 week 5)
		    (<= 0 day 6)
		    (list month week day)))
	  (else #f)))))

(define (string->transition-time str)
  (let ((date #f) (time "2") (junk #f))
    (and (or (eqv? 2 (sscanf str "%[JM.0-9]/%[:0-9]%s" date time junk))
	     (eqv? 1 (sscanf str "%[JM.0-9]" date junk)))
	 (let ((day (string->transition-day-time date))
	       (tim (string->time-offset time)))
	   (and day tim (append day (list tim)))))))

(define (string->time-offset str)
  (and str (string? str) (positive? (string-length str))
       (let ((hh #f) (mm 0) (ss 0) (junk #f))
	 (and (<= 1 (sscanf (if (memv (string-ref str 0) '(#\+ #\-))
				(substring str 1 (string-length str))
				str)
			    "%u:%u:%u%s" hh mm ss junk)
		  3)
	      hh (<= 0 hh 23) (<= 0 mm 59) (<= 0 ss 59)
	      (* (if (char=? #\- (string-ref str 0)) -1 1)
		 (+ ss (* 60 (+ mm (* hh 60)))))))))

(define (string->time-zone tz)
  (let ((tzname #f) (offset #f) (dtzname #f) (doffset #f)
		    (start-str #f) (end-str #f) (junk #f))
    (define found
      (sscanf
       tz "%[^0-9,+-]%[-:+0-9]%[^0-9,+-]%[-:+0-9],%[JM.0-9/:],%[JM.0-9/:]%s"
       tzname offset dtzname doffset start-str end-str junk))
    (set! offset (string->time-offset offset))
    (set! doffset (string->time-offset doffset))
    (cond
     ((and offset (eqv? 3 found))
      (set! doffset (+ -3600 offset))
      (set! found
	    (+ 1
	       (sscanf
		tz "%[^0-9,+-]%[-:+0-9]%[^0-9,+-],%[JM.0-9/:],%[JM.0-9/:]%s"
		tzname offset dtzname start-str end-str junk)))
      (set! offset (string->time-offset offset))))
    (case found
      ((2) (vector 'tz:fixed tz tzname offset))
      ((4) (vector 'tz:rule tz tzname dtzname offset doffset
		   (list 4 1 0 7200) (list 10 5 0 7200)))
      ((6) (let ((start (string->transition-time start-str))
		 (end   (string->transition-time   end-str)))
	     (and
	      start end
	      (vector 'tz:rule tz tzname dtzname offset doffset start end))))
      (else #f))))

(define (time-zone tz)
  (cond ((not tz) (read-tzfile #f))
	((vector? tz) tz)
	((eqv? #\: (string-ref tz 0))
	 (read-tzfile (substring tz 1 (string-length tz))))
	(else (string->time-zone tz))))

;;; Use the timezone

(define (tzrule->caltime year previous-gmt-offset
			 tr-month tr-week tr-day tr-time)
  (define leap? (leap-year? year))
  (define gmmt
    (time:invert time:gmtime
		 (vector 0 0 0 1 (if tr-month (+ -1 tr-month) 0) year #f #f 0)))
  (offset-time
   gmmt
   (+ tr-time previous-gmt-offset
      (* 3600 24
	 (if tr-month
	     (let* ((fdow (vector-ref (time:gmtime gmmt) 6)))
	       (case tr-week
		 ((1 2 3 4) (+ (modulo (- tr-day fdow) 7)
			       (* 7 (+ -1 tr-week))))
		 ((5)
		  (do ((mmax (vector-ref
			      (vector-ref time:days/month (if leap? 1 0))
			      (+ -1 tr-month)))
		       (d (modulo (- tr-day fdow) 7) (+ 7 d)))
		      ((>= d mmax) (+ -7 d))))
		 (else (slib:error 'tzrule->caltime
				   "week out of range" tr-week))))
	     (+ tr-day
		(if (and (not tr-week) (>= tr-day 60) (leap-year? year))
		    1 0)))))))

(define (tz:params caltime tz)
  (case (vector-ref tz 0)
    ((tz:fixed) (list 0 (vector-ref tz 3) (vector-ref tz 2)))
    ((tz:rule)
     (let* ((year (vector-ref (time:gmtime caltime) 5))
	    (ttime0 (apply tzrule->caltime
			   year (vector-ref tz 4) (vector-ref tz 6)))
	    (ttime1 (apply tzrule->caltime
			   year (vector-ref tz 5) (vector-ref tz 7)))
	    (dst (if (and (not (negative? (difftime caltime ttime0)))
			  (negative? (difftime caltime ttime1)))
		     1 0)))
       (list dst (vector-ref tz (+ 4 dst)) (vector-ref tz (+ 2 dst)))
       ;;(for-each display (list (gtime ttime0) (gtime caltime) (gtime ttime1)))
       ))
    ((tz:file) (let ((zone-spec (tzfile:get-zone-spec caltime tz)))
		 (list (if (vector-ref zone-spec 2) 1 0)
		       (- (vector-ref zone-spec 1))
		       (vector-ref zone-spec 0))))
    (else (slib:error 'tz:params "unknown timezone type" tz))))

(define (tz:std-offset zone)
  (case (vector-ref zone 0)
    ((tz:fixed) (vector-ref zone 3))
    ((tz:rule) (vector-ref zone 4))
    ((tz:file)
     (let ((mode-table (vector-ref zone 2)))
       (do ((type-idx 0 (+ 1 type-idx)))
	   ((or (>= type-idx (vector-length mode-table))
		(not (vector-ref (vector-ref mode-table type-idx) 2)))
	    (if (>= type-idx (vector-length mode-table))
		(vector-ref (vector-ref mode-table 0) 1)
		(- (vector-ref (vector-ref mode-table type-idx) 1)))))))
    (else (slib:error 'tz:std-offset "unknown timezone type" tz))))

;;; Interpret the TZ envariable.
(define (tzset . opt-tz)
  (define tz (if (null? opt-tz)
		 (getenv "TZ")
		 (car opt-tz)))
  (if (or (not tz:default)
	  (and (string? tz) (not (string-ci=? tz (vector-ref tz:default 1)))))
      (set! tz:default (or (time-zone tz) '#(tz:fixed "UTC" "GMT" 0))))
  (case (vector-ref tz:default 0)
    ((tz:fixed)
     (set! tzname (vector (vector-ref tz:default 2) "???"))
     (set! daylight? #f)
     (set! *timezone* (vector-ref tz:default 3)))
    ((tz:rule)
     (set! tzname (vector (vector-ref tz:default 2)
			  (vector-ref tz:default 3)))
     (set! daylight? #t)
     (set! *timezone* (vector-ref tz:default 4)))
    ((tz:file)
     (let ((mode-table (vector-ref tz:default 2))
	   (transition-types (vector-ref tz:default 5)))
       (set! daylight? #f)
       (set! *timezone* (vector-ref (vector-ref mode-table 0) 1))
       (set! tzname (make-vector 2 #f))
       (do ((type-idx 0 (+ 1 type-idx)))
	   ((>= type-idx (vector-length mode-table)))
	 (let ((rec (vector-ref mode-table type-idx)))
	   (if (vector-ref rec 2)
	       (set! daylight? #t)
	       (set! *timezone* (- (vector-ref rec 1))))))

       (do ((transition-idx (+ -1 (vector-length transition-types))
			    (+ -1 transition-idx)))
	   ((or (negative? transition-idx)
		(and (vector-ref tzname 0) (vector-ref tzname 1))))
	 (let ((rec (vector-ref mode-table
				(vector-ref transition-types transition-idx))))
	   (if (vector-ref rec 2)
	       (if (not (vector-ref tzname 1))
		   (vector-set! tzname 1 (vector-ref rec 0)))
	       (if (not (vector-ref tzname 0))
		   (vector-set! tzname 0 (vector-ref rec 0))))))))
    (else (slib:error 'tzset "unknown timezone type" tz)))
  tz:default)
