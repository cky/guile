; "tzfile.scm", Read sysV style (binary) timezone file.
; Copyright (c) 1997 Aubrey Jaffer
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

(require 'byte)

(define (tzfile:read-long port)
  (let ((hibyte (read-byte port)))
    (do ((idx 3 (+ -1 idx))
	 (val (if (> hibyte 127) (+ #x-100 hibyte) hibyte)
	      (+ (ash val 8) (read-byte port))))
	((zero? idx) val))))
(define (tzfile:read-longs len port)
  (define ra (make-vector len 0))
  (do ((idx 0 (+ 1 idx)))
      ((>= idx len) ra)
    (vector-set! ra idx (tzfile:read-long port))))

(define (tzfile:read-bool port)
  (let ((c (read-char port)))
    (if (eof-object? c) c (if (zero? (char->integer c)) #f #t))))

(define (tzfile:read path)
  (define null (integer->char 0))
  (call-with-input-file path
    (lambda (port)
      (do ((idx 0 (+ 1 idx)))		;reserved.
	  ((>= idx 20))
	(read-char port))
      (let* ((ttisgmtcnt (tzfile:read-long port))
	     (ttisstdcnt (tzfile:read-long port))
	     (leapcnt (tzfile:read-long port))
	     (timecnt (tzfile:read-long port))
	     (typecnt (tzfile:read-long port))
	     (charcnt (tzfile:read-long port))
	     (transition-times (tzfile:read-longs timecnt port))
	     (transition-types
	      (do ((ra (make-vector timecnt 0))
		   (idx 0 (+ 1 idx)))
		  ((>= idx timecnt) ra)
		(vector-set! ra idx (read-byte port))))
	     ;;(printf "  typecnt = %d\n" typecnt)
	     (mode-table (do ((tt (make-vector typecnt #f))
			      (idx 0 (+ 1 idx)))
			     ((>= idx typecnt) tt)
			   (let* ((gmt-offset (tzfile:read-long port))
				  (isdst (tzfile:read-bool port))
				  (abbrev-index (read-byte port)))
			     (vector-set! tt idx
					  (vector abbrev-index gmt-offset
						  isdst #f #f)))))
	     ;;(printf "  %d bytes of abbreviations:\n" charcnt)
	     (abbrevs (do ((ra (make-bytes charcnt 0))
			   (idx 0 (+ 1 idx)))
			  ((>= idx charcnt) ra)
			(string-set! ra idx (read-char port))))
	     (leap-seconds (tzfile:read-longs (* 2 leapcnt) port)))
	(cond ((not (or (eqv? 0 ttisstdcnt) (eqv? typecnt ttisstdcnt)))
	       (slib:warn 'tzfile:read "format error" ttisstdcnt typecnt)))
	(cond ((not (or (eqv? 0 ttisgmtcnt) (eqv? typecnt ttisgmtcnt)))
	       (slib:warn 'tzfile:read "format error" ttisgmtcnt typecnt)))
	;;(printf " reading %d transition attributes\n" ttisstdcnt)
	(do ((idx 0 (+ 1 idx)))
	    ((>= idx ttisstdcnt))
	  (vector-set! (vector-ref mode-table idx) 3 (tzfile:read-bool port)))
	;;(printf " reading %d transition attributes\n" ttisgmtcnt)
	(do ((idx 0 (+ 1 idx)))
	    ((>= idx ttisgmtcnt))
	  (vector-set! (vector-ref mode-table idx) 4 (tzfile:read-bool port)))
	(cond ((not (eof-object? (peek-char port)))
	       (slib:warn 'tzfile:read "bytes left at end")))
	(do ((idx 0 (+ 1 idx)))
	    ((>= idx ttisstdcnt))
	  (let ((rec (vector-ref mode-table idx)))
	    (vector-set!
	     rec 0 (let loop ((pos (vector-ref rec 0)))
		     (cond ((>= pos (string-length abbrevs))
			    (slib:warn 'tzfile:read "format error" abbrevs) #f)
			   ((char=? null (string-ref abbrevs pos))
			    (substring abbrevs (vector-ref rec 0) pos))
			   (else (loop (+ 1 pos))))))))
	(list path mode-table leap-seconds transition-times transition-types)
	))))

(define (tzfile:transition-index time zone)
  (and zone
       (apply
	(lambda (path mode-table leap-seconds transition-times transition-types)
	  (let ((ntrns (vector-length transition-times)))
	    (if (zero? ntrns) -1
		(let loop ((lidx (ash (+ 1 ntrns) -1))
			   (jmp (ash (+ 1 ntrns) -2)))
		  (let* ((idx (max 0 (min lidx (+ -1 ntrns))))
			 (idx-time (vector-ref transition-times idx)))
		    (cond ((<= jmp 0)
			   (+ idx (if (>= time idx-time) 0 -1)))
			  ((= time idx-time) idx)
			  ((and (zero? idx) (< time idx-time)) -1)
			  ((and (not (= idx lidx)) (not (< time idx-time))) idx)
			  (else
			   (loop ((if (< time idx-time) - +) idx jmp)
				 (if (= 1 jmp) 0 (ash (+ 1 jmp) -1))))))))))
	(cdr (vector->list zone)))))

(define (tzfile:get-std-spec mode-table)
  (do ((type-idx 0 (+ 1 type-idx)))
      ((or (>= type-idx (vector-length mode-table))
	   (not (vector-ref (vector-ref mode-table type-idx) 2)))
       (if (>= type-idx (vector-length mode-table))
	   (vector-ref mode-table 0)
	   (vector-ref mode-table type-idx)))))

(define (tzfile:get-zone-spec time zone)
  (apply
   (lambda (path mode-table leap-seconds transition-times transition-types)
     (let* ((trans-idx (tzfile:transition-index time zone)))
       (if (zero? (vector-length transition-types))
	   (vector-ref mode-table 0)
	   (if (negative? trans-idx)
	       (tzfile:get-std-spec mode-table)
	       (vector-ref mode-table
			   (vector-ref transition-types trans-idx))))))
   (cdr (vector->list zone))))
