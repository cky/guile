;;;; "chap.scm" Chapter ordering		-*-scheme-*-
;;; Copyright 1992, 1993, 1994 Aubrey Jaffer.
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

;;; The CHAP: functions deal with strings which are ordered like
;;; chapters in a book.  For instance, a_9 < a_10 and 4c < 4aa.  Each
;;; section of the string consists of consecutive numeric or
;;; consecutive aphabetic characters.

(define (chap:string<? s1 s2)
  (let ((l1 (string-length s1))
	(l2 (string-length s2)))
    (define (match-so-far i ctypep)
      (cond ((>= i l1) (not (>= i l2)))
	    ((>= i l2) #f)
	    (else
	     (let ((c1 (string-ref s1 i))
		   (c2 (string-ref s2 i)))
	       (cond ((char=? c1 c2)
		      (if (ctypep c1)
			  (match-so-far (+ 1 i) ctypep)
			  (delimited i)))
		     ((ctypep c1)
		      (if (ctypep c2)
			  (length-race (+ 1 i) ctypep (char<? c1 c2))
			  #f))
		     ((ctypep c2) #t)
		     (else
		      (let ((ctype1 (ctype c1)))
			(cond
			 ((and ctype1 (eq? ctype1 (ctype c2)))
			  (length-race (+ 1 i) ctype1 (char<? c1 c2)))
			 (else (char<? c1 c2))))))))))
    (define (length-race i ctypep def)
      (cond ((>= i l1) (if (>= i l2) def #t))
	    ((>= i l2) #f)
	    (else
	     (let ((c1 (string-ref s1 i))
		   (c2 (string-ref s2 i)))
	       (cond ((ctypep c1)
		      (if (ctypep c2)
			  (length-race (+ 1 i) ctypep def)
			  #f))
		     ((ctypep c2) #t)
		     (else def))))))
    (define (ctype c1)
      (cond
       ((char-numeric? c1) char-numeric?)
       ((char-lower-case? c1) char-lower-case?)
       ((char-upper-case? c1) char-upper-case?)
       (else #f)))
    (define (delimited i)
      (cond ((>= i l1) (not (>= i l2)))
	    ((>= i l2) #f)
	    (else
	     (let* ((c1 (string-ref s1 i))
		    (c2 (string-ref s2 i))
		    (ctype1 (ctype c1)))
	       (cond ((char=? c1 c2)
		      (if ctype1 (match-so-far (+ i 1) ctype1)
			  (delimited (+ i 1))))
		     ((and ctype1 (eq? ctype1 (ctype c2)))
		      (length-race (+ 1 i) ctype1 (char<? c1 c2)))
		     (else (char<? c1 c2)))))))
    (delimited 0)))

(define chap:char-incr (- (char->integer #\2) (char->integer #\1)))

(define (chap:inc-string s p)
  (let ((c (string-ref s p)))
    (cond ((char=? c #\z)
	   (string-set! s p #\a)
	   (cond ((zero? p) (string-append "a" s))
		 ((char-lower-case? (string-ref s (+ -1 p)))
		  (chap:inc-string s (+ -1 p)))
		 (else
		  (string-append
		   (substring s 0 p)
		   "a"
		   (substring s p (string-length s))))))
	  ((char=? c #\Z)
	   (string-set! s p #\A)
	   (cond ((zero? p) (string-append "A" s))
		 ((char-upper-case? (string-ref s (+ -1 p)))
		  (chap:inc-string s (+ -1 p)))
		 (else
		  (string-append
		   (substring s 0 p)
		   "A"
		   (substring s p (string-length s))))))
	  ((char=? c #\9)
	   (string-set! s p #\0)
	   (cond ((zero? p) (string-append "1" s))
		 ((char-numeric? (string-ref s (+ -1 p)))
		  (chap:inc-string s (+ -1 p)))
		 (else
		  (string-append
		   (substring s 0 p)
		   "1"
		   (substring s p (string-length s))))))
	  ((or (char-alphabetic? c) (char-numeric? c))
	   (string-set! s p (integer->char
			     (+ chap:char-incr
				(char->integer (string-ref s p)))))
	   s)
	  (else (slib:error "inc-string error" s p)))))

(define (chap:next-string s)
  (do ((i (+ -1 (string-length s)) (+ -1 i)))
      ((or (negative? i)
	   (char-numeric? (string-ref s i))
	   (char-alphabetic? (string-ref s i)))
       (if (negative? i) (string-append s "0")
	   (chap:inc-string (string-copy s) i)))))

;;; testing utilities
;(define (ns s1) (chap:next-string s1))

;(define (ts s1 s2)
;  (let ((s< (chap:string<? s1 s2))
;	(s> (chap:string<? s2 s1)))
;    (cond (s<
;	   (display s1)
;	   (display " < ")
;	   (display s2)
;	   (newline)))
;    (cond (s>
;	   (display s1)
;	   (display " > ")
;	   (display s2)
;	   (newline)))))

(define (chap:string>? s1 s2) (chap:string<? s2 s1))
(define (chap:string>=? s1 s2) (not (chap:string<? s1 s2)))
(define (chap:string<=? s1 s2) (not (chap:string<? s2 s1)))
