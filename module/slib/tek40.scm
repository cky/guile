;"tek40.scm", Tektronix 4000 series graphics support in Scheme.
;Copyright (C) 1992, 1994 Aubrey Jaffer
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

;THIS FILE NEEDS MORE WORK.

;The Tektronix 4000 series graphics protocol gives the user a 1024 by
;1024 square drawing area.  The origin is in the lower left corner of
;the screen.  Increasing y is up and increasing x is to the right.

;The graphics control codes are sent over the current-output-port and
;can be mixed with regular text and ANSI or other terminal control
;sequences.

;  (tek40:init)						procedure

(define (tek40:init) 'noop)

(define esc-string (string (integer->char #o33)))

(define tek40:graphics-str
  (string-append
   (string slib:form-feed)
   esc-string  (string (integer->char #o14))
   ;; clear the screen
   ))

(define (tek40:graphics) (display tek40:graphics-str) (force-output))

(define (tek40:text)
  (tek40:move 0 12)
  (write-char (integer->char #o37)))

(define (tek40:linetype linetype)
  (cond ((or (negative? linetype) (> linetype 15))
	 (slib:error "bad linetype" linetype))
	(else
	 (display esc-string)
	 (write-char (integer->char (+ (char->integer #\`) linetype))))))

(define (tek40:move x y)
  (write-char (integer->char #o35))
  (tek40:draw x y))

(define (tek40:draw x y)
  (display (string
	    (integer->char (+ #x20 (quotient y 32)))
	    (integer->char (+ #x60 (remainder y 32)))
	    (integer->char (+ #x20 (quotient x 32)))
	    (integer->char (+ #x40 (remainder x 32))))))

(define (tek40:put-text x y str)
  (tek40:move x (+ y -11))
  (write-char (integer->char #o37))
  (display str))

(define (tek40:reset) (display tek40:graphics-str) (force-output))

(define (tek40:test)
  (tek40:init)
;  (tek40:reset)
  (tek40:graphics)
  (tek40:linetype 0)
  (tek40:move 100 100)
  (tek40:draw 200 100)
  (tek40:draw 200 200)
  (tek40:draw 100 200)
  (tek40:draw 100 100)
  (do ((i 0 (+ 1 i)))
      ((> i 15))
    (tek40:linetype i)
    (tek40:move (+ (* 50 i) 100) 100)
    (tek40:put-text (+ (* 50 i) 100) 100 (number->string i))
    (tek40:move (+ (* 50 i) 100) 100)
    (tek40:draw (+ (* 50 i) 200) 200))
  (tek40:linetype 0)
  (tek40:text))
