;"tek41.scm", Tektronix 4100 series graphics support in Scheme.
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

;THIS FILE NEEDS MORE WORK.  Let me know if you test or fix it.

;The graphics control codes are sent over the current-output-port and
;can be mixed with regular text and ANSI or other terminal control
;sequences.

(define esc-string (string (integer->char #o33)))

(define tek41:init
  (string-append
   esc-string "%!0"
   ;;1. set tek mode
   esc-string "MN0"
   ;;2. set character path to 0 (characters placed equal to rotation)
   esc-string "MCB7C;"
   ;;3. set character size to 59 height
   esc-string "MQ1"
   ;;4. set character precision to string
   esc-string "MT1"
   ;;5. set character text index to 1
   esc-string "MG1"
   ;;6. set character write mode to overstrike
   esc-string "RK!"
   ;;7. clear the view
   esc-string "SK!"
   ;;8. clear the segments
   esc-string "LZ"
   ;;9. clear the dialog buffer
   esc-string "%!1"
   ;;10. set ansi mode
   ))

(define (tek41:init) (display tek41:init-str) (force-output))

(define (tek41:reset)
  (string-append
   esc-string "%!0"
   ;;1. set tek mode
   esc-string "LZ"
   ;;2. clear the dialog buffer
   esc-string "%!1"
   ;;3. set ansi mode
   ))

(define (tek41:reset) (display tek41:reset-str) (force-output))

(define tek41:graphics-str
  (string-append
   esc-string  "%!0"
   ;;1. set tek mode
   esc-string  (string (integer->char #o14))
   ;;2. clear the screen
   esc-string  "LV0"
   ;;3. set dialog area invisible
   ))

(define (tek41:graphics) (display tek41:graphics-str) (force-output))

(define tek41:text-str
  (string-append
  esc-string  "LV1"
  ;;1. set dialog area visible
  esc-string  "%!1"
  ;;2. set ansi mode
  ))

(define (tek41:text) (display tek41:text-str) (force-output))

(define tek41:move-str
  (string-append esc-string  "LF"))

(define (tek41:move x y)
  (display tek41:move-str)
  (tek41:encode-x-y x y)
  (force-output))

(define tek41:draw-str
  (string-append esc-string  "LG"))

(define (tek41:draw x y)
  (display tek41:draw-str)
  (tek41:encode-x-y x y)
  (force-output))

(define tek41:set-marker-str (string-append esc-string "MM"))
(define tek41:draw-marker-str (string-append esc-string "LH"))

(define (tek41:point x y number)
  (display tek41:set-marker-str)
  (tek41:encode-int (remainder (max number 0) 11))
  (display tek41:draw-marker-str)
  (tek41:encode-x-y x y)
  (force-output))

(define (tek41:encode-x-y x y)
  (let ((hix (+ (quotient x 128) 32))
	(lox (+ (modulo (quotient x 4) 32) 64))
	(hiy (+ (quotient y 128) 32))
	(loy (+ (modulo (quotient y 4) 32) 96))
	(eb (+ (* (modulo y 4) 4) (modulo x 4) 96)))
    (if (positive? hiy) (write-char (integer->char hiy)))
    (if (positive? eb) (write-char (integer->char eb)))
    (if (positive? (+ loy eb hix)) (write-char (integer->char loy)))
    (if (positive? hix) (write-char (integer->char hix)))
    (write-char (integer->char lox))))

(define (tek41:encode-int number)
  (let* ((mag (abs number))
	 (hi1 (+ (quotient mag 1024) 64))
	 (hi2 (+ (modulo (quotient mag 16) 64) 64))
	 (lo (+ (modulo mag 16) 32)))
    (if (>= number 0) (set! lo (+ lo 16)))
    (if (not (= hi1 64)) (write-char (integer->char hi1)))
    (if (or (not (= hi2 64))
	    (not (= hi1 64)))
	(write-char (integer->char hi2)))
    (write-char (integer->char lo))))

(define (test)
  (tek41:init)
  (tek41:reset)
  (tek41:graphics)
  (do ((i 0 (+ 1 i)))
      ((> i 15))
    (tek41:linetype i)
    (tek41:move (+ (* 200 i) 1000) 1000)
    (tek41:draw (+ (* 200 i) 2000) 2000))
  (tek41:text))
