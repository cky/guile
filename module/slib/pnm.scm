;;; "pnm.scm" Read PNM image files.
; Copyright 2000 Aubrey Jaffer
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

(require 'scanf)
(require 'printf)
(require 'array)
(require 'array-for-each)
(require 'byte)
(require 'line-i/o)

(define (pnm:read+integer port)
  (define uint #f)
  (do ((chr (peek-char port) (peek-char port)))
      ((not (and (char? chr) (or (char-whitespace? chr) (eqv? #\# chr)))))
    (if (eqv? #\# chr)
	(read-line port)
	(read-char port)))
  (if (eof-object? (peek-char port))
      (peek-char port)
      (and (eqv? 1 (fscanf port " %u" uint)) uint)))

(define (pnm:type-dimensions port)
  (if (input-port? port)
      (let* ((c1 (read-char port))
	     (c2 (read-char port)))
	(cond
	 ((and (eqv? #\P c1)
	       (char? c2)
	       (char-numeric? c2)
	       (char-whitespace? (peek-char port)))
	  (let* ((format (string->symbol (string #\p c2)))
		 (width (pnm:read+integer port))
		 (height (pnm:read+integer port))
		 (ret
		  (case format
		    ((p1) (list 'pbm     width height 1))
		    ((p4) (list 'pbm-raw width height 1))
		    ((p2) (list 'pgm     width height (pnm:read+integer port)))
		    ((p5) (list 'pgm-raw width height (pnm:read+integer port)))
		    ((p3) (list 'ppm     width height (pnm:read+integer port)))
		    ((p6) (list 'ppm-raw width height (pnm:read+integer port)))
		    (else #f))))
	    (and (char-whitespace? (read-char port)) ret)))
	 (else #f)))
      (call-with-input-file port pnm:type-dimensions)))

(define (pnm:read-binary! array port)
  (array-map! array (lambda () (read-byte port))))

(define (pnm:image-file->array path . array)
  (set! array (and (not (null? array)) (car array)))
  (call-with-input-file path
    (lambda (port)
      (apply (lambda (type width height max-pixel)
	       (define (read-binary)
		 (pnm:read-binary! array port)
		 (if (eof-object? (peek-char port)) array
		     (slib:error type 'not 'at 'file 'end)))
	       (define (read-text)
		 (array-map! array (lambda () (pnm:read+integer port)))
		 (if (eof-object? (pnm:read+integer port)) array
		     (slib:error type 'not 'at 'file 'end)))
	       (define (read-pbm)
		 (array-map! array (lambda () (eqv? 1 (pnm:read+integer port))))
		 (if (eof-object? (pnm:read+integer port)) array
		     (slib:error type 'not 'at 'file 'end)))
	       (case type
		 ((pbm)
		  (or array
		      (set! array (make-array #t height width)))
		  (read-pbm))
		 ((pgm)
		  (or array
		      (set! array (make-array max-pixel height width)))
		  (read-text))
		 ((ppm)
		  (or array
		      (set! array (make-array max-pixel height width 3)))
		  (read-text))
		 ((pbm-raw)
		  (or array
		      (set! array (make-array #t height (quotient width 8))))
		  (read-binary))
		 ((pgm-raw)
		  (or array
		      (set! array (make-array max-pixel height width)))
		  (read-binary))
		 ((ppm-raw)
		  (or array
		      (set! array (make-array max-pixel height width 3)))
		  (read-binary))))
	     (pnm:type-dimensions port)))))

(define (pnm:image-file->uniform-array path . array)
  (fluid-let ((make-array make-uniform-array)
	      (pnm:read-binary!
	       (lambda (ra port)
		 (if (array? ra #t)
		     (error 'pnm:image-file->array
			    "pbm-raw support unimplemented")
		     (let ((bytes (apply make-uniform-array #\a 
					 (array-dimensions ra))))
		       (uniform-array-read! bytes port)
		       (array-map! ra char->integer bytes))))))
    (apply pnm:image-file->array path array)))

;; ARRAY is required to be zero-based.
(define (pnm:array-write type array maxval port)
  (define (write-header type height width maxval)
    (let ((magic
	   (case type
	     ((pbm) "P1")
	     ((pgm) "P2")
	     ((ppm) "P3")
	     ((pbm-raw) "P4")
	     ((pgm-raw) "P5")
	     ((ppm-raw) "P6")
	     (else (error 'pnm:array-write "bad type" type)))))
      (fprintf port "%s\n%d %d" magic width height)
      (if maxval (fprintf port "\n%d" maxval))))
  (define (write-pixels type array maxval)
    (let* ((shp (array-dimensions array))
	   (height (car shp))
	   (width (cadr shp)))
      (case type
	((pbm-raw)
	 (newline port)
	 (if (array? array #t)
	     (uniform-array-write array port)
	     (error 'pnm:array-write "expected bit-array" array)))
	((pgm-raw ppm-raw)
	 (newline port)
;;;	 (let ((bytes (apply make-uniform-array #\a shp)))
;;;	   (array-map! bytes integer->char array)
;;;	   (uniform-array-write bytes port))
	 (uniform-array-write array port))
	((pbm)
	 (do ((i 0 (+ i 1)))
	     ((>= i height))
	   (do ((j 0 (+ j 1)))
	       ((>= j width))
	     (display (if (zero? (remainder j 35)) #\newline #\space) port)
	     (display (if (array-ref array i j) #\1 #\0) port)))
	 (newline port))
	((pgm)
	 (do ((i 0 (+ i 1)))
	     ((>= i height))
	   (do ((j 0 (+ j 1)))
	       ((>= j width))
	     (display (if (zero? (remainder j 17)) #\newline #\space) port)
	     (display (array-ref array i j) port)))
	 (newline port))
	((ppm)
	 (do ((i 0 (+ i 1)))
	     ((>= i height))
	   (do ((j 0 (+ j 1)))
	       ((>= j width))
	     (display (if (zero? (remainder j 5)) #\newline "  ") port)
	     (display (array-ref array i j 0) port)
	     (display #\space port)
	     (display (array-ref array i j 1) port)
	     (display #\space port)
	     (display (array-ref array i j 2) port)))
	 (newline port)))))

  (if (output-port? port)
      (let ((rnk (array-rank array))
	    (shp (array-dimensions array)))
	(case type
	  ((pbm pbm-raw)
	   (or (and (eqv? 2 rnk)
		    (integer? (car shp))
		    (integer? (cadr shp)))
	       (error 'pnm:array-write "bad shape" type array))
	   (or (eqv? 1 maxval)
	       (error 'pnm:array-write "maxval supplied not 1" type))
	   (write-header type (car shp) (cadr shp) #f)
	   (write-pixels type array 1))
	  ((pgm pgm-raw)
	   (or (and (eqv? 2 rnk)
		    (integer? (car shp))
		    (integer? (cadr shp)))
	       (error 'pnm:array-write "bad shape" type array))
	   (write-header type (car shp) (cadr shp) maxval)
	   (write-pixels type array maxval))
	  ((ppm ppm-raw)
	   (or (and (eqv? 3 rnk)
		    (integer? (car shp))
		    (integer? (cadr shp))
		    (eqv? 3 (caddr shp)))
	       (error 'pnm:array-write "bad shape" type array))
	   (write-header type (car shp) (cadr shp) maxval)
	   (write-pixels type array maxval))
	  (else (error 'pnm:array-write type 'unrecognized 'type))))
      (call-with-output-file port
	(lambda (port)
	  (pnm:array-write type array maxval port)))))
