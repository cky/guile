;;;; "charplot.scm", plotting on character devices for Scheme
;;; Copyright (C) 1992, 1993 Aubrey Jaffer.
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

(require 'sort)
(require 'printf)
(require 'array)
(require 'array-for-each)

(define charplot:rows 24)
(define charplot:columns (output-port-width (current-output-port)))

(define charplot:xborder #\_)
(define charplot:yborder #\|)
(define charplot:xaxchar #\-)
(define charplot:yaxchar #\:)
(define charplot:curve1 #\*)
(define charplot:xtick #\.)

(define charplot:height (- charplot:rows 5))
(define charplot:width (- charplot:columns 15))

(define (charplot:printn! n char)
  (cond ((positive? n)
	 (write-char char)
	 (charplot:printn! (+ n -1) char))))

(define (charplot:center-print! str width)
  (let ((lpad (quotient (- width (string-length str)) 2)))
    (charplot:printn! lpad #\ )
    (display str)
    (charplot:printn! (- width (+ (string-length  str) lpad)) #\ )))

(define (charplot:number->string x)
  (sprintf #f "%g" x))

(define (charplot:scale-it z scale)
  (if (and (exact? z) (integer? z))
      (quotient (* z (car scale)) (cadr scale))
      (inexact->exact (round (/ (* z (car scale)) (cadr scale))))))

(define (charplot:find-scale isize delta)
  (define (fs2)
    (cond ((< (* delta 8) isize) 8)
	  ((< (* delta 6) isize) 6)
	  ((< (* delta 5) isize) 5)
	  ((< (* delta 4) isize) 4)
	  ((< (* delta 3) isize) 3)
	  ((< (* delta 2) isize) 2)
	  (else 1)))
  (cond ((zero? delta) (set! delta 1))
	((inexact? delta) (set! isize (exact->inexact isize))))
  (do ((d 1 (* d 10)))
      ((<= delta isize)
       (do ((n 1 (* n 10)))
	   ((>= (* delta 10) isize)
	    (list (* n (fs2)) d))
	 (set! delta (* delta 10))))
    (set! isize (* isize 10))))

(define (charplot:iplot! data xlabel ylabel xmin xscale ymin yscale)
  (define xaxis (- (charplot:scale-it ymin yscale)))
  (define yaxis (- (charplot:scale-it xmin xscale)))
  (charplot:center-print! ylabel 11)
  (charplot:printn! (+ charplot:width 1) charplot:xborder)
  (newline)
  (set! data (sort! data (lambda (x y) (if (= (cdr x) (cdr y))
					   (< (car x) (car y))
					   (> (cdr x) (cdr y))))))
  (do ((ht (- charplot:height 1) (- ht 1)))
      ((negative? ht))
    (let ((a (make-string (+ charplot:width 1)
			  (if (= ht xaxis) charplot:xaxchar #\ )))
	  (ystep (if (= 1 (gcd (car yscale) 3)) 2 3)))
      (string-set! a charplot:width charplot:yborder)
      (if (< -1 yaxis charplot:width) (string-set! a yaxis charplot:yaxchar))
      (do ()
	  ((or (null? data) (not (>= (cdar data) ht))))
	(string-set! a (caar data) charplot:curve1)
	(set! data (cdr data)))
      (if (zero? (modulo (- ht xaxis) ystep))
	  (let* ((v (charplot:number->string (/ (* (- ht xaxis) (cadr yscale))
						(car yscale))))
		 (l (string-length v)))
	    (if (> l 10)
		(display (substring v 0 10))
		(begin
		  (charplot:printn! (- 10 l) #\ )
		  (display v)))
	    (display charplot:yborder)
	    (display charplot:xaxchar))
	  (begin
	    (charplot:printn! 10 #\ )
	    (display charplot:yborder)
	    (display #\ )))
      (display a) (newline)))
  (let* ((xstep (if (= 1 (gcd (car xscale) 3)) 10 12))
	 (xstep/2 (quotient (- xstep 2) 2))
	 (fudge (modulo yaxis xstep)))
    (charplot:printn! 10 #\ ) (display charplot:yborder)
    (charplot:printn! (+ 1 fudge) charplot:xborder)
    (display charplot:yaxchar)
    (do ((i fudge (+ i xstep)))
	((> (+ i xstep) charplot:width)
	 (charplot:printn! (modulo (- charplot:width (+ i 1)) xstep)
			   charplot:xborder))
      (charplot:printn! xstep/2 charplot:xborder)
      (display charplot:xtick)
      (charplot:printn! xstep/2 charplot:xborder)
      (display charplot:yaxchar))
    (display charplot:yborder) (newline)
    (charplot:center-print! xlabel (+ 12 fudge (- xstep/2)))
    (do ((i fudge (+ i xstep)))
	((>= i charplot:width))
      (charplot:center-print! (charplot:number->string
			       (/ (* (- i yaxis) (cadr xscale))
				  (car xscale)))
			      xstep))
    (newline)))

(define (charplot:plot! data xlabel ylabel)
  (cond ((array? data)
	 (case (array-rank data)
	   ((1) (set! data (map cons
				(let ((ra (apply make-array #f
						 (array-shape data))))
				  (array-index-map! ra identity)
				  (array->list ra))
				(array->list data))))
	   ((2) (set! data (map (lambda (lst) (cons (car lst) (cadr lst)))
				(array->list data)))))))
  (let* ((xmax (apply max (map car data)))
	 (xmin (apply min (map car data)))
	 (xscale (charplot:find-scale charplot:width (- xmax xmin)))
	 (ymax (apply max (map cdr data)))
	 (ymin (apply min (map cdr data)))
	 (yscale (charplot:find-scale charplot:height (- ymax ymin)))
	 (ixmin (charplot:scale-it xmin xscale))
	 (iymin (charplot:scale-it ymin yscale)))
    (charplot:iplot! (map (lambda (p)
			    (cons (- (charplot:scale-it (car p) xscale) ixmin)
				  (- (charplot:scale-it (cdr p) yscale) iymin)))
			  data)
		     xlabel ylabel xmin xscale ymin yscale)))

(define (plot-function! func vlo vhi . npts)
  (set! npts (if (null? npts) 100 (car npts)))
  (let ((dats (make-array 0.0 npts 2)))
    (array-index-map! (make-shared-array dats (lambda (idx) (list idx 0)) npts)
		      (lambda (idx) (+ vlo (* (- vhi vlo) (/ idx npts)))))
    (array-map! (make-shared-array dats (lambda (idx) (list idx 1)) npts)
		func
		(make-shared-array dats (lambda (idx) (list idx 0)) npts))
    (charplot:plot! dats "" "")))

(define plot! charplot:plot!)
