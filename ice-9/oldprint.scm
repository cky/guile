;;;; 	Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 


;;; {Print}
;;;
;;; This code was removed from boot-9.scm by MDJ 970301
;;; <djurfeldt@nada.kth.se>.  It is placed here for archival
;;; purposes.

(define (print obj . args)
  (let ((default-args (list (current-output-port) 0 0 default-print-style #f)))
    (apply-to-args (append args (list-cdr-ref default-args (length args)))
      (lambda (port depth length style table)
	(cond
	 ((and table (print-table-ref table obj))
	  ((print-style-tag-hook style 'eq-val) 
	   obj port depth length style table))
	 (else
	  (and table (print-table-add! table obj))
	  (cond
	   ((print-style-max-depth? style depth)
	    ((print-style-excess-depth-hook style)))
	   ((print-style-max-length? style length)
	    ((print-style-excess-length-hook style)))
	   (else
	    ((print-style-hook style obj)
	     obj port depth length style table)))))))))

(define (make-print-style) (make-vector 59 '()))

(define (extend-print-style! style utag printer)
  (hashq-set! style utag printer))

(define (print-style-hook style obj)
  (let ((type-tag (tag obj)))
    (or (hashq-ref style type-tag)
	(hashq-ref style (logand type-tag 255))
	print-obj)))

(define (print-style-tag-hook style type-tag)
  (or (hashq-ref style type-tag)
      print-obj))

(define (print-style-max-depth? style d) #f)
(define (print-style-max-length? style l) #f)
(define (print-style-excess-length-hook style)
  (hashq-ref style 'excess-length-hook))
(define (print-style-excess-depth-hook style)
  (hashq-ref style 'excess-depth-hook))

(define (make-print-table) (make-vector 59 '()))
(define (print-table-ref table obj) (hashq-ref table obj))
(define (print-table-add! table obj) (hashq-set! table obj (gensym 'ref)))

(define (print-obj obj port depth length style table) (write obj port))

(define (print-pair pair port depth length style table)
  (if (= 0 length)
      (display #\( port))

  (print (car pair) port (+ 1 depth) 0 style table)

  (cond
   ((and (pair? (cdr pair))
	 (or (not table)
	     (not (print-table-ref table (cdr pair)))))

    (display #\space port)
    (print (cdr pair) port depth (+ 1 length) style table))

   ((null? (cdr pair))		(display #\) port))
   
   (else			(display " . " port)
				(print (cdr pair) port (+ 1 depth) 0
				       style table)
				(display #\) port))))

(define (print-vector obj port depth length style table)
  (if (= 0 length)
      (cond
       ((weak-key-hash-table? obj)	(display "#wh(" port))
       ((weak-value-hash-table? obj)	(display "#whv(" port))
       ((doubly-weak-hash-table? obj)	(display "#whd(" port))
       (else		  		(display "#(" port))))
	  
  (if (< length (vector-length obj))
      (print (vector-ref obj length) port (+ 1 depth) 0 style table))

  (cond
   ((>= (+ 1 length) (vector-length obj))	(display #\) port))
   (else					(display #\space port)
						(print obj port depth
						       (+ 1 length)
						       style table))))

(define default-print-style (make-print-style))

(extend-print-style! default-print-style utag_vector print-vector)
(extend-print-style! default-print-style utag_wvect print-vector)
(extend-print-style! default-print-style utag_pair print-pair)
(extend-print-style! default-print-style 'eq-val
		     (lambda (obj port depth length style table)
		       (if (symbol? obj)
			   (display obj)
			   (begin
			     (display "##" port)
			     (display (print-table-ref table obj))))))
