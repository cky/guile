;;; "report.scm" relational-database-utility
; Copyright 1995 Aubrey Jaffer
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

;;;; Considerations for report generation:
; * columnar vs. fixed-multi-line vs. variable-multi-line
; * overflow lines within column boundaries.
; * break overflow across page?
; * Page headers and footers (need to know current/previous record-number
;   and next record-number).
; * Force page break on general expression (needs next row as arg).
; * Hierachical reports.

;================================================================

(require 'format)
(require 'database-utilities)

(define (dbutil:database arg)
  (cond ((procedure? arg) arg)
	((string? arg) (dbutil:open-database arg))
	((symbol? arg) (slib:eval arg))
	(else (slib:error "can't coerce to database: " arg))))

(define (dbutil:table arg)
  (cond ((procedure? arg) arg)
	((and (list? arg) (= 2 (length arg)))
	 (((dbutil:database (car arg)) 'open-table) (cadr arg) #f))))

(define (dbutil:print-report table header reporter footer . args)
  (define output-port (and (pair? args) (car args)))
  (define page-height (and (pair? args) (pair? (cdr args)) (cadr args)))
  (define minimum-break
    (and (pair? args) (pair? (cdr args)) (pair? (cddr args)) (caddr args)))
  (set! table (dbutil:table table))
  ((lambda (fun)
     (cond ((output-port? output-port)
	    (fun output-port))
	   ((string? output-port)
	    (call-with-output-file output-port fun))
	   ((or (boolean? output-port) (null? output-port))
	    (fun (current-output-port)))
	   (else (slib:error "can't coerce to output-port: " arg))))
   (lambda (output-port)
     (set! page-height (or page-height (output-port-height output-port)))
     (set! minimum-break (or minimum-break 0))
     (let ((output-page 0)
	   (output-line 0)
	   (nth-newline-index
	    (lambda (str n)
	      (define len (string-length str))
	      (do ((i 0 (+ i 1)))
		  ((or (zero? n) (> i len)) (+ -1 i))
		(cond ((char=? #\newline (string-ref str i))
		       (set! n (+ -1 n)))))))
	   (count-newlines
	    (lambda (str)
	      (define cnt 0)
	      (do ((i (+ -1 (string-length str)) (+ -1 i)))
		  ((negative? i) cnt)
		(cond ((char=? #\newline (string-ref str i))
		       (set! cnt (+ 1 cnt)))))))
	   (format (let ((oformat format))
		     (lambda (dest fmt arg)
		       (cond ((not (procedure? fmt)) (oformat dest fmt arg))
			     ((output-port? dest) (fmt dest arg))
			     ((eq? #t dest) (fmt (current-output-port) arg))
			     ((eq? #f dest) (call-with-output-string
					     (lambda (port) (fmt port arg))))
			     (else (oformat dest fmt arg)))))))
       (define column-names (table 'column-names))
       (define (do-header)
	 (let ((str (format #f header column-names)))
	   (display str output-port)
	   (set! output-line (count-newlines str))))
       (define (do-lines str inc)
	 (cond
	  ((< (+ output-line inc) page-height)
	   (display str output-port)
	   (set! output-line (+ output-line inc)))
	  (else				;outputting footer
	   (cond ((and (not (zero? minimum-break))
		       (> cnt (* 2 minimum-break))
		       (> (- page-height output-line) minimum-break))
		  (let ((break (nth-newline-index
				str (- page-height output-line))))
		    (display (substring str 0 (+ 1 break) output-port))
		    (set! str (substring str (+ 1 break) (string-length str)))
		    (set! inc (- inc (- page-height output-line))))))
	   (format output-port footer column-names)
	   (display slib:form-feed output-port)
	   (set! output-page (+ 1 output-page))
	   (do-header)
	   (do-lines str inc))))

       (do-header)
       ((table 'for-each-row)
	(lambda (row)
	  (let ((str (format #f reporter row)))
	    (do-lines str (count-newlines str)))))
       output-page))))
