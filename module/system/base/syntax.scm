;;; Guile VM specific syntaxes and utilities

;; Copyright (C) 2001 Free Software Foundation, Inc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA

;;; Code:

(define-module (system base syntax)
  :use-module (ice-9 try)
  :use-module (ice-9 match)
  :use-module (ice-9 receive)
  :use-module (ice-9 and-let-star)
  :export (try stack-catch match syntax-error receive and-let*))


;;;
;;; Keywords by `:KEYWORD
;;;

(read-set! keywords 'prefix)


;;;
;;; Dot expansion
;;;

;; FOO.BAR -> (slot FOO 'BAR)

(define (expand-dot! x)
  (cond ((symbol? x) (expand-symbol x))
	((pair? x)
	 (cond ((eq? (car x) 'quote) x)
	       (else (set-car! x (expand-dot! (car x)))
		     (set-cdr! x (expand-dot! (cdr x)))
		     x)))
	(else x)))

(define (expand-symbol x)
  (let loop ((s (symbol->string x)))
    (let ((i (string-rindex s #\.)))
      (if i
	  `(slot ,(loop (substring s 0 i))
		 (quote ,(string->symbol (substring s (1+ i)))))
	  (string->symbol s)))))

(export-syntax syntax)
(define syntax expand-dot!)


;;;
;;; Type
;;;

(export-syntax define-type)
(define-macro (define-type name sig) sig)

;;;
;;; Record
;;;

(export-syntax define-record)
(define-macro (define-record def)
  (let ((name (car def)) (slots (cdr def)))
    `(begin
       (define (,name . args)
	 (vector ',name (%make-struct
			 args
			 (list ,@(map (lambda (slot)
					(if (pair? slot)
					  `(cons ',(car slot) ,(cadr slot))
					  `',slot))
				      slots)))))
       (define (,(symbol-append name '?) x)
	 (and (vector? x) (eq? (vector-ref x 0) ',name)))
       ,@(do ((n 1 (1+ n))
	      (slots (cdr def) (cdr slots))
	      (ls '() (cons (let* ((slot (car slots))
				   (slot (if (pair? slot) (car slot) slot)))
			      `(define ,(string->symbol
					 (format #f "~A-~A" name n))
				 (lambda (x) (slot x ',slot))))
			    ls)))
	     ((null? slots) (reverse! ls))))))

(define *unbound* "#<unbound>")

(define-public (%make-struct args slots)
  (map (lambda (slot)
	 (let* ((key (if (pair? slot) (car slot) slot))
		(def (if (pair? slot) (cdr slot) *unbound*))
		(val (get-key args (symbol->keyword key) def)))
	   (if (eq? val *unbound*)
	     (error "Slot unbound:" key)
	     (cons key val))))
       slots))

(define (get-key klist key def)
  (do ((ls klist (cddr ls)))
      ((or (null? ls) (eq? (car ls) key))
       (if (null? ls) def (cadr ls)))))

(define-public slot
  (make-procedure-with-setter
   (lambda (struct name)
     (let ((data (assq name (vector-ref struct 1))))
       (cond ((not data)
	      (error "Unknown slot:" name))
	     (else (cdr data)))))
   (lambda (struct name val)
     (let ((data (assq name (vector-ref struct 1))))
       (cond ((not data)
	      (error "Unknown slot:" name))
	     (else (set-cdr! data val)))))))

;;;
;;; Variants
;;;

(export-syntax |)
(define-macro (| . rest)
  `(begin ,@(map %make-variant-type rest)))

(define (%make-variant-type def)
  (let ((name (car def)) (slots (cdr def)))
    `(begin
       (define ,def (vector ',name ,@slots))
       (define (,(symbol-append name '?) x)
	 (and (vector? x) (eq? (vector-ref x 0) ',name)))
       ,@(do ((n 1 (1+ n))
	      (slots slots (cdr slots))
	      (ls '() (cons `(define ,(string->symbol
				       (format #f "~A-~A" name n))
			       ,(string->symbol (format #f "%slot-~A" n)))
			    ls)))
	     ((null? slots) (reverse! ls))))))

(define-public (%slot-1 x) (vector-ref x 1))
(define-public (%slot-2 x) (vector-ref x 2))
(define-public (%slot-3 x) (vector-ref x 3))
(define-public (%slot-4 x) (vector-ref x 4))
(define-public (%slot-5 x) (vector-ref x 5))
(define-public (%slot-6 x) (vector-ref x 6))
(define-public (%slot-7 x) (vector-ref x 7))
(define-public (%slot-8 x) (vector-ref x 8))
(define-public (%slot-9 x) (vector-ref x 9))


;;;
;;; Utilities
;;;

(define-public (list-fold f d l)
  (if (null? l)
      d
      (list-fold f (f (car l) d) (cdr l))))
