;;; Guile VM code converters

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (system vm conv)
  :use-module (system vm core)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :export (code-pack code-unpack object->code object->dump-code code->object))

(define (code-pack code)
  (match code
    ((inst (? integer? n))
     (cond ((< n 10)
	    (let ((abbrev (string->symbol (format #f "~A:~A" inst n))))
	      (if (instruction? abbrev) (list abbrev) code)))
	   ((> n 255)
	    (let ((double (string->symbol (format #f "~A*2" inst))))
	      (if (instruction? double)
		  (list double (quotient n 256) (modulo n 256))
		  (apply error "Index out of range:" code))))
	   (else code)))
    (else code)))

(define (code-unpack code)
  (let ((inst (symbol->string (car code))))
    (cond
     ((string-match "^([^:]*):([0-9]+)$" inst) =>
      (lambda (data)
	(cons* (string->symbol (match:substring data 1))
	       (string->number (match:substring data 2))
	       (cdr code))))
     (else code))))

(define (object->code x)
  (cond ((eq? x #t) `(make-true))
	((eq? x #f) `(make-false))
	((null? x) `(make-eol))
	((integer? x)
	 (cond ((and (<= -128 x) (< x 128))
		`(make-int8 ,(modulo x 256)))
	       ((and (<= -32768 x) (< x 32768))
		(let ((n (if (< x 0) (+ x 65536) x)))
		  `(make-int16 ,(quotient n 256) ,(modulo n 256))))
	       (else #f)))
	((char? x) `(make-char8 ,(char->integer x)))
	(else #f)))

(define (object->dump-code x)
  (let ((stack '()))
    (define (push-code! code)
      (set! stack (cons code stack)))
    (let dump! ((x x))
      (cond
       ((object->code x) => push-code!)
       ((integer? x)
	(let ((str (do ((n x (quotient n 256))
			(l '() (cons (modulo n 256) l)))
		       ((= n 0)
			(list->string (map integer->char l))))))
	  (push-code! `(load-integer ,str))))
       ((string? x)
	(push-code! `(load-string ,x)))
       ((symbol? x)
	(push-code! `(load-symbol ,(symbol->string x))))
       ((keyword? x)
	(push-code! `(load-keyword ,(symbol->string (keyword-dash-symbol x)))))
       ((list? x)
	(for-each dump! x)
	(push-code! `(list ,(length x))))
       ((pair? x)
	(dump! (car x))
	(dump! (cdr x))
	(push-code! `(cons)))
       ((vector? x)
	(for-each dump! (vector->list x))
	(push-code! `(vector ,(vector-length x))))
       (else
	(error "Cannot dump:" x))))
    (reverse! stack)))

(define (code->object code)
  (match code
    (('make-true) #t)
    (('make-false) #f) ;; FIXME: Same as the `else' case!
    (('make-eol) '())
    (('make-int8 n)
     (if (< n 128) n (- n 256)))
    (('make-int16 n1 n2)
     (let ((n (+ (* n1 256) n2)))
       (if (< n 32768) n (- n 65536))))
    (('make-char8 n)
     (integer->char n))
    (('load-string s) s)
    (('load-symbol s) (string->symbol s))
    (('load-keyword s) (symbol->keyword (string->symbol s)))
    (else #f)))

(define-public (make-byte-decoder bytes)
  (let ((addr 0) (size (string-length bytes)))
    (define (pop)
      (let ((byte (char->integer (string-ref bytes addr))))
	(set! addr (1+ addr))
	byte))
    (define (pop-length)
      (let ((len (pop)))
	(cond ((< len 254) len)
	      ((= len 254) (+ (* (pop) 256) (pop)))
	      (else (+ (* (pop) 256 256 256) (* (pop) 256 256)
		       (* (pop) 256) (pop))))))
    (lambda ()
      (if (< addr size)
	  (let* ((start addr)
		 (inst (opcode->instruction (pop)))
		 (n (instruction-length inst))
		 (code (if (< n 0)
			   ;; variable length
			   (let* ((end (+ (pop-length) addr))
				  (str (substring bytes addr end)))
			     (set! addr end)
			     (list inst str))
			   ;; fixed length
			   (do ((n n (1- n))
				(l '() (cons (pop) l)))
			       ((= n 0) (cons* inst (reverse! l)))))))
	    (values start code))
	  #f))))
