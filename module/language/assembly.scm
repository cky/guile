;;; Guile Virtual Machine Assembly

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

(define-module (language assembly)
  #:use-module (system base pmatch)
  #:use-module (system vm instruction)
  #:export (byte-length
            assembly-pack assembly-unpack
            object->assembly assembly->object))

;; nargs, nrest, nlocs, nexts, len, metalen
(define *program-header-len* (+ 1 1 1 1 4 4))

;; lengths are encoded in 3 bytes
(define *len-len* 3)

(define (byte-length assembly)
  (pmatch assembly
    (,label (guard (not (pair? label)))
     0)
    ((load-integer ,str)
     (+ 1 *len-len* (string-length str)))
    ((load-number ,str)
     (+ 1 *len-len* (string-length str)))
    ((load-string ,str)
     (+ 1 *len-len* (string-length str)))
    ((load-symbol ,str)
     (+ 1 *len-len* (string-length str)))
    ((load-keyword ,str)
     (+ 1 *len-len* (string-length str)))
    ((define ,str)
     (+ 1 *len-len* (string-length str)))
    ((load-program ,nargs ,nrest ,nlocs ,nexts ,labels ,len ,metalen . ,code)
     (+ 1 *program-header-len* len metalen))
    ((,inst . _) (guard (>= (instruction-length inst) 0))
     (+ 1 (instruction-length inst)))
    (else (error "unknown instruction" assembly))))

;;;
;;; Code compress/decompression
;;;

(define *abbreviations*
  '(((make-int8 0) . (make-int8:0))
    ((make-int8 1) . (make-int8:1))))
  
(define *expansions*
  (map (lambda (x) (cons (cdr x) (car x))) *abbreviations*))

(define (assembly-pack code)
  (or (assoc-ref code *abbreviations*)
      code))

(define (assembly-unpack code)
  (or (assoc-ref code *expansions*)
      code))


;;;
;;; Encoder/decoder
;;;

(define (object->assembly x)
  (cond ((eq? x #t) `(make-true))
	((eq? x #f) `(make-false))
	((null? x) `(make-eol))
	((and (integer? x) (exact? x))
	 (cond ((and (<= -128 x) (< x 128))
		`(make-int8 ,(modulo x 256)))
	       ((and (<= -32768 x) (< x 32768))
		(let ((n (if (< x 0) (+ x 65536) x)))
		  `(make-int16 ,(quotient n 256) ,(modulo n 256))))
	       (else #f)))
	((char? x) `(make-char8 ,(char->integer x)))
	(else #f)))

(define (assembly->object code)
  (pmatch code
    ((make-true) #t)
    ((make-false) #f) ;; FIXME: Same as the `else' case!
    ((make-eol) '())
    ((make-int8 ,n)
     (if (< n 128) n (- n 256)))
    ((make-int16 ,n1 ,n2)
     (let ((n (+ (* n1 256) n2)))
       (if (< n 32768) n (- n 65536))))
    ((make-char8 ,n)
     (integer->char n))
    ((load-string ,s) s)
    ((load-symbol ,s) (string->symbol s))
    ((load-keyword ,s) (symbol->keyword (string->symbol s)))
    (else #f)))
