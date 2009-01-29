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
  #:use-module (system vm instruction)
  #:use-module (system base pmatch)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-1)
  #:export (code-pack code-unpack object->code code->object code->bytes
	   make-byte-decoder))

;;;
;;; Code compress/decompression
;;;

(define (code-pack code)
  (pmatch code
    ((,inst ,n) (guard (integer? n))
     (cond ((< n 10)
	    (let ((abbrev (string->symbol (format #f "~A:~A" inst n))))
	      (if (instruction? abbrev) (list abbrev) code)))
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


;;;
;;; Encoder/decoder
;;;

(define (object->code x)
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

(define (code->object code)
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

; (let ((c->o code->object))
;   (set! code->object
; 	(lambda (code)
; 	  (format #t "code->object: ~a~%" code)
; 	  (let ((ret (c->o code)))
; 	    (format #t "code->object returned ~a~%" ret)
; 	    ret))))

(define (code->bytes code)
  (define (string->u8vector str)
    (apply u8vector (map char->integer (string->list str))))

  (let* ((code (code-pack code))
	 (inst (car code))
	 (rest (cdr code))
	 (len (instruction-length inst))
	 (head (instruction->opcode inst)))
    (cond ((< len 0)
	   ;; Variable-length code
	   ;; Typical instructions are `link' and `load-program'.
	   (if (string? (car rest))
	       (set-car! rest (string->u8vector (car rest))))
	   (let* ((str (car rest))
		  (str-len (u8vector-length str))
		  (encoded-len (encode-length str-len))
		  (encoded-len-len (u8vector-length encoded-len)))
	     (apply u8vector
		    (append (cons head (u8vector->list encoded-len))
			    (u8vector->list str)))))
	  ((= len (length rest))
	   ;; Fixed-length code
	   (apply u8vector (cons head rest)))
	  (else
	   (error "Invalid code:" code)))))

; (let ((c->b code->bytes))
;   ;; XXX: Debugging output
;   (set! code->bytes
; 	(lambda (code)
; 	  (format #t "code->bytes: ~a~%" code)
; 	  (let ((result (c->b code)))
; 	    (format #t "code->bytes: returned ~a~%" result)
; 	    result))))


(define (make-byte-decoder bytes)
  (let ((addr 8) (size (u8vector-length bytes)))
    (define (pop)
      (let ((byte (u8vector-ref bytes addr)))
	(set! addr (1+ addr))
	byte))
    (define (sublist lst start end)
      (take (drop lst start) (- end start)))
    (lambda ()
      (cond
       ((>= addr size)
        (values #f #f #f))
       (else
        (let* ((start addr)
               (inst (opcode->instruction (pop))))
          (cond
            ((eq? inst 'load-program)
             ;; FIXME just turn it into a bytecode slice?
             (pk 'yo addr size)
             (let* ((len (+ 8
                            (u8vector-ref bytes (+ addr 4))
                            (ash (u8vector-ref bytes (+ addr 5)) 8)
                            (ash (u8vector-ref bytes (+ addr 6)) 16)
                            (ash (u8vector-ref bytes (+ addr 7)) 24)))
                    (end (+ len addr))
                    (subbytes (sublist (u8vector->list bytes) addr end)))
               (set! addr end)
               (values start addr
                       (list inst (list->u8vector subbytes)))))
            ((< (instruction-length inst) 0)
             (let* ((end (+ (decode-length pop) addr))
                    (subbytes (sublist
                               (u8vector->list bytes)
                               addr end)))
               (set! addr end)
               (values start addr
                       (list inst
                             (list->string (map integer->char subbytes))))))
            (else
             ;; fixed length
             (do ((n (instruction-length inst) (1- n))
                  (l '() (cons (pop) l)))
                 ((= n 0) (values start addr (cons* inst (reverse! l)))))))))))))


;;;
;;; Variable-length interface
;;;

(define (decode-length pop)
  (let* ((a (pop)) (b (pop)) (c (pop)))
    (+ (ash a 16) (ash b 8) c)))
