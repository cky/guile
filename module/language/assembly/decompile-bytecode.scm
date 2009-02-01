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

(define-module (language assembly decompile-bytecode)
  #:use-module (system vm instruction)
  #:use-module (system base pmatch)
  #:use-module (srfi srfi-4)
  #:use-module (language assembly)
  #:export (decompile-bytecode))

(define (decompile-bytecode x env opts)
  (let ((i 0) (size (u8vector-length x)))
    (define (pop)
      (let ((b (cond ((< i size) (u8vector-ref x i))
                     ((= i size) #f)
                     (else (error "tried to decode too many bytes")))))
        (if b (set! i (1+ i)))
        b))
    (let ((ret (decode-load-program pop)))
      (if (= i size)
          (values ret env)
          (error "bad bytecode: only decoded ~a out of ~a bytes" i size)))))

(define (decode-load-program pop)
  (let* ((nargs (pop)) (nrest (pop)) (nlocs (pop)) (nexts (pop))
         (a (pop)) (b (pop)) (c (pop)) (d (pop))
         (e (pop)) (f (pop)) (g (pop)) (h (pop))
         (len (+ a (ash b 8) (ash c 16) (ash d 24)))
         (metalen (+ e (ash f 8) (ash g 16) (ash h 24)))
         (totlen (+ len metalen))
         (i 0))
    (define (sub-pop) ;; ...records. ha. ha.
      (let ((b (cond ((< i len) (pop))
                     ((= i len) #f)
                     (else (error "tried to decode too many bytes")))))
        (if b (set! i (1+ i)))
        b))
    (let lp ((out '()))
      (cond ((> i len)
             (error "error decoding program -- read too many bytes" out))
            ((= i len)
             `(load-program ,nargs ,nrest ,nlocs ,nexts () ,len
                            ,(if (zero? metalen) #f (decode-load-program pop))
                            ,@(reverse! out)))
            (else
             (let ((exp (decode-bytecode sub-pop)))
               ;; replace with labels?
               (lp (cons exp out))))))))

(define (decode-bytecode pop)
  (and=> (pop)
         (lambda (opcode)
           (let ((inst (opcode->instruction opcode)))
             (cond
              ((eq? inst 'load-program)
               (decode-load-program pop))
              ((< (instruction-length inst) 0)
               (let* ((len (decode-length pop))
                      (str (make-string len)))
                 (let lp ((i 0))
                   (if (= i len)
                       `(,inst ,str)
                       (begin
                         (string-set! str i (integer->char (pop)))
                         (lp (1+ i)))))))
              (else
               ;; fixed length
               (let lp ((n (instruction-length inst)) (out (list inst)))
                 (if (zero? n)
                     (reverse! out)
                     (lp (1- n) (cons (pop) out))))))))))
