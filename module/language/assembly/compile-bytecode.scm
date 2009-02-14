;;; Guile VM assembler

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

(define-module (language assembly compile-bytecode)
  #:use-module (system base pmatch)
  #:use-module (language assembly)
  #:use-module (system vm instruction)
  #:use-module (srfi srfi-4)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((system vm objcode) #:select (byte-order))
  #:export (compile-bytecode write-bytecode))

(define (compile-bytecode assembly env . opts)
  (pmatch assembly
    ((load-program . _)
     ;; the 1- and -1 are so that we drop the load-program byte
     (letrec ((v (make-u8vector (1- (byte-length assembly))))
              (i -1)
              (write-byte (lambda (b)
                            (if (>= i 0) (u8vector-set! v i b))
                            (set! i (1+ i))))
              (get-addr (lambda () i)))
       (write-bytecode assembly write-byte get-addr '())
       (if (= i (u8vector-length v))
           (values v env)
           (error "incorrect length in assembly" i (u8vector-length v)))))
    (else (error "bad assembly" assembly))))

(define (write-bytecode asm write-byte get-addr labels)
  (define (write-char c)
    (write-byte (char->integer c)))
  (define (write-string s)
    (string-for-each write-char s))
  (define (write-uint16-be x)
    (write-byte (logand (ash x -8) 255))
    (write-byte (logand x 255)))
  (define (write-uint16-le x)
    (write-byte (logand x 255))
    (write-byte (logand (ash x -8) 255)))
  (define (write-uint32-be x)
    (write-byte (logand (ash x -24) 255))
    (write-byte (logand (ash x -16) 255))
    (write-byte (logand (ash x -8) 255))
    (write-byte (logand x 255)))
  (define (write-uint32-le x)
    (write-byte (logand x 255))
    (write-byte (logand (ash x -8) 255))
    (write-byte (logand (ash x -16) 255))
    (write-byte (logand (ash x -24) 255)))
  (define (write-loader-len len)
    (write-byte (ash len -16))
    (write-byte (logand (ash len -8) 255))
    (write-byte (logand len 255)))
  (define (write-loader str)
    (write-loader-len (string-length str))
    (write-string str))
  (define (write-break label)
    (write-uint16-be (- (assq-ref labels label) (+ (get-addr) 2))))
  
  (let ((inst (car asm))
        (args (cdr asm))
        (write-uint32 (case byte-order
                        ((1234) write-uint32-le)
                        ((4321) write-uint32-be)
                        (else (error "unknown endianness" byte-order)))))
    (let ((opcode (instruction->opcode inst))
          (len (instruction-length inst)))
      (write-byte opcode)
      (pmatch asm
        ((load-program ,nargs ,nrest ,nlocs ,nexts
                       ,labels ,length ,meta . ,code)
         (write-byte nargs)
         (write-byte nrest)
         (write-byte nlocs)
         (write-byte nexts)
         (write-uint32 length)
         (write-uint32 (if meta (1- (byte-length meta)) 0))
         (letrec ((i 0)
                  (write (lambda (x) (set! i (1+ i)) (write-byte x)))
                  (get-addr (lambda () i)))
           (for-each (lambda (asm)
                       (write-bytecode asm write get-addr labels))
                     code))
         (if meta
             ;; don't write the load-program byte for metadata
             (letrec ((i -1)
                      (write (lambda (x)
                               (set! i (1+ i))
                               (if (> i 0) (write-byte x))))
                      (get-addr (lambda () i)))
               (write-bytecode meta write get-addr '()))))
        ((load-integer ,str) (write-loader str))
        ((load-number ,str) (write-loader str))
        ((load-string ,str) (write-loader str))
        ((load-symbol ,str) (write-loader str))
        ((load-keyword ,str) (write-loader str))
        ((define ,str) (write-loader str))
        ((br ,l) (write-break l))
        ((br-if ,l) (write-break l))
        ((br-if-not ,l) (write-break l))
        ((br-if-eq ,l) (write-break l))
        ((br-if-not-eq ,l) (write-break l))
        ((br-if-null ,l) (write-break l))
        ((br-if-not-null ,l) (write-break l))
        ((mv-call ,n ,l) (write-byte n) (write-break l))
        (else
         (cond
          ((< (instruction-length inst) 0)
           (error "unhanded variable-length instruction" asm))
          ((not (= (length args) len))
           (error "bad number of args to instruction" asm len))
          (else
           (for-each write-byte args))))))))
