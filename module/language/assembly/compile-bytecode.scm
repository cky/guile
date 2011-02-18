;;; Guile VM assembler

;; Copyright (C) 2001, 2009, 2010 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language assembly compile-bytecode)
  #:use-module (system base pmatch)
  #:use-module (language assembly)
  #:use-module (system vm instruction)
  #:use-module (srfi srfi-4)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-26) #:select (cut))
  #:export (compile-bytecode))

;; Gross.
(define (port-position port)
  (seek port 0 SEEK_CUR))

(define (compile-bytecode assembly env . opts)
  (pmatch assembly
    ((load-program . _)
     (call-with-values open-bytevector-output-port
       (lambda (port get-bytevector)
         ;; Don't emit the `load-program' byte.
         (write-bytecode assembly port '() 0 #f)
         (values (get-bytevector) env env))))
    (else (error "bad assembly" assembly))))

(define (write-bytecode asm port labels address emit-opcode?)
  ;; Write ASM's bytecode to PORT, a (binary) output port.  If EMIT-OPCODE? is
  ;; false, don't emit bytecode for the first opcode encountered.  Assume code
  ;; starts at ADDRESS (an integer).  LABELS is assumed to be an alist mapping
  ;; labels to addresses.
  (define u32-bv (make-bytevector 4))
  (define write-byte (cut put-u8 port <>))
  (define get-addr
    (let ((start (port-position port)))
      (lambda ()
        (+ address (- (port-position port) start)))))
  (define (write-latin1-string s)
    (write-loader-len (string-length s))
    (string-for-each (lambda (c) (write-byte (char->integer c))) s))
  (define (write-int24-be x)
    (bytevector-s32-set! u32-bv 0 x (endianness big))
    (put-bytevector port u32-bv 1 3))
  (define (write-uint32-be x)
    (bytevector-u32-set! u32-bv 0 x (endianness big))
    (put-bytevector port u32-bv))
  (define (write-uint32 x)
    (bytevector-u32-native-set! u32-bv 0 x)
    (put-bytevector port u32-bv))
  (define (write-wide-string s)
    (write-loader-len (* 4 (string-length s)))
    (put-bytevector port (string->utf32 s (native-endianness))))
  (define (write-loader-len len)
    (write-byte (ash len -16))
    (write-byte (logand (ash len -8) 255))
    (write-byte (logand len 255)))
  (define (write-bytevector bv)
    (write-loader-len (bytevector-length bv))
    (put-bytevector port bv))
  (define (write-break label)
    (let ((offset (- (assq-ref labels label) (+ (get-addr) 3))))
      (cond ((>= offset (ash 1 23)) (error "jump too far forward" offset))
            ((< offset (- (ash 1 23))) (error "jump too far backwards" offset))
            (else (write-int24-be offset)))))
  
  (let ((inst (car asm))
        (args (cdr asm)))
    (let ((opcode (instruction->opcode inst))
          (len (instruction-length inst)))
      (if emit-opcode?
          (write-byte opcode))
      (pmatch asm
        ((load-program ,labels ,length ,meta . ,code)
         (write-uint32 length)
         (write-uint32 (if meta (1- (byte-length meta)) 0))
         (fold (lambda (asm address)
                 (let ((start (port-position port)))
                   (write-bytecode asm port labels address #t)
                   (+ address (- (port-position port) start))))
               0
               code)
         (if meta
             ;; Don't emit the `load-program' byte for metadata.  Note that
             ;; META's bytecode meets the alignment requirements of
             ;; `scm_objcode', thanks to the alignment computed in `(language
             ;; assembly)'.
             (write-bytecode meta port '() 0 #f)))
        ((make-char32 ,x) (write-uint32-be x))
        ((load-number ,str) (write-latin1-string str))
        ((load-string ,str) (write-latin1-string str))
        ((load-wide-string ,str) (write-wide-string str))
        ((load-symbol ,str) (write-latin1-string str))
        ((load-array ,bv) (write-bytevector bv))
        ((br ,l) (write-break l))
        ((br-if ,l) (write-break l))
        ((br-if-not ,l) (write-break l))
        ((br-if-eq ,l) (write-break l))
        ((br-if-not-eq ,l) (write-break l))
        ((br-if-null ,l) (write-break l))
        ((br-if-not-null ,l) (write-break l))
        ((br-if-nargs-ne ,hi ,lo ,l) (write-byte hi) (write-byte lo) (write-break l))
        ((br-if-nargs-lt ,hi ,lo ,l) (write-byte hi) (write-byte lo) (write-break l))
        ((br-if-nargs-gt ,hi ,lo ,l) (write-byte hi) (write-byte lo) (write-break l))
        ((mv-call ,n ,l) (write-byte n) (write-break l))
        ((prompt ,escape-only? ,l) (write-byte escape-only?) (write-break l))
        (else
         (cond
          ((< (instruction-length inst) 0)
           (error "unhanded variable-length instruction" asm))
          ((not (= (length args) len))
           (error "bad number of args to instruction" asm len))
          (else
           (for-each write-byte args))))))))
