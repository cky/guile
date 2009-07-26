;;; Guile VM code converters

;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.

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

(define-module (language assembly decompile-bytecode)
  #:use-module (system vm instruction)
  #:use-module (system base pmatch)
  #:use-module (srfi srfi-4)
  #:use-module (rnrs bytevector)
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

(define (br-instruction? x)
  (memq x '(br br-if br-if-not br-if-eq br-if-not-eq br-if-null br-if-not-null)))

(define (bytes->s16 a b)
  (let ((x (+ (ash a 8) b)))
    (if (zero? (logand (ash 1 15) x))
        x
        (- x (ash 1 16)))))

;; FIXME: this is a little-endian disassembly!!!
(define (decode-load-program pop)
  (let* ((nargs (pop)) (nrest (pop)) (nlocs0 (pop)) (nlocs1 (pop))
         (nlocs (+ nlocs0 (ash nlocs1 8)))
         (a (pop)) (b (pop)) (c (pop)) (d (pop))
         (e (pop)) (f (pop)) (g (pop)) (h (pop))
         (len (+ a (ash b 8) (ash c 16) (ash d 24)))
         (metalen (+ e (ash f 8) (ash g 16) (ash h 24)))
         (totlen (+ len metalen))
         (pad0 (pop)) (pad1 (pop)) (pad2 (pop)) (pad3 (pop))
         (labels '())
         (i 0))
    (define (ensure-label rel1 rel2)
      (let ((where (+ i (bytes->s16 rel1 rel2))))
        (or (assv-ref labels where)
            (begin
              (let ((l (gensym ":L")))
                (set! labels (acons where l labels))
                l)))))
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
             `(load-program ,nargs ,nrest ,nlocs 
                            ,(map (lambda (x) (cons (cdr x) (car x)))
                                  (reverse labels))
                            ,len
                            ,(if (zero? metalen) #f (decode-load-program pop))
                            ,@(reverse! out)))
            (else
             (let ((exp (decode-bytecode sub-pop)))
               (pmatch exp
                 ((,br ,rel1 ,rel2) (guard (br-instruction? br))
                  (lp (cons `(,br ,(ensure-label rel1 rel2)) out)))
                 ((mv-call ,n ,rel1 ,rel2)
                  (lp (cons `(mv-call ,n ,(ensure-label rel1 rel2)) out)))
                 (else 
                  (lp (cons exp out))))))))))

(define (decode-bytecode pop)
  (and=> (pop)
         (lambda (opcode)
           (let ((inst (opcode->instruction opcode)))
             (cond
              ((eq? inst 'load-program)
               (decode-load-program pop))
              ((< (instruction-length inst) 0)
               (let* ((make-sequence
                       (if (eq? inst 'load-array)
                           make-bytevector
                           make-string))
                      (sequence-set!
                       (if (eq? inst 'load-array)
                           bytevector-u8-set!
                           (lambda (str pos value)
                             (string-set! str pos (integer->char value)))))

                      (len (let* ((a (pop)) (b (pop)) (c (pop)))
                             (+ (ash a 16) (ash b 8) c)))
                      (seq (make-sequence len)))
                 (let lp ((i 0))
                   (if (= i len)
                       `(,inst ,seq)
                       (begin
                         (sequence-set! seq i (pop))
                         (lp (1+ i)))))))
              (else
               ;; fixed length
               (let lp ((n (instruction-length inst)) (out (list inst)))
                 (if (zero? n)
                     (reverse! out)
                     (lp (1- n) (cons (pop) out))))))))))
