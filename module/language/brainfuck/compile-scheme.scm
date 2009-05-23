;;; Brainfuck for GNU Guile

;; Copyright (C) 2009 Free Software Foundation, Inc.

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

(define-module (language brainfuck compile-scheme)
  #:export (compile-scheme))

(define tape-size 30000)

(define (compile-scheme exp env opts)
  (values
    `(let ((pointer 0)
           (tape (make-vector ,tape-size 0)))
       ,@(if (not (eq? '<brainfuck> (car exp)))
           (error "expected brainfuck program")
           `(begin
              ,@(compile-body (cdr exp))
              (write-char #\newline))))
    env
    env))

(define (compile-body instructions)
  (let iterate ((cur instructions)
                (result '()))
    (if (null? cur)
      (reverse result)
      (let ((compiled (compile-instruction (car cur))))
        (iterate (cdr cur) (cons compiled result))))))

(define (compile-instruction ins)
  (case (car ins)

    ((<bf-move>)
     (let ((dir (cadr ins)))
       `(set! pointer (+ pointer ,dir))))

    ((<bf-increment>)
     (let ((inc (cadr ins)))
       `(vector-set! tape pointer (+ (vector-ref tape pointer) ,inc))))

    ((<bf-print>)
     '(write-char (integer->char (vector-ref tape pointer))))

    ((<bf-read>)
     '(vector-set! tape pointer (char->integer (read-char))))

    ((<bf-loop>)
     `(let iter ()
        (if (not (= (vector-ref tape pointer) 0))
          (begin
            ,@(compile-body (cdr ins))
            (iter)))))

    (else (error "unknown brainfuck instruction " (car ins)))))
