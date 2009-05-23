;;; Brainfuck for GNU Guile.

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

(define-module (language brainfuck parse)
  #:export (read-brainfuck))

(define (read-brainfuck p)
  `(<brainfuck> ,@(read-body p)))

(define (reverse-without-nops lst)
  (let iterate ((cur lst)
                (result '()))
    (if (null? cur)
      result
      (let ((head (car cur))
            (tail (cdr cur)))
        (if (eq? (car head) '<bf-nop>)
          (iterate tail result)
          (iterate tail (cons head result)))))))

(define (read-body p)
  (let iterate ((parsed '()))
    (let ((chr (read-char p)))
      (if (or (eof-object? chr) (eq? #\] chr))
        (reverse-without-nops parsed)
        (iterate (cons (process-input-char chr p) parsed))))))

(define (process-input-char chr p)
  (case chr
    ((#\>) '(<bf-move> 1))
    ((#\<) '(<bf-move> -1))
    ((#\+) '(<bf-increment> 1))
    ((#\-) '(<bf-increment> -1))
    ((#\.) '(<bf-print>))
    ((#\,) '(<bf-read>))
    ((#\[) `(<bf-loop> ,@(read-body p)))
    (else '(<bf-nop>))))
