;;;; srfi-14.scm --- SRFI-14 procedures for Guile
;;;;
;;;; 	Copyright (C) 2001 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 2, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA

(define-module (srfi srfi-14))

(export 
;;; General procedures
 char-set?
 char-set=
 char-set<=
 char-set-hash

;;; Iterating over character sets
 char-set-cursor
 char-set-ref
 char-set-cursor-next
 end-of-char-set?
 char-set-fold
 char-set-unfold char-set-unfold!
 char-set-for-each
 char-set-map

;;; Creating character sets
 char-set-copy
 char-set
 list->char-set list->char-set!
 string->char-set string-char-set!
 char-set-filter char-set-filter!
 ucs-range->char-set ucs-range->char-set!
 ->char-set

;;; Querying character sets
 char-set-size
 char-set-count
 char-set->list
 char-set->string
 char-set-contains?
 char-set-every
 char-set-any

;;; Character set algebra
 char-set-adjoin char-set-adjoin!
 char-set-delete char-set-delete!
 char-set-complement
 char-set-union
 char-set-intersection
 char-set-difference
 char-set-xor
 char-set-diff+intersection
 char-set-complement!
 char-set-union!
 char-set-intersection!
 char-set-difference!
 char-set-xor!
 char-set-diff+intersection!

;;; Standard character sets
 char-set:lower-case
 char-set:upper-case
 char-set:title-case
 char-set:letter
 char-set:digit
 char-set:letter+digit
 char-set:graphic
 char-set:printing
 char-set:whitespace
 char-set:iso-control
 char-set:punctuation
 char-set:symbol
 char-set:hex-digit
 char-set:blank
 char-set:ascii
 char-set:empty
 char-set:full
 )

(dynamic-call "scm_init_srfi_13_14" (dynamic-link "libguile-srfi-srfi-13-14"))

(define (->char-set x)
  (cond
   ((string? x)   (string->char-set x))
   ((char? x)     (char-set x))
   ((char-set? x) x)
   (else (error "invalid argument to `->char-set'"))))

(define char-set:full (ucs-range->char-set 0 256))

(define char-set:lower-case (char-set-filter char-lower-case? char-set:full))

(define char-set:upper-case (char-set-filter char-upper-case? char-set:full))

(define char-set:title-case (char-set))

(define char-set:letter (char-set-union char-set:lower-case
					char-set:upper-case))

(define char-set:digit (string->char-set "0123456789"))

(define char-set:letter+digit
  (char-set-union char-set:letter char-set:digit))

(define char-set:punctuation (string->char-set "!\"#%&'()*,-./:;?@[\\]_{}"))

(define char-set:symbol (string->char-set "$+<=>^`|~"))

(define char-set:whitespace (char-set #\space #\newline #\tab #\cr #\vt #\np))

(define char-set:blank (char-set #\space #\tab))

(define char-set:graphic
  (char-set-union char-set:letter+digit char-set:punctuation char-set:symbol))

(define char-set:printing
  (char-set-union char-set:graphic char-set:whitespace))

(define char-set:iso-control
  (char-set-adjoin
   (char-set-filter (lambda (ch) (< (char->integer ch) 31)) char-set:full)
   (integer->char 127)))

(define char-set:hex-digit (string->char-set "0123456789abcdefABCDEF"))

(define char-set:ascii
  (char-set-filter (lambda (ch) (< (char->integer ch) 128)) char-set:full))

(define char-set:empty (char-set))
