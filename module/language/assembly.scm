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
  #:export (byte-length))

(define (byte-length x)
  (pmatch x
    (,label (guard (not (pair? label)))
     0)
    ;; instructions take one byte, hence the 1+.
    ((load-integer ,str)
     (1+ (string-length str)))
    ((load-number ,str)
     (1+ (string-length str)))
    ((load-string ,str)
     (1+ (string-length str)))
    ((load-symbol ,str)
     (1+ (string-length str)))
    ((load-keyword ,str)
     (1+ (string-length str)))
    ((define ,str)
     (1+ (string-length str)))
    ((assembly ,nargs ,nrest ,nlocs ,nexts ,labels ,len . ,code)
     ;; lengths of nargs, nrest, nlocs, nexts, len, and code, respectively
     (+ 1 1 1 1 4 len))
    ((,inst . _) (guard (>= (instruction-length inst) 0))
     (1+ (instruction-length inst)))
    (else (error "unknown instruction" x))))
