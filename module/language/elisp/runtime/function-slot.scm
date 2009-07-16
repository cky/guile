;;; Guile Emac Lisp

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

(define-module (language elisp runtime function-slot)
  #:use-module (language elisp runtime))

; This module contains the function-slots of elisp symbols.  Elisp built-in
; functions are implemented as predefined function bindings here.


; Number predicates.

(built-in-func floatp (lambda (num)
                        (elisp-bool (and (real? num)
                                         (or (inexact? num)
                                             ((@ (guile) not)
                                               (integer? num)))))))

(built-in-func integerp (lambda (num)
                          (elisp-bool (and (exact? num)
                                           (integer? num)))))

(built-in-func numberp (lambda (num)
                         (elisp-bool (real? num))))

(built-in-func wholenump (lambda (num)
                           (elisp-bool (and (exact? num)
                                            (integer? num)
                                            ((@ (guile) >=) num 0)))))

(built-in-func zerop (lambda (num)
                       (elisp-bool ((@ (guile) =) num 0))))


; Number comparisons.

(built-in-func = (lambda (num1 num2)
                   (elisp-bool ((@ (guile) =) num1 num2))))
(built-in-func /= (lambda (num1 num2)
                    (elisp-bool ((@ (guile) not) ((@ (guile) =) num1 num2)))))

(built-in-func < (lambda (num1 num2)
                   (elisp-bool ((@ (guile) <) num1 num2))))
(built-in-func <= (lambda (num1 num2)
                    (elisp-bool ((@ (guile) <=) num1 num2))))
(built-in-func > (lambda (num1 num2)
                   (elisp-bool ((@ (guile) >) num1 num2))))
(built-in-func >= (lambda (num1 num2)
                    (elisp-bool ((@ (guile) >=) num1 num2))))

(built-in-func max (lambda (. nums)
                     ((@ (guile) apply) (@ (guile) max) nums)))
(built-in-func min (lambda (. nums)
                     ((@ (guile) apply) (@ (guile) min) nums)))

(built-in-func abs (lambda (num)
                     ((@ (guile) abs) num)))


; Number conversion.

(built-in-func float (lambda (num)
                       (if (exact? num)
                         (exact->inexact num)
                         num)))

; TODO: truncate, floor, ceiling, round.


; Arithmetic functions.

(built-in-func 1+ (@ (guile) 1+))
(built-in-func 1- (@ (guile) 1-))
(built-in-func + (@ (guile) +))
(built-in-func - (@ (guile) -))
(built-in-func * (@ (guile) *))
(built-in-func % (@ (guile) modulo))

; TODO: / with correct integer/real behaviour, mod (for floating-piont values).


; Floating-point rounding operations.

(built-in-func ffloor (@ (guile) floor))
(built-in-func fceiling (@ (guile) ceiling))
(built-in-func ftruncate (@ (guile) truncate))
(built-in-func fround (@ (guile) round))


; Miscellaneous.

(built-in-func not (lambda (x)
                     (if x nil-value t-value)))
