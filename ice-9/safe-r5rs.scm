;;;; 	Copyright (C) 2000 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

;;;; Safe subset of R5RS bindings

(define-module (ice-9 safe-r5rs))

(define null-interface (resolve-interface '(ice-9 null)))

(module-use! %module-public-interface null-interface)

(export eqv? eq? equal?
	number?	complex? real? rational? integer?
	exact? inexact?
	= < > <= >=
	zero? positive?	negative? odd? even?
	max min
	+ * - /
	abs
	quotient remainder modulo
	gcd lcm
	numerator denominator
	floor ceiling truncate round
	rationalize
	exp log sin cos tan asin acos atan
	sqrt
	expt
	make-rectangular make-polar real-part imag-part magnitude angle
	exact->inexact inexact->exact

	number->string string->number

	boolean?
	not

	pair?
	cons car cdr
	set-car! set-cdr!
	caar cadr cdar cddr
	caaar caadr cadar caddr cdaar cdadr cddar cdddr
	caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	null?
	list?
	list
	length
	append
	reverse
	list-tail list-ref
	memq memv member
	assq assv assoc
	
	symbol?
	symbol->string string->symbol

	char?
	char=? char<? char>? char<=? char>=?
	char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	char-alphabetic? char-numeric? char-whitespace?
	char-upper-case? char-lower-case?
	char->integer integer->char
	char-upcase
	char-downcase

	string?
	make-string
	string
	string-length
	string-ref string-set!
	string=? string-ci=?
	string<? string>? string<=? string>=?
	string-ci<? string-ci>? string-ci<=? string-ci>=?
	substring
	string-length
	string-append
	string->list list->string
	string-copy string-fill!

	vector?
	make-vector
	vector
	vector-length
	vector-ref vector-set!
	vector->list list->vector
	vector-fill!

	procedure?
	apply
	map
	for-each
	force
	
	call-with-current-continuation

	values
	call-with-values
	dynamic-wind

	eval
	null-environment

	input-port? output-port?
	current-input-port current-output-port

	read
	read-char
	peek-char
	eof-object?
	char-ready?

	write
	display
	newline
	write-char

	;;transcript-on
	;;transcript-off
	)

(define (null-environment n)
  (if (not (= n 5))
      (scm-error 'misc-error 'null-environment
		 "~A is not a valid version"
		 (list n)
		 '()))
  ;; Note that we need to create a *fresh* interface
  (let ((interface (make-module 31)))
    (set-module-kind! interface 'interface)
    (module-use! interface null-interface)
    interface))
