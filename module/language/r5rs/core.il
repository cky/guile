;;; R5RS core environment

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

;; Non standard procedures

(@define void (@lambda () (@void)))

;; 6. Standard procedures

;;; 6.1 Equivalence predicates

(@define eq? (@lambda (x y) (@eq? x y)))
(@define eqv? (@ Core::eqv?))
(@define equal? (@ Core::equal?))

;;; 6.2 Numbers

(@define number? (@ Core::number?))
(@define complex? (@ Core::complex?))
(@define real? (@ Core::real?))
(@define rational? (@ Core::rational?))
(@define integer? (@ Core::integer?))

(@define exact? (@ Core::exact?))
(@define inexact? (@ Core::inexact?))

(@define = (@ Core::=))
(@define < (@ Core::<))
(@define > (@ Core::>))
(@define <= (@ Core::<=))
(@define >= (@ Core::>=))

(@define zero? (@ Core::zero?))
(@define positive? (@ Core::positive?))
(@define negative? (@ Core::negative?))
(@define odd? (@ Core::odd?))
(@define even? (@ Core::even?))

(@define max (@ Core::max))
(@define min (@ Core::min))

(@define + (@ Core::+))
(@define * (@ Core::*))
(@define - (@ Core::-))
(@define / (@ Core::/))

(@define abs (@ Core::abs))

(@define quotient (@ Core::quotient))
(@define remainder (@ Core::remainder))
(@define modulo (@ Core::modulo))

(@define gcd (@ Core::gcd))
(@define lcm (@ Core::lcm))

;; (@define numerator (@ Core::numerator))
;; (@define denominator (@ Core::denominator))

(@define floor (@ Core::floor))
(@define ceiling (@ Core::ceiling))
(@define truncate (@ Core::truncate))
(@define round (@ Core::round))

;; (@define rationalize (@ Core::rationalize))

(@define exp (@ Core::exp))
(@define log (@ Core::log))
(@define sin (@ Core::sin))
(@define cos (@ Core::cos))
(@define tan (@ Core::tan))
(@define asin (@ Core::asin))
(@define acos (@ Core::acos))
(@define atan (@ Core::atan))

(@define sqrt (@ Core::sqrt))
(@define expt (@ Core::expt))

(@define make-rectangular (@ Core::make-rectangular))
(@define make-polar (@ Core::make-polar))
(@define real-part (@ Core::real-part))
(@define imag-part (@ Core::imag-part))
(@define magnitude (@ Core::magnitude))
(@define angle (@ Core::angle))

(@define exact->inexact (@ Core::exact->inexact))
(@define inexact->exact (@ Core::inexact->exact))

(@define number->string (@ Core::number->string))
(@define string->number (@ Core::string->number))

;;; 6.3 Other data types

;;;; 6.3.1 Booleans

(@define not (@lambda (x) (@not x)))
(@define boolean? (@ Core::boolean?))

;;;; 6.3.2 Pairs and lists

(@define pair? (@lambda (x) (@pair? x)))
(@define cons (@lambda (x y) (@cons x y)))

(@define car (@lambda (x) (@car x)))
(@define cdr (@lambda (x) (@cdr x)))
(@define set-car! (@ Core::set-car!))
(@define set-cdr! (@ Core::set-cdr!))

(@define caar (@lambda (x) (@caar x)))
(@define cadr (@lambda (x) (@cadr x)))
(@define cdar (@lambda (x) (@cdar x)))
(@define cddr (@lambda (x) (@cddr x)))
(@define caaar (@lambda (x) (@caaar x)))
(@define caadr (@lambda (x) (@caadr x)))
(@define cadar (@lambda (x) (@cadar x)))
(@define caddr (@lambda (x) (@caddr x)))
(@define cdaar (@lambda (x) (@cdaar x)))
(@define cdadr (@lambda (x) (@cdadr x)))
(@define cddar (@lambda (x) (@cddar x)))
(@define cdddr (@lambda (x) (@cdddr x)))
(@define caaaar (@lambda (x) (@caaaar x)))
(@define caaadr (@lambda (x) (@caaadr x)))
(@define caadar (@lambda (x) (@caadar x)))
(@define caaddr (@lambda (x) (@caaddr x)))
(@define cadaar (@lambda (x) (@cadaar x)))
(@define cadadr (@lambda (x) (@cadadr x)))
(@define caddar (@lambda (x) (@caddar x)))
(@define cadddr (@lambda (x) (@cadddr x)))
(@define cdaaar (@lambda (x) (@cdaaar x)))
(@define cdaadr (@lambda (x) (@cdaadr x)))
(@define cdadar (@lambda (x) (@cdadar x)))
(@define cdaddr (@lambda (x) (@cdaddr x)))
(@define cddaar (@lambda (x) (@cddaar x)))
(@define cddadr (@lambda (x) (@cddadr x)))
(@define cdddar (@lambda (x) (@cdddar x)))
(@define cddddr (@lambda (x) (@cddddr x)))

(@define null? (@lambda (x) (@null? x)))
(@define list? (@lambda (x) (@list? x)))

(@define list (@lambda x x))

(@define length (@ Core::length))
(@define append (@ Core::append))
(@define reverse (@ Core::reverse))
(@define list-tail (@ Core::list-tail))
(@define list-ref (@ Core::list-ref))

(@define memq (@ Core::memq))
(@define memv (@ Core::memv))
(@define member (@ Core::member))

(@define assq (@ Core::assq))
(@define assv (@ Core::assv))
(@define assoc (@ Core::assoc))

;;;; 6.3.3 Symbols

(@define symbol? (@ Core::symbol?))
(@define symbol->string (@ Core::symbol->string))
(@define string->symbol (@ Core::string->symbol))

;;;; 6.3.4 Characters

(@define char? (@ Core::char?))
(@define char=? (@ Core::char=?))
(@define char<? (@ Core::char<?))
(@define char>? (@ Core::char>?))
(@define char<=? (@ Core::char<=?))
(@define char>=? (@ Core::char>=?))
(@define char-ci=? (@ Core::char-ci=?))
(@define char-ci<? (@ Core::char-ci<?))
(@define char-ci>? (@ Core::char-ci>?))
(@define char-ci<=? (@ Core::char-ci<=?))
(@define char-ci>=? (@ Core::char-ci>=?))
(@define char-alphabetic? (@ Core::char-alphabetic?))
(@define char-numeric? (@ Core::char-numeric?))
(@define char-whitespace? (@ Core::char-whitespace?))
(@define char-upper-case? (@ Core::char-upper-case?))
(@define char-lower-case? (@ Core::char-lower-case?))
(@define char->integer (@ Core::char->integer))
(@define integer->char (@ Core::integer->char))
(@define char-upcase (@ Core::char-upcase))
(@define char-downcase (@ Core::char-downcase))

;;;; 6.3.5 Strings

(@define string? (@ Core::string?))
(@define make-string (@ Core::make-string))
(@define string (@ Core::string))
(@define string-length (@ Core::string-length))
(@define string-ref (@ Core::string-ref))
(@define string-set! (@ Core::string-set!))

(@define string=? (@ Core::string=?))
(@define string-ci=? (@ Core::string-ci=?))
(@define string<? (@ Core::string<?))
(@define string>? (@ Core::string>?))
(@define string<=? (@ Core::string<=?))
(@define string>=? (@ Core::string>=?))
(@define string-ci<? (@ Core::string-ci<?))
(@define string-ci>? (@ Core::string-ci>?))
(@define string-ci<=? (@ Core::string-ci<=?))
(@define string-ci>=? (@ Core::string-ci>=?))

(@define substring (@ Core::substring))
(@define string-append (@ Core::string-append))
(@define string->list (@ Core::string->list))
(@define list->string (@ Core::list->string))
(@define string-copy (@ Core::string-copy))
(@define string-fill! (@ Core::string-fill!))

;;;; 6.3.6 Vectors

(@define vector? (@ Core::vector?))
(@define make-vector (@ Core::make-vector))
(@define vector (@ Core::vector))
(@define vector-length (@ Core::vector-length))
(@define vector-ref (@ Core::vector-ref))
(@define vector-set! (@ Core::vector-set!))
(@define vector->list (@ Core::vector->list))
(@define list->vector (@ Core::list->vector))
(@define vector-fill! (@ Core::vector-fill!))

;;; 6.4 Control features

(@define procedure? (@ Core::procedure?))
(@define apply (@ Core::apply))
(@define map (@ Core::map))
(@define for-each (@ Core::for-each))
(@define force (@ Core::force))

(@define call-with-current-continuation (@ Core::call-with-current-continuation))
(@define values (@ Core::values))
(@define call-with-values (@ Core::call-with-values))
(@define dynamic-wind (@ Core::dynamic-wind))

;;; 6.5 Eval

(@define eval
  (@let ((l (@ Language::r5rs::spec::r5rs)))
    (@lambda (x e)
      (((@ System::Base::language::compile-in) x e l)))))

;; (@define scheme-report-environment
;;   (@lambda (version)
;;     (@if (@= version 5)
;; 	 (@ Language::R5RS::Core)
;; 	 (@error "Unsupported environment version" version))))
;; 
;; (@define null-environment
;;   (@lambda (version)
;;     (@if (@= version 5)
;; 	 (@ Language::R5RS::Null)
;; 	 (@error "Unsupported environment version" version))))

(@define interaction-environment (@lambda () (@current-module)))

;;; 6.6 Input and output

;;;; 6.6.1 Ports

(@define call-with-input-file (@ Core::call-with-input-file))
(@define call-with-output-file (@ Core::call-with-output-file))

(@define input-port? (@ Core::input-port?))
(@define output-port? (@ Core::output-port?))
(@define current-input-port (@ Core::current-input-port))
(@define current-output-port (@ Core::current-output-port))

(@define with-input-from-file (@ Core::with-input-from-file))
(@define with-output-to-file (@ Core::with-output-to-file))

(@define open-input-file (@ Core::open-input-file))
(@define open-output-file (@ Core::open-output-file))
(@define close-input-port (@ Core::close-input-port))
(@define close-output-port (@ Core::close-output-port))

;;;; 6.6.2 Input

(@define read (@ Core::read))
(@define read-char (@ Core::read-char))
(@define peek-char (@ Core::peek-char))
(@define eof-object? (@ Core::eof-object?))
(@define char-ready? (@ Core::char-ready?))

;;;; 6.6.3 Output

(@define write (@ Core::write))
(@define display (@ Core::display))
(@define newline (@ Core::newline))
(@define write-char (@ Core::write-char))

;;;; 6.6.4 System interface

(@define load
  (@lambda (file)
    (call-with-input-file file
      (@lambda (port)
	(@let ((loop (@lambda (x)
		       (@if (@not (eof-object? x))
			    (@begin
			      (eval x (interaction-environment))
			      (loop (read port)))))))
	  (loop (read port)))))))

;; transcript-on
;; transcript-off
