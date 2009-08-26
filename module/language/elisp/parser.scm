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

(define-module (language elisp parser)
  #:use-module (language elisp lexer)
  #:use-module (language ecmascript parse-lalr)
  #:export (read-elisp))

; The parser (reader) for elisp expressions.  It is implemented using the
; (text parse-lalr) parser generator and uses my hand-written lexer as
; the tokenizer.


; Build the parser itself using parse-lalr.

(define elisp-parser
  (lalr-parser (integer float symbol character string
                paren-open paren-close square-open square-close
                dot quote backquote unquote unquote-splicing)

    ; Expressions are our main interest.
    ; It seems the symbol we're interested for return from the parser must
    ; come very first, so here it is.
    (expression (integer) -> $1
                (float) -> $1
                (symbol) -> $1
                (character) -> $1
                (string) -> $1
                (list) -> $1
                (quotation) -> $1
                (vector) -> $1)

    ; Pairs, lists and dotted lists.
    (partial-list (expression) -> (list $1)
                  (expression dot expression) -> (cons $1 $3)
                  (expression partial-list) -> (cons $1 $2))
    (list (paren-open paren-close) -> '()
          (paren-open dot expression paren-close) -> $3
          (paren-open partial-list paren-close) -> $2)

    ; Quotation and unquotation expressions.
    (quotation (quote expression) -> `(quote ,$2)
               (backquote expression) -> `(\` ,$2)
               (unquote expression) -> `(\, ,$2)
               (unquote-splicing expression) -> `(\,@ ,$2))

    ; Vectors.
    (vector-elements (expression) -> (list $1)
                     (expression vector-elements) -> (cons $1 $2))
    (vector (square-open square-close) -> (make-vector 0)
            (square-open vector-elements square-close) -> (list->vector $2))))


; Use the parser to define the elisp reader function.
; We only want to read a single expression at a time, so use get-lexer/1.

(define (read-elisp port)
  (elisp-parser (get-lexer/1 port) error))
