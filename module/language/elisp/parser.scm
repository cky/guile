;;; Guile Emac Lisp

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

(define-module (language elisp parser)
  #:use-module (language elisp lexer)
  #:export (read-elisp))

; The parser (reader) for elisp expressions.
; Is is hand-written (just as the lexer is) instead of using some parser
; generator because this allows easier transfer of source properties from the
; lexer, makes the circular syntax parsing easier (as it would be with
; (text parse-lalr) and is easy enough anyways.


; Report a parse error.  The first argument is some current lexer token
; where source information is available should it be useful.

(define (parse-error token msg . args)
  (apply error msg args))


; We need peek-functionality for the next lexer token, this is done with some
; single token look-ahead storage.  This is handled by a closure which allows
; getting or peeking the next token.
; When one expression is fully parsed, we don't want a look-ahead stored here
; because it would miss from future parsing.  This is verified by the finish
; action.

(define (make-lexer-buffer lex)
  (let ((look-ahead #f))
    (lambda (action)
      (if (eq? action 'finish)
        (if look-ahead
          (error "lexer-buffer is not empty when finished")
          #f)
        (begin
          (if (not look-ahead)
            (set! look-ahead (lex)))
          (case action
            ((peek) look-ahead)
            ((get)
             (let ((result look-ahead))
               (set! look-ahead #f)
               result))
            (else (error "invalid lexer-buffer action" action))))))))


; Get the contents of a list, where the opening parentheses has already been
; found.  The same code is used for vectors and lists, where lists allow the
; dotted tail syntax and vectors not; additionally, the closing parenthesis
; must of course match.

(define (get-list lex allow-dot close-square)
  (let* ((next (lex 'peek))
         (type (car next)))
    (cond
      ((eq? type (if close-square 'square-close 'paren-close))
       (begin
         (if (not (eq? (car (lex 'get)) type))
           (error "got different token than peeked"))
         '()))
      ((and allow-dot (eq? type 'dot))
       (begin
         (if (not (eq? (car (lex 'get)) type))
           (error "got different token than peeked"))
         (let ((tail (get-list lex #f close-square)))
           (if (not (= (length tail) 1))
             (parse-error next "expected exactly one element after dot"))
           (car tail))))
      (else
        ; Do both parses in exactly this sequence!
        (let* ((head (get-expression lex))
               (tail (get-list lex allow-dot close-square)))
          (cons head tail))))))



; Parse a single expression from a lexer-buffer.  This is the main routine in
; our recursive-descent parser.

(define quotation-symbols '((quote . quote)
                            (backquote . \`)
                            (unquote . \,)
                            (unquote-splicing . \,@)))

(define (get-expression lex)
  (let* ((token (lex 'get))
         (type (car token))
         (return (lambda (result)
                   (if (pair? result)
                     (set-source-properties! result (source-properties token)))
                   result)))
    (case type
      ((integer float symbol character string)
       (return (cdr token)))
      ((quote backquote unquote unquote-splicing)
       (return (list (assq-ref quotation-symbols type) (get-expression lex))))
      ((paren-open)
       (return (get-list lex #t #f)))
      ((square-open)
       (return (list->vector (get-list lex #f #t))))
      (else
        (parse-error token "expected expression, got" token)))))


; Define the reader function based on this; build a lexer, a lexer-buffer,
; and then parse a single expression to return.

(define (read-elisp port)
  (let* ((lexer (get-lexer port))
         (lexbuf (make-lexer-buffer lexer))
         (result (get-expression lexbuf)))
    (lexbuf 'finish)
    result))
