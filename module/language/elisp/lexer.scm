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

(define-module (language elisp lexer)
  #:use-module (ice-9 regex)
  #:export (get-lexer))

; This is the lexical analyzer for the elisp reader.  It is hand-written
; instead of using some generator because I think that's most viable in this
; case and easy enough.


; Read a symbol or number from a port until something follows that marks the
; start of a new token (like whitespace or parentheses).  The data read is
; returned as a string for further conversion to the correct type, but we also
; return what this is (integer/float/symbol).
; If any escaped character is found, it must be a symbol.  Otherwise we
; at the end check the result-string against regular expressions to determine
; if it is possibly an integer or a float.

(define integer-regex (make-regexp "^[+-]?[0-9]+\\.?$"))
(define float-regex
  (make-regexp "^[+-]?([0-9]+\\.?[0-9]*|[0-9]*\\.?[0-9]+)(e[+-]?[0-9]+)?$"))

; A dot is also allowed literally, only a single dort alone is parsed as the
; 'dot' terminal for dotted lists.
(define no-escape-punctuation (string->char-set "-+=*/_~!@$%^&:<>{}?."))

(define (get-symbol-or-number port)
  (let iterate ((result-chars '())
                (had-escape #f))
    (let* ((c (read-char port))
           (finish (lambda ()
                     (let ((result (list->string (reverse result-chars))))
                       (values
                         (cond
                           ((and (not had-escape)
                                 (regexp-exec integer-regex result))
                            'integer)
                           ((and (not had-escape)
                                 (regexp-exec float-regex result))
                            'float)
                           (else 'symbol))
                         result))))
           (need-no-escape? (lambda (c)
                              (or (char-numeric? c)
                                  (char-alphabetic? c)
                                  (char-set-contains? no-escape-punctuation
                                                      c)))))
      (cond
        ((eof-object? c) (finish))
        ((need-no-escape? c) (iterate (cons c result-chars) had-escape))
        ((char=? c #\\) (iterate (cons (read-char port) result-chars) #t))
        (else
          (unread-char c port)
          (finish))))))
  

; Main lexer routine, which is given a port and does look for the next token.

(define (lex port)
  (let ((return (let ((file (if (file-port? port) (port-filename port) #f))
                      (line (1+ (port-line port)))
                      (column (1+ (port-column port))))
                  (lambda (token value)
                    (let ((obj (cons token value)))
                      (set-source-property! obj 'filename file)
                      (set-source-property! obj 'line line)
                      (set-source-property! obj 'column column)
                      obj))))
        ; Read afterwards so the source-properties are correct above
        ; and actually point to the very character to be read.
        (c (read-char port)))
    (cond

      ; End of input must be specially marked to the parser.
      ((eof-object? c) '*eoi*)

      ; Whitespace, just skip it.
      ((char-whitespace? c) (lex port))

      ; The dot is only the one for dotted lists if followed by
      ; whitespace.  Otherwise it is considered part of a number of symbol.
      ((and (char=? c #\.)
            (char-whitespace? (peek-char port)))
       (return 'dot #f))


      ; Continue checking for literal character values.
      (else
        (case c

          ; A line comment, skip until end-of-line is found.
          ((#\;)
           (let iterate ()
             (let ((cur (read-char port)))
               (if (or (eof-object? cur) (char=? cur #\newline))
                 (lex port)
                 (iterate)))))

          ; Parentheses and other special-meaning single characters.
          ((#\() (return 'paren-open #f))
          ((#\)) (return 'paren-close #f))
          ((#\[) (return 'square-open #f))
          ((#\]) (return 'square-close #f))
          ((#\') (return 'quote #f))
          ((#\`) (return 'backquote #f))
          ((#\,) (return 'unquote #f))

          ; Remaining are numbers and symbols.  Process input until next
          ; whitespace is found, and see if it looks like a number
          ; (float/integer) or symbol and return accordingly.
          (else
            (unread-char c port)
            (call-with-values
              (lambda ()
                (get-symbol-or-number port))
              (lambda (type str)
                (case type
                  ((symbol) (return 'symbol (string->symbol str)))
                  ((integer)
                   ; In elisp, something like "1." is an integer, while
                   ; string->number returns an inexact real.  Thus we
                   ; need a conversion here, but it should always result in
                   ; an integer!
                   (return 'integer
                           (let ((num (inexact->exact (string->number str))))
                             (if (not (integer? num))
                               (error "Expected integer" str num))
                             num)))
                  ((float)
                   (return 'float (let ((num (string->number str)))
                                    (if (exact? num)
                                      (error "Expected inexact float" str num))
                                    num)))
                  (else (error "Wrong number/symbol type" type)))))))))))


; Build a lexer thunk for a port.  This is the exported routine which can be
; used to create a lexer for the parser to use.

(define (get-lexer port)
  (lambda ()
    (lex port)))
