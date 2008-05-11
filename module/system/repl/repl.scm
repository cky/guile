;;; Read-Eval-Print Loop

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

(define-module (system repl repl)
  :use-syntax (system base syntax)
  :use-module (system base compile)
  :use-module (system base language)
  :use-module (system repl common)
  :use-module (system repl command)
  :use-module (system vm core)
  :use-module (ice-9 rdelim)
  :export (start-repl))

(define meta-command-token (cons 'meta 'command))

(define (meta-reader read)
  (lambda read-args
    (with-input-from-port
        (if (pair? read-args) (car read-args) (current-input-port))
      (lambda ()
        (if (eqv? (next-char #t) #\,)
            (begin (read-char) meta-command-token)
            (read))))))
        
;; repl-reader is a function defined in boot-9.scm, and is replaced by
;; something else if readline has been activated. much of this hoopla is
;; to be able to re-use the existing readline machinery.
(define (prompting-meta-read repl)
  (let ((prompt (lambda () (repl-prompt repl)))
        (lread (language-reader (cenv-language (repl-env repl)))))
    (with-fluid* current-reader (meta-reader lread)
      (lambda () (repl-reader (lambda () (repl-prompt repl)))))))

(define (start-repl lang)
  (let ((repl (make-repl lang)))
    (repl-welcome repl)
    (let prompt-loop ()
      (let ((exp (prompting-meta-read repl)))
        (cond
         ((eq? exp meta-command-token)
          (meta-command repl (read-line)))
         ((eof-object? exp)
          (throw 'quit))
         (else
          (catch 'vm-error
                 (lambda ()
                   (call-with-values (lambda ()
                                       (run-hook before-eval-hook exp)
                                       (repl-eval repl exp))
                     (lambda l
                       (for-each (lambda (v)
                                   (run-hook before-print-hook v)
                                   (repl-print repl v))
                                 l))))
                 (lambda (key fun msg args)
                   (display "ERROR: ")
                   (apply format #t msg args)
                   (newline)))))
        (next-char #f) ;; consume trailing whitespace
        (prompt-loop)))))

(define (next-char wait)
  (if (or wait (char-ready?))
      (let ((ch (peek-char)))
	(cond ((eof-object? ch) (throw 'quit))
	      ((char-whitespace? ch) (read-char) (next-char wait))
	      (else ch)))
      #f))
