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
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (system repl common)
  #:use-module (system repl command)
  #:use-module (system vm vm)
  #:use-module (system vm debug)
  #:use-module (ice-9 rdelim)
  #:export (start-repl call-with-backtrace))

(define meta-command-token (cons 'meta 'command))

(define (meta-reader read)
  (lambda read-args
    (with-input-from-port
        (if (pair? read-args) (car read-args) (current-input-port))
      (lambda ()
        (let ((ch (next-char #t)))
          (cond ((eof-object? ch)
                 ;; apparently sometimes even if this is eof, read will
                 ;; wait on somethingorother. strange.
                 ch)
                ((eqv? ch #\,)
                 (read-char)
                 meta-command-token)
                (else (read))))))))
        
;; repl-reader is a function defined in boot-9.scm, and is replaced by
;; something else if readline has been activated. much of this hoopla is
;; to be able to re-use the existing readline machinery.
(define (prompting-meta-read repl)
  (let ((prompt (lambda () (repl-prompt repl)))
        (lread (language-reader (repl-language repl))))
    (with-fluid* current-reader (meta-reader lread)
      (lambda () (repl-reader (lambda () (repl-prompt repl)))))))

(define (default-catch-handler . args)
  (pmatch args
    ((quit . _)
     (apply throw args))
    ((,key ,subr ,msg ,args . ,rest)
     (let ((cep (current-error-port)))
       (cond ((not (stack? (fluid-ref the-last-stack))))
             ((memq 'backtrace (debug-options-interface))
              (let ((highlights (if (or (eq? key 'wrong-type-arg)
                                        (eq? key 'out-of-range))
                                    (car rest)
                                    '())))
                (run-hook before-backtrace-hook)
                (newline cep)
                (display "Backtrace:\n")
                (display-backtrace (fluid-ref the-last-stack) cep
                                   #f #f highlights)
                (newline cep)
                (run-hook after-backtrace-hook))))
       (run-hook before-error-hook)
       (display-error (fluid-ref the-last-stack) cep subr msg args rest)
       (run-hook after-error-hook)
       (set! stack-saved? #f)
       (force-output cep)))
    (else
     (format (current-error-port) "\nERROR: uncaught throw to `~a', args: ~a\n"
             (car args) (cdr args)))))

(define (call-with-backtrace thunk)
  (catch #t
         (lambda () (%start-stack #t thunk))
         default-catch-handler
         default-pre-unwind-handler))

(define-macro (with-backtrace form)
  `(call-with-backtrace (lambda () ,form)))

(define (start-repl lang)
  (let ((repl (make-repl lang))
        (status #f))
    (repl-welcome repl)
    (let prompt-loop ()
      (let ((exp (with-backtrace (prompting-meta-read repl))))
        (cond
         ((eqv? exp (if #f #f))) ; read error, pass
         ((eq? exp meta-command-token)
          (with-backtrace (meta-command repl (read-line))))
         ((eof-object? exp)
          (newline)
          (set! status '()))
         (else
          (with-backtrace
           (catch 'quit
                  (lambda ()
                    (call-with-values
                        (lambda ()
                          (run-hook before-eval-hook exp)
                          (start-stack #t
                                       (repl-eval repl (repl-parse repl exp))))
                      (lambda l
                        (for-each (lambda (v)
                                    (run-hook before-print-hook v)
                                    (repl-print repl v))
                                  l))))
                  (lambda (k . args)
                    (set! status args))))))
        (or status
            (begin
              (next-char #f) ;; consume trailing whitespace
              (prompt-loop)))))))

(define (next-char wait)
  (if (or wait (char-ready?))
      (let ((ch (peek-char)))
	(cond ((eof-object? ch) ch)
	      ((char-whitespace? ch) (read-char) (next-char wait))
	      (else ch)))
      #f))
