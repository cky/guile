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
  :use-module (system base pmatch)
  :use-module (system base compile)
  :use-module (system base language)
  :use-module (system repl common)
  :use-module (system repl command)
  :use-module (system vm vm)
  :use-module (system vm debug)
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
        (lread (language-reader (repl-language repl))))
    (with-fluid* current-reader (meta-reader lread)
      (lambda () (repl-reader (lambda () (repl-prompt repl)))))))

(define (default-pre-unwind-handler key . args)
  (save-stack default-pre-unwind-handler)
  (vm-save-stack (the-vm))
  (apply throw key args))

(define (default-catch-handler . args)
  (pmatch args
    ((quit . _)
     (apply throw args))
    ((vm-error ,fun ,msg ,args)
     (vm-backtrace (the-vm))
     (display "\nVM error: \n")
     (apply format #t msg args)
     (newline))
    ((,key ,subr ,msg ,args . ,rest)
     (vm-backtrace (the-vm))
     (newline)
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
       (apply display-error (fluid-ref the-last-stack) cep subr msg args rest)
       (run-hook after-error-hook)
       (set! stack-saved? #f)
       (force-output cep)))
    (else
     (apply bad-throw args))))

(define (call-with-backtrace thunk)
  (catch #t
         thunk
         default-catch-handler
         default-pre-unwind-handler))

(eval-case
 ((compile-toplevel)
  (define-macro (start-stack tag expr)
    expr)))

(define (start-repl lang)
  (let ((repl (make-repl lang)))
    (repl-welcome repl)
    (let prompt-loop ()
      (let ((exp (prompting-meta-read repl)))
        (cond
         ((eq? exp meta-command-token)
          (call-with-backtrace
           (lambda ()
             (meta-command repl (read-line)))))
         ((eof-object? exp)
          (throw 'quit))
         (else
          (call-with-backtrace
           (lambda ()
             (call-with-values (lambda ()
                                 (run-hook before-eval-hook exp)
                                 (start-stack repl-eval
                                              (repl-eval repl exp)))
               (lambda l
                 (for-each (lambda (v)
                             (run-hook before-print-hook v)
                             (repl-print repl v))
                           l)))))))
        (next-char #f) ;; consume trailing whitespace
        (prompt-loop)))))

(define (next-char wait)
  (if (or wait (char-ready?))
      (let ((ch (peek-char)))
	(cond ((eof-object? ch) (throw 'quit))
	      ((char-whitespace? ch) (read-char) (next-char wait))
	      (else ch)))
      #f))
