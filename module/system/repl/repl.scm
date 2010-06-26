;;; Read-Eval-Print Loop

;; Copyright (C) 2001, 2009, 2010 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

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
  #:export (start-repl))

(define meta-command-token (cons 'meta 'command))

(define (meta-reader read env)
  (lambda read-args
    (let ((port (if (pair? read-args) (car read-args) (current-input-port))))
      (with-input-from-port port
        (lambda ()
          (let ((ch (next-char #t)))
            (cond ((eof-object? ch)
                   ;; apparently sometimes even if this is eof, read will
                   ;; wait on somethingorother. strange.
                   ch)
                  ((eqv? ch #\,)
                   (read-char port)
                   meta-command-token)
                  (else (read port env)))))))))
        
;; repl-reader is a function defined in boot-9.scm, and is replaced by
;; something else if readline has been activated. much of this hoopla is
;; to be able to re-use the existing readline machinery.
;;
;; Catches read errors, returning *unspecified* in that case.
(define (prompting-meta-read repl)
  (call-with-error-handling
   (lambda ()
     (repl-reader (lambda () (repl-prompt repl))
                  (meta-reader (language-reader (repl-language repl))
                               (current-module))))
   #:on-error 'pass))



;;;
;;; The repl
;;;

(define* (start-repl #:optional (lang (current-language)))
  (let ((repl (make-repl lang))
        (status #f))
    (with-fluids ((*repl-stack* (cons repl
                                      (or (fluid-ref *repl-stack*) '())))
                  (*debug-input-port*
                   (or (fluid-ref *debug-input-port*) (current-input-port)))
                  (*debug-output-port*
                   (or (fluid-ref *debug-output-port*) (current-output-port))))
      (if (null? (cdr (fluid-ref *repl-stack*)))
          (repl-welcome repl))
      (let prompt-loop ()
        (let ((exp (prompting-meta-read repl)))
          (cond
           ((eqv? exp (if #f #f)))      ; read error, pass
           ((eq? exp meta-command-token)
            (with-error-handling (meta-command repl)))
           ((eof-object? exp)
            (newline)
            (set! status '()))
           (else
            ;; since the input port is line-buffered, consume up to the
            ;; newline
            (flush-to-newline)
            (with-error-handling
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
                (prompt-loop))))))))

(define (next-char wait)
  (if (or wait (char-ready?))
      (let ((ch (peek-char)))
	(cond ((eof-object? ch) ch)
	      ((char-whitespace? ch) (read-char) (next-char wait))
	      (else ch)))
      #f))

(define (flush-to-newline) 
  (if (char-ready?)
      (let ((ch (peek-char)))
        (if (and (not (eof-object? ch)) (char-whitespace? ch))
            (begin
              (read-char)
              (if (not (char=? ch #\newline))
                  (flush-to-newline)))))))
