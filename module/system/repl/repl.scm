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
  #:use-module (system vm vm)
  #:use-module (system repl error-handling)
  #:use-module (system repl common)
  #:use-module (system repl command)
  #:use-module (ice-9 control)
  #:export (start-repl run-repl))



;;;
;;; Meta commands
;;;

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
  (catch #t
    (lambda ()
      (repl-reader (lambda () (repl-prompt repl))
                   (meta-reader (language-reader (repl-language repl))
                                (current-module))))
    (lambda (key . args)
      (case key
        ((quit)
         (apply throw key args))
        (else
         (pmatch args
           ((,subr ,msg ,args . ,rest)
            (format #t "Throw to key `~a' while reading expression:\n" key)
            (display-error #f (current-output-port) subr msg args rest))
           (else
            (format #t "Throw to key `~a' with args `~s' while reading expression.\n"
                    key args)))
         (force-output)
         *unspecified*)))))



;;;
;;; The repl
;;;

(define* (start-repl #:optional (lang (current-language)) #:key debug)
  (run-repl (make-repl lang debug)))

;; (put 'abort-on-error 'scheme-indent-function 1)
(define-syntax abort-on-error
  (syntax-rules ()
    ((_ string exp)
     (catch #t
       (lambda () exp)
       (lambda (key . args)
         (format #t "While ~A:~%" string)
         (pmatch args
           ((,subr ,msg ,args . ,rest)
            (display-error #f (current-output-port) subr msg args rest))
           (else
            (format #t "ERROR: Throw to key `~a' with args `~s'.\n" key args)))
         (force-output)
         (abort))))))

(define (run-repl repl)
  (define (with-stack-and-prompt thunk)
    (call-with-prompt (default-prompt-tag)
                      (lambda () (start-stack #t (thunk)))
                      (lambda (k proc)
                        (with-stack-and-prompt (lambda () (proc k))))))
  
  (% (with-fluids ((*repl-stack*
                    (cons repl (or (fluid-ref *repl-stack*) '()))))
       (if (null? (cdr (fluid-ref *repl-stack*)))
           (repl-welcome repl))
       (let prompt-loop ()
         (let ((exp (prompting-meta-read repl)))
           (cond
            ((eqv? exp *unspecified*))  ; read error, pass
            ((eq? exp meta-command-token)
             (catch #t
               (lambda ()
                 (meta-command repl))
               (lambda (k . args)
                 (if (eq? k 'quit)
                     (abort args)
                     (begin
                       (format #t "While executing meta-command:~%" string)
                       (pmatch args
                         ((,subr ,msg ,args . ,rest)
                          (display-error #f (current-output-port) subr msg args rest))
                         (else
                          (format #t "ERROR: Throw to key `~a' with args `~s'.\n" k args)))
                       (force-output))))))
            ((eof-object? exp)
             (newline)
             (abort '()))
            (else
             ;; since the input port is line-buffered, consume up to the
             ;; newline
             (flush-to-newline)
             (call-with-error-handling
              (lambda ()
                (catch 'quit
                  (lambda ()
                    (call-with-values
                        (lambda ()
                          (% (let ((thunk
                                    (abort-on-error "compiling expression"
                                      (repl-prepare-eval-thunk
                                       repl
                                       (abort-on-error "parsing expression"
                                         (repl-parse repl exp))))))
                               (run-hook before-eval-hook exp)
                               (with-error-handling
                                 (with-stack-and-prompt thunk)))
                             (lambda (k) (values))))
                      (lambda l
                        (for-each (lambda (v)
                                    (repl-print repl v))
                                  l))))
                  (lambda (k . args)
                    (abort args))))
              #:trap-handler 'disabled)))
           (next-char #f) ;; consume trailing whitespace
           (prompt-loop))))
     (lambda (k status)
       status)))

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
