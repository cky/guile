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
  :use-module (system repl common)
  :use-module (system repl command)
  :use-module (system vm frame)
  :use-module (ice-9 rdelim)
  :export (start-repl))

(define (start-repl lang)
  (let ((repl (make-repl lang)))
;;    (set-current-vmodule! repl.module)
;;    (set-current-evaluator! repl.vm)
    (repl-welcome repl)
    (let prompt-loop ()
      (repl-prompt repl)
      (call-with-error-handlers
       (lambda ()
	 (if (eq? (next-char #t) #\,)
	     ;; meta command
	     (begin (read-char)
		    (meta-command repl (read-line)))
	     ;; evaluation
	     (let rep-loop ()
	       (repl-print repl (repl-eval repl (repl-read repl)))
	       (if (next-char #f) (rep-loop))))))
      (prompt-loop))))

(define (next-char wait)
  (if (or wait (char-ready?))
      (let ((ch (peek-char)))
	(cond ((eof-object? ch) (throw 'quit))
	      ((char-whitespace? ch) (read-char) (next-char wait))
	      (else ch)))
      #f))

;;;
;;; Error handler
;;;

(define (call-with-error-handlers thunk)
  (catch 'vm-error
    (lambda () (catch 'user-error thunk error-handler))
    error-handler))

(define (error-handler key . args)
;;   (case key
;;     ((vm-error)
;;      (write (frame->call (cadddr args)))
;;      (newline)))
  (display "ERROR: ")
  (apply format #t (cadr args) (caddr args))
  (newline))
