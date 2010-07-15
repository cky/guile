;;; Error handling in the REPL

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

(define-module (system repl error-handling)
  #:use-module (system base pmatch)
  #:use-module (system repl debug)
  #:export (call-with-error-handling
            with-error-handling))




;;;
;;; Error handling via repl debugging
;;;

(define* (call-with-error-handling thunk #:key
                                   (on-error 'debug) (post-error 'catch)
                                   (pass-keys '(quit)))
  (let ((in (current-input-port))
        (out (current-output-port))
        (err (current-error-port)))
    (define (with-saved-ports thunk)
      (with-input-from-port in
        (lambda ()
          (with-output-to-port out
            (lambda ()
              (with-error-to-port err
                thunk))))))
    
    (catch #t
      (lambda () (%start-stack #t thunk))

      (case post-error
        ((catch)
         (lambda (key . args)
           (if (memq key pass-keys)
               (apply throw key args)
               (begin
                 (pmatch args
                  ((,subr ,msg ,args . ,rest)
                   (with-saved-ports
                    (lambda ()
                      (run-hook before-error-hook)
                      (display-error #f err subr msg args rest)
                      (run-hook after-error-hook)
                      (force-output err))))
                  (else
                   (format err "\nERROR: uncaught throw to `~a', args: ~a\n"
                           key args)))
                 (if #f #f)))))
        (else
         (if (procedure? post-error)
             post-error ; a handler proc
             (error "Unknown post-error strategy" post-error))))
    
      (case on-error
        ((debug)
         (lambda (key . args)
           (let* ((tag (and (pair? (fluid-ref %stacks))
                            (cdar (fluid-ref %stacks))))
                  (stack (narrow-stack->vector
                          (make-stack #t)
                          ;; Cut three frames from the top of the stack:
                          ;; make-stack, this one, and the throw handler.
                          3 
                          ;; Narrow the end of the stack to the most recent
                          ;; start-stack.
                          tag
                          ;; And one more frame, because %start-stack invoking
                          ;; the start-stack thunk has its own frame too.
                          0 (and tag 1)))
                  (debug (make-debug stack 0)))
             (with-saved-ports
              (lambda ()
                (pmatch args
                  ((,subr ,msg ,args . ,rest)
                   (display-error (vector-ref stack 0) (current-output-port)
                                  subr msg args rest))
                  (else
                   (format #t "Throw to key `~a' with args `~s'." key args)))
                (format #t "Entering a new prompt.  ")
                (format #t "Type `,bt' for a backtrace or `,q' to continue.\n")
                ((@ (system repl repl) start-repl) #:debug debug))))))
        ((pass)
         (lambda (key . args)
           ;; fall through to rethrow
           #t))
        (else
         (if (procedure? on-error)
             on-error ; pre-unwind handler
             (error "Unknown on-error strategy" on-error)))))))

(define-syntax with-error-handling
  (syntax-rules ()
    ((_ form)
     (call-with-error-handling (lambda () form)))))
