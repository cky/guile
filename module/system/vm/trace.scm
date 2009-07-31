;;; Guile VM tracer

;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system vm trace)
  #:use-module (system base syntax)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (ice-9 format)
  #:export (vm-trace vm-trace-on vm-trace-off))

(define (vm-trace vm objcode . opts)
  (dynamic-wind
      (lambda () (apply vm-trace-on vm opts))
      (lambda () (vm-load vm objcode))
      (lambda () (apply vm-trace-off vm opts))))

(define (vm-trace-on vm . opts)
  (set-vm-option! vm 'trace-first #t)
  (if (memq #:b opts) (add-hook! (vm-next-hook vm) trace-next))
  (set-vm-option! vm 'trace-options opts)
  (add-hook! (vm-apply-hook vm) trace-apply)
  (add-hook! (vm-return-hook vm) trace-return))

(define (vm-trace-off vm . opts)
  (if (memq #:b opts) (remove-hook! (vm-next-hook vm) trace-next))
  (remove-hook! (vm-apply-hook vm) trace-apply)
  (remove-hook! (vm-return-hook vm) trace-return))

(define (trace-next vm)
  (define (puts x) (display #\tab) (write x))
  (define (truncate! x n)
    (if (> (length x) n)
      (list-cdr-set! x (1- n) '(...))) x)
  ;; main
  (format #t "0x~8X  ~16S" (vm:ip vm) (vm-fetch-code vm))
  (do ((opts (vm-option vm 'trace-options) (cdr opts)))
      ((null? opts) (newline))
    (case (car opts)
      ((:s) (puts (truncate! (vm-fetch-stack vm) 3)))
      ((:l) (puts (vm-fetch-locals vm))))))

(define (trace-apply vm)
  (if (vm-option vm 'trace-first)
    (set-vm-option! vm 'trace-first #f)
    (let ((chain (vm-current-frame-chain vm)))
      (print-indent chain)
      (print-frame-call (car chain))
      (newline))))

(define (trace-return vm)
  (let ((chain (vm-current-frame-chain vm)))
    (print-indent chain)
    (write (vm-return-value vm))
    (newline)))

(define (print-indent chain)
  (cond ((pair? (cdr chain))
	 (display "| ")
	 (print-indent (cdr chain)))))
