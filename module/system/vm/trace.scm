;;; Guile VM tracer

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

(define-module (system vm trace)
  :use-syntax (system base syntax)
  :use-module (system vm core)
  :use-module (ice-9 format)
  :export (vm-trace vm-trace-on vm-trace-off))

(define (vm-trace vm objcode . opts)
  (dynamic-wind
      (lambda () (apply vm-trace-on vm opts))
      (lambda () (vm (objcode->program objcode)))
      (lambda () (apply vm-trace-off vm opts))))

(define (vm-trace-on vm . opts)
  (set-vm-option! vm 'trace-first #t)
  (if (memq :b opts) (add-hook! (vm-next-hook vm) trace-next))
  (set-vm-option! vm 'trace-options opts)
  (add-hook! (vm-apply-hook vm) trace-apply)
  (add-hook! (vm-return-hook vm) trace-return))

(define (vm-trace-off vm . opts)
  (if (memq :b opts) (remove-hook! (vm-next-hook vm) trace-next))
  (remove-hook! (vm-apply-hook vm) trace-apply)
  (remove-hook! (vm-return-hook vm) trace-return))

(define (trace-next vm)
  (let ((frame (vm-current-frame vm)))
    (format #t "0x~8X  ~20S" (vm:ip vm) (vm-fetch-code vm))
    (do ((opts (vm-option vm 'trace-options) (cdr opts)))
	((null? opts) (newline))
      (case (car opts)
	((:s) (format #t "~20S" (vm-fetch-stack vm)))
	((:v) (format #t "~20S" (frame-variables frame)))
	((:e) (format #t "~20A" (object->string (frame-external-link frame))))))))

(define (trace-apply vm)
  (if (vm-option vm 'trace-first)
      (set-vm-option! vm 'trace-first #f)	;; skip the initial program
      (let ((frame (vm-current-frame vm)))
	(print-prefix (frame-dynamic-link frame))
	(write (frame->call frame))
	(newline))))

(define (trace-return vm)
  (let ((frame (vm-current-frame vm)))
    (print-prefix (frame-dynamic-link frame))
    (write (car (vm-fetch-stack vm)))
    (newline)))

(define (print-prefix frame)
  (and-let* ((link (frame-dynamic-link frame)))
    (display "| ")
    (print-prefix link)))
