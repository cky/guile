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
  :use-module (system vm core)
  :use-module (system vm frame)
  :use-module (ice-9 format)
  :export (vm-trace))

(define (vm-trace vm prog . opts)
  (let ((flag (vm-option vm 'debug)))
    (dynamic-wind
	(lambda ()
	  (set-vm-option! vm 'debug #t)
	  (set-vm-option! vm 'first-apply #t)
	  (if (memq :a opts)
	      (add-hook! (vm-next-hook vm) trace-next))
	  (add-hook! (vm-apply-hook vm) trace-apply)
	  (add-hook! (vm-return-hook vm) trace-return))
	(lambda ()
	  (vm prog))
	(lambda ()
	  (set-vm-option! vm 'debug flag)
	  (if (memq :a opts)
	      (remove-hook! (vm-next-hook vm) trace-next))
	  (remove-hook! (vm-apply-hook vm) trace-apply)
	  (remove-hook! (vm-return-hook vm) trace-return)))))

(define (trace-next vm)
  (let ((frame (vm-current-frame vm)))
    (format #t "0x~X  ~20S~S\t~S\n"
	    (vm:ip vm)
	    (vm-fetch-code vm)
	    (frame-variables frame)
	    (vm-fetch-stack vm))))

(define (trace-apply vm)
  (if (vm-option vm 'first-apply)
      (set-vm-option! vm 'first-apply #f)	;; skip the initial program
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
