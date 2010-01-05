;;; Guile VM tracer

;; Copyright (C) 2001, 2009, 2010 Free Software Foundation, Inc.

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
  #:export (vm-trace vm-trace-on! vm-trace-off!))

(define (vm-trace vm thunk . opts)
  (dynamic-wind
      (lambda () (apply vm-trace-on! vm opts))
      (lambda () (vm-apply vm thunk '()))
      (lambda () (apply vm-trace-off! vm opts))))

(define* (vm-trace-on! vm #:key (calls? #t) (instructions? #f))
  (if calls?
      (begin
        (add-hook! (vm-exit-hook vm) trace-exit)
        (add-hook! (vm-enter-hook vm) trace-enter)
        (add-hook! (vm-apply-hook vm) trace-apply)
        (add-hook! (vm-return-hook vm) trace-return)))
  
  (if instructions?
      (add-hook! (vm-next-hook vm) trace-next))

  ;; boot, halt, and break are the other ones

  (set-vm-trace-level! vm (1+ (vm-trace-level vm))))

(define* (vm-trace-off! vm #:key (calls? #t) (instructions? #f))
  (set-vm-trace-level! vm (1- (vm-trace-level vm)))

  (if calls?
      (begin
        (remove-hook! (vm-exit-hook vm) trace-exit)
        (remove-hook! (vm-enter-hook vm) trace-enter)
        (remove-hook! (vm-apply-hook vm) trace-apply)
        (remove-hook! (vm-return-hook vm) trace-return)))
  
  (if instructions?
      (remove-hook! (vm-next-hook vm) trace-next)))

(define (trace-next frame)
  (format #t "0x~8X" (frame-instruction-pointer frame))
  ;; should disassemble the thingy; could print stack, or stack trace,
  ;; ...
  )

(define *call-depth* 0)
(define *last-printed-call-depth* 0)

(define (trace-enter frame)
  (set! *call-depth* (1+ *call-depth*)))

(define (trace-exit frame)
  (set! *call-depth* (1- *call-depth*)))

(define (trace-apply frame)
  (if (< *call-depth* 0) (set! *call-depth* 0))
  (let ((last-depth *last-printed-call-depth*))
    (set! *last-printed-call-depth* *call-depth*)
    (format (current-error-port) "~a ~a~{ ~a~}\n"
            (make-string *call-depth* #\*)
            (let ((p (frame-procedure frame)))
              (or (procedure-name p) p))
            (frame-arguments frame))))

(define (trace-return frame)
  ;; nop, though we could print the return i guess
  #t)
