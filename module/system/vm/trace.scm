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
  #:export (vm-trace))

(define* (vm-trace vm thunk #:key (calls? #t) (instructions? #f) (width 80))
  (define *call-depth* #f)
  (define *saved-call-depth* #f)

  (define (trace-enter frame)
    (cond
     (*call-depth*
      (set! *call-depth* (1+ *call-depth*)))))

  (define (trace-exit frame)
    (cond
     ((not *call-depth*))
     (else
      (set! *call-depth* (1- *call-depth*)))))
  
  (define (trace-apply frame)
    (cond
     (*call-depth*
      (format (current-error-port) "~a~v:@y\n"
              (make-string (1- *call-depth*) #\|)
              (max (- width *call-depth* 1) 1)
              (frame-call-representation frame)))
     ((eq? (frame-procedure frame) thunk)
      (set! *call-depth* 1))))

  (define (trace-return frame)
    ;; nop, though we could print the return i guess
    (cond
     ((and *call-depth* (< *call-depth* 0))
      ;; leaving the thunk
      (set! *call-depth* #f))
     (*call-depth*
      (let* ((len (frame-num-locals frame))
             (nvalues (frame-local-ref frame (1- len))))
        (cond
         ((= nvalues 1)
          (format (current-error-port) "~a~v:@y\n"
                  (make-string *call-depth* #\|)
                  width (frame-local-ref frame (- len 2))))
         (else
          ;; this should work, but there appears to be a bug
          ;; "~a~d values:~:{ ~v:@y~}\n"
          (format (current-error-port) "~a~d values:~{ ~a~}\n"
                  (make-string *call-depth* #\|)
                  nvalues
                  (let lp ((vals '()) (i 0))
                    (if (= i nvalues)
                        vals
                        (lp (cons (format #f "~v:@y" width
                                          (frame-local-ref frame (- len 2 i)))
                                  vals)
                            (1+ i)))))))))))
  
  (define (trace-next frame)
    (format #t "0x~8X" (frame-instruction-pointer frame))
    ;; should disassemble the thingy; could print stack, or stack trace,
    ;; ...
    )

  (define (vm-trace-on!)
    (if calls?
        (begin
          (add-hook! (vm-exit-hook vm) trace-exit)
          (add-hook! (vm-enter-hook vm) trace-enter)
          (add-hook! (vm-apply-hook vm) trace-apply)
          (add-hook! (vm-return-hook vm) trace-return)))
  
    (if instructions?
        (add-hook! (vm-next-hook vm) trace-next))

    ;; boot, halt, and break are the other ones

    (set-vm-trace-level! vm (1+ (vm-trace-level vm)))
    (set! *call-depth* *saved-call-depth*))
  
  (define (vm-trace-off!)
    (set! *saved-call-depth* *call-depth*)
    (set! *call-depth* #f)
    (set-vm-trace-level! vm (1- (vm-trace-level vm)))

    (if calls?
        (begin
          (remove-hook! (vm-exit-hook vm) trace-exit)
          (remove-hook! (vm-enter-hook vm) trace-enter)
          (remove-hook! (vm-apply-hook vm) trace-apply)
          (remove-hook! (vm-return-hook vm) trace-return)))
  
    (if instructions?
        (remove-hook! (vm-next-hook vm) trace-next)))

  (dynamic-wind
    vm-trace-on!
    (lambda () (vm-apply vm thunk '()))
    vm-trace-off!))
