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
  #:use-module (system vm program)
  #:use-module (system vm objcode)
  #:use-module (rnrs bytevectors)
  #:use-module (system vm instruction)
  #:use-module (ice-9 format)
  #:export (vm-trace))

;; FIXME: this constant needs to go in system vm objcode
(define *objcode-header-len* 8)

(define* (vm-trace vm thunk #:key (calls? #t) (instructions? #f) (width 80))
  (define *call-depth* #f)
  (define *saved-call-depth* #f)

  (define (print-application frame depth)
    (format (current-error-port) "~a~v:@y\n"
            (make-string depth #\|)
            (max (- width depth) 1)
            (frame-call-representation frame)))

  (define (print-return frame depth)
    (let* ((len (frame-num-locals frame))
           (nvalues (frame-local-ref frame (1- len))))
      (cond
       ((= nvalues 1)
        (format (current-error-port) "~a~v:@y\n"
                (make-string depth #\|)
                width (frame-local-ref frame (- len 2))))
       (else
        ;; this should work, but there appears to be a bug
        ;; "~a~d values:~:{ ~v:@y~}\n"
        (format (current-error-port) "~a~d values:~{ ~a~}\n"
                (make-string depth #\|)
                nvalues
                (let lp ((vals '()) (i 0))
                  (if (= i nvalues)
                      vals
                      (lp (cons (format #f "~v:@y" width
                                        (frame-local-ref frame (- len 2 i)))
                                vals)
                          (1+ i)))))))))

  (define (trace-push frame)
    (if *call-depth*
        (set! *call-depth* (1+ *call-depth*))))

  (define (trace-pop frame)
    (if *call-depth*
        (begin
          (print-return frame *call-depth*)
          (set! *call-depth*
                (if (zero? *call-depth*)
                    #f
                    (1- *call-depth*))))))
  
  (define (trace-apply frame)
    (cond
     (*call-depth*
      (print-application frame *call-depth*))
     ((eq? (frame-procedure frame) thunk)
      (set! *call-depth* 0))))

  (define (trace-next frame)
    (let* ((ip (frame-instruction-pointer frame))
           (objcode (program-objcode (frame-procedure frame)))
           (opcode (bytevector-u8-ref (objcode->bytecode objcode)
                                      (+ ip *objcode-header-len*)))
           (inst (opcode->instruction opcode)))
      (format #t "0x~8X: ~a: ~a\n" ip opcode inst)))
  
  (define (vm-trace-on!)
    (if calls?
        (begin
          (add-hook! (vm-push-continuation-hook vm) trace-push)
          (add-hook! (vm-pop-continuation-hook vm) trace-pop)
          (add-hook! (vm-apply-hook vm) trace-apply)))

    (if instructions?
        (add-hook! (vm-next-hook vm) trace-next))

    (set-vm-trace-level! vm (1+ (vm-trace-level vm)))
    (set! *call-depth* *saved-call-depth*))
  
  (define (vm-trace-off!)
    (set! *saved-call-depth* *call-depth*)
    (set! *call-depth* #f)
    (set-vm-trace-level! vm (1- (vm-trace-level vm)))

    (if calls?
        (begin
          (remove-hook! (vm-push-continuation-hook vm) trace-push)
          (remove-hook! (vm-pop-continuation-hook vm) trace-pop)
          (remove-hook! (vm-apply-hook vm) trace-apply)))
    
    (if instructions?
        (remove-hook! (vm-next-hook vm) trace-next)))

  (dynamic-wind
    vm-trace-on!
    (lambda () (vm-apply vm thunk '()))
    vm-trace-off!))
