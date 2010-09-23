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
  #:use-module (system vm traps)
  #:use-module (rnrs bytevectors)
  #:use-module (system vm instruction)
  #:use-module (ice-9 format)
  #:export (trace-calls-in-procedure
            trace-calls-to-procedure
            trace-instructions-in-procedure
            vm-trace))

;; FIXME: this constant needs to go in system vm objcode
(define *objcode-header-len* 8)

(define (frame-return-values frame)
  (let* ((len (frame-num-locals frame))
         (nvalues (frame-local-ref frame (1- len))))
    (map (lambda (i)
           (frame-local-ref frame (+ (- len nvalues) i)))
         (iota nvalues))))
  
(define (print-application frame depth width prefix)
  (format (current-error-port) "~a~a~v:@y\n"
          prefix (make-string depth #\|)
          (max (- width depth) 1)
          (frame-call-representation frame)))

(define (print-return frame depth width prefix)
  (let* ((len (frame-num-locals frame))
         (nvalues (frame-local-ref frame (1- len))))
    (cond
     ((= nvalues 1)
      (format (current-error-port) "~a~a~v:@y\n"
              prefix (make-string depth #\|)
              width (frame-local-ref frame (- len 2))))
     (else
      ;; this should work, but there appears to be a bug
      ;; "~a~d values:~:{ ~v:@y~}\n"
      (format (current-error-port) "~a ~a~d values:~{ ~a~}\n"
              prefix (make-string depth #\|)
              nvalues
              (map (lambda (val)
                     (format #f "~v:@y" width val))
                   (frame-return-values frame)))))))
  
(define* (trace-calls-to-procedure proc #:key (width 80) (vm (the-vm))
                                   (prefix "trace: "))
  (define (apply-handler frame depth)
    (print-application frame depth width prefix))
  (define (return-handler frame depth)
    (print-return frame depth width prefix))
  (trap-calls-to-procedure proc apply-handler return-handler
                           #:vm vm))

(define* (trace-calls-in-procedure proc #:key (width 80) (vm (the-vm))
                                   (prefix "trace: "))
  (define (apply-handler frame depth)
    (print-application frame depth width prefix))
  (define (return-handler frame depth)
    (print-return frame depth width prefix))
  (trap-calls-in-dynamic-extent proc apply-handler return-handler
                                #:vm vm))

(define* (trace-instructions-in-procedure proc #:key (width 80) (vm (the-vm)))
  (define (trace-next frame)
    (let* ((ip (frame-instruction-pointer frame))
           (objcode (program-objcode (frame-procedure frame)))
           (opcode (bytevector-u8-ref (objcode->bytecode objcode)
                                      (+ ip *objcode-header-len*))))
      (format #t "~8d: ~a\n" ip (opcode->instruction opcode))))
  
  (trap-instructions-in-dynamic-extent proc trace-next
                                       #:vm vm))

;; Note that because this procedure manipulates the VM trace level
;; directly, it doesn't compose well with traps at the REPL.
;;
(define* (vm-trace vm thunk #:key (calls? #t) (instructions? #f) (width 80))
  (let ((call-trap #f)
        (inst-trap #f))
    (dynamic-wind
      (lambda ()
        (if calls?
            (set! call-trap
                  (trace-calls-in-procedure thunk #:vm vm #:width width)))
        (if instructions?
            (set! inst-trap
                  (trace-instructions-in-procedure thunk #:vm vm #:width width)))
        (set-vm-trace-level! vm (1+ (vm-trace-level vm))))
      thunk
      (lambda ()
        (set-vm-trace-level! vm (1- (vm-trace-level vm)))
        (if call-trap (call-trap))
        (if inst-trap (inst-trap))
        (set! call-trap #f)
        (set! inst-trap #f)))))
