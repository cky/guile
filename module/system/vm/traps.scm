;;; Traps: stepping, breakpoints, and such.

;; Copyright (C)  2010 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; Guile's debugging capabilities come from the hooks that its VM
;;; provides. For example, there is a hook that is fired when a function
;;; is called, and even a hook that gets fired at every retired
;;; instruction.
;;;
;;; But as the firing of these hooks is interleaved with the program
;;; execution, if we want to debug a program, we have to write an
;;; imperative program that mutates the state of these hooks, and to
;;; dispatch the hooks to a more semantic context.
;;;
;;; For example if we have placed a breakpoint at foo.scm:38, and
;;; determined that that location maps to the 18th instruction in
;;; procedure `bar', then we will need per-instruction hooks within
;;; `bar' -- but when running other procedures, we can have the
;;; per-instruction hooks off.
;;;
;;; Our approach is to define "traps". The behavior of a trap is
;;; specified when the trap is created. After creation, traps expose a
;;; limited, uniform interface: they are either on or off.
;;;
;;; To take our foo.scm:38 example again, we can define a trap that
;;; calls a function when control transfers to that source line --
;;; trap-at-source-location below. Calling the trap-at-source-location
;;; function adds to the VM hooks in such at way that it can do its job.
;;; The result of calling the function is a "disable-hook" closure that,
;;; when called, will turn off that trap.
;;;
;;; The result of calling the "disable-hook" closure, in turn, is an
;;; "enable-hook" closure, which when called turns the hook back on, and
;;; returns a "disable-hook" closure.
;;;
;;; It's a little confusing. The summary is, call these functions to add
;;; a trap; and call their return value to disable the trap.
;;;
;;; Code:

(define-module (system vm traps)
  #:use-module (system base pmatch)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:use-module (system vm objcode)
  #:use-module (system xref)
  #:use-module (rnrs bytevectors)
  #:export (trap-at-procedure-call
            trap-in-procedure
            trap-instructions-in-procedure
            trap-at-procedure-ip-in-range
            trap-at-source-location))

(define-syntax arg-check
  (syntax-rules ()
    ((_ arg predicate? message)
     (if (not (predicate? arg))
         (error "bad argument ~a: ~a" 'arg message)))
    ((_ arg predicate?)
     (if (not (predicate? arg))
         (error "bad argument ~a: expected ~a" 'arg 'predicate?)))))

(define (new-disabled-trap vm enable disable)
  (let ((enabled? #f))
    (define-syntax disabled?
      (identifier-syntax
       (disabled? (not enabled?))
       ((set! disabled? val) (set! enabled? (not val)))))
    
    (define* (enable-trap #:optional frame)
      (if enabled? (error "trap already enabled"))
      (enable frame)
      (set-vm-trace-level! vm (1+ (vm-trace-level vm)))
      (set! enabled? #t)
      disable-trap)
    
    (define* (disable-trap #:optional frame)
      (if disabled? (error "trap already disabled"))
      (disable frame)
      (set-vm-trace-level! vm (1- (vm-trace-level vm)))
      (set! disabled? #t)
      enable-trap)

    enable-trap))

(define (new-enabled-trap vm frame enable disable)
  ((new-disabled-trap vm enable disable) frame))

;; A basic trap, fires when a procedure is called.
;;
(define* (trap-at-procedure-call proc handler #:key (vm (the-vm)))
  (arg-check proc procedure?)
  (arg-check handler procedure?)
  (let ()
    (define (apply-hook frame)
      (if (eq? (frame-procedure frame) proc)
          (handler frame)))

    (new-enabled-trap
     vm #f
     (lambda (frame)
       (add-hook! (vm-apply-hook vm) apply-hook))
     (lambda (frame)
       (remove-hook! (vm-apply-hook vm) apply-hook)))))

;; A more complicated trap, traps when control enters a procedure.
;;
;; Control can enter a procedure via:
;;  * A procedure call.
;;  * A return to a procedure's frame on the stack.
;;  * A continuation returning directly to an application of this
;;    procedure.
;;
;; Control can leave a procedure via:
;;  * A normal return from the procedure.
;;  * An application of another procedure.
;;  * An invocation of a continuation.
;;  * An abort.
;;
(define* (trap-in-procedure proc enter-handler exit-handler
                            #:key current-frame (vm (the-vm)))
  (arg-check proc procedure?)
  (arg-check enter-handler procedure?)
  (arg-check exit-handler procedure?)
  (let ((in-proc? #f))
    (define (enter-proc frame)
      (if in-proc?
          (warn "already in proc" frame)
          (begin
            (enter-handler frame)
            (set! in-proc? #t))))

    (define (exit-proc frame)
      (if in-proc?
          (begin
            (exit-handler frame)
            (set! in-proc? #f))
          (warn "not in proc" frame)))
    
    (define (apply-hook frame)
      (if in-proc?
          (exit-proc frame))
      (if (eq? (frame-procedure frame) proc)
          (enter-proc frame)))

    (define (push-cont-hook frame)
      (if in-proc?
          (exit-proc frame)))
    
    (define (pop-cont-hook frame)
      (if in-proc?
          (exit-proc frame))
      (if (eq? (frame-procedure (frame-previous frame)) proc)
          (enter-proc frame)))

    (define (abort-hook frame)
      (if in-proc?
          (exit-proc frame))
      (if (eq? (frame-procedure frame) proc)
          (enter-proc frame)))

    (define (restore-hook frame)
      (if in-proc?
          (exit-proc frame))
      (if (eq? (frame-procedure frame) proc)
          (enter-proc frame)))

    (new-enabled-trap
     vm current-frame
     (lambda (frame)
       (add-hook! (vm-apply-hook vm) apply-hook)
       (add-hook! (vm-push-continuation-hook vm) push-cont-hook)
       (add-hook! (vm-pop-continuation-hook vm) pop-cont-hook)
       (add-hook! (vm-abort-continuation-hook vm) abort-hook)
       (add-hook! (vm-restore-continuation-hook vm) restore-hook)
       (if (and frame (eq? (frame-procedure frame) proc))
           (enter-proc frame)))
     (lambda (frame)
       (if in-proc?
           (exit-proc frame))
       (remove-hook! (vm-apply-hook vm) apply-hook)
       (remove-hook! (vm-push-continuation-hook vm) push-cont-hook)
       (remove-hook! (vm-pop-continuation-hook vm) pop-cont-hook)
       (remove-hook! (vm-abort-continuation-hook vm) abort-hook)
       (remove-hook! (vm-restore-continuation-hook vm) restore-hook)))))

;; Building on trap-in-procedure, we have trap-instructions-in-procedure
;;
(define* (trap-instructions-in-procedure proc next-handler exit-handler
                                         #:key current-frame (vm (the-vm)))
  (arg-check proc procedure?)
  (arg-check next-handler procedure?)
  (arg-check exit-handler procedure?)
  (let ()
    (define (next-hook frame)
      (if (eq? (frame-procedure frame) proc)
          (next-handler frame)))
    
    (define (enter frame)
      (add-hook! (vm-next-hook vm) next-hook)
      (if frame (next-hook frame)))

    (define (exit frame)
      (exit-handler frame)
      (remove-hook! (vm-next-hook vm) next-hook))

    (trap-in-procedure proc enter exit
                       #:current-frame current-frame #:vm vm)))

(define (non-negative-integer? x)
  (and (number? x) (integer? x) (exact? x) (not (negative? x))))

(define (range? x)
  (and (list? x)
       (and-map (lambda (x)
                  (and (pair? x)
                       (non-negative-integer? (car x))
                       (non-negative-integer? (cdr x))))
                x)))

(define (in-range? range i)
  (or-map (lambda (bounds)
            (<= (car bounds) i (cdr bounds)))
          range))

;; Building on trap-instructions-in-procedure, we have
;; trap-instructions-in-procedure.
;;
(define* (trap-at-procedure-ip-in-range proc range handler
                                        #:key current-frame (vm (the-vm)))
  (arg-check proc procedure?)
  (arg-check range range?)
  (arg-check handler procedure?)
  (let ((was-in-range? #f))
    (define (next-handler frame)
      (let ((now-in-range? (in-range? range (frame-instruction-pointer frame))))
        (cond
         (was-in-range? (set! was-in-range? now-in-range?))
         (now-in-range? (handler frame) (set! was-in-range? #t)))))
    
    (define (exit-handler frame)
      (set! was-in-range? #f))
    
    (trap-instructions-in-procedure proc next-handler exit-handler
                                    #:current-frame current-frame #:vm vm)))

;; FIXME: define this in objcode somehow. We are reffing the first
;; uint32 in the objcode, which is the length of the program (without
;; the meta).
(define (program-last-ip prog)
  (bytevector-u32-native-ref (objcode->bytecode (program-objcode prog)) 0))

(define (program-sources-by-line proc file)
  (let lp ((sources (program-sources proc))
           (out '()))
    (if (pair? sources)
        (lp (cdr sources)
            (pmatch (car sources)
              ((,start-ip ,start-file ,start-line . ,start-col)
               (if (equal? start-file file)
                   (cons (cons start-line
                               (if (pair? (cdr sources))
                                   (pmatch (cadr sources)
                                     ((,end-ip . _)
                                      (cons start-ip end-ip))
                                     (else (error "unexpected")))
                                   (cons start-ip (program-last-ip proc))))
                         out)
                   out))
              (else (error "unexpected"))))
        (let ((alist '()))
          (for-each
           (lambda (pair)
             (set! alist
                   (assv-set! alist (car pair)
                              (cons (cdr pair)
                                    (or (assv-ref alist (car pair))
                                        '())))))
           out)
          (sort! alist (lambda (x y) (< (car x) (car y))))
          alist))))

(define (source->ip-range proc file line)
  (or (or-map (lambda (line-and-ranges)
                (cond
                 ((= (car line-and-ranges) line)
                  (cdr line-and-ranges))
                 ((> (car line-and-ranges) line)
                  (warn "no instructions found at" file ":" line
                        "; using line" (car line-and-ranges) "instead")
                  (cdr line-and-ranges))
                 (else #f)))
              (program-sources-by-line proc file))
      (begin
        (warn "no instructions found for" file ":" line)
        '())))

;; Building on trap-on-instructions-in-procedure, we have
;; trap-at-source-location.
;;
(define* (trap-at-source-location file line handler
                                  #:key current-frame (vm (the-vm)))
  (arg-check file string?)
  (arg-check line non-negative-integer?)
  (arg-check handler procedure?)
  (let ((traps #f))
    (new-enabled-trap
     vm current-frame
     (lambda (frame)
       (set! traps (map
                    (lambda (proc)
                      (let ((range (source->ip-range proc file line)))
                        (trap-at-procedure-ip-in-range proc range handler
                                                       #:current-frame current-frame
                                                       #:vm vm)))
                    (source-procedures file line)))
       (if (null? traps)
           (error "No procedures found at ~a:~a." file line)))
     (lambda (frame)
       (for-each (lambda (trap) (trap frame)) traps)
       (set! traps #f)))))
