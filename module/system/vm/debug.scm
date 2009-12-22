;;; Guile VM debugging facilities

;;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.
;;;
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

(define-module (system vm debug)
  #:use-module (system base syntax)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (ice-9 format)
  #:export (run-debugger debug-pre-unwind-handler))


;;;
;;; Debugger
;;;

(define-record <debugger> vm level breakpoints module)

(define (make-debugger-module)
  (let ((m (make-fresh-user-module)))
    m))

(define vm-debugger
  (let ((prop (make-object-property)))
    (lambda (vm)
      (or (prop vm)
          (let ((debugger (make-debugger vm (make-fluid) '() (make-debugger-module))))
            (set! (prop vm) debugger)
            debugger)))))

(define* (run-debugger frame #:optional (vm (the-vm)))
  (let* ((db (vm-debugger vm))
         (level (debugger-level db)))
    (with-fluids ((level (or (and=> (fluid-ref level) 1+) 0)))
      (debugger-repl db frame))))

(define (debugger-repl db frame)
  (let ((top frame))
    (define (frame-index frame)
      (let lp ((idx 0) (walk top))
        (if (= (frame-return-address frame) (frame-return-address walk))
            idx
            (lp (1+ idx) (frame-previous walk)))))
    (let loop ()
      (let ((index (frame-index frame))
            (level (fluid-ref (debugger-level db))))
        (let ((cmd (repl-reader
                    (lambda ()
                      (format #f "debug[~a@~a]> " level index))
                    read)))
          (if (not (or (eof-object? cmd)
                       (memq cmd '(q quit c continue))))
              (begin
                (case cmd
                  ((bt)
                   (display-backtrace (make-stack frame) (current-output-port)))
                  ((bindings)
                   (format #t "~a\n" (frame-bindings frame)))
                  ((frame f)
                   (format #t "~s\n" frame))
                  ((up)
                   (let ((prev (frame-previous frame)))
                     (if prev
                         (begin
                           (set! index (1+ index))
                           (set! frame prev)
                           (format #t "~s\n" frame))
                         (format #t "Already at outermost frame.\n"))))
                  ((down)
                   (if (zero? index)
                       (format #t "Already at innermost frame.\n")
                       (begin
                         (set! frame (let lp ((n (1- index)) (frame top))
                                       (if (zero? n)
                                           frame
                                           (lp (1- n) (frame-previous top)))))
                         (format #t "~s\n" frame))))
                  ((help ?)
                   (format #t "Type `c' to continue.\n"))
                  (else
                   (format #t "Unknown command: ~A\n" cmd)))
                (loop))))))))

;; things this debugger should do:
;;
;; eval expression in context of frame
;; up/down stack for inspecting
;; print procedure and args for frame
;; print local variables for frame
;; set local variable in frame
;; display backtrace
;; display full backtrace
;; step until next instruction
;; step until next function call/return
;; step until return from frame
;; step until different source line
;; step until greater source line
;; watch expression
;; break on a function
;; remove breakpoints
;; set printing width
;; display a truncated backtrace
;; go to a frame by index
;; (reuse gdb commands perhaps)
;; help
;; disassemble a function
;; disassemble the current function
;; inspect any object
;; hm, trace via reassigning global vars. tricksy.
;; (state associated with vm ?)

(define (debug-pre-unwind-handler key . args)
  ;; herald
  (format #t "Throw to key `~a' with args `~s'.
Entering the debugger. Type `bt' for a backtrace or `c' to continue.
This debugger implementation is temporary. See system/vm/debug.scm for
some ideas on how to make it better.\n" key args)
  (run-debugger (stack-ref (make-stack #t) 1))
  (save-stack 1)
  (apply throw key args))
