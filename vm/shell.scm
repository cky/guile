;;; shell.scm --- interactive VM operations

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; This file is part of Guile VM.

;; Guile VM is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; Guile VM is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with Guile VM; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (vm shell)
  :use-module (vm vm)
  :use-module (vm utils)
  :use-module (vm compile)
  :use-module (ice-9 format))

;;;
;;; VM Shell
;;;

(define *vm-default-prompt* "VM> ")

(define *vm-boot-message* "\
Copyright (C) 2000 Free Software Foundation, Inc.
Guile VM is free software, covered by the GNU General Public License,
and you are welcome to change it and/or distribute copies of it under
certain conditions.  There is absolutely no warranty for Guile VM.\n")

(define (vm-init vm)
  (vm-set-option! vm 'prompt *vm-default-prompt*)
  (vm-set-option! vm 'verbose #f)
  (vm-set-option! vm 'history-count 1))

(define-public (vm-boot vm)
  (format #t "Guile Virtual Machine ~A\n" (vm-version))
  (display *vm-boot-message*)
  (display "\nType \"help\" for information\n")
  (vm-shell vm))

(define-public (vm-shell vm)
  (vm-init vm)
  (let ((read-expr (lambda () (read (current-input-port)))))
    (let loop ()
      (display (or (vm-option vm 'prompt) *vm-default-prompt*))
      (let ((cmd (read-expr)))
	(if (not (eof-object? cmd))
	    (case cmd
	      ((eval) (vm-eval vm (read-expr)) (loop))
	      ((trace) (vm-trace vm (read-expr)) (loop))
	      ((parse) (vm-parse vm (read-expr)) (loop))
	      ((compile) (vm-compile vm (read-expr)) (loop))
	      ((set) (vm-set-option! vm (read-expr) (read-expr)) (loop))
	      (else
	       (error "Unknown command: ~S" cmd))))))))

(define-public (vm-repl vm)
  (vm-init vm)
  (let loop ()
    (display (or (vm-option vm 'prompt) *vm-default-prompt*))
    (let ((form (read (current-input-port))))
      (if (not (eof-object? form))
	  (begin
	    (vm-eval vm form)
	    (loop))))))

(define (vm-eval vm form)
  (let ((result (vm-run vm (compile form))))
    (if (not (eq? result *unspecified*))
	(let* ((n (or (vm-option vm 'history-count) 1))
	       (var (symbol-append "$" (number->string n))))
	  (intern-symbol #f var)
	  (symbol-set! #f var result)
	  (format #t "~A = ~S\n" var result)
	  (vm-set-option! vm 'history-count (1+ n))
	  result))))

(define (vm-parse vm form)
  (parse form (make-top-level-env)))

(define (vm-compile vm form)
  #f)


;;;
;;; Step
;;;

(define (vm-step-boot vm)
  (format #t "VM: Starting a program ~S:~%"
	  (frame-program (vm-current-frame vm))))

(define (vm-step-halt vm)
  (display "VM: Program terminated with the return value: ")
  (display (vm:ac vm))
  (newline))

(define (vm-step-next vm)
  (if (vm-option vm 'verbose)
      (let ((frame (vm-current-frame vm)))
	(display "--------------------------------------------------\n")
	(format #t "PC = 0x~X  SP = 0x~X  FP = 0x~X  AC = ~S~%"
		(vm:pc vm) (vm:sp vm) (vm:fp vm) (vm:ac vm))
	(do ((frame frame (frame-dynamic-link frame))
	     (frames '() (cons frame frames)))
	    ((not frame)
	     (for-each (lambda (frame)
			 (format #t "Frame = [~S 0x~X 0x~X]~%"
				 (frame-program frame)
				 (frame-stack-pointer frame)
				 (frame-return-address frame)))
		       frames)))
	(format #t "Local variables    = ~S~%" (frame-variables frame))
	(format #t "External variables = ~S~%" (frame-external-link frame))
	(format #t "Stack = ~S~%" (vm-stack->list vm))))
  (format #t "0x~X:" (vm:pc vm))
  (for-each (lambda (obj) (display " ") (write obj))
	    (vm-fetch-code vm (vm:pc vm)))
  (newline))

(define-public (vm-step vm form . opts)
  (let ((debug-flag (vm-option vm 'debug)))
    (dynamic-wind
	(lambda ()
	  (add-hook! (vm-boot-hook vm) vm-step-boot)
	  (add-hook! (vm-halt-hook vm) vm-step-halt)
	  (add-hook! (vm-next-hook vm) vm-step-next)
	  (vm-set-option! vm 'debug #t))
	(lambda ()
	  (if (pair? opts)
	      (vm-set-option! vm 'verbose #t))
	  (vm-run vm (compile form)))
	(lambda ()
	  (remove-hook! (vm-boot-hook vm) vm-step-boot)
	  (remove-hook! (vm-halt-hook vm) vm-step-halt)
	  (remove-hook! (vm-next-hook vm) vm-step-next)
	  (vm-set-option! vm 'debug debug-flag)))))


;;;
;;; Trace
;;;

(define (vm-trace-prefix frame)
  (and-let* ((link (frame-dynamic-link frame)))
    (display "| ")
    (vm-trace-prefix link)))

(define (vm-frame->call frame)
  (define (truncate! list n)
    (let loop ((list list) (n n))
      (if (<= n 1)
	  (set-cdr! list '())
	  (loop (cdr list) (1- n))))
    list)
  (let* ((prog (frame-program frame))
	 (name (or (program-name prog) prog)))
    (cons name (reverse! (vector->list (frame-variables frame))))))

(define (vm-trace-apply vm)
  (let ((frame (vm-current-frame vm)))
    (vm-trace-prefix frame)
    (display (vm-frame->call frame))
    (newline)))

(define (vm-trace-return vm)
  (vm-trace-prefix (vm-current-frame vm))
  (display (vm:ac vm))
  (newline))

(define-public (vm-trace vm form)
  (let ((debug-flag (vm-option vm 'debug)))
    (dynamic-wind
	(lambda ()
	  (add-hook! (vm-apply-hook vm) vm-trace-apply)
	  (add-hook! (vm-return-hook vm) vm-trace-return)
	  (vm-set-option! vm 'debug #t))
	(lambda ()
	  (vm-run vm (compile form)))
	(lambda ()
	  (remove-hook! (vm-apply-hook vm) vm-trace-apply)
	  (remove-hook! (vm-return-hook vm) vm-trace-return)
	  (vm-set-option! vm 'debug debug-flag)))))


;;;
;;; Disassemble
;;;

(define-public (disassemble program)
  (format #t "Program at ~X:" (program-base program))
  (let ((subprogs '())
	(list (vector->list (bytecode-decode (program-code program)))))
    (for-each (lambda (obj)
		(cond ((opcode? obj)
		       (newline)
		       (display obj))
		      ((program? obj)
		       (set! subprogs (cons subprogs obj))
		       (display " ")
		       (display obj))
		      (else
		       (display " ")
		       (display obj))))
	      list)
    (newline)
    (for-each disassemble (reverse! subprogs))))

;;; shell.scm ends here
