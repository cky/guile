;;; bytecomp.scm --- convert an intermediate code to an assemble code

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

(define-module (vm bytecomp)
  :use-module (vm vm)
  :use-module (vm utils)
  :use-module (vm types)
  :export (byte-compile))

(define (byte-compile nreqs restp code)
  (vector (byte-header nreqs restp (code-env code))
	  (byte-finalize (byte-optimize (byte-translate code)))))


;;;
;;; Bytecode header
;;;

(define (byte-header nreqs restp env)
  (list->vector (cons* nreqs restp (env-header env))))


;;;
;;; Bytecode translation
;;;

(define (byte-translate code)
  (let ((stack '()))
    ;; push opcode
    (define (push-code! . args)
      (set! stack (cons args stack)))
    (let trans ((code code) (use-stack #f) (tail #t))
      (let ((tag (code-tag code))
	    (env (code-env code))
	    (args (code-args code)))
	;;;
	;;; Utilities
	;;;
	;; push the result into the stack
	(define (trans-use-stack code) (trans code #t #f))
	;; just set the accumulator
	(define (trans-non-stack code) (trans code #f #f))
	;; code can be a tail position
	(define (trans-tail code) (trans code #f tail))
	;; set unspecified when a tail position
	(define (unspecified-position) (if tail (push-code! '%load-unspecified)))
	;; return here when a tail position
	(define (return-position) (if tail (push-code! '%return)))
	;; push the result into the stack
	(define (push-position) (if use-stack (push-code! '%push)))
	;; return or push
	(define (return-or-push) (return-position) (push-position))

	;;;
	;;; Translators
	;;;
	(define (translate-unspecified)
	  ;; #:unspecified
	  ;;   %load-unspecified
	  (push-code! '%load-unspecified)
	  (return-or-push))

	(define (translate-constant obj)
	  ;; #:constant OBJ
	  ;;   %pushc OBJ (if use-stack)
	  ;;   %loadc OBJ (if non-stack)
	  (if use-stack
	      (push-code! '%pushc obj)
	      (push-code! '%loadc obj))
	  (return-position))

	(define (translate-local-var name var)
	  (let* ((offset (env-variable-address env var))
		 (abbrev (string->symbol (format #f "~A:~A" name offset))))
	    (if (instruction-name? abbrev)
		(push-code! abbrev)
		(push-code! name offset))))

	(define (translate-external-var name var)
	  (let* ((addr (env-variable-address env var))
		 (depth (car addr))
		 (offset (cdr addr))
		 (abbrev1 (string->symbol
			   (format #f "~A:~A" name depth)))
		 (abbrev2 (string->symbol
			   (format #f "~A:~A:~A" name depth offset))))
	    (cond ((instruction-name? abbrev2) (push-code! abbrev2))
		  ((instruction-name? abbrev1) (push-code! abbrev1 offset))
		  (else (push-code! name addr)))))

	(define (translate-top-level-var name var)
	  (push-code! name (variable-name var)))

	(define (translate-ref var)
	  (assert variable? var)
	  (cond
	   ((local-variable? var)
	    ;; #:ref #<vm:local-var>
	    ;;   %pushl OFFSET (if use-stack)
	    ;;   %loadl OFFSET (if non-stack)
	    (translate-local-var (if use-stack '%pushl '%loadl) var))
	   ((external-variable? var)
	    ;; #:ref #<vm:external-var>
	    ;;   %pushe (DEPTH . OFFSET) (if use-stack)
	    ;;   %loade (DEPTH . OFFSET) (if non-stack)
	    (translate-external-var (if use-stack '%pushe '%loade) var))
	   ((top-level-variable? var)
	    ;; #:ref #<vm:top-level-var>
	    ;;   %pusht SYMBOL (if use-stack)
	    ;;   %loadt SYMBOL (if non-stack)
	    (translate-top-level-var (if use-stack '%pusht '%loadt) var)))
	  (return-position))

	(define (translate-set var obj)
	  (assert variable? var)
	  (trans-non-stack obj)
	  (cond
	   ((local-variable? var)
	    ;; #:set #<vm:local-var> OBJ
	    ;;   OBJ
	    ;;   %savel OFFSET
	    ;;   %name  NAME
	    (translate-local-var '%savel var))
	   ((external-variable? var)
	    ;; #:set #<vm:external-var> OBJ
	    ;;   OBJ
	    ;;   %savee (DEPTH . OFFSET)
	    ;;   %name  NAME
	    (translate-external-var '%savee var))
	   ((top-level-variable? var)
	    ;; #:set #<vm:top-level-var> OBJ
	    ;;   OBJ
	    ;;   %savet SYMBOL
	    ;;   %name  NAME
	    (translate-top-level-var '%savet var)))
	  ;; FIXME: Giving name to every objects is bad, but
	  ;; FIXME: this is useful for debugging.
	  (push-code! '%name (variable-name var))
	  (unspecified-position)
	  (return-or-push))

	(define (translate-and . args)
	  ;; #:and ARG1 ARG2... LAST
	  ;;      ARG1
	  ;;      %br-if-not L0
	  ;;      ARG2
	  ;;      %br-if-not L0
	  ;;      ...
	  ;;      LAST
	  ;;  L0:
	  (assert-for-each code? args)
	  (let* ((list (reverse args))
		 (last (car list))
		 (args (reverse! (cdr list))))
	    (let ((L0 (make-label)))
	      (for-each (lambda (arg)
			  (trans-non-stack arg)
			  (push-code! '%br-if-not L0))
			args)
	      (trans-tail last)
	      (push-code! #:label L0)))
	  (return-or-push))

	(define (translate-or . args)
	  ;; #:or ARG1 ARG2... LAST
	  ;;      ARG1
	  ;;      %br-if L0
	  ;;      ARG2
	  ;;      %br-if L0
	  ;;      ...
	  ;;      LAST
	  ;;  L0:
	  (assert-for-each code? args)
	  (let* ((list (reverse args))
		 (last (car list))
		 (args (reverse! (cdr list))))
	    (let ((L0 (make-label)))
	      (for-each (lambda (arg)
			  (trans-non-stack arg)
			  (push-code! '%br-if L0))
			args)
	      (trans-tail last)
	      (push-code! #:label L0)))
	  (return-or-push))

	(define (translate-program nreqs restp code)
	  ;; #:make-program NREQS RESTP CODE
	  ;;   %make-program BYTECODE
	  (push-code! '%make-program (byte-compile nreqs restp code))
	  (return-or-push))

	(define (translate-label label)
	  ;; #:label is processed by byte-finalize
	  (assert label? label)
	  (push-code! #:label label))

	(define (translate-goto label)
	  ;; #:goto LABEL
	  ;;   %jump ADDR (calculated in byte-finalize)
	  (assert label? label)
	  (push-code! '%jump label))

	(define (translate-if test then else)
	  ;; #:if TEST THEN ELSE
	  ;;       TEST
	  ;;       %br-if-not L1
	  ;;       THEN (tail position)
	  ;;       %jump L2 (if not tail)
	  ;;   L1: ELSE (tail position)
	  ;;   L2:
	  (assert code? test)
	  (assert code? then)
	  (assert code? else)
	  (let ((L1 (make-label))
		(L2 (make-label)))
	    (trans-non-stack test)
	    (push-code! '%br-if-not L1)
	    (trans-tail then)
	    (if (not tail)
		(push-code! '%jump L2))
	    (push-code! #:label L1)
	    (trans-tail else)
	    (push-code! #:label L2))
	  (push-position))

	(define (translate-until test . body)
	  ;; #:until TEST BODY...
	  ;;   L0: TEST
	  ;;       %br-if L1
	  ;;       BODY...
	  ;;       %jump L0
	  ;;   L1: 
	  (assert code? test)
	  (assert-for-each code? body)
	  (let ((L0 (make-label))
		(L1 (make-label)))
	    (push-code! #:label L0)
	    (trans-non-stack test)
	    (push-code! '%br-if L1)
	    (for-each trans-non-stack body)
	    (push-code! '%jump L0)
	    (push-code! #:label L1))
	  (unspecified-position)
	  (return-position))

	(define (translate-begin . body)
	  ;; #:begin BODY... TAIL
	  ;;   BODY...
	  ;;   TAIL (tail position)
	  (assert-for-each code? body)
	  (let* ((list (reverse body))
		 (tail (car list))
		 (body (reverse! (cdr list))))
	    (for-each trans-non-stack body)
	    (trans-tail tail))
	  (push-position))

	(define (translate-regular-call code . args)
	  ;; #:call CODE ARGS...
	  ;;   ARGS... (-> stack)
	  ;;   CODE
	  ;;   %(tail-)call NARGS
	  (let ((nargs (length args)))
	    (for-each trans-use-stack args)
	    (trans-non-stack code)
	    (if tail
		(push-code! '%tail-call nargs)
		(push-code! '%call nargs)))
	  (push-position))

	(define (translate-function-call inst . args)
	  ;; #:call INST ARGS...
	  (let ((name (instruction-name inst))
		(nargs (length args)))
	    (cond
	     ((cadr (instruction-arity inst))
	      ;;   ARGS... (-> stack)
	      ;;   INST NARGS
	      (for-each trans-use-stack args)
	      (push-code! name nargs))
	     (else
	      (case nargs
		((0)
		 ;;   INST
		 (push-code! name))
		((1)
		 ;;   ARG1
		 ;;   INST
		 (trans-non-stack (car args))
		 (push-code! name))
		((2)
		 ;;   ARG1 (-> stack)
		 ;;   ARG2
		 ;;   INST
		 (trans-use-stack (car args))
		 (trans-non-stack (cadr args))
		 (push-code! name))
		((3)
		 ;;   ARG1 (-> stack)
		 ;;   ARG2 (-> stack)
		 ;;   ARG3
		 ;;   INST
		 (trans-use-stack (car args))
		 (trans-use-stack (cadr args))
		 (trans-non-stack (caddr args))
		 (push-code! name))))))
	  (return-or-push))

	(define (translate-call obj . args)
	  (assert-for-each code? args)
	  (if (variable? obj)
	      (if (eq? (variable-type obj) 'function)
		  (cond
		   ((and (variable-bound? obj)
			 (and-let* ((obj (variable-value obj))
				    (def (assq-ref *vm-function-table* obj)))
			   (or (list-ref def (min (length args) 4))
			       (error "Wrong number of arguments"))))
		    => (lambda (inst)
			 (apply translate-function-call inst args)))
		   ((top-level-variable? obj)
		    (apply translate-regular-call
			   (make-code #:ref env obj) args)))
		  (apply translate-regular-call
			 (make-code #:ref env obj) args))
	      (apply translate-regular-call obj args)))

	;;;
	;;; Dispatch
	;;;
	(case tag
	  ((#:unspecified)
	   ;; #:unspecified
	   (check-nargs args = 0)
	   (translate-unspecified))
	  ((#:constant)
	   ;; #:constant OBJ
	   (check-nargs args = 1)
	   (translate-constant (car args)))
	  ((#:ref)
	   ;; #:ref VAR
	   (check-nargs args = 1)
	   (translate-ref (car args)))
	  ((#:set)
	   ;; #:set VAR OBJ
	   (check-nargs args = 2)
	   (translate-set (car args) (cadr args)))
	  ((#:and)
	   ;; #:and ARGS...
	   (apply translate-and args))
	  ((#:or)
	   ;; #:or ARGS...
	   (apply translate-or args))
	  ((#:make-program)
	   ;; #:make-program NREQS RESTP CODE
	   (check-nargs args = 3)
	   (translate-program (car args) (cadr args) (caddr args)))
	  ((#:label)
	   ;; #:label LABEL
	   (check-nargs args = 1)
	   (translate-label (car args)))
	  ((#:goto)
	   ;; #:goto LABEL
	   (check-nargs args = 1)
	   (translate-goto (car args)))
	  ((#:if)
	   ;; #:if TEST THEN ELSE
	   (check-nargs args = 3)
	   (translate-if (car args) (cadr args) (caddr args)))
	  ((#:until)
	   ;; #:until TEST BODY...
	   (check-nargs args >= 2)
	   (apply translate-until (car args) (cdr args)))
	  ((#:begin)
	   ;; #:begin BODY...
	   (check-nargs args >= 1)
	   (apply translate-begin args))
	  ((#:call)
	   ;; #:call OBJ ARGS...
	   (check-nargs args >= 1)
	   (apply translate-call (car args) (cdr args)))
	  (else
	   (error "Unknown tag:" tag)))))
    ;; that's it for this stage
    (reverse! stack)))


;;;
;;; Bytecode optimization
;;;

(define (byte-optimize code)
  (let loop ((last (car code)) (code (cdr code)) (result '()))
    (define (continue) (loop (car code) (cdr code) (cons last result)))
    (if (null? code)
	(reverse! (cons last result))
	(let ((this (car code)))
	  (case (car this)
	    ((%br-if)
	     (case (car last)
	       ((null?)
		(loop (cons '%br-if-null (cdr this)) (cdr code) result))
	       (else
		(continue))))
	    ((%br-if-not)
	     (case (car last)
	       ((null?)
		(loop (cons '%br-if-not-null (cdr this)) (cdr code) result))
	       (else
		(continue))))
	    (else
	     (continue)))))))


;;;
;;; Bytecode finalization
;;;

(define (byte-finalize code)
  (let loop ((code code) (result '()))
    (cond
     ((null? code)
      ;; Return the final assemble code
      (let ((finalize (lambda (obj)
			(if (label? obj)
			    (label-position obj)
			    obj))))
	(list->vector (reverse! (map finalize result)))))
     ((eq? (caar code) #:label)
      ;; Calculate the label position
      (set! (label-position (cadar code)) (length result))
      (loop (cdr code) result))
     (else
      ;; Append to the result
      (loop (cdr code) (append! (reverse! (car code)) result))))))


;;;
;;; Function table
;;;

(define (functional-instruction-alist)
  (let ((alist '()))
    (define (add! name inst)
      (let ((pair (assq name alist)))
	(if pair
	    (set-cdr! pair (cons inst (cdr pair)))
	    (set! alist (acons name (list inst) alist)))))
    (for-each (lambda (inst)
		(and-let* ((name (instruction-scheme-name inst)))
		  (add! name inst)))
	      (instruction-list))
    alist))

(define (build-table-data pair)
  (let ((name (car pair)) (insts (cdr pair)))
    (let ((vec (make-vector 5 #f)))
      (define (build-data! inst)
	(let ((arity (instruction-arity inst)))
	  (let ((nargs (car arity))
		(restp (cadr arity)))
	    (if restp
		(do ((i nargs (1+ i)))
		    ((>= i 4)
		     (vector-set! vec 4 inst))
		  (if (not (vector-ref vec i))
		      (vector-set! vec i inst)))
		(vector-set! vec nargs inst)))))
      (for-each build-data! insts)
      (let ((func (eval name (interaction-environment))))
	(cons func (vector->list vec))))))

(define *vm-function-table*
  (map build-table-data (functional-instruction-alist)))

;;; bytecomp.scm ends here
