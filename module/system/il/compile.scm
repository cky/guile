;;; GHIL -> GLIL compiler

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

(define-module (system il compile)
  :use-module (oop goops)
  :use-syntax (system base syntax)
  :use-module (system il glil)
  :use-module (system il ghil)
  :use-module (ice-9 match)
  :use-module (ice-9 common-list)
  :export (compile))

(define (compile x e . opts)
  (if (memq :O opts) (set! x (optimize x)))
  (codegen x))


;;;
;;; Stage 2: Optimization
;;;

(define (optimize x)
  (match x
    (($ <ghil-set> env var val)
     (make-<ghil-set> env var (optimize val)))

    (($ <ghil-if> test then else)
     (make-<ghil-if> (optimize test) (optimize then) (optimize else)))

    (($ <ghil-begin> exps)
     (make-<ghil-begin> (map optimize exps)))

    (($ <ghil-bind> env vars vals body)
     (make-<ghil-bind> env vars (map optimize vals) (optimize body)))

    (($ <ghil-lambda> env vars rest body)
     (make-<ghil-lambda> env vars rest (optimize body)))

    (($ <ghil-inst> inst args)
     (make-<ghil-inst> inst (map optimize args)))

    (($ <ghil-call> env proc args)
     (match proc
       ;; ((@lambda (VAR...) BODY...) ARG...) =>
       ;;   (@let ((VAR ARG) ...) BODY...)
       (($ <ghil-lambda> lambda-env vars #f body)
	(for-each (lambda (v)
		    (if (eq? v.kind 'argument) (set! v.kind 'local))
		    (set! v.env env)
		    (ghil-env-add! env v))
		  lambda-env.variables)
	(optimize (make-<ghil-bind> env vars args body)))
       (else
	(make-<ghil-call> env (optimize proc) (map optimize args)))))
    (else x)))


;;;
;;; Stage 3: Code generation
;;;

(define *ia-void* (make-<glil-void>))
(define *ia-drop* (make-<glil-call> 'drop 0))
(define *ia-return* (make-<glil-call> 'return 0))

(define (make-label) (gensym ":L"))

(define (make-glil-var op env var)
  (case var.kind
    ((argument)
     (make-<glil-argument> op var.index))
    ((local)
     (make-<glil-local> op var.index))
    ((external)
     (do ((depth 0 (1+ depth))
	  (e env e.parent))
	 ((eq? e var.env)
	  (make-<glil-external> op depth var.index))))
    ((module)
     (make-<glil-module> op var.env var.name))
    (else (error "Unknown kind of variable:" var))))

(define (codegen ghil)
  (let ((stack '()))
    (define (push-code! code)
      (set! stack (cons code stack)))
    (define (comp tree tail drop)
      (define (push-label! label)
	(push-code! (make-<glil-label> label)))
      (define (push-branch! inst label)
	(push-code! (make-<glil-branch> inst label)))
      (define (push-call! inst args)
	(for-each comp-push args)
	(push-code! (make-<glil-call> inst (length args))))
      ;; possible tail position
      (define (comp-tail tree) (comp tree tail drop))
      ;; push the result
      (define (comp-push tree) (comp tree #f #f))
      ;; drop the result
      (define (comp-drop tree) (comp tree #f #t))
      ;; drop the result if unnecessary
      (define (maybe-drop)
	(if drop (push-code! *ia-drop*)))
      ;; return here if necessary
      (define (maybe-return)
	(if tail (push-code! *ia-return*)))
      ;; return this code if necessary
      (define (return-code! code)
	(if (not drop) (push-code! code))
	(maybe-return))
      ;; return void if necessary
      (define (return-void!)
	(return-code! *ia-void*))
      ;; return object if necessary
      (define (return-object! obj)
	(return-code! (make-<glil-const> obj)))
      ;;
      ;; dispatch
      (match tree
	(($ <ghil-void>)
	 (return-void!))

	(($ <ghil-quote> env loc obj)
	 (return-object! obj))

	(($ <ghil-quasiquote> env loc exp)
	 (let loop ((x exp))
	   (match x
	     ((? list? ls)
	      (push-call! 'mark '())
	      (for-each loop ls)
	      (push-call! 'list-mark '()))
	     ((? pair? pp)
	      (loop (car pp))
	      (loop (cdr pp))
	      (push-code! (make-<glil-call> 'cons 2)))
	     (($ <ghil-unquote> env loc exp)
	      (comp-push exp))
	     (($ <ghil-unquote-splicing> env loc exp)
	      (comp-push exp)
	      (push-call! 'list-break '()))
	     (else
	      (push-code! (make-<glil-const> x)))))
	 (maybe-drop)
	 (maybe-return))

	(($ <ghil-ref> env loc var)
	 (return-code! (make-glil-var 'ref env var)))

	((or ($ <ghil-set> env loc var val)
	     ($ <ghil-define> env loc var val))
	 (comp-push val)
	 (push-code! (make-glil-var 'set env var))
	 (return-void!))

	(($ <ghil-if> env loc test then else)
	 ;;     TEST
	 ;;     (br-if-not L1)
	 ;;     THEN
	 ;;     (br L2)
	 ;; L1: ELSE
	 ;; L2:
	 (let ((L1 (make-label)) (L2 (make-label)))
	   (comp-push test)
	   (push-branch! 'br-if-not L1)
	   (comp-tail then)
	   (if (not tail) (push-branch! 'br L2))
	   (push-label! L1)
	   (comp-tail else)
	   (if (not tail) (push-label! L2))))

	(($ <ghil-and> env loc exps)
	 ;;     EXP
	 ;;     (br-if-not L1)
	 ;;     ...
	 ;;     TAIL
	 ;;     (br L2)
	 ;; L1: (const #f)
	 ;; L2:
	 (let ((L1 (make-label)) (L2 (make-label)))
	   (if (null? exps)
	       (return-object! #t)
	       (do ((exps exps (cdr exps)))
		   ((null? (cdr exps))
		    (comp-tail (car exps))
		    (if (not tail) (push-branch! 'br L2))
		    (push-label! L1)
		    (return-object! #f)
		    (if (not tail) (push-label! L2))
		    (maybe-drop)
		    (maybe-return))
		 (comp-push (car exps))
		 (push-branch! 'br-if-not L1)))))

	(($ <ghil-or> env loc exps)
	 ;;     EXP
	 ;;     (dup)
	 ;;     (br-if L1)
	 ;;     (drop)
	 ;;     ...
	 ;;     TAIL
	 ;; L1:
	 (let ((L1 (make-label)))
	   (if (null? exps)
	       (return-object! #f)
	       (do ((exps exps (cdr exps)))
		   ((null? (cdr exps))
		    (comp-tail (car exps))
		    (push-label! L1)
		    (maybe-drop)
		    (maybe-return))
		 (comp-push (car exps))
		 (push-call! 'dup '())
		 (push-branch! 'br-if L1)
		 (push-call! 'drop '())))))

	(($ <ghil-begin> env loc exps)
	 ;; EXPS...
	 ;; TAIL
	 (if (null? exps)
	     (return-void!)
	     (do ((exps exps (cdr exps)))
		 ((null? (cdr exps))
		  (comp-tail (car exps)))
	       (comp-drop (car exps)))))

	(($ <ghil-bind> env loc vars vals body)
	 ;; VALS...
	 ;; (set VARS)...
	 ;; BODY
	 (for-each comp-push vals)
	 (for-each (lambda (var) (push-code! (make-glil-var 'set env var)))
		   (reverse vars))
	 (comp-tail body))

	(($ <ghil-lambda> env loc vars rest body)
	 (return-code! (codegen tree)))

	(($ <ghil-inline> env loc inst args)
	 ;; ARGS...
	 ;; (INST NARGS)
	 (push-call! inst args)
	 (maybe-drop)
	 (maybe-return))

	(($ <ghil-call> env loc proc args)
	 ;; PROC
	 ;; ARGS...
	 ;; ([tail-]call NARGS)
	 (comp-push proc)
	 (push-call! (if tail 'tail-call 'call) args)
	 (maybe-drop))))
    ;;
    ;; main
    (match ghil
      (($ <ghil-lambda> env loc args rest body)
       (let* ((vars env.variables)
	      (locs (pick (lambda (v) (eq? v.kind 'local)) vars))
	      (exts (pick (lambda (v) (eq? v.kind 'external)) vars)))
	 ;; initialize variable indexes
	 (finalize-index! args)
	 (finalize-index! locs)
	 (finalize-index! exts)
	 ;; export arguments
	 (do ((n 0 (1+ n)) (l args (cdr l)))
	     ((null? l))
	   (let ((v (car l)))
	     (if (eq? v.kind 'external)
		 (begin (push-code! (make-<glil-argument> 'ref n))
			(push-code! (make-<glil-external> 'set 0 v.index))))))
	 ;; compile body
	 (comp body #t #f)
	 ;; create GLIL
	 (make-<glil-asm> (length args) (if rest 1 0) (length locs)
			  (length exts) (reverse! stack)))))))

(define (finalize-index! list)
  (do ((n 0 (1+ n))
       (l list (cdr l)))
      ((null? l))
    (let ((v (car l))) (set! v.index n))))
