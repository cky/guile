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
  :use-module (system base module)
  :use-module (system il glil)
  :use-module (system il ghil)
  :use-module (ice-9 common-list)
  :export (compile))

(define (compile x e . opts)
  (set! x (parse-ghil x e))
  (if (memq :O opts) (set! x (optimize x)))
  (codegen x))


;;;
;;; Stage 2: Optimization
;;;

(define (optimize x)
  (match x
    (($ <ghil-call> proc args)
     (match proc
       ;; ((@lambda (VAR...) BODY...) ARG...) =>
       ;;   (@let ((VAR ARG) ...) BODY...)
       (($ <ghil-lambda> env vars #f body)
	(optimize (make-<ghil-bind> vars args body)))
       (else
	(make-<ghil-call> (optimize proc) (for-each optimize args)))))
    (else x)))


;;;
;;; Stage 3: Code generation
;;;

(define *ia-void* (make-<glil-void>))
(define *ia-drop* (make-<glil-inst> 'drop))
(define *ia-return* (make-<glil-inst> 'return))

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
      ;; possible tail position
      (define (comp-tail tree) (comp tree tail drop))
      ;; push the result
      (define (comp-push tree) (comp tree #f #f))
      ;; drop the result
      (define (comp-drop tree) (comp tree #f #t))
      ;; return this code if necessary
      (define (return-code! code)
	(if (not drop) (push-code! code))
	(if tail (push-code! *ia-return*)))
      ;; return void if necessary
      (define (return-void!) (return-code! *ia-void*))
      ;;
      ;; dispatch
      (match tree
	(($ <ghil-void>)
	 (return-void!))

	(($ <ghil-quote> obj)
	 (return-code! (make-<glil-const> obj)))

	(($ <ghil-ref> env var)
	 (return-code! (make-glil-var 'ref env var)))

	(($ <ghil-set> env var val)
	 (comp-push val)
	 (push-code! (make-glil-var 'set env var))
	 (return-void!))

	(($ <ghil-if> test then else)
	 ;;     TEST
	 ;;     (br-if-not L1)
	 ;;     THEN
	 ;;     (jump L2)
	 ;; L1: ELSE
	 ;; L2:
	 (let ((L1 (make-label)) (L2 (make-label)))
	   (comp-push test)
	   (push-code! (make-<glil-branch> 'br-if-not L1))
	   (comp-tail then)
	   (if (not tail) (push-code! (make-<glil-jump> L2)))
	   (push-code! (make-<glil-label> L1))
	   (comp-tail else)
	   (if (not tail) (push-code! (make-<glil-label> L2)))))

	(($ <ghil-begin> exps)
	 ;; EXPS...
	 ;; TAIL
	 (if (null? exps)
	     (return-void!)
	     (do ((exps exps (cdr exps)))
		 ((null? (cdr exps))
		  (comp-tail (car exps)))
	       (comp-drop (car exps)))))

	(($ <ghil-bind> env vars vals body)
	 ;; VALS...
	 ;; (set VARS)...
	 ;; BODY
	 (for-each comp-push vals)
	 (for-each (lambda (var) (push-code! (make-glil-var 'set env var)))
		   (reverse vars))
	 (comp-tail body))

	(($ <ghil-lambda> vars rest body)
	 (return-code! (codegen tree)))

	(($ <ghil-call> proc args)
	 ;; ARGS...
	 ;; PROC
	 ;; ([tail-]call NARGS)
	 (for-each comp-push args)
	 (comp-push proc)
	 (let ((inst (if tail 'tail-call 'call)))
	   (push-code! (make-<glil-call> inst (length args))))
	 (if drop (push-code! *ia-drop*)))

	(($ <ghil-inst> inst args)
	 ;; ARGS...
	 ;; (INST)
	 (for-each comp-push args)
	 (push-code! (make-<glil-inst> inst))
	 (if drop (push-code! *ia-drop*))
	 (if tail (push-code! *ia-return*)))))
    ;;
    ;; main
    (match ghil
      (($ <ghil-lambda> env args rest body)
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
