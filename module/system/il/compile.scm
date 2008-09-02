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
  :use-syntax (system base syntax)
  :use-module (system il glil)
  :use-module (system il ghil)
  :use-module (ice-9 common-list)
  :export (compile))

(define (compile x e . opts)
  (if (memq :O opts) (set! x (optimize x)))
  (codegen x))


;;;
;;; Stage 2: Optimization
;;;

(define (optimize x)
  (record-case x
    ((<ghil-set> env loc var val)
     (make-ghil-set env var (optimize val)))

    ((<ghil-if> env loc test then else)
     (make-ghil-if env loc (optimize test) (optimize then) (optimize else)))

    ((<ghil-begin> env loc exps)
     (make-ghil-begin env loc (map optimize exps)))

    ((<ghil-bind> env loc vars vals body)
     (make-ghil-bind env loc vars (map optimize vals) (optimize body)))

    ((<ghil-lambda> env loc vars rest meta body)
     (make-ghil-lambda env loc vars rest meta (optimize body)))

    ((<ghil-inline> env loc instruction args)
     (make-ghil-inline env loc instruction (map optimize args)))

    ((<ghil-call> env loc proc args)
     (let ((parent-env env))
       (record-case proc
         ;; ((@lambda (VAR...) BODY...) ARG...) =>
         ;;   (@let ((VAR ARG) ...) BODY...)
         ((<ghil-lambda> env loc vars rest meta body)
          (cond
           ((not rest)
            (for-each (lambda (v)
                        (case (ghil-var-kind v)
                          ((argument) (set! (ghil-var-kind v) 'local)))
                        (set! (ghil-var-env v) parent-env)
                        (ghil-env-add! parent-env v))
                      (ghil-env-variables env)))
           (else
            (make-ghil-call parent-env loc (optimize proc) (map optimize args)))))
         (else
          (make-ghil-call parent-env loc (optimize proc) (map optimize args))))))

    (else x)))


;;;
;;; Stage 3: Code generation
;;;

(define *ia-void* (make-glil-void))
(define *ia-drop* (make-glil-call 'drop 0))
(define *ia-return* (make-glil-call 'return 0))

(define (make-label) (gensym ":L"))

(define (make-glil-var op env var)
  (case (ghil-var-kind var)
    ((argument)
     (make-glil-argument op (ghil-var-index var)))
    ((local)
     (make-glil-local op (ghil-var-index var)))
    ((external)
     (do ((depth 0 (1+ depth))
	  (e env (ghil-env-parent e)))
	 ((eq? e (ghil-var-env var))
	  (make-glil-external op depth (ghil-var-index var)))))
    ((module)
     (make-glil-module op (ghil-var-env var) (ghil-var-name var)))
    (else (error "Unknown kind of variable:" var))))

(define (constant? x)
  (cond ((or (number? x) (string? x) (symbol? x) (keyword? x) (boolean? x)) #t)
        ((pair? x) (and (constant? (car x))
                        (constant? (cdr x))))
        ((vector? x) (let lp ((i (vector-length x)))
                       (or (zero? i)
                           (and (constant? (vector-ref x (1- i)))
                                (lp (1- i))))))))

(define (codegen ghil)
  (let ((stack '()))
    (define (push-code! loc code)
      (set! stack (cons code stack))
      (if loc (set! stack (cons (make-glil-source loc) stack))))
    (define (push-bindings! loc vars)
      (if (not (null? vars))
          (push-code!
           loc
           (make-glil-bind
            (map list
                 (map ghil-var-name vars)
                 (map ghil-var-kind vars)
                 (map ghil-var-index vars))))))
    (define (comp tree tail drop)
      (define (push-label! label)
	(push-code! #f (make-glil-label label)))
      (define (push-branch! loc inst label)
	(push-code! loc (make-glil-branch inst label)))
      (define (push-call! loc inst args)
	(for-each comp-push args)
	(push-code! loc (make-glil-call inst (length args))))
      ;; possible tail position
      (define (comp-tail tree) (comp tree tail drop))
      ;; push the result
      (define (comp-push tree) (comp tree #f #f))
      ;; drop the result
      (define (comp-drop tree) (comp tree #f #t))
      ;; drop the result if unnecessary
      (define (maybe-drop)
	(if drop (push-code! #f *ia-drop*)))
      ;; return here if necessary
      (define (maybe-return)
	(if tail (push-code! #f *ia-return*)))
      ;; return this code if necessary
      (define (return-code! loc code)
	(if (not drop) (push-code! loc code))
	(maybe-return))
      ;; return void if necessary
      (define (return-void!)
	(return-code! #f *ia-void*))
      ;; return object if necessary
      (define (return-object! loc obj)
	(return-code! loc (make-glil-const #:obj obj)))
      ;;
      ;; dispatch
      (record-case tree
	((<ghil-void>)
	 (return-void!))

	((<ghil-quote> env loc obj)
	 (return-object! loc obj))

	((<ghil-quasiquote> env loc exp)
	 (let loop ((x exp))
           (cond
            ((list? x)
             (push-call! #f 'mark '())
             (for-each loop x)
             (push-call! #f 'list-mark '()))
            ((pair? x)
             (loop (car x))
             (loop (cdr x))
             (push-code! #f (make-glil-call 'cons 2)))
            ((record? x)
             (record-case x
              ((<ghil-unquote> env loc exp)
               (comp-push exp))
              ((<ghil-unquote-splicing> env loc exp)
               (comp-push exp)
               (push-call! #f 'list-break '()))))
            ((constant? x)
             (push-code! #f (make-glil-const #:obj x)))
            (else
             (error "element of quasiquote can't be compiled" x))))
	 (maybe-drop)
	 (maybe-return))

	((<ghil-ref> env loc var)
	 (return-code! loc (make-glil-var 'ref env var)))

	((<ghil-set> env loc var val)
	 (comp-push val)
	 (push-code! loc (make-glil-var 'set env var))
	 (return-void!))

	((<ghil-define> env loc var val)
	 (comp-push val)
	 (push-code! loc (make-glil-var 'define env var))
	 (return-void!))

	((<ghil-if> env loc test then else)
	 ;;     TEST
	 ;;     (br-if-not L1)
	 ;;     THEN
	 ;;     (br L2)
	 ;; L1: ELSE
	 ;; L2:
	 (let ((L1 (make-label)) (L2 (make-label)))
	   (comp-push test)
	   (push-branch! loc 'br-if-not L1)
	   (comp-tail then)
	   (if (not tail) (push-branch! #f 'br L2))
	   (push-label! L1)
	   (comp-tail else)
	   (if (not tail) (push-label! L2))))

	((<ghil-and> env loc exps)
	 ;;     EXP
	 ;;     (br-if-not L1)
	 ;;     ...
	 ;;     TAIL
	 ;;     (br L2)
	 ;; L1: (const #f)
	 ;; L2:
         (cond ((null? exps) (return-object! loc #t))
               ((null? (cdr exps)) (comp-tail (car exps)))
               (else
                (let ((L1 (make-label)) (L2 (make-label)))
                  (let lp ((exps exps))
                    (cond ((null? (cdr exps))
                           (comp-tail (car exps))
                           (push-branch! #f 'br L2)
                           (push-label! L1)
                           (return-object! #f #f)
                           (push-label! L2)
                           (maybe-return))
                          (else
                           (comp-push (car exps))
                           (push-branch! #f 'br-if-not L1)
                           (lp (cdr exps)))))))))

	((<ghil-or> env loc exps)
	 ;;     EXP
	 ;;     (dup)
	 ;;     (br-if L1)
	 ;;     (drop)
	 ;;     ...
	 ;;     TAIL
	 ;; L1:
         (cond ((null? exps) (return-object! loc #f))
               ((null? (cdr exps)) (comp-tail (car exps)))
               (else
                (let ((L1 (make-label)))
                  (let lp ((exps exps))
                    (cond ((null? (cdr exps))
                           (comp-tail (car exps))
                           (push-label! L1)
                           (maybe-return))
                          (else
                           (comp-push (car exps))
                           (push-call! #f 'dup '())
                           (push-branch! #f 'br-if L1)
                           (push-call! #f 'drop '())
                           (lp (cdr exps)))))))))

	((<ghil-begin> env loc exps)
	 ;; EXPS...
	 ;; TAIL
	 (if (null? exps)
	     (return-void!)
	     (do ((exps exps (cdr exps)))
		 ((null? (cdr exps))
		  (comp-tail (car exps)))
	       (comp-drop (car exps)))))

	((<ghil-bind> env loc vars vals body)
	 ;; VALS...
	 ;; (set VARS)...
	 ;; BODY
	 (for-each comp-push vals)
         (push-bindings! loc vars)
	 (for-each (lambda (var) (push-code! #f (make-glil-var 'set env var)))
		   (reverse vars))
	 (comp-tail body)
	 (push-code! #f (make-glil-unbind)))

	((<ghil-lambda> env loc vars rest meta body)
	 (return-code! loc (codegen tree)))

	((<ghil-inline> env loc inline args)
	 ;; ARGS...
	 ;; (INST NARGS)
	 (push-call! loc inline args)
	 (maybe-drop)
	 (maybe-return))

	((<ghil-call> env loc proc args)
	 ;; PROC
	 ;; ARGS...
	 ;; ([tail-]call NARGS)
	 (comp-push proc)
	 (push-call! loc (if tail 'tail-call 'call) args)
	 (maybe-drop))))
    ;;
    ;; main
    (record-case ghil
      ((<ghil-lambda> env loc vars rest meta body)
       (let* ((evars (ghil-env-variables env))
	      (locs (pick (lambda (v) (eq? (ghil-var-kind v) 'local)) evars))
	      (exts (pick (lambda (v) (eq? (ghil-var-kind v) 'external)) evars)))
	 ;; initialize variable indexes
	 (finalize-index! vars)
	 (finalize-index! locs)
	 (finalize-index! exts)
	 ;; meta bindings
         (push-bindings! #f vars)
	 ;; export arguments
	 (do ((n 0 (1+ n))
	      (l vars (cdr l)))
	     ((null? l))
	   (let ((v (car l)))
	     (case (ghil-var-kind v)
               ((external)
                (push-code! #f (make-glil-argument 'ref n))
                (push-code! #f (make-glil-external 'set 0 (ghil-var-index v)))))))
	 ;; compile body
	 (comp body #t #f)
	 ;; create GLIL
	 (let ((vars (make-glil-vars :nargs (length vars)
                                     :nrest (if rest 1 0)
                                     :nlocs (length locs)
                                     :nexts (length exts))))
	   (make-glil-asm vars meta (reverse! stack))))))))

(define (finalize-index! list)
  (do ((n 0 (1+ n))
       (l list (cdr l)))
      ((null? l))
    (let ((v (car l))) (set! (ghil-var-index v) n))))
