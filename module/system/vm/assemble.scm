;;; Guile VM assembler

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

(define-module (system vm assemble)
  :use-syntax (system base syntax)
  :use-module (system il glil)
  :use-module (system vm core)
  :use-module (system vm conv)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module (ice-9 common-list)
  :export (assemble))

(define (assemble glil env . opts)
  (codegen (preprocess glil #f) #t))


;;;
;;; Types
;;;

(define-structure (<vm-asm> venv glil body))
(define-structure (venv parent nexts closure?))
(define-structure (vmod id))
(define-structure (vlink module name))
(define-structure (bytespec nargs nrest nlocs nexts bytes objs))


;;;
;;; Stage 1: Preprocess
;;;

(define (preprocess x e)
  (match x
    (($ <glil-asm> nargs nrest nlocs nexts body)
     (let* ((venv (make-venv e nexts #f))
	    (body (map (lambda (x) (preprocess x venv)) body)))
       (make-<vm-asm> venv x body)))
    (($ <glil-external> op depth index)
     (do ((d depth (1- d))
	  (e e (venv-parent e)))
	 ((= d 0))
       (set-venv-closure?! e #t))
     x)
    (else x)))


;;;
;;; Stage 2: Bytecode generation
;;;

(define (codegen glil toplevel)
  (match glil
    (($ <vm-asm> venv ($ <glil-asm> nargs nrest nlocs nexts _) body)
     (let ((stack '())
	   (label-alist '())
	   (object-alist '()))
       (define (push-code! code)
	 (set! stack (optimizing-push code stack)))
       (define (push-object! x)
	 (cond ((object->code x) => push-code!)
	       (toplevel
		;; top-level object-dump
		(cond ((object-assoc x object-alist) =>
		       (lambda (obj+index)
			 (cond ((not (cdr obj+index))
				(set-cdr! obj+index nlocs)
				(set! nlocs (+ nlocs 1))))
			 (push-code! `(local-ref ,(cdr obj+index)))))
		      (else
		       (set! object-alist (acons x #f object-alist))
		       (push-code! `(object-dump ,x)))))
	       (else
		;; local object-ref
		(let ((i (cond ((object-assoc x object-alist) => cdr)
			       (else
				(let ((i (length object-alist)))
				  (set! object-alist (acons x i object-alist))
				  i)))))
		  (push-code! `(object-ref ,i))))))
       (define (label-ref key)
	 (assq-ref label-alist key))
       (define (label-set key)
	 (let ((addr (apply + (map length stack))))
	   (set! label-alist (assq-set! label-alist key addr))))
       (define (generate-code x)
	 (match x
	   (($ <vm-asm> venv)
	    (push-object! (codegen x #f))
	    (if (venv-closure? venv) (push-code! `(make-closure))))

	   (($ <glil-void>)
	    (push-code! `(void)))

	   (($ <glil-const> x)
	    (push-object! x))

	   (($ <glil-argument> op index)
	    (if (eq? op 'ref)
		(push-code! `(local-ref ,index))
		(push-code! `(local-set ,index))))

	   (($ <glil-local> op index)
	    (if (eq? op 'ref)
		(push-code! `(local-ref ,(+ nargs index)))
		(push-code! `(local-set ,(+ nargs index)))))

	   (($ <glil-external> op depth index)
	    (do ((e venv (venv-parent e))
		 (d depth (1- d))
		 (n 0 (+ n (venv-nexts e))))
		((= d 0)
		 (if (eq? op 'ref)
		     (push-code! `(external-ref ,(+ n index)))
		     (push-code! `(external-set ,(+ n index)))))))

	   (($ <glil-module> op module name)
	    (push-object! (make-vlink #f name))	;; FIXME: (make-vmod module)
	    (if (eq? op 'ref)
		(push-code! '(variable-ref))
		(push-code! '(variable-set))))

	   (($ <glil-label> label)
	    (label-set label))

	   (($ <glil-branch> inst label)
	    (let ((setter (lambda (addr) (- (label-ref label) addr))))
	      (push-code! (list inst setter))))

	   (($ <glil-call> inst nargs)
	    (if (instruction? inst)
		(let ((pops (instruction-pops inst)))
		  (cond ((< pops 0)
			 (push-code! (list inst nargs)))
			((= pops nargs)
			 (push-code! (list inst)))
			(else
			 (error "Wrong number of arguments:" inst nargs))))
		(error "Unknown instruction:" inst)))))
       ;;
       ;; main
       (for-each generate-code body)
       (if toplevel
	   ;; top-level
	   (let ((new '()))
	     (define (push-code! x)
	       (set! new (cons x new)))
	     (do ((stack (reverse! stack) (cdr stack)))
		 ((null? stack)
		  (make-dumpcode nlocs nexts (stack->bytes (reverse! new))))
	       (if (eq? (caar stack) 'object-dump)
		   (let ((x (cadar stack)))
		     (dump-object! push-code! x)
		     (cond ((object-assoc x object-alist) =>
			    (lambda (obj+index)
			      (cond ((cdr obj+index) =>
				     (lambda (n)
				       (push-code! '(dup))
				       (push-code! `(local-set ,n)))))))))
		   (push-code! (car stack)))))
	   ;; closures
	   (let ((bytes (stack->bytes (reverse! stack)))
		 (objs (map car (reverse! object-alist))))
	     (make-bytespec nargs nrest nlocs nexts bytes objs)))))))

(define (object-assoc x alist)
  (if (vlink? x) (assoc x alist) (assq x alist)))

(define (stack->bytes stack)
  (let loop ((result '()) (stack stack) (addr 0))
    (if (null? stack)
	(apply string-append (reverse! result))
	(let* ((orig (car stack))
	       (addr (+ addr (length orig)))
	       (code (if (and (pair? (cdr orig)) (procedure? (cadr orig)))
			 `(,(car orig) ,((cadr orig) addr))
			 orig)))
	  (loop (cons (code->bytes code) result) (cdr stack) addr)))))


;;;
;;; Bytecode optimization
;;;

(define *optimization-table*
  '((not       (not       . not-not)
	       (eq?       . not-eq?)
	       (null?     . not-null?)
	       (not-not   . not)
	       (not-eq?   . eq?)
	       (not-null? . null?))
    (br-if     (not       . br-if-not)
	       (eq?       . br-if-eq)
	       (null?     . br-if-null)
	       (not-not   . br-if)
	       (not-eq?   . br-if-not-eq)
	       (not-null? . br-if-not-null))
    (br-if-not (not       . br-if)
	       (eq?       . br-if-not-eq)
	       (null?     . br-if-not-null)
	       (not-not   . br-if-not)
	       (not-eq?   . br-if-eq)
	       (not-null? . br-if-null))))

(define (optimizing-push code stack)
  (let ((alist (assq-ref *optimization-table* (car code))))
    (cond ((and alist (pair? stack) (assq-ref alist (caar stack))) =>
	   (lambda (inst) (cons (cons inst (cdr code)) (cdr stack))))
	  (else (cons (code-pack code) stack)))))


;;;
;;; Object dump
;;;

;; NOTE: undumpped in vm_load.c.

(define (dump-object! push-code! x)
  (let dump! ((x x))
    (cond
     ((object->code x) => push-code!)
     ((bytespec? x)
      (match x
	(($ bytespec nargs nrest nlocs nexts bytes objs)
	 ;; dump parameters
	 (cond
	  ((and (< nargs 4) (< nlocs 8) (< nexts 4))
	   ;; 8-bit representation
	   (let ((x (+ (* nargs 64) (* nrest 32) (* nlocs 4) nexts)))
	     (push-code! `(make-int8 ,x))))
	  ((and (< nargs 16) (< nlocs 128) (< nexts 16))
	   ;; 16-bit representation
	   (let ((x (+ (* nargs 4096) (* nrest 2048) (* nlocs 16) nexts)))
	     (push-code! `(make-int16 ,(quotient x 256) ,(modulo x 256)))))
	  (else
	   ;; Other cases
	   (push-code! (object->code nargs))
	   (push-code! (object->code nrest))
	   (push-code! (object->code nlocs))
	   (push-code! (object->code nexts))
	   (push-code! (object->code #f))))
	 ;; dump object table
	 (cond ((not (null? objs))
		(for-each dump! objs)
		(push-code! `(vector ,(length objs)))))
	 ;; dump bytecode
	 (push-code! `(load-program ,bytes)))))
     ((vlink? x)
      (dump! (vlink-module x))
      (dump! (vlink-name x))
      (push-code! `(link)))
     ((vmod? x)
      (push-code! `(load-module ,(vmod-id x))))
     ((and (integer? x) (exact? x))
      (let ((str (do ((n x (quotient n 256))
		      (l '() (cons (modulo n 256) l)))
		     ((= n 0)
		      (list->string (map integer->char l))))))
	(push-code! `(load-integer ,str))))
     ((number? x)
      (push-code! `(load-number ,(number->string x))))
     ((string? x)
      (push-code! `(load-string ,x)))
     ((symbol? x)
      (push-code! `(load-symbol ,(symbol->string x))))
     ((keyword? x)
      (push-code! `(load-keyword ,(symbol->string (keyword-dash-symbol x)))))
     ((list? x)
      (for-each dump! x)
      (push-code! `(list ,(length x))))
     ((pair? x)
      (dump! (car x))
      (dump! (cdr x))
      (push-code! `(cons)))
     ((vector? x)
      (for-each dump! (vector->list x))
      (push-code! `(vector ,(vector-length x))))
     (else
      (error "Cannot dump:" x)))))
