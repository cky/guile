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
  (dump (codegen (preprocess glil #f) #t)))


;;;
;;; Types
;;;

(define-structure (<vm-asm> venv glil body))
(define-structure (venv parent nexts closure?))
(define-structure (vmod id))
(define-structure (vlink module name))
(define-structure (bytespec nargs nrest nlocs bytes objs))


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
	   (object-alist '())
	   (nvars (+ nargs nlocs -1)))
       (define (push-code! code)
	 (set! stack (optimizing-push code stack)))
       (define (push-object! x)
	 (let ((index (or ((if (vlink? x) assoc-ref assq-ref) object-alist x)
			  (let ((index (length object-alist)))
			    (set! object-alist (acons x index object-alist))
			    index))))
	   (push-code! `(object-ref ,index))))
       (define (label-ref key)
	 (assq-ref label-alist key))
       (define (label-set key)
	 (let ((addr (apply + (map length stack))))
	   (set! label-alist (assq-set! label-alist key addr))))
       (define (generate-code x)
	 (match x
	   (($ <vm-asm> env)
	    (push-object! (codegen x #f))
	    (if (venv-closure? env) (push-code! `(make-closure))))

	   (($ <glil-void>)
	    (push-code! `(void)))

	   (($ <glil-const> x)
	    (if toplevel
		(for-each push-code! (object->dump-code x))
		(cond ((object->code x) => push-code!)
		      (else (push-object! x)))))

	   (($ <glil-argument> op index)
	    (push-code! `(,(symbol-append 'local- op) ,(- nvars index))))

	   (($ <glil-local> op index)
	    (push-code! `(,(symbol-append 'local- op)
			  ,(- nvars (+ nargs index)))))

	   (($ <glil-external> op depth index)
	    (do ((e venv (venv-parent e))
		 (d depth (1- d))
		 (i 0 (+ i (venv-nexts e))))
		((= d 0)
		 (push-code! `(,(symbol-append 'external- op) ,(+ index i))))))

	   (($ <glil-module> op module name)
	    (if toplevel
		(begin
		  ;; (push-code! `(load-module ,module))
		  (for-each push-code! (object->dump-code name))
		  (push-code! `(link/current-module)))
		;; (let ((vlink (make-vlink (make-vmod module) name)))
		(push-object! (make-vlink #f name)))
	    (push-code! (list (symbol-append 'variable- op))))

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
       (if (> nexts 0) (push-code! `(external ,nexts)))
       (for-each generate-code body)
       (let ((bytes (apply string-append (stack-finalize (reverse! stack))))
	     (objs (map car (reverse! object-alist))))
	 (make-bytespec nargs nrest nlocs bytes objs))))))

(define (stack-finalize stack)
  (let loop ((list '()) (stack stack) (addr 0))
    (if (null? stack)
	(reverse! list)
	(let* ((orig (car stack))
	       (addr (+ addr (length orig)))
	       (code (if (and (pair? (cdr orig)) (procedure? (cadr orig)))
			 `(,(car orig) ,((cadr orig) addr))
			 orig)))
	  (loop (cons (code->bytes code) list) (cdr stack) addr)))))

;; Optimization

(define *optimize-table*
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
  (let ((alist (assq-ref *optimize-table* (car code))))
    (cond ((and alist (pair? stack) (assq-ref alist (caar stack))) =>
	   (lambda (inst) (cons (cons inst (cdr code)) (cdr stack))))
	  (else (cons (code-pack code) stack)))))


;;;
;;; Stage3: Dumpcode generation
;;;

(define (dump bytespec)
  (let* ((table (build-object-table bytespec))
	 (bytes (bytespec->bytecode bytespec table '(return))))
    (if (null? table)
	bytes
	(let ((spec (make-bytespec 0 0 (length table) bytes '())))
	  (bytespec->bytecode spec '() '(tail-call 0))))))

(define (bytespec->bytecode bytespec object-table last-code)
  (let ((stack '()))
    (define (push-code! x)
      (set! stack (cons x stack)))
    (define (object-index x)
      (cond ((object-find object-table x) => cdr)
	    (else #f)))
    (define (dump-table-object! obj+index)
      (let dump! ((x (car obj+index)))
	(cond
	 ((vlink? x)
	  ;; (push-code! `(local-ref ,(object-index (vlink-module x))))
	  (for-each push-code! (object->dump-code (vlink-name x)))
	  (push-code! `(link/current-module)))
	 ;;((vmod? x)
	 ;;  (push-code! `(load-module ,(vmod-id x))))
	 (else
	  (for-each push-code! (object->dump-code x)))))
      (push-code! `(local-set ,(cdr obj+index))))
    (define (dump-object! x)
      (let dump! ((x x))
	(cond
	 ((bytespec? x) (dump-bytecode! x))
	 ((object-index x) => (lambda (i) (push-code! `(local-ref ,i))))
	 (else
	  (error "Cannot dump:" x)))))
    (define (dump-bytecode! spec)
      (let ((nargs (bytespec-nargs spec))
	    (nrest (bytespec-nrest spec))
	    (nlocs (bytespec-nlocs spec))
	    (objs  (bytespec-objs spec)))
	;; dump parameters
	(if (and (< nargs 4) (< nlocs 16))
	    (push-code! (object->code (+ (* nargs 32) (* nrest 16) nlocs)))
	    (begin
	      (push-code! (object->code nargs))
	      (push-code! (object->code nrest))
	      (push-code! (object->code nlocs))
	      (push-code! (object->code #f))))
	;; dump object table
	(cond ((not (null? objs))
	       (for-each dump-object! objs)
	       (push-code! `(vector ,(length objs)))))
	;; dump bytecode
	(push-code! `(load-program ,(bytespec-bytes spec)))))
    ;;
    ;; main
    (for-each dump-table-object! object-table)
    (dump-bytecode! bytespec)
    (push-code! last-code)
    (apply string-append
	   (map code->bytes (map code-pack (reverse! stack))))))

(define (object-find table x)
  ((if (or (vlink? x) (vmod? x)) assoc assq) x table))

(define (build-object-table bytespec)
  (let ((table '()) (index 0))
    (define (insert! x)
      ;; (if (vlink? x) (begin (insert! (vlink-module x))))
      (if (not (object-find table x))
	  (begin
	    (set! table (acons x index table))
	    (set! index (1+ index)))))
    (let loop ((spec bytespec))
      (for-each (lambda (x)
		  (if (bytespec? x) (loop x) (insert! x)))
		(bytespec-objs spec)))
    (reverse! table)))
