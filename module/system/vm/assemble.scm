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
       (define (current-address) (length stack))
       (define (push-code! code)
	 (set! stack (optimizing-push code stack)))
       (define (object-index obj)
	 (cond ((assq-ref object-alist obj))
	       (else (let ((index (length object-alist)))
		       (set! object-alist (acons obj index object-alist))
		       index))))
       (define (label-ref key)
	 (assq-ref label-alist key))
       (define (label-set key pos)
	 (set! label-alist (assq-set! label-alist key pos)))
       (define (generate-code x)
	 (match x
	   (($ <vm-asm> env)
	    (push-code! `(object-ref ,(object-index (codegen x #f))))
	    (if (venv-closure? env) (push-code! `(make-closure))))

	   (($ <glil-void>)
	    (push-code! `(void)))

	   (($ <glil-const> x)
	    (if toplevel
		(for-each push-code! (object->dump-code x))
		(cond ((object->code x) => push-code!)
		      (else (push-code! `(object-ref ,(object-index x)))))))

	   (($ <glil-argument> op index)
	    (push-code! (list (symbol-append 'local- op)
			      (- nvars index))))

	   (($ <glil-local> op index)
	    (push-code! (list (symbol-append 'local- op)
			      (- nvars (+ nargs index)))))

	   (($ <glil-external> op depth index)
	    (do ((e venv (venv-parent e))
		 (d depth (1- d))
		 (i 0 (+ i (venv-nexts e))))
		((= d 0)
		 (push-code! (list (symbol-append 'external- op)
				   (+ index i))))))

	   (($ <glil-module> op module name)
	    (if toplevel
		(begin
		  ;; (push-code! `(load-module ,module))
		  (push-code! `(load-symbol ,name))
		  (push-code! `(link/current-module)))
		;; (let ((vlink (make-vlink (make-vmod module) name)))
		(let ((vlink (make-vlink #f name)))
		  (push-code! `(object-ref ,(object-index vlink)))))
	    (push-code! (list (symbol-append 'variable- op))))

	   (($ <glil-label> label)
	    (label-set label (current-address)))

	   (($ <glil-branch> inst label)
	    (let ((setter (lambda (addr) (- (label-ref label) (1+ addr)))))
	      (push-code! (list inst setter))))

	   (($ <glil-call> inst n)
	    (push-code! (list inst n)))

	   (($ <glil-inst> inst)
	    (if (instruction? inst)
		(push-code! (list inst))
		(error "Unknown instruction:" inst)))))
       ;;
       ;; main
       (if (> nexts 0) (push-code! `(external ,nexts)))
       (for-each generate-code body)
       (let ((bytes (code->bytes
		     (map/index (lambda (v n) (if (procedure? v) (v n) v))
				(reverse! stack))))
	     (objs (map car (reverse! object-alist))))
	 (make-bytespec nargs nrest nlocs bytes objs))))))

(define (map/index f l)
  (do ((n 0 (1+ n))
       (l l (cdr l))
       (r '() (cons (f (car l) n) r)))
      ((null? l) (reverse! r))))

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
    (cond ((and alist (pair? stack) (assq-ref alist (car stack))) =>
	   (lambda (inst) (append! (reverse! (cons inst (cdr code)))
				   (cdr stack))))
	  (else (append! (reverse! (code-finalize code)) stack)))))


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
	  (push-code! `(load-symbol ,(vlink-name x)))
	  (push-code! `(link/current-module)))
	 ((vmod? x)
	  (push-code! `(load-module ,(vmod-id x))))
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
	(if (and (null? objs) (< nargs 4) (< nlocs 16))
	    ;; zero-object encoding
	    (push-code! (object->code (+ (* nargs 32) (* nrest 16) nlocs)))
	    (begin
	      ;; dump parameters
	      (push-code! (object->code nargs))
	      (push-code! (object->code nrest))
	      (push-code! (object->code nlocs))
	      ;; dump object table
	      (cond ((null? objs) (push-code! (object->code #f)))
		    (else
		     (push-code! `(mark))
		     (for-each dump-object! objs)
		     (push-code! `(vector))))))
	;; dump bytecode
	(push-code! `(load-program ,(bytespec-bytes spec)))))
    ;;
    ;; main
    (for-each dump-table-object! object-table)
    (dump-bytecode! bytespec)
    (push-code! last-code)
    (code->bytes (apply append! (map code-finalize (reverse! stack))))))

;; object table

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

;; code generation

(define (code-finalize code)
  (match code
    ((inst (? symbol? s))
     (let ((str (symbol->string s)))
       `(,inst ,(string-length str) ,str)))
    ((inst (? string? s))
     `(,inst ,(string-length s) ,s))
    (else (code-pack code))))

(define (integer->string n) (make-string 1 (integer->char n)))

(define (length->string len)
  (define C integer->char)
  (list->string
   (cond ((< len 254) (list (C len)))
	 ((< len 65536)
	  (list (C 254) (C (quotient len 256)) (C (modulo len 256))))
	 ((< len most-positive-fixnum)
	  (list (C 255)
		(C (quotient len (* 256 256 256)))
		(C (modulo (quotient len (* 256 256)) 256))
		(C (modulo (quotient len 256) 256))
		(C (modulo len 256))))
	 (else (error "Too long" len)))))

(define (code->bytes code)
  (let* ((code (list->vector code))
	 (size (vector-length code)))
    (let loop ((i 0))
      (if (>= i size)
	  (apply string-append (vector->list code))
	  (let ((inst (vector-ref code i)))
	    (if (not (instruction? inst))
		(error "Unknown instruction:" inst))
	    (vector-set! code i (integer->string (instruction->opcode inst)))
	    (let ((bytes (instruction-length inst)))
	      (cond ((< bytes 0)
		     (vector-set! code i
				  (integer->string (instruction->opcode inst)))
		     (vector-set! code (+ i 1)
				  (length->string (vector-ref code (1+ i))))
		     (loop (+ i 3)))
		    ((= bytes 0) (loop (+ i 1)))
		    (else
		     (let ((end (+ i 1 bytes)))
		       (do ((j (+ i 1) (1+ j)))
			   ((= j end) (loop end))
			 (vector-set! code j (integer->string
					      (vector-ref code j)))))))))))))
