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

(define-record (<vm-asm> venv glil body))
(define-record (<venv> parent nexts closure?))
(define-record (<vmod> id))
(define-record (<vlink> module name))
(define-record (<bytespec> vars bytes meta objs closure?))


;;;
;;; Stage 1: Preprocess
;;;

(define (preprocess x e)
  (match x
    (($ <glil-asm> vars body)
     (let* ((venv (<venv> :parent e :nexts vars.nexts :closure? #f))
	    (body (map (lambda (x) (preprocess x venv)) body)))
       (<vm-asm> :venv venv :glil x :body body)))
    (($ <glil-external> op depth index)
     (do ((d depth (1- d))
	  (e e e.parent))
	 ((= d 0))
       (set! e.closure? #t))
     x)
    (else x)))


;;;
;;; Stage 2: Bytecode generation
;;;

(define (codegen glil toplevel)
  (match glil
    (($ <vm-asm> venv ($ <glil-asm> vars _) body)
     (let ((stack '())
	   (binding-alist '())
	   (source-alist '())
	   (label-alist '())
	   (object-alist '()))
       (define (push-code! code)
	 (set! stack (cons (code->bytes code) stack)))
       (define (push-object! x)
	 (cond ((object->code x) => push-code!)
	       (toplevel (dump-object! push-code! x))
	       (else
		(let ((i (cond ((object-assoc x object-alist) => cdr)
			       (else
				(let ((i (length object-alist)))
				  (set! object-alist (acons x i object-alist))
				  i)))))
		  (push-code! `(object-ref ,i))))))
       (define (current-address)
	 (define (byte-length x)
	   (cond ((string? x) (string-length x))
		 (else 3)))
	 (apply + (map byte-length stack)))
       (define (generate-code x)
	 (match x
	   (($ <vm-asm> venv)
	    (push-object! (codegen x #f))
	    (if venv.closure? (push-code! `(make-closure))))

	   (($ <glil-bind> binds)
	    (let ((bindings
		   (map (lambda (v)
			  (let ((name (car v)) (type (cadr v)) (i (caddr v)))
			    (case type
			      ((argument) (make-binding name #f i))
			      ((local) (make-binding name #f (+ vars.nargs i)))
			      ((external) (make-binding name #t i)))))
			binds)))
	      (set! binding-alist
		    (acons (current-address) bindings binding-alist))))

	   (($ <glil-unbind>)
	    (set! binding-alist (acons (current-address) #f binding-alist)))

	   (($ <glil-source> loc)
	    (set! source-alist (acons (current-address) loc source-alist)))

	   (($ <glil-void>)
	    (push-code! '(void)))

	   (($ <glil-const> x)
	    (push-object! x))

	   (($ <glil-argument> op index)
	    (if (eq? op 'ref)
		(push-code! `(local-ref ,index))
		(push-code! `(local-set ,index))))

	   (($ <glil-local> op index)
	    (if (eq? op 'ref)
		(push-code! `(local-ref ,(+ vars.nargs index)))
		(push-code! `(local-set ,(+ vars.nargs index)))))

	   (($ <glil-external> op depth index)
	    (do ((e venv e.parent)
		 (d depth (1- d))
		 (n 0 (+ n e.nexts)))
		((= d 0)
		 (if (eq? op 'ref)
		     (push-code! `(external-ref ,(+ n index)))
		     (push-code! `(external-set ,(+ n index)))))))

	   (($ <glil-module> op module name)
	    (push-object! (<vlink> :module #f :name name))
	    (if (eq? op 'ref)
		(push-code! '(variable-ref))
		(push-code! '(variable-set))))

	   (($ <glil-label> label)
	    (set! label-alist (assq-set! label-alist label (current-address))))

	   (($ <glil-branch> inst label)
	    (set! stack (cons (list inst label) stack)))

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
       (let ((bytes (stack->bytes (reverse! stack) label-alist)))
	 (if toplevel
	     (bytecode->objcode bytes vars.nlocs vars.nexts)
	     (<bytespec> :vars vars :bytes bytes
			 :meta (if (and (null? binding-alist)
					(null? source-alist))
				 #f
				 (cons (reverse! binding-alist)
				       (reverse! source-alist)))
			 :objs (let ((objs (map car (reverse! object-alist))))
				 (if (null? objs) #f (list->vector objs)))
			 :closure? venv.closure?)))))))

(define (object-assoc x alist)
  (match x
    (($ <vlink>) (assoc x alist))
    (else        (assq x alist))))

(define (stack->bytes stack label-alist)
  (let loop ((result '()) (stack stack) (addr 0))
    (if (null? stack)
	(apply string-append (reverse! result))
	(let ((bytes (car stack)))
	  (if (pair? bytes)
	      (let* ((offset (- (assq-ref label-alist (cadr bytes))
				(+ addr 3)))
		     (n (if (< offset 0) (+ offset 65536) offset)))
		(set! bytes (code->bytes (list (car bytes)
					       (quotient n 256)
					       (modulo n 256))))))
	  (loop (cons bytes result)
		(cdr stack)
		(+ addr (string-length bytes)))))))


;;;
;;; Object dump
;;;

;; NOTE: undumpped in vm_load.c.

(define (dump-object! push-code! x)
  (let dump! ((x x))
    (cond
     ((object->code x) => push-code!)
     (else
      (match x
	(($ <bytespec> vars bytes meta objs closure?)
	 ;; dump parameters
	 (let ((nargs vars.nargs) (nrest vars.nrest)
	       (nlocs vars.nlocs) (nexts vars.nexts))
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
	      (push-code! (object->code #f)))))
	 ;; dump object table
	 (if objs (dump! objs))
	 ;; dump meta data
	 (if meta (dump! meta))
	 ;; dump bytecode
	 (push-code! `(load-program ,bytes)))
	(($ <vlink> module name)
	 ;; FIXME: dump module
	 (push-code! `(link ,(symbol->string name))))
	(($ <vmod> id)
	 (push-code! `(load-module ,id)))
	((and ($ integer) ($ exact))
	 (let ((str (do ((n x (quotient n 256))
			 (l '() (cons (modulo n 256) l)))
			((= n 0)
			 (list->string (map integer->char l))))))
	   (push-code! `(load-integer ,str))))
	(($ number)
	 (push-code! `(load-number ,(number->string x))))
	(($ string)
	 (push-code! `(load-string ,x)))
	(($ symbol)
	 (push-code! `(load-symbol ,(symbol->string x))))
	(($ keyword)
	 (push-code! `(load-keyword
		       ,(symbol->string (keyword-dash-symbol x)))))
	(($ list)
	 (for-each dump! x)
	 (push-code! `(list ,(length x))))
	(($ pair)
	 (dump! (car x))
	 (dump! (cdr x))
	 (push-code! `(cons)))
	(($ vector)
	 (for-each dump! (vector->list x))
	 (push-code! `(vector ,(vector-length x))))
	(else
	 (error "Cannot dump:" x)))))))
