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
  :use-module (system vm instruction)
  :use-module (system vm objcode)
  :use-module ((system vm program) :select (make-binding))
  :use-module (system vm conv)
  :use-module (ice-9 regex)
  :use-module (ice-9 common-list)
  :use-module (srfi srfi-4)
  :use-module ((srfi srfi-1) :select (append-map))
  :export (preprocess codegen assemble))

(define (assemble glil env . opts)
  (codegen (preprocess glil #f) #t))


;;;
;;; Types
;;;

(define-record (<vm-asm> venv glil body))
(define-record (<venv> parent nexts closure?))
(define-record (<vlink-now> name))
(define-record (<vlink-later> module name))
(define-record (<vdefine> module name))
(define-record (<bytespec> vars bytes meta objs closure?))


;;;
;;; Stage 1: Preprocess
;;;

(define (preprocess x e)
  (record-case x
    ((<glil-asm> vars meta body)
     (let* ((venv (make-venv :parent e :nexts (glil-vars-nexts vars) :closure? #f))
	    (body (map (lambda (x) (preprocess x venv)) body)))
       (make-vm-asm :venv venv :glil x :body body)))
    ((<glil-external> op depth index)
     (do ((d depth (- d 1))
 	  (e e (venv-parent e)))
 	 ((= d 0))
       (set! (venv-closure? e) #t))
     x)
    (else x)))


;;;
;;; Stage 2: Bytecode generation
;;;

(define (codegen glil toplevel)
  (record-case glil
    ((<vm-asm> venv glil body) (record-case glil ((<glil-asm> vars meta) ; body?
     (let ((stack '())
	   (binding-alist '())
	   (source-alist '())
	   (label-alist '())
	   (object-alist '()))
       (define (push-code! code)
;	 (format #t "push-code! ~a~%" code)
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
	   (cond ((u8vector? x) (u8vector-length x))
		 (else 3)))
	 (apply + (map byte-length stack)))
       (define (generate-code x)
	 (record-case x
	   ((<vm-asm> venv)
	    (push-object! (codegen x #f))
	    (if (venv-closure? venv) (push-code! `(make-closure))))

	   ((<glil-bind> (binds vars))
	    (let ((bindings
		   (map (lambda (v)
			  (let ((name (car v)) (type (cadr v)) (i (caddr v)))
			    (case type
			      ((argument) (make-binding name #f i))
			      ((local) (make-binding name #f (+ (glil-vars-nargs vars) i)))
			      ((external) (make-binding name #t i)))))
			binds)))
	      (set! binding-alist
		    (acons (current-address) bindings binding-alist))))

	   ((<glil-unbind>)
	    (set! binding-alist (acons (current-address) #f binding-alist)))

	   ((<glil-source> loc)
	    (set! source-alist (acons (current-address) loc source-alist)))

	   ((<glil-void>)
	    (push-code! '(void)))

	   ((<glil-const> obj)
	    (push-object! obj))

	   ((<glil-argument> op index)
	    (if (eq? op 'ref)
		(push-code! `(local-ref ,index))
		(push-code! `(local-set ,index))))

	   ((<glil-local> op index)
	    (if (eq? op 'ref)
		(push-code! `(local-ref ,(+ (glil-vars-nargs vars) index)))
		(push-code! `(local-set ,(+ (glil-vars-nargs vars) index)))))

	   ((<glil-external> op depth index)
	    (do ((e venv (venv-parent e))
		 (d depth (1- d))
		 (n 0 (+ n (venv-nexts e))))
		((= d 0)
		 (if (eq? op 'ref)
		     (push-code! `(external-ref ,(+ n index)))
		     (push-code! `(external-set ,(+ n index)))))))

	   ((<glil-module> op module name)
            (case op
              ((ref set)
               (cond
                (toplevel
                 (push-object! (make-vlink-now :name name))
                 (push-code! (case op
                               ((ref) '(variable-ref))
                               ((set) '(variable-set)))))
                (else
                 (let* ((var (make-vlink-later :module module :name name))
                        (i (cond ((object-assoc var object-alist) => cdr)
                                 (else
                                  (let ((i (length object-alist)))
                                    (set! object-alist (acons var i object-alist))
                                    i)))))
                   (push-code! (case op
                                 ((ref) `(late-variable-ref ,i))
                                 ((set) `(late-variable-set ,i))))))))
              ((define)
               (push-object! (make-vdefine :module module :name name))
               (push-code! '(variable-set)))
              (else
               (error "unknown toplevel var kind" op name))))

	   ((<glil-label> label)
	    (set! label-alist (assq-set! label-alist label (current-address))))

	   ((<glil-branch> inst label)
	    (set! stack (cons (list inst label) stack)))

	   ((<glil-call> inst nargs)
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
;       (format #t "codegen: stack = ~a~%" (reverse stack))
       (let ((bytes (stack->bytes (reverse! stack) label-alist)))
	 (if toplevel
	     (bytecode->objcode bytes (glil-vars-nlocs vars) (glil-vars-nexts vars))
	     (make-bytespec :vars vars :bytes bytes
                            :meta (if (and (null? binding-alist)
                                           (null? source-alist)
                                           (null? meta))
                                      #f
                                      (cons* (reverse! binding-alist)
                                             (reverse! source-alist)
                                             meta))
                            :objs (let ((objs (map car (reverse! object-alist))))
                                    (if (null? objs) #f (list->vector objs)))
                            :closure? (venv-closure? venv))))))))))

(define (object-assoc x alist)
  (record-case x
    ((<vlink-now>) (assoc x alist))
    ((<vlink-later>) (assoc x alist))
    (else        (assq x alist))))

(define (stack->bytes stack label-alist)
  (let loop ((result '()) (stack stack) (addr 0))
    (if (null? stack)
	(list->u8vector(append-map u8vector->list
                                   (reverse! result)))
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
		(+ addr (u8vector-length bytes)))))))


;;;
;;; Object dump
;;;

;; NOTE: undumpped in vm_system.c

(define (dump-object! push-code! x)
  (define (too-long x)
    (error (string-append x " too long")))

  (let dump! ((x x))
    (cond
     ((object->code x) => push-code!)
     ((record? x)
      (record-case x
	((<bytespec> vars bytes meta objs closure?)
	 ;; dump parameters
	 (let ((nargs (glil-vars-nargs vars)) (nrest (glil-vars-nrest vars))
	       (nlocs (glil-vars-nlocs vars)) (nexts (glil-vars-nexts vars)))
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
	((<vlink-later> module name)
         (dump! (module-name module))
         (dump! name)
	 (push-code! '(link-later)))
	((<vlink-now> name)
         (dump! name)
	 (push-code! '(link-now)))
	((<vdefine> module name)
	 ;; FIXME: dump module
	 (push-code! `(define ,(symbol->string name))))
        (else
         (error "assemble: unknown record type" (record-type-descriptor x)))))
     ((and (integer? x) (exact? x))
      (let ((str (do ((n x (quotient n 256))
                      (l '() (cons (modulo n 256) l)))
                     ((= n 0)
                      (apply u8vector l)))))
        (push-code! `(load-integer ,str))))
     ((number? x)
      (push-code! `(load-number ,(number->string x))))
     ((string? x)
      (push-code! `(load-string ,x)))
     ((symbol? x)
      (push-code! `(load-symbol ,(symbol->string x))))
     ((keyword? x)
      (push-code! `(load-keyword ,(symbol->string (keyword->symbol x)))))
     ((list? x)
      (for-each dump! x)
      (let ((len (length x)))
        (if (>= len 65536) (too-long 'list))
        (push-code! `(list ,(quotient len 256) ,(modulo len 256)))))
     ((pair? x)
      (dump! (car x))
      (dump! (cdr x))
      (push-code! `(cons)))
     ((vector? x)
      (for-each dump! (vector->list x))
      (let ((len (vector-length x)))
        (if (>= len 65536) (too-long 'vector))
        (push-code! `(vector ,(quotient len 256) ,(modulo len 256)))))
     (else
      (error "assemble: unrecognized object" x)))))
