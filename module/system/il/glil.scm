;;; Guile Low Intermediate Language

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

(define-module (system il glil)
  :use-syntax (system base syntax)
  :export
  (pprint-glil
   <glil-vars> make-glil-vars
   glil-vars-nargs glil-vars-nrest glil-vars-nlocs glil-vars-nexts

   <glil-asm> make-glil-asm glil-asm?
   glil-asm-vars glil-asm-body

   <glil-bind> make-glil-bind glil-bind?
   glil-bind-vars

   <glil-unbind> make-glil-unbind glil-unbind?

   <glil-source> make-glil-source glil-source?
   glil-source-loc

   <glil-void> make-glil-void glil-void?

   <glil-const> make-glil-const glil-const?
   glil-const-obj

   <glil-argument> make-glil-argument glil-argument?
   glil-argument-op glil-argument-index

   <glil-local> make-glil-local glil-local?
   glil-local-op glil-local-index

   <glil-external> make-glil-external glil-external?
   glil-external-op glil-external-depth glil-external-index

   <glil-module> make-glil-module glil-module?
   glil-module-op glil-module-module glil-module-index

   <glil-late-bound> make-glil-late-bound glil-late-bound?
   glil-late-bound-op glil-late-bound-name

   <glil-label> make-glil-label glil-label?
   glil-label-label

   <glil-branch> make-glil-branch glil-branch?
   glil-branch-int glil-branch-label

   <glil-call> make-glil-call glil-call?
   glil-call-int glil-call-nargs))

(define-record (<glil-vars> nargs nrest nlocs nexts))

(define-type <glil>
  (|
   ;; Meta operations
   (<glil-asm> vars body)
   (<glil-bind> vars)
   (<glil-unbind>)
   (<glil-source> loc)
   ;; Objects
   (<glil-void>)
   (<glil-const> obj)
   ;; Variables
   (<glil-argument> op index)
   (<glil-local> op index)
   (<glil-external> op depth index)
   (<glil-module> op module name)
   (<glil-late-bound> op name)
   ;; Controls
   (<glil-label> label)
   (<glil-branch> inst label)
   (<glil-call> inst nargs)))


;;;
;;; Parser
;;;

;;; (define (parse-glil x)
;;;   (match x
;;;     (('@asm args . body)
;;;      (let* ((env (make-new-env e))
;;; 	    (args (parse-args args env)))
;;;        (make-asm env args (map-parse body env))))
;;;     (else
;;;      (error "Invalid assembly code:" x))))
;;; 
;;; (define (parse-args x e)
;;;   (let ((args (cond ((symbol? x) (make-args (list (make-local-var x)) #t))
;;; 		    ((list? x) (make-args (map make-local-var x) #f))
;;; 		    (else (let loop ((l x) (v '()))
;;; 			    (if (pair? l)
;;; 				(loop (cdr l) (cons (car l) v))
;;; 				(make-args (map make-local-var
;;; 						(reverse! (cons l v)))
;;; 					   #t)))))))
;;;     (for-each (lambda (v) (env-add! e v)) (args-vars args))
;;;     args))
;;; 
;;; (define (map-parse x e)
;;;   (map (lambda (x) (parse x e)) x))
;;; 
;;; (define (parse x e)
;;;   (match x
;;;     ;; (@asm ARGS BODY...)
;;;     (('@asm args . body)
;;;      (parse-asm x e))
;;;     ;; (@bind VARS BODY...)
;;;     ;; (@block VARS BODY...)
;;;     (((or '@bind '@block) vars . body)
;;;      (let* ((offset (env-nvars e))
;;; 	    (vars (args-vars (parse-args vars e)))
;;; 	    (block (make-block (car x) offset vars (map-parse body e))))
;;;        (for-each (lambda (v) (env-remove! e)) vars)
;;;        block))
;;;     ;; (void)
;;;     (('void)
;;;      (make-void))
;;;     ;; (const OBJ)
;;;     (('const obj)
;;;      (make-const obj))
;;;     ;; (ref NAME)
;;;     ;; (set NAME)
;;;     (((or 'ref 'set) name)
;;;      (make-access (car x) (env-ref e name)))
;;;     ;; (label LABEL)
;;;     (('label label)
;;;      (make-label label))
;;;     ;; (br-if LABEL)
;;;     ;; (jump LABEL)
;;;     (((or 'br-if 'jump) label)
;;;      (make-instl (car x) label))
;;;     ;; (call NARGS)
;;;     ;; (tail-call NARGS)
;;;     (((or 'call 'tail-call) n)
;;;      (make-instn (car x) n))
;;;     ;; (INST)
;;;     ((inst)
;;;      (if (instruction? inst)
;;; 	 (make-inst inst)
;;; 	 (error "Unknown instruction:" inst)))))


;;;
;;; Unparser
;;;

(define (unparse glil)
  (record-case glil
    ;; meta
    ((<glil-asm> vars body)
     `(@asm (,(glil-vars-nargs vars) ,(glil-vars-nrest vars)
             ,(glil-vars-nlocs vars) ,(glil-vars-nexts vars))
	    ,@(map unparse body)))
    ((<glil-bind> vars) `(@bind ,@vars))
    ((<glil-unbind>) `(@unbind))
    ((<glil-source> loc) `(@source ,(car loc) ,(cdr loc)))
    ;; constants
    ((<glil-void>) `(void))
    ((<glil-const> obj) `(const ,obj))
    ;; variables
    ((<glil-argument> op index)
     `(,(symbol-append 'argument- op) ,index))
    ((<glil-local> op index)
     `(,(symbol-append 'local- op) ,index))
    ((<glil-external> op depth index)
     `(,(symbol-append 'external- op) ,depth ,index))
    ((<glil-module> op module name)
     `(,(symbol-append 'module- op) ,module ,name))
    ;; controls
    ((<glil-label> label) label)
    ((<glil-branch> inst label) `(,inst ,label))
    ((<glil-call> inst nargs) `(,inst ,nargs))))


;;;
;;; Printer
;;;

(define (pprint-glil glil . port)
  (let ((port (if (pair? port) (car port) (current-output-port))))
    (let print ((code (unparse glil)) (column 0))
      (display (make-string column #\space) port)
      (cond ((and (pair? code) (eq? (car code) '@asm))
	     (format port "(@asm ~A\n" (cadr code))
	     (let ((col (+ column 2)))
	       (let loop ((l (cddr code)))
		 (print (car l) col)
		 (if (null? (cdr l))
		   (display ")" port)
		   (begin (newline port) (loop (cdr l)))))))
	    (else (write code port))))
    (newline port)))
