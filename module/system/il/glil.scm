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
  :use-module (ice-9 match)
  :export
  (pprint-glil
   <glil-vars>
   <glil-asm> <glil-asm>?
   <glil-asm>-1 <glil-asm>-2 <glil-asm>-3 <glil-asm>-4 <glil-asm>-5
   <glil-bind> <glil-bind>? <glil-bind>-1
   <glil-unbind> <glil-unbind>?
   <glil-source> <glil-source>? <glil-source>-1 <glil-source>-2

   <glil-void> <glil-void>?
   <glil-const> <glil-const>? <glil-const>-1

   <glil-argument> <glil-argument>? <glil-argument>-1 <glil-argument>-2
   <glil-local> <glil-local>? <glil-local>-1 <glil-local>-2
   <glil-external> <glil-external>?
   <glil-external>-1 <glil-external>-2 <glil-external>-3
   <glil-module> <glil-module>?
   <glil-module>-1 <glil-module>-2 <glil-module>-3

   <glil-label> <glil-label>? <glil-label>-1
   <glil-branch> <glil-branch>? <glil-branch>-1 <glil-branch>-2
   <glil-call> <glil-call>? <glil-call>-1 <glil-call>-2
   ))

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
  (match glil
    ;; meta
    (($ <glil-asm> vars body)
     `(@asm (,vars.nargs ,vars.nrest ,vars.nlocs ,vars.nexts)
	    ,@(map unparse body)))
    (($ <glil-bind> vars) `(@bind ,@vars))
    (($ <glil-unbind>) `(@unbind))
    (($ <glil-source> loc) `(@source ,(car loc) ,(cdr loc)))
    ;; constants
    (($ <glil-void>) `(void))
    (($ <glil-const> obj) `(const ,obj))
    ;; variables
    (($ <glil-argument> op index)
     `(,(symbol-append 'argument- op) ,index))
    (($ <glil-local> op index)
     `(,(symbol-append 'local- op) ,index))
    (($ <glil-external> op depth index)
     `(,(symbol-append 'external- op) ,depth ,index))
    (($ <glil-module> op module name)
     `(,(symbol-append 'module- op) ,module ,name))
    ;; controls
    (($ <glil-label> label) label)
    (($ <glil-branch> inst label) `(,inst ,label))
    (($ <glil-call> inst nargs) `(,inst ,nargs))))


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
