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
  :use-module (ice-9 match)
  :export
  (pprint-glil
   make-<glil-asm> <glil-asm>?
   <glil-asm>-1 <glil-asm>-2 <glil-asm>-3 <glil-asm>-4 <glil-asm>-5
   make-<glil-vars> <glil-vars>? <glil-vars>-1 <glil-vars>-2

   make-<glil-void> <glil-void>?
   make-<glil-const> <glil-const>? <glil-const>-1

   make-<glil-argument> <glil-argument>? <glil-argument>-1 <glil-argument>-2
   make-<glil-local> <glil-local>? <glil-local>-1 <glil-local>-2
   make-<glil-external> <glil-external>?
   <glil-external>-1 <glil-external>-2 <glil-external>-3
   make-<glil-module> <glil-module>?
   <glil-module>-1 <glil-module>-2 <glil-module>-3

   make-<glil-label> <glil-label>? <glil-label>-1
   make-<glil-branch> <glil-branch>? <glil-branch>-1 <glil-branch>-2
   make-<glil-call> <glil-call>? <glil-call>-1 <glil-call>-2
   make-<glil-inst> <glil-inst>? <glil-inst>-1
   ))

;; Meta operations
(define-structure (<glil-asm> nargs nrest nlocs nexts body))
(define-structure (<glil-vars> type syms))

;; Constants
(define-structure (<glil-void>))
(define-structure (<glil-const> obj))

;; Variables
(define-structure (<glil-argument> op index))
(define-structure (<glil-local> op index))
(define-structure (<glil-external> op depth index))
(define-structure (<glil-module> op module name))

;; Controls
(define-structure (<glil-label> label))
(define-structure (<glil-branch> inst label))
(define-structure (<glil-call> inst n))
(define-structure (<glil-inst> inst))


;;;
;;; Parser
;;;

;; FIXME: This is not working now

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
    (($ <glil-asm> nargs nrest nlocs nexts body)
     `(@asm (,nargs ,nrest ,nlocs ,nexts) ,@(map unparse body)))
    (($ <glil-vars> type syms) `(,type ,@syms))
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
    (($ <glil-label> label) `(label ,label))
    (($ <glil-branch> inst label) `(,inst ,label))
    (($ <glil-call> inst n) `(,inst ,n))
    (($ <glil-inst> inst) `(,inst))))


;;;
;;; Printer
;;;

(define (pprint-glil glil)
  (let print ((code (unparse glil)) (column 0))
    (display (make-string column #\space))
    (case (car code)
      ((@asm)
       (format #t "(@asm ~A\n" (cadr code))
       (let ((col (+ column 2)))
	 (let loop ((l (cddr code)))
	   (print (car l) col)
	   (if (null? (cdr l))
	       (display ")")
	       (begin (newline) (loop (cdr l)))))))
      (else (write code))))
  (newline))
