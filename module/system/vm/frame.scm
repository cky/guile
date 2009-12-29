;;; Guile VM frame functions

;;; Copyright (C) 2001, 2005, 2009 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system vm frame)
  #:use-module (system base pmatch)
  #:use-module (system vm program)
  #:use-module (system vm instruction)
  #:use-module (system vm objcode)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export (frame-bindings
            frame-lookup-binding
            frame-binding-ref frame-binding-set!
            frame-source frame-call-representation
            frame-environment
            frame-object-binding frame-object-name))

(define (frame-bindings frame)
  (program-bindings-for-ip (frame-procedure frame)
                           (frame-instruction-pointer frame)))

(define (frame-lookup-binding frame var)
  (let lp ((bindings (frame-bindings frame)))
    (cond ((null? bindings)
           (error "variable not bound in frame" var frame))
          ((eq? (binding:name (car bindings)) var)
           (car bindings))
          (else
           (lp (cdr bindings))))))

(define (frame-binding-set! frame var val)
  (frame-local-set! frame
                    (binding:index (frame-lookup-binding frame var))
                    val))

(define (frame-binding-ref frame var)
  (frame-local-ref frame
                   (binding:index (frame-lookup-binding frame var))))


;; Basically there are two cases to deal with here:
;;
;;   1. We've already parsed the arguments, and bound them to local
;;      variables. In a standard (lambda (a b c) ...) call, this doesn't
;;      involve any argument shuffling; but with rest, optional, or
;;      keyword arguments, the arguments as given to the procedure may
;;      not correspond to what's on the stack. We reconstruct the
;;      arguments using e.g. for the case above: `(,a ,b ,c). This works
;;      for rest arguments too: (a b . c) => `(,a ,b . ,c)
;;
;;   2. We have failed to parse the arguments. Perhaps it's the wrong
;;      number of arguments, or perhaps we're doing a typed dispatch and
;;      the types don't match. In that case the arguments are all on the
;;      stack, and nothing else is on the stack.
(define (frame-arguments frame)
  (cond
   ((program-lambda-list (frame-procedure frame)
                         (frame-instruction-pointer frame))
    ;; case 1
    => (lambda (formals)
         (let lp ((formals formals))
           (pmatch formals
             (() '())
             ((,x . ,rest) (guard (symbol? x))
              (cons (frame-binding-ref frame x) (lp rest)))
             ((,x . ,rest)
              ;; could be a keyword
              (cons x (lp rest)))
             (,rest (guard (symbol? rest))
              (frame-binding-ref frame rest))
             ;; let's not error here, as we are called during
             ;; backtraces...
             (else '???)))))
   (else
    ;; case 2
    (map (lambda (i)
           (frame-local-ref frame i))
         (iota (frame-num-locals frame))))))


;;;
;;; Pretty printing
;;;

(define (frame-source frame)
  (program-source (frame-procedure frame)
                  (frame-instruction-pointer frame)))

(define (frame-call-representation frame)
  (let ((p (frame-procedure frame)))
    (cons (or (procedure-name p) p) (frame-arguments frame))))



;;; Misc
;;;

(define (frame-environment frame)
  (map (lambda (binding)
	 (cons (binding:name binding) (frame-binding-ref frame binding)))
       (frame-bindings frame)))

(define (frame-object-binding frame obj)
  (do ((bs (frame-bindings frame) (cdr bs)))
      ((or (null? bs) (eq? obj (frame-binding-ref frame (car bs))))
       (and (pair? bs) (car bs)))))

(define (frame-object-name frame obj)
  (cond ((frame-object-binding frame obj) => binding:name)
	(else #f)))
