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
  #:export (frame-local-ref frame-local-set!
            frame-instruction-pointer
            frame-return-address frame-mv-return-address
            frame-dynamic-link
            frame-num-locals

            frame-bindings frame-binding-ref frame-binding-set!
            ; frame-arguments

            frame-number frame-address
            make-frame-chain
            print-frame print-frame-chain-as-backtrace
            frame-local-variables
            frame-environment
            frame-variable-exists? frame-variable-ref frame-variable-set!
            frame-object-name
            frame-local-ref frame-local-set!
            frame-return-address frame-program
            frame-dynamic-link heap-frame?))

(load-extension "libguile" "scm_init_frames")

(define (frame-bindings frame)
  (map (lambda (b)
         (cons (binding:name b) (binding:index b)))
       (program-bindings-for-ip (frame-procedure frame)
                                (frame-instruction-pointer frame))))

(define (frame-binding-set! frame var val)
  (let ((i (assq-ref (frame-bindings frame) var)))
    (if i
        (frame-local-set! frame i val)
        (error "variable not bound in frame" var frame))))

(define (frame-binding-ref frame var)
  (let ((i (assq-ref (frame-bindings frame) var)))
    (if i
        (frame-local-ref frame i)
        (error "variable not bound in frame" var frame))))

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
;;; Frame chain
;;;

(define frame-number (make-object-property))
(define frame-address (make-object-property))

;; FIXME: the header.
(define (bootstrap-frame? frame)
  (let ((code (objcode->bytecode (program-objcode (frame-program frame)))))
    (and (= (uniform-vector-ref code (1- (uniform-vector-length code)))
            (instruction->opcode 'halt)))))

(define (make-frame-chain frame addr)
  (define (make-rest)
    (make-frame-chain (frame-dynamic-link frame)
                      (frame-return-address frame)))
  (cond
   ((or (eq? frame #t) (eq? frame #f))
    ;; handle #f or #t dynamic links
    '())
   ((bootstrap-frame? frame)
    (make-rest))
   (else
    (let ((chain (make-rest)))
      (set! (frame-number frame) (length chain))
      (set! (frame-address frame)
            (- addr (program-base (frame-program frame))))
      (cons frame chain)))))


;;;
;;; Pretty printing
;;;

(define (frame-line-number frame)
  (let ((addr (frame-address frame)))
    (cond ((assv addr (program-sources (frame-program frame)))
           => source:line)
          (else (format #f "@~a" addr)))))

(define (frame-file frame prev)
  (let ((sources (program-sources (frame-program frame))))
    (if (null? sources)
        prev
        (or (source:file (car sources))
            "current input"))))

(define (print-frame frame)
  (format #t "~4@a: ~a   ~s\n" (frame-line-number frame) (frame-number frame)
          (frame-call-representation frame)))


(define (frame-call-representation frame)
  (define (abbrev x)
    (cond ((list? x)
           (if (> (length x) 4)
               (list (abbrev (car x)) (abbrev (cadr x)) '...)
               (map abbrev x)))
	  ((pair? x)
           (cons (abbrev (car x)) (abbrev (cdr x))))
	  ((vector? x)
           (case (vector-length x)
             ((0) x)
             ((1) (vector (abbrev (vector-ref x 0))))
             (else (vector (abbrev (vector-ref x 0)) '...))))
	  (else x)))
  (abbrev (cons (frame-program-name frame) (frame-arguments frame))))

(define (print-frame-chain-as-backtrace frames)
  (if (null? frames)
      (format #t "No backtrace available.\n")
      (begin
        (format #t "VM backtrace:\n")
        (fold (lambda (frame file)
                (let ((new-file (frame-file frame file)))
                  (if (not (equal? new-file file))
                      (format #t "In ~a:\n" new-file))
                  (print-frame frame)
                  new-file))
              'no-file
              frames))))

(define (frame-program-name frame)
  (let ((prog (frame-program frame))
	(link (frame-dynamic-link frame)))
    (or (program-name prog)
        (object-property prog 'name)
        (and (heap-frame? link) (frame-address link)
             (frame-object-name link (1- (frame-address link)) prog))
	(hash-fold (lambda (s v d) (if (and (variable-bound? v)
                                            (eq? prog (variable-ref v)))
                                       s d))
		   prog (module-obarray (current-module))))))


;;; Frames
;;;

(define (frame-local-variables frame)
  (let* ((prog (frame-program frame))
	 (arity (program-arity prog)))
    (do ((n (+ (arity:nargs arity) (arity:nlocs arity) -1) (1- n))
	 (l '() (cons (frame-local-ref frame n) l)))
	((< n 0) l))))

(define (frame-lookup-binding frame addr sym)
  (assq sym (reverse (frame-bindings frame addr))))

(define (frame-object-binding frame addr obj)
  (do ((bs (frame-bindings frame addr) (cdr bs)))
      ((or (null? bs) (eq? obj (frame-binding-ref frame (car bs))))
       (and (pair? bs) (car bs)))))

(define (frame-environment frame addr)
  (map (lambda (binding)
	 (cons (binding:name binding) (frame-binding-ref frame binding)))
       (frame-bindings frame addr)))

(define (frame-variable-exists? frame addr sym)
  (if (frame-lookup-binding frame addr sym) #t #f))

(define (frame-variable-ref frame addr sym)
  (cond ((frame-lookup-binding frame addr sym) =>
	 (lambda (binding) (frame-binding-ref frame binding)))
	(else (error "Unknown variable:" sym))))

(define (frame-variable-set! frame addr sym val)
  (cond ((frame-lookup-binding frame addr sym) =>
	 (lambda (binding) (frame-binding-set! frame binding val)))
	(else (error "Unknown variable:" sym))))

(define (frame-object-name frame addr obj)
  (cond ((frame-object-binding frame addr obj) => binding:name)
	(else #f)))
