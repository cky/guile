;;; Guile VM frame functions

;;; Copyright (C) 2001 Free Software Foundation, Inc.
;;; Copyright (C) 2005 Ludovic Courtès  <ludovic.courtes@laas.fr>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;;; Code:

(define-module (system vm frame)
  :use-module (system vm program)
  :use-module (system vm instruction)
  :use-module ((srfi srfi-1) :select (fold))
  :export (frame-number frame-address
           make-frame-chain
           print-frame print-frame-chain-as-backtrace
           frame-arguments frame-local-variables frame-external-variables
           frame-environment
           frame-variable-exists? frame-variable-ref frame-variable-set!
           frame-object-name
           frame-local-ref frame-external-link frame-local-set!
           frame-return-address frame-program
           frame-dynamic-link heap-frame?))

;; fixme: avoid the dynamic-call?
(dynamic-call "scm_init_frames" (dynamic-link "libguile"))

;;;
;;; Frame chain
;;;

(define frame-number (make-object-property))
(define frame-address (make-object-property))

(define (bootstrap-frame? frame)
  (let ((code (program-bytecode (frame-program frame))))
    (and (= (uniform-vector-length code) 3)
         (= (uniform-vector-ref code 2)
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
  (abbrev (cons (program-name frame) (frame-arguments frame))))

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

(define (program-name frame)
  (let ((prog (frame-program frame))
	(link (frame-dynamic-link frame)))
    (or (object-property prog 'name)
        (and (heap-frame? link) (frame-address link)
             (frame-object-name link (1- (frame-address link)) prog))
	(hash-fold (lambda (s v d) (if (eq? prog (variable-ref v)) s d))
		   prog (module-obarray (current-module))))))


;;;
;;; Frames
;;;

(define (frame-arguments frame)
  (let* ((prog (frame-program frame))
	 (arity (program-arity prog)))
    (do ((n (+ (arity:nargs arity) -1) (1- n))
	 (l '() (cons (frame-local-ref frame n) l)))
	((< n 0) l))))

(define (frame-local-variables frame)
  (let* ((prog (frame-program frame))
	 (arity (program-arity prog)))
    (do ((n (+ (arity:nargs arity) (arity:nlocs arity) -1) (1- n))
	 (l '() (cons (frame-local-ref frame n) l)))
	((< n 0) l))))

(define (frame-external-variables frame)
  (frame-external-link frame))

(define (frame-external-ref frame index)
  (list-ref (frame-external-link frame) index))

(define (frame-external-set! frame index val)
  (list-set! (frame-external-link frame) index val))

(define (frame-binding-ref frame binding)
  (if (binding:extp binding)
    (frame-external-ref frame (binding:index binding))
    (frame-local-ref frame (binding:index binding))))

(define (frame-binding-set! frame binding val)
  (if (binding:extp binding)
    (frame-external-set! frame (binding:index binding) val)
    (frame-local-set! frame (binding:index binding) val)))

(define (frame-bindings frame addr)
  (do ((bs (program-bindings (frame-program frame)) (cdr bs))
       (ls '() (if (cdar bs) (cons (cdar bs) ls) (cdr ls))))
      ((or (null? bs) (> (caar bs) addr))
       (apply append ls))))

(define (frame-lookup-binding frame addr sym)
  (do ((bs (frame-bindings frame addr) (cdr bs)))
      ((or (null? bs) (eq? sym (binding:name (car bs))))
       (and (pair? bs) (car bs)))))

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
