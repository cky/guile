;;; Guile VM code converters

;; Copyright (C) 2001, 2009, 2010 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language glil decompile-assembly)
  #:use-module (system base pmatch)
  #:use-module (system vm program)
  #:use-module (language assembly)
  #:use-module (language glil)
  #:export (decompile-assembly))

(define (decompile-assembly x env opts)
  (values (decompile-toplevel x)
          env))

(define (decompile-toplevel x)
  (pmatch x
    ((load-program ,labels ,len ,meta . ,body)
     (decompile-load-program (decompile-meta meta)
                             body labels #f))
    (else
     (error "invalid assembly" x))))

(define (decompile-meta meta)
  (and meta
      (let ((prog (decompile-toplevel meta)))
        (if (and (glil-program? prog)
                 (= (length (glil-program-body prog)) 2)
                 (glil-const? (car (glil-program-body prog))))
            (glil-const-obj (car (glil-program-body prog)))
            (error "metadata not a thunk returning a const" prog)))))

(define *placeholder* (list 'placeholder))

(define (emit-constants l out)
  (let lp ((in (reverse l)) (out out))
    (cond ((null? in) out)
          ((eq? (car in) *placeholder*) (lp (cdr in) out))
          ((glil-program? (car in)) (lp (cdr in) (cons (car in) out)))
          (else (lp (cdr in) (cons (make-glil-const (car l)) out))))))

(define (decompile-load-program meta body labels
                                objects)
  (let ((glil-labels (sort (map (lambda (x)
                                  (cons (cdr x) (make-glil-label (car x))))
                                labels)
                           (lambda (x y) (< (car x) (car y)))))
        (bindings (sort (if meta (car meta) '())
                        (lambda (x y) (< (binding:start x) (binding:start y)))))
        (unbindings (sort (if meta (car meta) '())
                          (lambda (x y) (< (binding:end x) (binding:end y)))))
        (sources (if meta (cadr meta) '()))
        (filename #f)
        (props (if meta (cddr meta) '())))
    (define (pop-bindings! addr)
      (let lp ((in bindings) (out '()))
        (if (or (null? in) (> (binding:start (car in)) addr))
            (begin
              (set! bindings in)
              (if (null? out) #f (reverse out)))
            (lp (cdr in) (cons (car in) out)))))
    (define (pop-unbindings! addr)
      (let lp ((in unbindings) (out '()))
        (if (or (null? in) (> (binding:end (car in)) addr))
            (begin
              (set! unbindings in)
              (if (null? out) #f (reverse out)))
            (lp (cdr in) (cons (car in) out)))))
    (define (pop-source! addr)
      ;; a fragile algorithm.
      (cond ((null? sources) #f)
            ((eq? (caar sources) 'filename)
             (set! filename (cdar sources))
             (pop-source! addr))
            ((eqv? (caar sources) addr)
             (let ((x (car sources)))
               (set! sources (cdr sources))
               `((filename . ,filename)
                 (line . ,(cadr x))
                 (column . ,(cddr x)))))
            (else #f)))
    (let lp ((in body) (stack '()) (out '()) (pos 0))
      (cond
       ((null? in)
        (or (null? stack) (error "leftover stack insts" stack body))
        (make-glil-program props (reverse out)))
       ((pop-bindings! pos)
        => (lambda (bindings)
             (lp in stack
                 (cons (make-glil-bind bindings)
                       out)
                 pos)))
       ((pop-unbindings! pos)
        => (lambda (bindings)
             (lp in stack (cons (make-glil-unbind) out) pos)))
       ((pop-source! pos)
        => (lambda (s)
             (lp in stack (cons (make-glil-source s) out) pos)))
       ((and (or (null? out) (not (glil-label? (car out))))
             (assv-ref glil-labels pos))
        => (lambda (label)
             (lp in stack (cons label out) pos)))
       (else
        (pmatch (car in)
          ((nop)
           (lp (cdr in) stack out (1+ pos)))
          ((make-false)
           (lp (cdr in) (cons #f stack) out (1+ pos)))
          ((make-nil)
           (lp (cdr in) (cons #nil stack) out (1+ pos)))
          ((load-program ,labels ,sublen ,meta . ,body)
           (lp (cdr in)
               (cons (decompile-load-program (decompile-meta meta)
                                             body labels (car stack))
                     (cdr stack))
               out
               (+ pos (byte-length (car in)))))
          ((load-symbol ,str)
           (lp (cdr in) (cons (string->symbol str) stack) out
               (+ pos 1 (string-length str))))
          ((make-int8:0)
           (lp (cdr in) (cons 0 stack) out (1+ pos)))
          ((make-int8:1)
           (lp (cdr in) (cons 1 stack) out (1+ pos)))
          ((make-int8 ,n)
           (lp (cdr in) (cons n stack) out (+ pos 2)))
          ((cons)
           (let ((head (list-head stack 2))
                 (stack (list-tail stack 2)))
             (if (memq *placeholder* head)
                 (lp (cdr in) (cons *placeholder* stack)
                     (cons (make-glil-call 'cons 2) (emit-constants head out))
                     (+ pos 1))
                 (lp (cdr in) (cons (cons (cadr head) (car head)) stack)
                     out (+ pos 3)))))
          ((list ,a ,b)
           (let* ((len (+ (ash a 8) b))
                  (head (list-head stack len))
                  (stack (list-tail stack len)))
             (if (memq *placeholder* head)
                 (lp (cdr in) (cons *placeholder* stack)
                     (cons (make-glil-call 'list len) (emit-constants head out))
                     (+ pos 3))
                 (lp (cdr in) (cons (reverse head) stack) out (+ pos 3)))))
          ((make-eol)
           (lp (cdr in) (cons '() stack) out (1+ pos)))
          ((return)
           (lp (cdr in) (cdr stack)
               (cons (make-glil-call 'return 1)
                     (emit-constants (list-head stack 1) out))
               (1+ pos)))
          ((local-ref ,n)
           (lp (cdr in) (cons *placeholder* stack)
               (cons (make-glil-local 'ref n)
                     out) (+ pos 2)))
          ((local-set ,n)
           (lp (cdr in) (cdr stack)
               (cons (make-glil-local 'set n)
                     (emit-constants (list-head stack 1) out))
               (+ pos 2)))
          ((br-if-not ,l)
           (lp (cdr in) (cdr stack)
               (cons (make-glil-branch 'br-if-not l) out)
               (+ pos 3)))
          ((mul)
           (lp (cdr in) (cons *placeholder* (cddr stack))
               (cons (make-glil-call 'mul 2)
                     (emit-constants (list-head stack 2) out))
               (+ pos 1)))
          ((tail-call ,n)
           (lp (cdr in) (list-tail stack (1+ n))
               (cons (make-glil-call 'tail-call n)
                     (emit-constants (list-head stack (1+ n)) out))
               (+ pos 2)))
          (else (error "unsupported decompilation" (car in)))))))))
