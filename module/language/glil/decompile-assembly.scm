;;; Guile VM code converters

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

(define-module (language glil decompile-assembly)
  #:use-module (system base pmatch)
  #:use-module (language assembly)
  #:use-module (language glil)
  #:export (decompile-assembly))

(define (decompile-assembly x env opts)
  (values (decompile-toplevel x)
          env))

(define (decompile-toplevel x)
  (pmatch x
    ((load-program ,nargs ,nrest ,nlocs ,nexts ,labels ,len ,meta . ,body)
     (decompile-load-program nargs nrest nlocs nexts
                             (decompile-meta meta)
                             body labels))
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
          (else (lp (cdr in) (cons (make-glil-const (car l)) out))))))

(define (decompile-load-program nargs nrest nlocs nexts meta body labels)
  (let ((glil-labels (sort (map (lambda (x)
                                  (cons (cdr x) (make-glil-label (car x))))
                                labels)
                           (lambda (x y) (< (car x) (car y)))))
        (bindings (if meta (car meta) '()))
        (sources (if meta (cadr meta) '()))
        (props (if meta (cddr meta) '())))
    (let lp ((in body) (stack '()) (out '()) (pos 0))
      (cond
       ((and (or (null? out) (not (glil-label? (car out))))
             (assv-ref glil-labels pos))
        => (lambda (label)
             (lp in stack (cons label out) pos)))
       ((null? in)
        (or (null? stack) (error "leftover stack insts" stack body))
        (make-glil-program nargs nrest nlocs nexts props (reverse out) #f))
       (else
        (pmatch (car in)
          ((nop)
           (lp (cdr in) stack out (1+ pos)))
          ((make-false)
           (lp (cdr in) (cons #f stack) out (1+ pos)))
          ((load-program ,a ,b ,c ,d ,labels ,sublen ,meta . ,body)
           (lp (cdr in) (cons *placeholder* (cdr stack))
               (cons (decompile-load-program a b c d (decompile-meta meta)
                                             body labels)
                     (emit-constants (list-head stack 1) out))
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
               (cons (if (< n nargs)
                         (make-glil-argument 'ref n)
                         (make-glil-local 'ref (- n nargs)))
                     out) (+ pos 2)))
          ((local-set ,n)
           (lp (cdr in) (cdr stack)
               (cons (if (< n nargs)
                         (make-glil-argument 'set n)
                         (make-glil-local 'set (- n nargs)))
                     (emit-constants (list-head stack 1) out))
               (+ pos 2)))
          ((br-if-not ,l)
           (lp (cdr in) (cdr stack)
               (cons (make-glil-branch
                      'br-if-not
                      (assv-ref glil-labels (assq-ref labels l)))
                     out)
               (+ pos 3)))
          ((mul)
           (lp (cdr in) (cons *placeholder* (cddr stack))
               (cons (make-glil-call 'mul 2)
                     (emit-constants (list-head stack 2) out))
               (+ pos 1)))
          ((goto/args ,n)
           (lp (cdr in) (list-tail stack (1+ n))
               (cons (make-glil-call 'goto/args n)
                     (emit-constants (list-head stack (1+ n)) out))
               (+ pos 2)))
          (else (error "unsupported decompilation" (car in)))))))))
