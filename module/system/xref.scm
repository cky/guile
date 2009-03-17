;;;; 	Copyright (C) 2009 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;; 


(define-module (system xref)
  #:use-module (system base pmatch)
  #:use-module (system base compile)
  #:use-module (system vm program)
  #:export (procedure-callees))

(define (program-callees prog)
  (cond
   ((program-objects prog)
    => (lambda (objects)
          (let ((n (vector-length objects))
                (progv (make-vector (vector-length objects) #f))
                (asm (decompile (program-objcode prog) #:to 'assembly)))
            (pmatch asm
              ((load-program ,nargs ,nrest ,nlocs ,next ,labels ,len . ,body)
               (for-each
                (lambda (x)
                  (pmatch x
                    ((toplevel-ref ,n) (vector-set! progv n #t))
                    ((toplevel-set ,n) (vector-set! progv n #t))))
                body)))
            (let lp ((i 0) (out '()))
              (cond
               ((= i n) (reverse out))
               ((program? (vector-ref objects i))
                (lp (1+ i) (append (reverse (program-callees
                                             (vector-ref objects i)))
                                   out)))
               ((vector-ref progv i)
                (let ((obj (vector-ref objects i)))
                  (if (variable? obj)
                      (lp (1+ i) (cons (variable-ref obj) out))
                      ;; otherwise it's an unmemoized binding
                      (pmatch obj
                        (,sym (guard (symbol? sym))
                         (let ((v (module-variable (program-module prog) sym)))
                           (lp (1+ i)
                               (if v (cons (variable-ref v) out) out))))
                        ((,mod ,sym ,public?)
                         ;; hm, hacky.
                         (let ((m (nested-ref the-root-module
                                              (append '(%app modules) mod))))
                           (let ((v (and m (module-variable
                                            (if public? (module-interface m) m)
                                            sym))))
                             (lp (1+ i)
                                 (if v (cons (variable-ref v) out) out)))))))))
              (else (lp (1+ i) out)))))))
   (else '())))

(define (hacky-procedure-callees proc)
  ;; we could analyze the memoized source or something
  '())

(define (procedure-callees proc)
  (cond
   ((program? proc) (program-callees proc))
   ((procedure-source proc) (hacky-procedure-callees proc))
   (else '())))
