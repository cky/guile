;;; Guile VM code converters

;; Copyright (C) 2001 Free Software Foundation, Inc.

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

(define-module (language assembly disassemble)
  #:use-module (ice-9 format)
  #:use-module (system vm instruction)
  #:use-module (system vm program)
  #:use-module (system base pmatch)
  #:use-module (language assembly)
  #:use-module (system base compile)
  #:export (disassemble))

(define (disassemble x)
  (format #t "Disassembly of ~A:\n\n" x)
  (call-with-values
      (lambda () (decompile x #:from 'value #:to 'assembly))
    disassemble-load-program))

(define (disassemble-load-program asm env)
  (pmatch asm
    ((load-program ,nargs ,nrest ,nlocs ,nexts ,labels ,len ,meta . ,code)
     (let ((objs  (and env (assq-ref env 'objects)))
           (meta  (and env (assq-ref env 'meta)))
           (exts  (and env (assq-ref env 'exts)))
           (blocs (and env (assq-ref env 'blocs)))
           (bexts (and env (assq-ref env 'bexts)))
           (srcs  (and env (assq-ref env 'sources))))
       (let lp ((pos 0) (code code) (programs '()))
         (cond
          ((null? code)
           (newline)
           (for-each
            (lambda (sym+asm)
              (format #t "Embedded program ~A:\n\n" (car sym+asm))
              (disassemble-load-program (cdr sym+asm) '()))
            (reverse! programs)))
          (else
           (let* ((asm (car code))
                  (len (byte-length asm))
                  (end (+ pos len)))
             (pmatch asm
               ((load-program . _)
                (let ((sym (gensym "")))
                  (print-info pos `(load-program ,sym) #f #f)
                  (lp (+ pos (byte-length asm)) (cdr code)
                      (acons sym asm programs))))
               (else
                (print-info pos asm
                            (code-annotation end asm objs nargs blocs bexts
                                             labels)
                            (and=> (and srcs (assq end srcs)) source->string))
                (lp (+ pos (byte-length asm)) (cdr code) programs)))))))
                 
       (if (pair? exts)
           (disassemble-externals exts))
       (if meta
           (disassemble-meta meta))

       ;; Disassemble other bytecode in it
       ;; FIXME: something about the module.
       (if objs
           (for-each
            (lambda (x)
              (if (program? x)
                  (begin (display "----------------------------------------\n")
                         (disassemble x))))
            (cdr (vector->list objs))))))
    (else
     (error "bad load-program form" asm))))

(define (disassemble-objects objs)
  (display "Objects:\n\n")
  (let ((len (vector-length objs)))
    (do ((n 0 (1+ n)))
	((= n len) (newline))
      (print-info n (vector-ref objs n) #f #f))))

(define (disassemble-externals exts)
  (display "Externals:\n\n")
  (let ((len (length exts)))
    (do ((n 0 (1+ n))
	 (l exts (cdr l)))
	((null? l) (newline))
      (print-info n (car l) #f #f))))

(define-macro (unless test . body)
  `(if (not ,test) (begin ,@body)))

(define *uninteresting-props* '(name))

(define (disassemble-meta meta)
  (let ((sources (cadr meta))
        (props (filter (lambda (x)
                         (not (memq (car x) *uninteresting-props*)))
                       (cddr meta))))
    (unless (null? props)
      (display "Properties:\n\n")
      (for-each (lambda (x) (print-info #f x #f #f)) props)
      (newline))))

(define (source->string src)
  (format #f "~a:~a:~a" (or (source:file src) "(unknown file)")
          (source:line src) (source:column src)))

(define (make-int16 byte1 byte2)
  (+ (* byte1 256) byte2))

(define (code-annotation end-addr code objs nargs blocs bexts labels)
  (let* ((code (assembly-unpack code))
         (inst (car code))
         (args (cdr code)))
    (case inst
      ((list vector) 
       (list "~a element~:p" (apply make-int16 args)))
      ((br br-if br-if-eq br-if-not br-if-not-eq br-if-not-null br-if-null)
       (list "-> ~A" (assq-ref labels (car args))))
      ((object-ref)
       (and objs (list "~s" (vector-ref objs (car args)))))
      ((local-ref local-set)
       (and blocs
            (let lp ((bindings (list-ref blocs (car args))))
              (and (pair? bindings)
                   (let ((b (car bindings)))
                     (if (and (< (binding:start (car bindings)) end-addr)
                              (>= (binding:end (car bindings)) end-addr))
                         (list "`~a'~@[ (arg)~]"
                               (binding:name b) (< (binding:index b) nargs))
                         (lp (cdr bindings))))))))
      ((external-ref external-set)
       (and bexts
            (if (< (car args) (length bexts))
                (let ((b (list-ref bexts (car args))))
                  (list "`~a'~@[ (arg)~]"
                        (binding:name b) (< (binding:index b) nargs)))
                (list "(closure variable)"))))
      ((toplevel-ref toplevel-set)
       (and objs
            (let ((v (vector-ref objs (car args))))
              (if (and (variable? v) (variable-bound? v))
                  (list "~s" (variable-ref v))
                  (list "`~s'" v)))))
      ((mv-call)
       (list "MV -> ~A" (assq-ref labels (cadr args))))
      (else
       (and=> (assembly->object code)
              (lambda (obj) (list "~s" obj)))))))

;; i am format's daddy.
(define (print-info addr info extra src)
  (format #t "~4@S    ~32S~@[;; ~1{~@?~}~]~@[~61t at ~a~]\n" addr info extra src))

(define (simplify x)
  (cond ((string? x)
	 (cond ((string-index x #\newline) =>
		(lambda (i) (set! x (substring x 0 i)))))
	 (cond ((> (string-length x) 16)
		(set! x (string-append (substring x 0 13) "..."))))))
  x)

